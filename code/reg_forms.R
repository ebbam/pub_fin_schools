run_model <- function(mods, df){
  res <- list()
  for(k in mods){
    res <- append(res, list(feols(as.formula(k), data = df, se = "cluster", cluster = "unit", panel.id = ~unit+year)))
  }
  return(res)
}

library(fixest)
library(dplyr)

# Manual 2SLS with AR(1) in second stage only
# 
# @param outcome_formula Formula for second stage outcome (LHS and controls)
# @param endog_var String name of endogenous variable (e.g., "log_weighted_annual_avg_wkly_wage")
# @param instrument_formula Formula for first stage instruments (e.g., "gdp_ss_2d + l1_gdp_ss_2d + l2_gdp_ss_2d")
# @param lag_endog String name of lagged endogenous variable for first stage (e.g., "l1_log_weighted_annual_avg_wkly_wage")
# @param lag_outcome String name of lagged outcome variable for second stage (e.g., "l1_log_real_Elem_Educ_Total_Exp_pp")
# @param data Dataframe
# @param panel.id Vector of panel ID variables
# @param cluster String name of clustering variable
# @param fixed_effects String for fixed effects (e.g., "unit + year")
# @return List containing first_stage, second_stage, and combined (for etable)
library(parallel)
library(future.apply)

manual_iv_ar <- function(outcome_formula,
                         endog_var,
                         instrument_formula,
                         lag_endog = NULL,
                         lag_outcome = NULL,
                         data,
                         panel.id = c("unit", "year"),
                         cluster = "unit",
                         fixed_effects = "unit + year",
                         keep_data = FALSE,
                         bootstrap_se = FALSE,
                         n_boot = 399,  # 399 is often sufficient
                         parallel = TRUE,
                         verbose = TRUE) {
  
  # Parse the outcome formula to extract components
  outcome_parts <- strsplit(outcome_formula, "~")[[1]]
  outcome_var <- trimws(outcome_parts[1])
  controls <- trimws(outcome_parts[2])
  
  # Build first stage formula
  fs_formula_parts <- c(instrument_formula, controls)
  if (!is.null(lag_endog)) {
    fs_formula_parts <- c(fs_formula_parts, lag_endog)
  }
  
  fs_formula_str <- paste0(
    endog_var, " ~ ",
    paste(fs_formula_parts, collapse = " + "),
    " | ", fixed_effects
  )
  
  # Clean data to ensure complete cases
  all_vars <- c(
    outcome_var, endog_var,
    unlist(strsplit(controls, "\\s*\\+\\s*")),
    unlist(strsplit(instrument_formula, "\\s*\\+\\s*"))
  )
  
  if (!is.null(lag_endog)) all_vars <- c(all_vars, lag_endog)
  if (!is.null(lag_outcome)) all_vars <- c(all_vars, lag_outcome)
  all_vars <- c(all_vars, panel.id, cluster)
  all_vars <- unique(trimws(all_vars))
  
  # Create clean dataset
  data_clean <- data %>%
    select(all_of(all_vars)) %>%
    na.omit()
  
  n_obs <- nrow(data_clean)
  n_removed <- nrow(data) - n_obs
  
  # Run first stage
  fs <- feols(
    as.formula(fs_formula_str),
    data = data_clean,
    panel.id = panel.id
  )
  
  # Calculate F-statistics (keep existing code)
  coef_names <- names(coef(fs))
  instrument_vars <- trimws(unlist(strsplit(instrument_formula, "\\s*\\+\\s*")))
  instruments_in_model <- intersect(instrument_vars, coef_names)
  
  if (length(instruments_in_model) > 0) {
    # CONDITIONAL F-STAT
    fstat_conditional <- tryCatch({
      controls_only <- trimws(unlist(strsplit(controls, "\\s*\\+\\s*")))
      fs_restricted_vars <- controls_only
      if (!is.null(lag_endog)) {
        fs_restricted_vars <- c(fs_restricted_vars, lag_endog)
      }
      
      fs_restricted_formula <- paste0(
        endog_var, " ~ ",
        paste(fs_restricted_vars, collapse = " + "),
        " | ", fixed_effects
      )
      
      fs_restricted <- feols(
        as.formula(fs_restricted_formula),
        data = data_clean,
        panel.id = panel.id
      )
      
      rss_restricted <- sum(residuals(fs_restricted)^2)
      rss_unrestricted <- sum(residuals(fs)^2)
      
      q <- length(instruments_in_model)
      k_unrestricted <- length(coef(fs))
      df_num <- q
      df_denom <- fs$nobs - k_unrestricted - 
        sum(fs$fixef_sizes[names(fs$fixef_sizes)], na.rm = TRUE)
      
      fstat <- ((rss_restricted - rss_unrestricted) / q) / 
        (rss_unrestricted / df_denom)
      p_val <- pf(fstat, df_num, df_denom, lower.tail = FALSE)
      
      list(stat = fstat, p = p_val, df_num = df_num, df_denom = df_denom)
    }, error = function(e2) {
      warning("Could not compute conditional F-statistic: ", e2$message)
      list(stat = NA, p = NA, df_num = NA, df_denom = NA)
    })
    
    # MARGINAL F-STAT
    fstat_marginal <- if (!is.null(lag_endog)) {
      tryCatch({
        controls_only <- trimws(unlist(strsplit(controls, "\\s*\\+\\s*")))
        
        fs_restricted_nolag_formula <- paste0(
          endog_var, " ~ ",
          paste(controls_only, collapse = " + "),
          " | ", fixed_effects
        )
        
        fs_restricted_nolag <- feols(
          as.formula(fs_restricted_nolag_formula),
          data = data_clean,
          panel.id = panel.id
        )
        
        fs_unrestricted_nolag_formula <- paste0(
          endog_var, " ~ ",
          paste(c(instrument_vars, controls_only), collapse = " + "),
          " | ", fixed_effects
        )
        
        fs_unrestricted_nolag <- feols(
          as.formula(fs_unrestricted_nolag_formula),
          data = data_clean,
          panel.id = panel.id
        )
        
        rss_restricted <- sum(residuals(fs_restricted_nolag)^2)
        rss_unrestricted <- sum(residuals(fs_unrestricted_nolag)^2)
        
        q <- length(instruments_in_model)
        k_unrestricted <- length(coef(fs_unrestricted_nolag))
        df_num <- q
        df_denom <- fs_unrestricted_nolag$nobs - k_unrestricted - 
          sum(fs_unrestricted_nolag$fixef_sizes[names(fs_unrestricted_nolag$fixef_sizes)], na.rm = TRUE)
        
        fstat <- ((rss_restricted - rss_unrestricted) / q) / 
          (rss_unrestricted / df_denom)
        p_val <- pf(fstat, df_num, df_denom, lower.tail = FALSE)
        
        list(stat = fstat, p = p_val, df_num = df_num, df_denom = df_denom)
      }, error = function(e2) {
        warning("Could not compute marginal F-statistic: ", e2$message)
        list(stat = NA, p = NA, df_num = NA, df_denom = NA)
      })
    } else {
      fstat_conditional
    }
  } else {
    warning("No instruments found in first stage model")
    fstat_conditional <- list(stat = NA, p = NA, df_num = NA, df_denom = NA)
    fstat_marginal <- list(stat = NA, p = NA, df_num = NA, df_denom = NA)
  }
  
  fs_fstat_cond <- fstat_conditional$stat
  fs_fstat_cond_p <- fstat_conditional$p
  fs_fstat_marg <- fstat_marginal$stat
  fs_fstat_marg_p <- fstat_marginal$p
  
  # Store first stage residuals for Wu-Hausman test
  fs_residuals <- residuals(fs)
  
  # Get fitted values
  data_clean$fitted_endog <- fitted(fs)
  data_clean$fs_resid <- fs_residuals
  
  # Build second stage formula using the fitted values
  ss_formula_parts <- c("fitted_endog", controls)
  if (!is.null(lag_outcome)) {
    ss_formula_parts <- c(lag_outcome, ss_formula_parts)
  }
  
  ss_formula_str <- paste0(
    outcome_var, " ~ ",
    paste(ss_formula_parts, collapse = " + "),
    " | ", fixed_effects
  )
  
  # Run second stage
  ss <- feols(
    as.formula(ss_formula_str),
    data = data_clean,
    panel.id = panel.id,
    cluster = cluster
  )
  
  # Rename fitted_endog to original variable name
  coef_names_ss <- names(coef(ss))
  coef_names_ss[coef_names_ss == "fitted_endog"] <- endog_var
  names(ss$coefficients) <- coef_names_ss
  
  # Wu-Hausman test
  wu_hausman_result <- tryCatch({
    ss_formula_wh <- paste0(
      outcome_var, " ~ fitted_endog + fs_resid + ",
      paste(c(lag_outcome, controls), collapse = " + "),
      " | ", fixed_effects
    )
    
    ss_wh <- feols(
      as.formula(ss_formula_wh),
      data = data_clean,
      panel.id = panel.id,
      cluster = cluster
    )
    
    wh_coef <- coef(ss_wh)["fs_resid"]
    wh_se <- summary(ss_wh)$coeftable["fs_resid", "Std. Error"]
    wh_stat <- (wh_coef / wh_se)^2
    wh_p <- pchisq(wh_stat, df = 1, lower.tail = FALSE)
    
    list(stat = wh_stat, p = wh_p)
  }, error = function(e) {
    warning("Could not compute Wu-Hausman test: ", e$message)
    list(stat = NA, p = NA)
  })
  
  wu_hausman <- wu_hausman_result$stat
  wu_hausman_p <- wu_hausman_result$p
  
  # Sargan overidentification test
  n_instruments <- length(instruments_in_model)
  n_endog <- 1
  
  if (n_instruments > n_endog) {
    sargan_result <- tryCatch({
      ss_resid <- residuals(ss)
      data_clean$ss_resid <- ss_resid
      
      all_exog <- unique(c(instruments_in_model, 
                           trimws(unlist(strsplit(controls, "\\s*\\+\\s*")))))
      if (!is.null(lag_endog)) all_exog <- c(all_exog, lag_endog)
      if (!is.null(lag_outcome)) all_exog <- c(all_exog, lag_outcome)
      
      sargan_formula <- paste0(
        "ss_resid ~ ",
        paste(all_exog, collapse = " + "),
        " | ", fixed_effects
      )
      
      sargan_reg <- feols(
        as.formula(sargan_formula),
        data = data_clean,
        panel.id = panel.id
      )
      
      sargan_stat <- n_obs * r2(sargan_reg, type = "r2")
      df_sargan <- n_instruments - n_endog
      sargan_p <- pchisq(sargan_stat, df = df_sargan, lower.tail = FALSE)
      
      list(stat = sargan_stat, p = sargan_p)
    }, error = function(e) {
      warning("Could not compute Sargan test: ", e$message)
      list(stat = NA, p = NA)
    })
  } else {
    sargan_result <- list(stat = NA, p = NA)
  }
  
  sargan <- sargan_result$stat
  sargan_p <- sargan_result$p
  
  # =====================================================================
  # BOOTSTRAP STANDARD ERRORS
  # =====================================================================
  
  if (bootstrap_se) {
    if (verbose) cat("\nComputing bootstrap standard errors (", n_boot, " replications)...\n")
    
    # Get unique clusters
    cluster_var <- cluster
    unique_clusters <- unique(data_clean[[cluster_var]])
    n_clusters <- length(unique_clusters)
    
    # Bootstrap function
    boot_once <- function(b) {
      tryCatch({
        # Resample clusters
        sampled_clusters <- sample(unique_clusters, size = n_clusters, replace = TRUE)
        
        # Reconstruct data
        boot_data <- do.call(rbind, lapply(sampled_clusters, function(cl) {
          data_clean[data_clean[[cluster_var]] == cl, ]
        }))
        
        # First stage
        fs_boot <- feols(
          as.formula(fs_formula_str),
          data = boot_data,
          panel.id = panel.id
        )
        
        # Get fitted values
        boot_data$fitted_endog <- fitted(fs_boot)
        
        # Second stage
        ss_boot <- feols(
          as.formula(ss_formula_str),
          data = boot_data,
          panel.id = panel.id,
          cluster = cluster_var
        )
        
        # Return coefficients
        coef(ss_boot)
      }, error = function(e) {
        rep(NA_real_, length(coef(ss)))
      })
    }
    
    # Run bootstrap in parallel or serial
    if (parallel && n_boot > 50) {
      # Use parallel processing
      n_cores <- min(parallel::detectCores() - 1, 8)  # Max 8 cores
      if (verbose) cat("Using", n_cores, "cores for parallel bootstrap...\n")
      
      boot_start <- Sys.time()
      boot_results <- parallel::mclapply(
        1:n_boot,
        boot_once,
        mc.cores = n_cores
      )
      boot_time <- as.numeric(difftime(Sys.time(), boot_start, units = "secs"))
      
      if (verbose) cat("Bootstrap completed in", round(boot_time, 1), "seconds\n")
    } else {
      # Serial processing with progress
      boot_start <- Sys.time()
      boot_results <- lapply(1:n_boot, function(b) {
        if (verbose && b %% 50 == 0) cat("  Replication", b, "/", n_boot, "\n")
        boot_once(b)
      })
      boot_time <- as.numeric(difftime(Sys.time(), boot_start, units = "secs"))
      
      if (verbose) cat("Bootstrap completed in", round(boot_time, 1), "seconds\n")
    }
    
    # Convert to matrix
    boot_matrix <- do.call(rbind, boot_results)
    
    # Calculate bootstrap standard errors
    boot_se <- apply(boot_matrix, 2, sd, na.rm = TRUE)
    
    # Calculate bootstrap confidence intervals (percentile method)
    boot_ci_lower <- apply(boot_matrix, 2, quantile, probs = 0.025, na.rm = TRUE)
    boot_ci_upper <- apply(boot_matrix, 2, quantile, probs = 0.975, na.rm = TRUE)
    
    # Store in model object
    ss$boot_se <- boot_se
    ss$boot_ci_lower <- boot_ci_lower
    ss$boot_ci_upper <- boot_ci_upper
    ss$boot_n_reps <- n_boot
    ss$boot_n_valid <- sum(complete.cases(boot_matrix))
    ss$se_method <- "cluster_bootstrap"
    
    # Print comparison
    # Print comparison
    if (verbose) {
      cat("\n=== Standard Error Comparison ===\n")
      comparison <- data.frame(
        Variable = names(coef(ss)),
        Naive_SE = summary(ss)$coeftable[, "Std. Error"],
        Bootstrap_SE = boot_se,
        Ratio = boot_se / summary(ss)$coeftable[, "Std. Error"],
        stringsAsFactors = FALSE
      )
      
      # Round numeric columns only
      comparison[, c("Naive_SE", "Bootstrap_SE", "Ratio")] <- 
        round(comparison[, c("Naive_SE", "Bootstrap_SE", "Ratio")], 4)
      
      print(comparison)
      cat("\nNote: Ratio > 1 indicates naive SEs understate uncertainty\n")
    }
  } else {
    ss$se_method <- "naive_2sls"
    if (verbose) {
      cat("\nUsing naive 2SLS standard errors (do not account for first-stage uncertainty)\n")
      cat("Set bootstrap_se = TRUE for corrected standard errors\n")
    }
  }
  
  # Store all diagnostics
  ss$iv_first_stage <- fs
  ss$iv_fstat <- fs_fstat_cond
  ss$iv_fstat_p <- fs_fstat_cond_p
  ss$iv_fstat_marginal <- fs_fstat_marg
  ss$iv_fstat_marginal_p <- fs_fstat_marg_p
  ss$wu_hausman <- wu_hausman
  ss$wu_hausman_p <- wu_hausman_p
  ss$sargan <- sargan
  ss$sargan_p <- sargan_p
  ss$r2_firststage <- r2(fs, type = "r2")
  ss$ar2_firststage <- r2(fs, type = "ar2")
  
  # Clean up
  rm(data_clean)
  gc()
  
  # Return structure
  result <- list(
    first_stage = fs,
    second_stage = ss,
    n_obs = n_obs,
    n_removed = n_removed
  )
  
  return(result)
}

# Update wrapper to pass through bootstrap options
run_manual_iv <- function(iv_formula, ss_formula, data, 
                          panel.id = c("unit", "year"), 
                          cluster = "unit",
                          include_lag_outcome = TRUE,
                          include_lag_endog = TRUE,
                          keep_data = FALSE,
                          bootstrap_se = FALSE,
                          n_boot = 399,
                          parallel = TRUE,
                          verbose = TRUE) {
  
  # Parse the IV formula
  parts <- strsplit(iv_formula, "\\|")[[1]]
  outcome_controls <- trimws(parts[1])
  fixed_effects <- trimws(parts[2])
  endog_instruments <- trimws(parts[3])
  
  # Parse second stage (outcome ~ controls)
  outcome_parts <- strsplit(outcome_controls, "~")[[1]]
  outcome_var <- trimws(outcome_parts[1])
  controls <- trimws(outcome_parts[2])
  
  # Parse endogenous variable and instruments from ss_formula
  ss_parts <- strsplit(ss_formula, "~")[[1]]
  endog_var <- trimws(ss_parts[1])
  instrument_formula <- trimws(ss_parts[2])
  
  # Determine lag variable names based on variable names
  lag_outcome <- NULL
  lag_endog <- NULL
  
  if (include_lag_outcome) {
    if (grepl("^diff_", outcome_var) || grepl("^gr_", outcome_var)) {
      lag_outcome <- NULL
    } else {
      lag_outcome <- paste0("l1_", outcome_var)
    }
  }
  
  if (include_lag_endog) {
    if (grepl("^diff_", endog_var) || grepl("^gr_", endog_var)) {
      lag_endog <- NULL
    } else {
      lag_endog <- paste0("l1_", endog_var)
    }
  }
  
  # Call the main function
  result <- manual_iv_ar(
    outcome_formula = outcome_controls,
    endog_var = endog_var,
    instrument_formula = instrument_formula,
    lag_endog = lag_endog,
    lag_outcome = lag_outcome,
    data = data,
    panel.id = panel.id,
    cluster = cluster,
    fixed_effects = fixed_effects,
    keep_data = keep_data,
    bootstrap_se = bootstrap_se,
    n_boot = n_boot,
    parallel = parallel,
    verbose = verbose
  )
  
  return(result)
}


run_model_fips <- function(mods, df){
  res <- list()
  for(k in mods){
    res <- append(res, list(feols(as.formula(k), data = df, se = "cluster", cluster = "fips", panel.id = ~fips+year)))
  }
  return(res)
}

# Property Tax ~ gdp

prop_taxes = c(log_real_Property_Tax ~ log_real_gdp_total + sw0(l(log_real_gdp_total, 1:4)) | unit + year, 
               log_real_Property_Tax_pp ~ log_real_gdp_total_pc + sw0(l(log_real_gdp_total_pc, 1:4)) | unit + year)


# Property Tax ~ gdp
educ_source = c(log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_Total_Rev_Own_Sources_pp + log_real_Total_IG_Revenue_pp,
                                                                      log_real_Property_Tax_pp,
                                                                      log_real_Property_Tax_pp + log_real_Total_IG_Revenue_pp,
                                                                      log_real_Property_Tax_pp + log_real_Total_Fed_IG_Revenue_pp + log_real_Total_State_IG_Revenue_pp) | unit + year,
                log_real_Elem_Educ_Total_Exp ~ sw(log_real_Property_Tax,
                                                                      log_real_Property_Tax + log_real_Total_IG_Revenue,
                                                                      log_real_Property_Tax + log_real_Total_Fed_IG_Revenue + log_real_Total_State_IG_Revenue,
                                                                      log_real_Total_Rev_Own_Sources + log_real_Total_IG_Revenue) | unit + year)



educ_gdp = c(log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_total_pc,
                                             log_real_gdp_priv_ind_pc, 
                                             log_real_gdp_o_g_mining_quarr_21_pc) | unit + year)

educ_gdp_statetrends = c(log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_total_pc,
                                       log_real_gdp_total_pc + l(log_real_Elem_Educ_Total_Exp_pp, 1),
                                       log_real_gdp_priv_ind_pc, 
                                       log_real_gdp_priv_ind_pc + l(log_real_Elem_Educ_Total_Exp_pp, 1)) + time:as.factor(state) | unit + year)

# W. state time trends
educ_gdp_lags = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc + sw0(time:as.factor(state)) | unit + year, 
                  log_real_Elem_Educ_Total_Exp_pp ~  log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + sw0(time:as.factor(state)) | unit + year)
                  #log_real_Elem_Educ_Total_Exp_pp ~  log_real_gdp_o_g_mining_quarr_21_pc + l1_log_real_gdp_o_g_mining_quarr_21_pc + l2_log_real_gdp_o_g_mining_quarr_21_pc + sw0(time:as.factor(state)) | unit + year)

educ_coal = c(log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + l(total_active_n, 0:2),
                                                                           log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc + l(total_active_n, 0:2)) | unit + year,
    log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + l(total_active_prod, 0:2),
                                                                           log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc + l(total_active_prod, 0:2)) | unit + year)

educ_coal_statetrends = c(log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + l(total_active_n, 0:2),
                                                                           log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc + l(total_active_n, 0:2)) + time:as.factor(state) | unit + year,
     log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + l(total_active_prod, 0:2),
                                                                           log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc + l(total_active_prod, 0:2)) + time:as.factor(state) | unit + year)
######### IV Approaches ########

                # Using total active production as an instrument for per capita property tax
iv_prod_tax = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ total_active_prod,
                # Using the lag of total active production as an instrument for per capita property tax
                log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ l(total_active_prod,1))
                # Using total active mines as an instrument for per capita property tax
iv_coaln_tax = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ total_active_n,
                 # Using lag of total active mines as an instrument for per capita property tax
                 log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ l(total_active_n,1))


# Using total active production as an instrument for per capita property tax
iv_prod_gdp = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ total_active_prod,
                # Using the lag of total active production as an instrument for per capita property tax
                log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ l(total_active_prod,1))
# Using total active mines as an instrument for per capita property tax
iv_coaln_gdp = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ total_active_n,
                 # Using lag of total active mines as an instrument for per capita property tax
                 log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ l(total_active_n,1))

selected_base_models = c(log_real_Property_Tax_pp ~ log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc | unit + year,
                   # log_real_Elem_Educ_Total_Exp_pp ~ log_real_Property_Tax_pp + log_real_Total_Fed_IG_Revenue_pp + log_real_Total_State_IG_Revenue_pp | unit + year,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc | unit + year,
                   log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp + log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc | unit + year)

selected_twfe_models_levs = c(log_real_Property_Tax ~ log_real_gdp_priv_ind + l1_log_real_gdp_priv_ind + l2_log_real_gdp_priv_ind + csw0(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage, log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue + log_real_Total_Fed_IG_Revenue) | unit + year,
                         log_real_Property_Tax ~ log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + csw0(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp) | unit + year,
                         log_real_Elem_Educ_Total_Exp ~ log_real_gdp_priv_ind + l1_log_real_gdp_priv_ind + l2_log_real_gdp_priv_ind + csw0(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage, log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue + log_real_Total_Fed_IG_Revenue, log_Enrollment, diff_log_pop_total) | unit + year,
                         log_real_Elem_Educ_Total_Exp ~ log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + csw0(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue + log_real_Total_Fed_IG_Revenue, log_Enrollment, diff_log_pop_total) | unit + year)

selected_twfe_models_levs_state_share_interaction = c(log_real_Property_Tax ~ state_share*(log_real_gdp_priv_ind + l1_log_real_gdp_priv_ind + l2_log_real_gdp_priv_ind) + csw0(state_share*(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage), state_share*(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi)) | unit + year,
                                                      log_real_Property_Tax ~ state_share*(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage) + csw0(state_share*(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi)) | unit + year,
                                                      log_real_Elem_Educ_Total_Exp ~ state_share*(log_real_gdp_priv_ind + l1_log_real_gdp_priv_ind + l2_log_real_gdp_priv_ind) + csw0(state_share*(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage), state_share*(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi), state_share*log_Enrollment, state_share*diff_log_pop_total) | unit + year,
                                                      log_real_Elem_Educ_Total_Exp ~ state_share*(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage) + csw0(state_share*(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi), state_share*log_Enrollment, state_share*diff_log_pop_total) | unit + year)

selected_twfe_models = c(log_real_Property_Tax_pp ~ log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + csw0(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage, log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp) | unit + year,
                         log_real_Property_Tax_pp ~ log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + csw0(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp) | unit + year,
                         log_real_Elem_Educ_Total_Exp_pp ~ log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + csw0(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage, log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp, log_Enrollment, diff_log_pop_total) | unit + year,
                         log_real_Elem_Educ_Total_Exp_pp ~ log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + csw0(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp, log_Enrollment, diff_log_pop_total) | unit + year)

# selected_twfe_models_state_share_interaction = 
#   c(log_real_Property_Tax_pp ~ state_share*(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc) + csw0(state_share*(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage), state_share*(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi)) | unit + year,
#     log_real_Property_Tax_pp ~ state_share*(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage) + csw0(state_share*(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi)) | unit + year,
#     log_real_Elem_Educ_Total_Exp_pp ~ sw(state_share*(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc), state_share*(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage), state_share*(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi)) + state_share*log_real_Total_Fed_IG_Revenue_pp + state_share*log_Enrollment | unit + year,
#     log_real_Elem_Educ_Total_Exp_pp ~ sw(state_share*(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage), state_share*(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi)) + state_share*log_real_Total_Fed_IG_Revenue_pp + state_share*log_Enrollment | unit + year)


selected_twfe_models_levs_state_trend = c(log_real_Property_Tax ~ time:as.factor(state) + log_real_gdp_priv_ind + l1_log_real_gdp_priv_ind + l2_log_real_gdp_priv_ind + csw0(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage, log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue + log_real_Total_Fed_IG_Revenue) | unit + year,
                              log_real_Property_Tax ~ time:as.factor(state) + log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + csw0(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp) | unit + year,
                              log_real_Elem_Educ_Total_Exp ~ time:as.factor(state) + log_real_gdp_priv_ind + l1_log_real_gdp_priv_ind + l2_log_real_gdp_priv_ind + csw0(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage, log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue + log_real_Total_Fed_IG_Revenue) | unit + year,
                              log_real_Elem_Educ_Total_Exp ~ time:as.factor(state) + log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + csw0(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue + log_real_Total_Fed_IG_Revenue) | unit + year)


selected_twfe_models_state_trend = c(log_real_Property_Tax_pp ~ time:as.factor(state) + log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + csw0(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage, log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp) | unit + year,
                                                            log_real_Property_Tax_pp ~ time:as.factor(state) + log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + csw0(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp) | unit + year,
                                                            log_real_Elem_Educ_Total_Exp_pp ~ time:as.factor(state) + log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + csw0(log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage, log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp) | unit + year,
                                                            log_real_Elem_Educ_Total_Exp_pp ~ time:as.factor(state) + log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + csw0(log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l4_log_hpi + l5_log_hpi, log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp) | unit + year)


selected_iv_models = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ total_active_prod,
                    # Using the lag of total active production as an instrument for per capita property tax
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ l(total_active_prod,1),
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ total_active_prod,
                    # Using the lag of total active production as an instrument for per capita property tax
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ l(total_active_prod,1))


solar_iv_models = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_area,
                       log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_cap_ac,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_cap_dc,
                       log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_area,
                       log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_cap_ac,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_cap_dc)

solar_iv_models_pc = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_area_pc,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_cap_ac_pc,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_cap_dc_pc,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_area_pc,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_cap_ac_pc,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_cap_dc_pc)

