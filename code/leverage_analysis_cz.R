#==============================================================================
# CZ-LEVEL DIAGNOSTIC ANALYSIS
# Deep dive into specific driver commuting zones
#==============================================================================
cat("=================================================================\n")
cat("COMMUTING ZONE DIAGNOSTIC ANALYSIS\n")
cat("Identifying and analyzing key driver CZs\n")
cat("=================================================================\n\n")

#==============================================================================
# STEP 1: Recreate the CZ Leverage Data
#==============================================================================

# Function to get conditional leverage for a state
get_conditional_cz_leverage <- function(state_code) {
  state_data <- df_ivs %>% filter(state == state_code)
  
  # Residualize SPENDING only (not wages)
  spending_on_controls <- feols(
    log_real_Elem_Educ_Total_Exp_pp ~ 
      l1_log_real_Elem_Educ_Total_Exp_pp + log_real_Total_IG_Revenue_pp +
      log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | year,
    data = state_data
  )
  
  # Filter complete cases first, then extract residuals
  state_data_resid <- state_data %>%
    filter(!is.na(log_real_Elem_Educ_Total_Exp_pp), !is.na(log_weighted_annual_avg_wkly_wage),
           !is.na(l1_log_real_Elem_Educ_Total_Exp_pp), !is.na(log_real_Total_IG_Revenue_pp),
           !is.na(log_real_gdp_priv_ind_pc), !is.na(log_Enrollment),
           !is.na(pct_black), !is.na(pct_hispanic)) %>%
    mutate(spending_resid = as.numeric(resid(spending_on_controls)))
  
  # Calculate CZ-level growth rates
  cz_conditional <- state_data_resid %>%
    arrange(unit, year) %>%
    group_by(unit) %>%
    summarize(
      cz_id = first(cz_id),
      state = first(state),
      n_obs = n(),
      sd_gr_wage = sd(gr_weighted_annual_avg_wkly_wage),
      wage_growth_uncond = mean(gr_weighted_annual_avg_wkly_wage, na.rm = TRUE),
      
      # Growth measures
      #wage_growth_uncond = last(log_weighted_annual_avg_wkly_wage) - first(log_weighted_annual_avg_wkly_wage),
      spending_growth_cond = last(spending_resid) - first(spending_resid),
      #spending_growth_uncond = last(log_real_Elem_Educ_Total_Exp_pp) - first(log_real_Elem_Educ_Total_Exp_pp),
      spending_growth_uncond = mean(diff_log_real_Total_Rev_Own_Sources_pp, na.rm = TRUE),
      
      # Control variable growth
      ig_revenue_growth = mean(diff_log_real_Total_IG_Revenue, na.rm = TRUE), # last(log_real_Total_IG_Revenue_pp) - first(log_real_Total_IG_Revenue_pp),
      gdp_growth = last(log_real_gdp_priv_ind_pc) - first(log_real_gdp_priv_ind_pc),
      enrollment_growth = last(log_Enrollment) - first(log_Enrollment),
      
      # Levels (first and last year)
      wage_first = first(weighted_annual_avg_wkly_wage),
      wage_last = last(weighted_annual_avg_wkly_wage),
      spending_pp_first = first(real_Elem_Educ_Total_Exp_pp),
      spending_pp_last = last(real_Elem_Educ_Total_Exp_pp),
      enrollment_first = first(Enrollment),
      enrollment_last = last(Enrollment),
      
      .groups = "drop"
    ) %>%
    mutate(
      state = state_code,
      # Driver scores
      driver_score_mixed = wage_growth_uncond * spending_growth_cond,
      driver_score_uncond = wage_growth_uncond * spending_growth_uncond,
      driver_magnitude_mixed = abs(driver_score_mixed),
      driver_magnitude_uncond = abs(driver_score_uncond)
    )
  
  return(cz_conditional)
}

# Get all significant states
#sig_states <- state_correlations$state

# Calculate for all states
cat("Calculating CZ-level leverage for all significant states...\n")
cz_conditional_all <- map_dfr(sig_states, get_conditional_cz_leverage)
cz_conditional_all_states <- map_dfr(reg_states, get_conditional_cz_leverage)

# Add mechanism classification
cz_conditional_all <- cz_conditional_all %>%
  left_join(.,
    state_correlations %>% select(state, mechanism, cor_mixed_cond, estimate, ftest),
    by = "state"
  )

#==============================================================================
# ADD CZ DESCRIPTIVE NAMES
#==============================================================================

# Join the descriptive names (assumes cz_names is loaded in parent environment)
if (exists("cz_names") && exists("cz_conditional_all")) {
  stopifnot(setdiff(cz_conditional_all$cz_id, cz_names$cz_id) %>% length(.) == 0)
  cz_conditional_all <- cz_conditional_all %>%
    left_join(
      cz_names %>% 
        mutate(unit = cz_id) %>% 
        select(unit, descriptor),
      by = "unit"
    ) %>%
    # Create display label: "descriptor (unit)"
    mutate(
      cz_label = ifelse(!is.na(descriptor) & descriptor != "", 
                        paste0("CZ", cz_id, "-", descriptor),
                        paste0("No Met Area; CZ: ", as.character(unit), "; FIPS: ", county_name))
    )
  stopifnot(setdiff(cz_conditional_all_states$cz_id, cz_names$cz_id) %>% length(.) == 0)
  
  cz_conditional_all_states <- cz_conditional_all_states %>%
    left_join(
      cz_names %>% 
      mutate(unit = cz_id) %>% 
        select(unit, descriptor),
      by = "unit"
    ) %>%
    # Create display label: "descriptor (unit)"
    mutate(
      cz_label = ifelse(!is.na(descriptor) & descriptor != "",
                        paste0(descriptor, " (CZ", cz_id, ")"),
                        paste0("No Met Area; CZ: ", as.character(unit), "; FIPS: ", county_name))
    )
}


cat("Complete! Analyzed", nrow(cz_conditional_all), "commuting zones across", 
    length(sig_states), "states.\n\n")

#==============================================================================
# STEP 2: Identify Top Driver CZs Overall
#==============================================================================

cat("## TOP 20 DRIVER CZs ACROSS ALL STATES\n\n")

top_drivers_overall <- cz_conditional_all %>%
  arrange(desc(sd_gr_wage)) %>% 
  #arrange(desc(driver_magnitude_mixed)) %>%
  head(20) %>%
  select(cz_id, state, unit, wage_growth_uncond, spending_growth_cond,
         driver_score_mixed, mechanism, cor_mixed_cond)

print(kable(
  top_drivers_overall,
  digits = 3,
  col.names = c("CZ Name", "State", "CZ ID", "Wage Growth", "Spending Growth (Resid)",
                "SD Wage GR",
                #"Driver Score", 
                "Mechanism", "State ρ"),
  caption = "Top 20 Driver Commuting Zones by Magnitude"
) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed")) %>%
  column_spec(6, bold = TRUE) %>%
  column_spec(7, background = ifelse(top_drivers_overall$mechanism == "Amplification", "#ffe6e6",
                                     ifelse(top_drivers_overall$mechanism == "Insulation", "#fff4e6", "#e6f4e6"))))

#==============================================================================
# STEP 3: Interactive CZ Search Function
#==============================================================================

# Function to get detailed diagnostics for a specific CZ
diagnose_cz <- function(cz_id_spec, show_time_series = TRUE) {
  
  # Get CZ summary data
  cz_summary <- cz_conditional_all %>% filter(unit == cz_id_spec)
  
  if (nrow(cz_summary) == 0) {
    cat("CZ", cz_id_spec, "not found in significant states.\n")
    return(NULL)
  }
  
  cat("\n=================================================================\n")
  cat("DIAGNOSTIC REPORT: CZ", cz_id_spec, "-", cz_summary$cz_id, "\n")
  cat("State:", cz_summary$state, "(", cz_summary$state, ")\n")
  cat("Mechanism:", as.character(cz_summary$mechanism), "\n")
  cat("=================================================================\n\n")
  
  # Basic info
  cat("## BASIC INFORMATION\n\n")
  cat("CZ Name:           ", cz_summary$cz_id, "\n")
  cat("State:             ", cz_summary$state, " (", cz_summary$state, ")\n", sep = "")
  cat("Mechanism Type:    ", as.character(cz_summary$mechanism), "\n")
  cat("State Correlation: ", round(cz_summary$cor_mixed_cond, 3), "\n")
  cat("State IV Estimate: ", round(cz_summary$estimate, 3), "\n")
  cat("N Observations:    ", cz_summary$n_obs, "\n\n")
  
  # Growth metrics
  cat("## GROWTH METRICS (First to Last Year)\n\n")
  cat("Wage Growth (unconditional):       ", sprintf("%+.3f", cz_summary$wage_growth_uncond), 
      " (", sprintf("%+.1f%%", 100 * cz_summary$wage_growth_uncond), ")\n", sep = "")
  cat("Spending Growth (unconditional):   ", sprintf("%+.3f", cz_summary$spending_growth_uncond), 
      " (", sprintf("%+.1f%%", 100 * cz_summary$spending_growth_uncond), ")\n", sep = "")
  cat("Spending Growth (conditional):     ", sprintf("%+.3f", cz_summary$spending_growth_cond), "\n")
  cat("IG Revenue Growth:                 ", sprintf("%+.3f", cz_summary$ig_revenue_growth), "\n")
  cat("GDP Growth:                        ", sprintf("%+.3f", cz_summary$gdp_growth), "\n")
  cat("Enrollment Growth:                 ", sprintf("%+.3f", cz_summary$enrollment_growth), 
      " (", sprintf("%+.1f%%", 100 * cz_summary$enrollment_growth), ")\n\n", sep = "")
  
  # Driver scores
  cat("## DRIVER SCORES\n\n")
  cat("Driver Score (conditional):    ", sprintf("%.4f", cz_summary$driver_score_mixed), "\n")
  cat("Driver Magnitude (conditional):", sprintf("%.4f", cz_summary$driver_magnitude_mixed), "\n")
  cat("Driver Score (unconditional):  ", sprintf("%.4f", cz_summary$driver_score_uncond), "\n\n")
  
  # Ranking
  rank_in_state <- cz_conditional_all %>%
    filter(state == cz_summary$state) %>%
    arrange(desc(driver_magnitude_mixed)) %>%
    mutate(rank = row_number()) %>%
    filter(unit == cz_id_spec) %>%
    pull(rank)
  
  rank_overall <- cz_conditional_all %>%
    arrange(desc(driver_magnitude_mixed)) %>%
    mutate(rank = row_number()) %>%
    filter(unit == cz_id_spec) %>%
    pull(rank)
  
  cat("## RANKINGS\n\n")
  cat("Rank within state: ", rank_in_state, " of ", 
      sum(cz_conditional_all$state == cz_summary$state), "\n", sep = "")
  cat("Rank overall:      ", rank_overall, " of ", nrow(cz_conditional_all), "\n\n")
  
  # Levels
  cat("## LEVELS (First vs Last Year)\n\n")
  cat("Average Weekly Wage:\n")
  cat("  First year: $", format(round(cz_summary$wage_first, 2), big.mark = ","), "\n", sep = "")
  cat("  Last year:  $", format(round(cz_summary$wage_last, 2), big.mark = ","), "\n", sep = "")
  cat("  Change:     $", format(round(cz_summary$wage_last - cz_summary$wage_first, 2), big.mark = ","), "\n\n", sep = "")
  
  cat("Education Spending per Pupil:\n")
  cat("  First year: $", format(round(cz_summary$spending_pp_first, 2), big.mark = ","), "\n", sep = "")
  cat("  Last year:  $", format(round(cz_summary$spending_pp_last, 2), big.mark = ","), "\n", sep = "")
  cat("  Change:     $", format(round(cz_summary$spending_pp_last - cz_summary$spending_pp_first, 2), big.mark = ","), "\n\n", sep = "")
  
  cat("Enrollment:\n")
  cat("  First year: ", format(round(cz_summary$enrollment_first), big.mark = ","), "\n", sep = "")
  cat("  Last year:  ", format(round(cz_summary$enrollment_last), big.mark = ","), "\n", sep = "")
  cat("  Change:     ", format(round(cz_summary$enrollment_last - cz_summary$enrollment_first), big.mark = ","), "\n\n", sep = "")
  
  # Time series plot
  if (show_time_series) {
    cat("## TIME SERIES VISUALIZATION\n\n")
    
    # Get time series data
    ts_data <- df_ivs %>%
      filter(unit == cz_id_spec) %>%
      select(year, log_weighted_annual_avg_wkly_wage, log_real_Elem_Educ_Total_Exp_pp,
             log_real_Total_IG_Revenue_pp, log_real_gdp_priv_ind_pc, log_Enrollment) %>%
      pivot_longer(-year, names_to = "variable", values_to = "value") %>%
      mutate(
        variable = recode(variable,
                          "log_weighted_annual_avg_wkly_wage" = "Wages",
                          "log_real_Elem_Educ_Total_Exp_pp" = "Spending per Pupil",
                          "log_real_Total_IG_Revenue_pp" = "IG Revenue per Pupil",
                          "log_real_gdp_priv_ind_pc" = "GDP per Capita",
                          "log_Enrollment" = "Enrollment")
      )
        
    # Create plot
    ts_plot <- ggplot(ts_data, aes(x = year, y = value, color = variable)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      facet_wrap(~variable, scales = "free_y", ncol = 2) +
      scale_color_brewer(palette = "Set1") +
      labs(
        title = paste0("Time Series: CZ ", cz_id_spec, " - ", cz_summary$cz_id),
        subtitle = paste0(cz_summary$state, " | Mechanism: ", cz_summary$mechanism),
        x = "Year",
        y = "Log Value"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "none",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "gray95", color = NA)
      )
    
    print(ts_plot)
    
    # Save plot
    ggsave(here(paste0("output/cz_diagnostic_", cz_id_spec, ".png")), ts_plot,
           width = 10, height = 8, dpi = 300)
    
    cat("Plot saved: output/cz_diagnostic_", cz_id_spec, ".png\n\n", sep = "")
  }
  
  # Return summary data invisibly
  invisible(cz_summary)
  invisible(ts_data)
}

#==============================================================================
# STEP 4: State-Specific Driver Lists
#==============================================================================



# Function to show top drivers for a specific state
show_state_drivers <- function(state_code, n = 10, print = FALSE) {
  
  state <- get_state(state_code)
  
  # state_mech <- cz_conditional_all %>% 
  #   filter(state == state_code) %>% 
  #   pull(mechanism) %>% 
  #   first()
  # 
  # if(state_code %in% reg_states){
  #     state_mech <- cz_conditional_all_states %>% 
  #   filter(state == state_code) %>% 
  #   pull(mechanism) %>% 
  #   first()
  # }
  
  cat("\n=================================================================\n")
  cat("TOP", n, "DRIVER CZs IN", state, "(", state_code, ")\n")
  #cat("Mechanism:", as.character(state_mech), "\n")
  cat("=================================================================\n\n")
  
  state_drivers <- cz_conditional_all_states %>%
    filter(state == state_code) %>%
    arrange(desc(sd_gr_wage)) %>% 
    #arrange(desc(driver_magnitude_mixed)) %>%
    head(n) %>%
    select(unit, cz_id, wage_growth_uncond, sd_gr_wage, spending_growth_cond, 
           driver_score_mixed, driver_magnitude_mixed)
  
  if(print){
    print(kable(
      state_drivers,
      digits = 4,
      col.names = c("CZ ID", "CZ Name", "Wage Growth", "SD Wage Growth", "Spending Growth (Resid)",
                    "Driver Score", "Driver Magnitude"),
      caption = paste0("Top ", n, " Drivers in ", state)
    ) %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed")))
  }

  cat("\n")
  
  return(state_drivers)
}

#==============================================================================
# STEP 5: Comparison Function - Compare Multiple CZs
#==============================================================================
compare_czs <- function(cz_id_specs, state_name = NULL, highlight = NULL) {

  
  cat("\n=================================================================\n")
  cat("COMPARATIVE ANALYSIS:", length(cz_id_specs), "COMMUTING ZONES\n")
  cat("=================================================================\n\n")
  
  comparison_data <- cz_conditional_all_states %>%
    filter(unit %in% cz_id_specs) %>%
    select(unit, cz_id, state, #mechanism,
           sd_gr_wage,
           wage_growth_uncond, spending_growth_cond, spending_growth_uncond,
           ig_revenue_growth, enrollment_growth,
           driver_score_mixed, driver_magnitude_mixed) %>% 
           arrange(desc(driver_magnitude_mixed))
  
  print(kable(
    comparison_data,
    digits = 3,
    col.names = c("CZ ID", "CZ Name", "State", 
    #"Mechanism", 
    "SD GR Wage",
                  "Wage Δ", "Spend Δ (Cond)", "Spend Δ (Uncond)",
                  "IG Rev Δ", "Enroll Δ", "Driver Score", "Driver Mag"),
    caption = "Comparative CZ Analysis"
  ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed"), 
                  font_size = 10)) #%>%
    # column_spec(4, background = ifelse(comparison_data$mechanism == "Amplification", "#ffe6e6",
    # ifelse(comparison_data$mechanism == "Insulation", "#fff4e6", "#e6f4e6"))))
  
  cat("\n")
  
  # Get time series data
  ts_data <- df_ivs %>%
    filter(unit %in% cz_id_specs) %>%
    select(year, unit, log_weighted_annual_avg_wkly_wage, log_real_Elem_Educ_Total_Exp_pp,
           log_real_Total_IG_Revenue_pp, log_real_gdp_priv_ind_pc, log_Enrollment) %>%
    pivot_longer(-c(year, unit), names_to = "variable", values_to = "value") %>%
    mutate(
      variable = recode(variable,
                        "log_weighted_annual_avg_wkly_wage" = "Wages",
                        "log_real_Elem_Educ_Total_Exp_pp" = "Spending per Pupil",
                        "log_real_Total_IG_Revenue_pp" = "IG Revenue per Pupil",
                        "log_real_gdp_priv_ind_pc" = "GDP per Capita",
                        "log_Enrollment" = "Enrollment")
    )
  
  # First join the labels
  ts_data <- ts_data %>%
    left_join(
      cz_conditional_all_states %>% select(unit, cz_label),
      by = "unit"
    )
  
  # Then use cz_label in the plot
  if (is.null(highlight)) {
    # Default: colour all CZs normally
    ts_plot <- ggplot(ts_data, aes(x = year, y = value, group = unit, color = cz_label)) +
      geom_line(linewidth = 1) +
      geom_point(size = 2) +
      facet_wrap(~variable, scales = "free_y", ncol = 2) +
      labs(
        title = paste0("CZ Time Series in ", state_name),
        x = "Year", y = "Log Value", color = "Commuting Zone"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "gray95", color = NA)
      ) +
      guides(color = guide_legend(ncol = 2))
  } else {
    # Highlighted mode: named CZs in colour, rest in grey
    ts_grey      <- ts_data %>% filter(!unit %in% highlight)
    ts_highlight <- ts_data %>% filter(unit %in% highlight)

    ts_plot <- ggplot() +
      # Grey background CZs (no legend entry)
      geom_line(data  = ts_grey,
                aes(x = year, y = value, group = unit),
                color = "grey75", linewidth = 0.7, alpha = 0.6) +
      geom_point(data = ts_grey,
                 aes(x = year, y = value, group = unit),
                 color = "grey75", size = 1.5, alpha = 0.6) +
      # Highlighted CZs in colour
      geom_line(data  = ts_highlight,
                aes(x = year, y = value, group = unit, color = cz_label),
                linewidth = 1.2) +
      geom_point(data = ts_highlight,
                 aes(x = year, y = value, group = unit, color = cz_label),
                 size = 2.5) +
      facet_wrap(~variable, scales = "free_y", ncol = 2) +
      labs(
        title = paste0("CZ Time Series in ", state_name),
        x = "Year", y = "Log Value", color = "Highlighted CZs"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        legend.position = "bottom",
        plot.title = element_text(face = "bold"),
        strip.text = element_text(face = "bold"),
        strip.background = element_rect(fill = "gray95", color = NA)
      ) +
      guides(color = guide_legend(ncol = 2))
  }

  
  print(ts_plot)
  
  return(comparison_data)

}

#==============================================================================
# STEP 6: Identify Anomalous CZs
#==============================================================================

cat("## IDENTIFYING ANOMALOUS CZs\n\n")

# Anomalies: High wage growth but negative spending growth
anomaly_negative <- cz_conditional_all_states %>%
  filter(wage_growth_uncond > 0.1 & spending_growth_cond < -0.05) %>%
  arrange(driver_score_mixed) %>%
  select(unit, cz_id, state, # mechanism, 
  wage_growth_uncond, 
         spending_growth_cond, ig_revenue_growth)

if (nrow(anomaly_negative) > 0) {
  cat("ANOMALY TYPE 1: High Wage Growth but Negative Spending Growth\n")
  cat("(Possible equalization or fiscal constraint)\n\n")
  
  print(kable(
    anomaly_negative,
    digits = 3,
    col.names = c("CZ ID", "CZ Name", "State", #"Mechanism", 
                  "Wage Growth", "Spending Growth", "IG Revenue Growth")
  ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")))
  
  cat("\n\n")
}

# Anomalies: Negative wage growth but positive spending growth
anomaly_positive <- cz_conditional_all_states %>%
  filter(wage_growth_uncond < -0.05 & spending_growth_cond > 0.05) %>%
  arrange(desc(driver_score_mixed)) %>%
  select(unit, cz_id, state, #mechanism, 
  wage_growth_uncond, 
         spending_growth_cond, ig_revenue_growth)

if (nrow(anomaly_positive) > 0) {
  cat("ANOMALY TYPE 2: Negative Wage Growth but Positive Spending Growth\n")
  cat("(Possible state intervention or compensatory funding)\n\n")
  
  print(kable(
    anomaly_positive,
    digits = 3,
    col.names = c("CZ ID", "CZ Name", "State", #"Mechanism", 
                  "Wage Growth", "Spending Growth", "IG Revenue Growth")
  ) %>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed")))
  
  cat("\n\n")
}

#==============================================================================
# USAGE EXAMPLES
#==============================================================================

cat("=================================================================\n")
cat("DIAGNOSTIC FUNCTIONS LOADED\n")
cat("=================================================================\n\n")

cat("Available functions:\n\n")

cat("1. diagnose_cz(cz_id_spec)\n")
cat("   - Full diagnostic report for a specific CZ\n")
cat("   - Example: diagnose_cz(684)\n\n")

cat("2. show_state_drivers(state_code, n = 10)\n")
cat("   - Show top N drivers in a state\n")
cat("   - Example: show_state_drivers('SD', n = 5)\n\n")

cat("3. compare_czs(c(cz_id_spec1, cz_id_spec2, ...))\n")
cat("   - Compare multiple CZs side by side\n")
cat("   - Example: compare_czs(c(684, 448, 154))\n\n")

cat("4. show_cz_industry_profile(cz_ids, state_code, top_n = 7)\n")
cat("   - Pie charts of employment composition + national wage TS vs CZ wage\n")
cat("   - Example: show_cz_industry_profile(c(663, 693), '08')\n\n")

cat("5. show_fiscal_decomposition(cz_ids, state_code)  [significant states]\n")
cat("   - Time series: Wages / Total Spending / IG Revenue / Local Revenue\n")
cat("   - Reveals whether the wage-spending channel runs through local tax base\n")
cat("     (amplification) or is offset by IG transfers (equalization)\n")
cat("   - Example: show_fiscal_decomposition(c(663, 693), '08')\n\n")

cat("6. diagnose_no_effect(state_code)  [non-significant states]\n")
cat("   - Tests: (A) low wage variation (weak instrument) vs.\n")
cat("            (B) equalization channel active (IG offsets wages)\n")
cat("   - Prints verdict + 3-panel figure\n")
cat("   - Example: diagnose_no_effect('01')  # Alabama\n\n")

cat("Data available:\n")
cat("  - cz_conditional_all: Full dataset with all CZ metrics\n")
cat("  - top_drivers_overall: Top 20 driver CZs\n\n")

cat("=================================================================\n\n")

#==============================================================================
# STEP 7: Industry Profile for Highlighted CZs
# Requires: ss_temp_filled, industry_mapping (from heterogeneity_industrial_share_analysis.R)
# Depends on: df_ivs, cz_names, get_state()
#==============================================================================

show_cz_industry_profile <- function(cz_ids, state_code, top_n = 7) {

  state_nm <- get_state(state_code)

  # ── 1. Column identification ───────────────────────────────────────────────
  share_cols <- grep("^share_annual_avg_emplvl_",       names(ss_temp_filled), value = TRUE)
  lwage_cols <- grep("^log_natl_annual_avg_wkly_wage_", names(ss_temp_filled), value = TRUE)

  strip_prefix <- function(x, prefix) str_remove(x, paste0("^", prefix))

  common_codes <- intersect(
    strip_prefix(share_cols, "share_annual_avg_emplvl_"),
    strip_prefix(lwage_cols, "log_natl_annual_avg_wkly_wage_")
  )
  share_cols <- paste0("share_annual_avg_emplvl_",       common_codes)
  lwage_cols <- paste0("log_natl_annual_avg_wkly_wage_", common_codes)

  # ── 2. State CZ IDs & CZ labels ───────────────────────────────────────────
  state_cz_ids <- df_ivs %>% filter(state == state_code) %>% pull(unit) %>% unique()

  cz_lbl <- cz_names %>%
    mutate(cz_label = ifelse(
      !is.na(descriptor) & descriptor != "",
      paste0(descriptor, " (CZ", cz_id, ")"),
      paste0("CZ ", cz_id)
    )) %>%
    select(cz_id, cz_label)

  # ── 3. Base-year employment shares for highlighted CZs ────────────────────
  base_shares <- ss_temp_filled %>%
    filter(cz_id %in% cz_ids) %>%
    group_by(cz_id) %>%
    filter(year == min(year)) %>%
    ungroup() %>%
    select(cz_id, all_of(share_cols)) %>%
    pivot_longer(all_of(share_cols), names_to = "var", values_to = "share") %>%
    mutate(ind_code = strip_prefix(var, "share_annual_avg_emplvl_")) %>%
    select(-var) %>%
    left_join(industry_mapping %>% select(industry_code, industry_name),
              by = c("ind_code" = "industry_code")) %>%
    mutate(category = coalesce(industry_name, paste0("Ind. ", ind_code))) %>%
    left_join(cz_lbl, by = "cz_id")

  # ── 4. Top-N industries by average employment share ───────────────────────
  top_inds <- base_shares %>%
    group_by(ind_code, category) %>%
    summarize(avg_share = mean(share, na.rm = TRUE), .groups = "drop") %>%
    slice_max(avg_share, n = top_n) %>%
    pull(ind_code)

  # Consistent factor order: largest share first (clockwise from 12 o'clock in pie)
  ind_order <- base_shares %>%
    filter(ind_code %in% top_inds) %>%
    group_by(category) %>%
    summarize(m = mean(share), .groups = "drop") %>%
    arrange(desc(m)) %>%
    pull(category)
  ind_order <- c(ind_order, "Other")

  # Shared color palette: Set2 for named industries, grey for Other
  pal <- c(scales::hue_pal(l = 60, c = 80)(length(ind_order) - 1), "grey80")
  ind_cols <- setNames(pal, ind_order)

  # ── 5. Pie chart data ─────────────────────────────────────────────────────
  pie_data <- base_shares %>%
    mutate(ind_label = ifelse(ind_code %in% top_inds, category, "Other")) %>%
    group_by(cz_id, cz_label, ind_label) %>%
    summarize(share = sum(share, na.rm = TRUE), .groups = "drop") %>%
    mutate(ind_label = factor(ind_label, levels = ind_order)) %>%
    # Label position: midpoint of each slice in cumulative-share space
    group_by(cz_label) %>%
    arrange(cz_label, ind_label) %>%
    mutate(
      ymax     = cumsum(share),
      ymin     = ymax - share,
      label_y  = (ymin + ymax) / 2,
      pct_text = ifelse(share >= 0.06,
                        paste0(ind_label, "\n", scales::percent(share, accuracy = 1)),
                        "")
    ) %>%
    ungroup()

  # ── 6. Build one pie per CZ ───────────────────────────────────────────────
  make_pie <- function(cz_id_i) {
    d   <- filter(pie_data, cz_id == cz_id_i)
    lbl <- unique(d$cz_label)

    ggplot(d, aes(x = "", y = share, fill = ind_label)) +
      geom_col(width = 1, color = "white", linewidth = 0.4) +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = ind_cols, drop = FALSE) +
      labs(title = lbl) +
      theme_void(base_size = 10) +
      theme(
        plot.title      = element_text(face = "bold", size = 9, hjust = 0.5,
                                       margin = margin(b = 4)),
        legend.position = "none"
      )
  }

  pie_plots <- purrr::map(cz_ids, make_pie)

  # Legend: invisible-bar plot centered in its panel — avoids grob extraction
  legend_only <- ggplot(
      tibble(ind_label = factor(ind_order, levels = ind_order), y = 1),
      aes(x = ind_label, y = y, fill = ind_label)
    ) +
    geom_col(alpha = 0) +
    scale_fill_manual(values = ind_cols, name = "Industry", drop = FALSE) +
    guides(fill = guide_legend(ncol = 1, override.aes = list(alpha = 1))) +
    theme_void() +
    theme(
      legend.position = c(0.5, 0.5),
      legend.title    = element_text(face = "bold", size = 9),
      legend.text     = element_text(size = 8),
      legend.key.size = unit(0.4, "cm")
    )

  pie_row <- wrap_plots(c(pie_plots, list(legend_only)),
                        nrow = 1,
                        widths = c(rep(1, length(cz_ids)), 0.55))

  # ── 7. National industry wage time series (indexed to base year = 0) ───────
  # National wages are the same across CZs — pull from one reference CZ
  ref_cz <- state_cz_ids[[1]]

  ind_wage_ts <- ss_temp_filled %>%
    filter(cz_id == ref_cz) %>%
    select(year, all_of(lwage_cols)) %>%
    pivot_longer(all_of(lwage_cols), names_to = "var", values_to = "log_natl_wage") %>%
    mutate(ind_code = strip_prefix(var, "log_natl_annual_avg_wkly_wage_")) %>%
    select(-var) %>%
    filter(ind_code %in% top_inds) %>%
    left_join(industry_mapping %>% select(industry_code, industry_name),
              by = c("ind_code" = "industry_code")) %>%
    mutate(category = coalesce(industry_name, paste0("Ind. ", ind_code)),
           ind_label = factor(category, levels = ind_order)) %>%
    group_by(ind_code) %>%
    mutate(log_natl_wage = log_natl_wage - log_natl_wage[year == min(year)]) %>%
    ungroup()

  # CZ overall wage — one series per highlighted CZ
  cz_wage_ts <- df_ivs %>%
    filter(unit %in% cz_ids) %>%
    select(year, cz_id = unit, log_wage = log_weighted_annual_avg_wkly_wage) %>%
    left_join(cz_lbl, by = "cz_id") %>%
    group_by(cz_id) %>%
    mutate(log_wage = log_wage - log_wage[year == min(year)]) %>%
    ungroup()

  # Replicate industry lines into each CZ facet via crossing
  ts_inds_all <- crossing(
    cz_wage_ts %>% select(cz_id, cz_label) %>% distinct(),
    ind_wage_ts
  )

  p_ts <- ggplot() +
    # Dashed coloured lines: national wage by industry
    geom_line(data  = ts_inds_all,
              aes(x = year, y = log_natl_wage, color = ind_label, group = ind_label),
              linewidth = 0.75, linetype = "dashed", alpha = 0.85) +
    # Solid black line: CZ observed average wage
    geom_line(data  = cz_wage_ts,
              aes(x = year, y = log_wage, group = cz_id),
              color = "black", linewidth = 1.4) +
    geom_point(data = cz_wage_ts,
               aes(x = year, y = log_wage),
               color = "black", size = 1.8) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey55", linewidth = 0.5) +
    scale_color_manual(values = ind_cols[ind_order[ind_order != "Other"]],
                       name   = "National industry wage") +
    facet_wrap(~cz_label, scales = "free_y", ncol = min(length(cz_ids), 3)) +
    labs(
      title    = "B.  National Industry Wages vs. CZ Average Wage (Indexed to Base Year = 0)",
      subtitle = "Dashed: national avg weekly wage by industry. Solid black: CZ observed wage (log_weighted_annual_avg_wkly_wage).",
      x = "Year", y = "Log wage change from base year"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 11),
      strip.text       = element_text(face = "bold", size = 9),
      strip.background = element_rect(fill = "grey95", color = NA),
      legend.position  = "none"
    )

  # ── 8. Combine ─────────────────────────────────────────────────────────────
  fig <- (pie_row / p_ts) +
    plot_annotation(
      title    = paste0("Industry Profile: Highlighted CZs \u2014 ", state_nm),
      subtitle = paste0(
        "Top ", top_n, " industries by base-year employment share. ",
        "Pie: employment composition. ",
        "Time series: national wage trends vs. CZ observed wage."
      ),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9, color = "grey30")
      )
    ) +
    plot_layout(heights = c(1, 1.4))

  print(fig)
  invisible(fig)
}

#==============================================================================
# STEP 10: National Map — Wage Episodes Transmitted to Spending
#
# For each CZ, identifies:
#   - the worst N-year wage-loss episode (bust) — amplified if local revenue
#     drove spending down; equalized if IG transfers absorbed the shock
#   - the best N-year wage-gain episode (boom) — amplified if local revenue
#     drove spending up
#
# Only the most extreme cases are flagged (bottom/top quartile), so the map
# shows confirmed episodes rather than routine fluctuation. Bust takes
# priority if a CZ qualifies for both categories.
#
# Categories:
#   "Amplified bust" – severe wage loss → spending fell, driven by local revenue
#   "Amplified boom" – severe wage gain → spending rose, driven by local revenue
#   "Equalized"      – severe wage loss → spending protected by IG transfers
#   "Other"          – no severe episode detected
#
# Usage: map_wage_bust_episodes()           # 3-year window, 25th-pctile thresholds
#        map_wage_bust_episodes(window = 2) # tighter window
#==============================================================================

map_wage_bust_episodes <- function(window       = 3,
                                   wage_pctile  = 0.25,
                                   spend_pctile = 0.25,
                                   exclude_states = NULL,
                                   include_states = NULL,
                                   sig_states = NULL,
                                   include_cities = FALSE,
                                   city_pop = 50000,
                                   wage_method = "raw",
                                   show_fiscal_insets = FALSE,
                                   inset_scale = 0.15) {
  # wage_method: one of
  #   "raw"       — actual log wages (original behaviour)
  #   "iv_pooled" — pooled first-stage instrument-only component (β·Z, all states)
  #   "iv_state"  — per-state first stages; each CZ gets its own state's β·Z

  stopifnot(wage_method %in% c("raw", "iv_pooled", "iv_state"))

  lag_n <- window - 1   # e.g. window=3 → lead 2 years forward

  # ── 1. Build CZ-year panel with fiscal components ──────────────────────────
  panel <- df_ivs %>%
    mutate(
      spending_pp = real_Elem_Educ_Total_Exp / Enrollment,
      ig_pp       = real_Total_IG_Revenue     / Enrollment,
      local_pp    = spending_pp - ig_pp,
      log_wage    = log_weighted_annual_avg_wkly_wage
    ) %>%
    arrange(unit, year)

  # ── 1b. Wage variable for episode detection ────────────────────────────────
  if (wage_method == "raw") {

    panel <- panel %>% mutate(wage_episode = log_wage)
    cat("wage_method = 'raw': using actual log wages.\n")

  } else if (wage_method == "iv_pooled") {

    # Pooled first stage (unit + year FEs). Extract only the instrument-driven
    # component β1·l1_lev_gdp_ss_2d + β2·l2_lev_gdp_ss_2d, stripped of FEs
    # and AR(1) persistence, so episodes reflect exogenous industry-mix shocks.
    fs <- feols(
      log_weighted_annual_avg_wkly_wage ~
        l1_log_weighted_annual_avg_wkly_wage +
        l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d | unit + year,
      data = panel, panel.id = c("unit", "year"), cluster = "unit"
    )
    b1 <- coef(fs)["l1_lev_gdp_ss_2d"]
    b2 <- coef(fs)["l2_lev_gdp_ss_2d"]
    cat(sprintf(
      "wage_method = 'iv_pooled': l1_lev_gdp_ss_2d = %.4f  l2_lev_gdp_ss_2d = %.4f\n",
      b1, b2
    ))
    panel <- panel %>%
      mutate(wage_episode = b1 * l1_lev_gdp_ss_2d + b2 * l2_lev_gdp_ss_2d)

  } else if (wage_method == "iv_state") {

    # Per-state first stages. Each state gets its own β·Z so the instrument
    # scaling reflects within-state variation — consistent with the state-by-
    # state causal estimation.
    state_betas <- panel %>%
      group_by(state) %>%
      group_map(function(state_data, state_key) {
        fs <- tryCatch(
          feols(
            log_weighted_annual_avg_wkly_wage ~
              l1_log_weighted_annual_avg_wkly_wage +
              l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d | unit + year,
            data = state_data, cluster = "unit"
          ),
          error = function(e) {
            message("  Could not fit first stage for state ", state_key$state,
                    ": ", conditionMessage(e))
            NULL
          }
        )
        if (is.null(fs)) return(NULL)
        tibble(
          state = state_key$state,
          b1    = coef(fs)["l1_lev_gdp_ss_2d"],
          b2    = coef(fs)["l2_lev_gdp_ss_2d"]
        )
      }, .keep = TRUE) %>%
      bind_rows()

    cat("wage_method = 'iv_state': per-state first-stage coefficients:\n")
    print(state_betas, n = Inf)

    panel <- panel %>%
      left_join(state_betas, by = "state") %>%
      mutate(wage_episode = b1 * l1_lev_gdp_ss_2d + b2 * l2_lev_gdp_ss_2d) %>%
      select(-b1, -b2)

  }

  # ── 2. Rolling window: delta from year t to t + lag_n ──────────────────────
  panel_rolled <- panel %>%
    group_by(unit) %>%
    mutate(
      delta_wage     = lead(wage_episode, lag_n) - wage_episode,
      delta_spending = lead(spending_pp,  lag_n) - spending_pp,
      delta_ig       = lead(ig_pp,        lag_n) - ig_pp,
      delta_local    = lead(local_pp,     lag_n) - local_pp
    ) %>%
    filter(!is.na(delta_wage)) %>%
    ungroup()

  # For iv_state, per-state betas can differ substantially in magnitude, so
  # national thresholds become meaningless when focusing on a subset of states.
  # Restrict the ranking pool to include_states so thresholds are relative to
  # the states being analysed. raw and iv_pooled use a common scale across all
  # CZs, so they always rank against the full national pool.
  panel_for_episodes <- if (wage_method == "iv_state" && !is.null(include_states)) {
    panel_rolled %>% filter(state %in% include_states)
  } else {
    panel_rolled
  }

  # ── 3a. Worst bust episode per CZ ──────────────────────────────────────────
  worst <- panel_for_episodes %>%
    group_by(unit, state) %>%
    slice_min(delta_wage, n = 1, with_ties = FALSE) %>%
    ungroup()

  # ── 3b. Best boom episode per CZ ───────────────────────────────────────────
  best <- panel_for_episodes %>%
    group_by(unit, state) %>%
    slice_max(delta_wage, n = 1, with_ties = FALSE) %>%
    ungroup()

  # ── 4a. Bust thresholds (bottom quartile, conditional) ─────────────────────
  wage_thresh_bust  <- quantile(worst$delta_wage, probs = wage_pctile, na.rm = TRUE)
  wage_loss_czs     <- worst %>% filter(delta_wage <= wage_thresh_bust)
  spend_thresh_bust <- quantile(wage_loss_czs$delta_spending,
                                probs = spend_pctile, na.rm = TRUE)

  # ── 4b. Boom thresholds (top quartile, conditional) ────────────────────────
  wage_thresh_boom  <- quantile(best$delta_wage, probs = 1 - wage_pctile, na.rm = TRUE)
  wage_gain_czs     <- best %>% filter(delta_wage >= wage_thresh_boom)
  spend_thresh_boom <- quantile(wage_gain_czs$delta_spending,
                                probs = 1 - spend_pctile, na.rm = TRUE)

  cat(sprintf(
    "Bust thresholds — wage loss: <= %.3f log pts | spending decline: <= $%.0f pp\n",
    wage_thresh_bust, spend_thresh_bust
  ))
  cat(sprintf(
    "Boom thresholds — wage gain: >= %.3f log pts | spending rise:    >= $%.0f pp\n",
    wage_thresh_boom, spend_thresh_boom
  ))

  # ── 5a. Classify bust episodes ─────────────────────────────────────────────
  bust_class <- worst %>%
    mutate(
      bust_class = case_when(
        # Amplified bust: severe wage loss + spending fell via local revenue
        delta_wage     <= wage_thresh_bust  &
          delta_spending <= spend_thresh_bust &
          delta_local    <  0                 &
          abs(delta_local) > abs(delta_ig)     ~ "Amplified bust",
        # Equalized: severe wage loss but IG protected spending
        delta_wage     <= wage_thresh_bust  &
          delta_spending >  spend_thresh_bust  ~ "Equalized",
        TRUE ~ "Other"
      )
    ) %>%
    select(unit, bust_class)

  # ── 5b. Classify boom episodes ─────────────────────────────────────────────
  boom_class <- best %>%
    mutate(
      boom_class = case_when(
        # Amplified boom: severe wage gain + spending rose via local revenue
        delta_wage     >= wage_thresh_boom  &
          delta_spending >= spend_thresh_boom &
          delta_local    >  0                 &
          abs(delta_local) > abs(delta_ig)     ~ "Amplified boom",
        TRUE ~ "Other"
      )
    ) %>%
    select(unit, boom_class)

  # ── 5c. Combine: bust takes priority over boom ─────────────────────────────
  classified <- bust_class %>%
    left_join(boom_class, by = "unit") %>%
    mutate(
      episode_class = case_when(
        bust_class == "Amplified bust" ~ "Amplified bust",
        bust_class == "Equalized"      ~ "Equalized",
        boom_class == "Amplified boom" ~ "Amplified boom",
        TRUE                           ~ "Other"
      )
    ) %>%
    left_join(worst %>% select(unit, bust_year  = year), by = "unit") %>%
    left_join(best  %>% select(unit, boom_year  = year), by = "unit") %>%
    mutate(
      episode_year = case_when(
        episode_class == "Amplified boom" ~ boom_year,
        TRUE                              ~ bust_year
      )
    )

  cat(sprintf("Amplified bust CZs: %d\n", sum(classified$episode_class == "Amplified bust")))
  cat(sprintf("Equalized CZs:      %d\n", sum(classified$episode_class == "Equalized")))
  cat(sprintf("Amplified boom CZs: %d\n", sum(classified$episode_class == "Amplified boom")))

  # ── 6. CZ → county FIPS crosswalk via cz_names ────────────────────────────
  cz_fips <- cz_names %>%
    select(cz_id, fips) %>%
    separate_rows(fips, sep = ";\\s*") %>%
    filter(nchar(trimws(fips)) == 5) %>%
    mutate(fips = trimws(fips))

  # ── 6b. CZ centroids for episode labels (non-Other CZs only) ──────────────
  label_czs <- cz_names %>%
    mutate(cz_id_chr = as.character(cz_id)) %>%
    left_join(
      classified %>%
        filter(episode_class != "Other") %>%
        mutate(cz_id_chr = as.character(unit)) %>%
        select(cz_id_chr, episode_class, episode_year),
      by = "cz_id_chr"
    ) %>%
    filter(!is.na(episode_class)) %>%
    mutate(
      cz_label = paste0(
        ifelse(!is.na(descriptor) & descriptor != "",
               paste0(descriptor, " (CZ", cz_id, ")"),
               paste0("CZ ", cz_id)),
        "\n", episode_year, "\u2013", episode_year + lag_n
      )
    ) %>%
    select(cz_id, cz_label, fips) %>%
    separate_rows(fips, sep = ";\\s*") %>%
    filter(nchar(trimws(fips)) == 5) %>%
    mutate(fips = trimws(fips))

  cz_centroids <- usmapdata::us_map(regions = "counties", include = include_states) %>%
    left_join(label_czs %>% select(fips, cz_id, cz_label), by = "fips") %>%
    filter(!is.na(cz_id)) %>%
    group_by(cz_id, cz_label) %>%
    summarise(.groups = "drop") %>%
    sf::st_point_on_surface()

  county_data <- cz_fips %>%
    left_join(
      classified %>%
        select(unit, episode_class) %>%
        mutate(cz_id = as.character(unit)),
      by = "cz_id"
    ) %>%
    mutate(episode_class = replace_na(episode_class, "Other"))

  # ── 7. Map ─────────────────────────────────────────────────────────────────
  ep_cols <- c(
    "Amplified bust" = "#d7191c",
    "Amplified boom" = "#1a9641",
    "Equalized"      = "#2c7bb6",
    "Other"          = "grey88"
  )

  # make temporary empty state set to filter the cities on
  if(!include_cities){temp_include_states <- c()}else{temp_include_states <- include_states}
  
    cities <- us.cities %>%
      filter(country.etc %in% sapply(temp_include_states, get_state), pop > city_pop) %>%
      rename(lon = long) %>%
      select(name, lon, lat, pop)
    
    cities_transformed <- usmap_transform(cities, input_names = c("lon", "lat"))

  
  fig <- plot_usmap(regions = "counties", data = county_data, values = "episode_class", include = include_states, color = "grey50", linewidth = 0.25) +
    labs(
      title    = "Wage Episodes Transmitted to K\u20136 Education Spending",
      subtitle = paste0(
        "Method: ", switch(wage_method,
          raw       = "raw log wages",
          iv_pooled = "IV predicted wages (pooled first stage)",
          iv_state  = "IV predicted wages (state-by-state first stage)"
        ), "  |  ",
        "Red: worst ", window, "-yr wage loss drove spending down via local revenue.  ",
        "Green: best ", window, "-yr wage gain drove spending up via local revenue.  ",
        "Blue: wage loss absorbed by IG transfers."
      ),
      caption  = paste0(
        "Episode = worst/best ", window, "-year cumulative wage change per CZ. ",
        "Bust: bottom ", round(wage_pctile * 100), "th pctile wage loss & spending decline. ",
        "Boom: top ", round(wage_pctile * 100), "th pctile wage gain & spending rise. ",
        "Local revenue must be dominant driver (|\u0394local| > |\u0394IG|). ",
        "Bust takes priority where a CZ qualifies for both."
      )
    ) +
    theme(
      plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
      plot.subtitle   = element_text(size = 9, hjust = 0.5, color = "grey30",
                                     margin = margin(b = 6)),
      plot.caption    = element_text(size = 7.5, hjust = 0.5, color = "grey45",
                                     margin = margin(t = 8)),
      legend.position = "bottom",
      legend.text     = element_text(size = 10)
    ) + 
    geom_sf(data = usmapdata::us_map(regions = "states", include = include_states),
            fill = NA, linewidth = 0.25, color = "black") +
    { if (!is.null(sig_states))
        geom_sf(data = usmapdata::us_map(regions = "states", include = sig_states),
                fill = NA, linewidth = 0.5, color = "red")
    } +
    geom_sf(data = cities_transformed, fill = "yellow", color = 'orange', aes(size = pop), shape = 23) + 
    scale_fill_manual(values = ep_cols, name = NULL, na.value = "grey88") +
    geom_sf_text(data = cz_centroids, aes(label = cz_label),
                 size = 2.2, lineheight = 0.85, color = "black",
                 inherit.aes = FALSE)

  # ── 8. Fiscal inset plots (optional) ──────────────────────────────────────
  # For each non-Other CZ, overlay a mini version of the show_fiscal_decomposition
  # decomp_bars chart: stacked IG (orange) + Local Revenue (green) bars with a
  # black total-spending-change line on top. Cumulative change from base year.
  if (show_fiscal_insets && nrow(cz_centroids) > 0) {

    comp_cols <- c("IG Revenue" = "#d95f02", "Local Revenue" = "#1b7837")

    # Inset dimensions in map projection units (fraction of state bounding box)
    map_bbox <- sf::st_bbox(
      usmapdata::us_map(regions = "counties", include = include_states)
    )
    inset_w <- (map_bbox[["xmax"]] - map_bbox[["xmin"]]) * inset_scale
    inset_h <- (map_bbox[["ymax"]] - map_bbox[["ymin"]]) * inset_scale

    # Extract centroid x/y in projection coordinates
    centroid_xy <- cz_centroids %>%
      mutate(
        cx = sf::st_coordinates(.)[, 1],
        cy = sf::st_coordinates(.)[, 2]
      ) %>%
      sf::st_drop_geometry()

    for (i in seq_len(nrow(centroid_xy))) {
      row <- centroid_xy[i, ]

      cz_id_i <- as.numeric(as.character(row$cz_id))

      # Cumulative change from base year in real $ per pupil
      decomp_data <- df_ivs %>%
        filter(unit == cz_id_i) %>%
        arrange(year) %>%
        mutate(
          spending_pp = real_Elem_Educ_Total_Exp / Enrollment,
          ig_pp       = real_Total_IG_Revenue     / Enrollment,
          local_pp    = spending_pp - ig_pp
        ) %>%
        group_by(unit) %>%
        mutate(
          delta_spending = spending_pp - spending_pp[year == min(year)],
          delta_ig       = ig_pp       - ig_pp[year == min(year)],
          delta_local    = local_pp    - local_pp[year == min(year)]
        ) %>%
        ungroup()

      if (nrow(decomp_data) == 0 || all(is.na(decomp_data$delta_spending))) next

      decomp_long <- decomp_data %>%
        pivot_longer(c(delta_ig, delta_local),
                     names_to = "component", values_to = "delta") %>%
        mutate(component = factor(
          recode(component,
                 "delta_ig"    = "IG Revenue",
                 "delta_local" = "Local Revenue"),
          levels = c("IG Revenue", "Local Revenue")
        ))

      mini_p <- ggplot() +
        geom_col(
          data     = decomp_long %>% filter(delta >= 0),
          aes(x = year, y = delta, fill = component),
          position = "stack", width = 0.75, alpha = 0.85
        ) +
        geom_col(
          data     = decomp_long %>% filter(delta < 0),
          aes(x = year, y = delta, fill = component),
          position = "stack", width = 0.75, alpha = 0.85
        ) +
        geom_line(data  = decomp_data,
                  aes(x = year, y = delta_spending),
                  color = "black", linewidth = 0.6) +
        geom_hline(yintercept = 0, linetype = "dotted",
                   color = "grey55", linewidth = 0.2) +
        scale_fill_manual(values = comp_cols) +
        theme_void() +
        theme(
          legend.position  = "none",
          panel.background = element_rect(fill  = alpha("white", 0.88),
                                          color = "grey40", linewidth = 0.4),
          plot.margin      = margin(2, 2, 2, 2)
        )

      fig <- fig +
        annotation_custom(
          grob = ggplotGrob(mini_p),
          xmin = row$cx - inset_w / 2,
          xmax = row$cx + inset_w / 2,
          ymin = row$cy - inset_h / 2,
          ymax = row$cy + inset_h / 2
        )
    }
  }

  print(fig)
  invisible(list(fig = fig, data = classified))
}


map_state <- function(state_name = NULL, highlight_czs = NULL, include_cities = FALSE, city_pop = 50000) {

  county_data <- cz_names %>%
    mutate(cz_label = ifelse(!is.na(descriptor) & descriptor != "",
                             paste0(descriptor, " (CZ", cz_id, ")"),
                             paste0("CZ ", cz_id))) %>%
    select(cz_id, cz_label, descriptor, fips) %>%
    separate_rows(fips, sep = ";\\s*") %>%
    filter(nchar(trimws(fips)) == 5) %>%
    mutate(fips = trimws(fips),
           highlighted = cz_id %in% highlight_czs)

  # CZ-level label points: dissolve counties → one interior point per CZ
  cz_centroids <- usmapdata::us_map(regions = "counties", include = state_name) %>%
    left_join(county_data %>% select(fips, cz_id, cz_label), by = "fips") %>%
    filter(!is.na(cz_id)) %>%
    group_by(cz_id, cz_label) %>%
    summarise(.groups = "drop") %>%      # sf auto-unions county polygons by CZ
    sf::st_point_on_surface()            # guaranteed inside the CZ polygon

  
  # Highlighted CZ outer borders: dissolve highlighted counties to CZ level
  highlight_borders <- usmapdata::us_map(regions = "counties", include = state_name) %>%
    left_join(county_data %>% select(fips, cz_id, highlighted), by = "fips") %>%
    filter(highlighted) %>%
    group_by(cz_id) %>%
    summarise(.groups = "drop")   # sf unions counties → only outer CZ boundary survives


  # Cities
  if (!include_cities) {
    temp_include_states <- c()
  } else {
    temp_include_states <- state_name
  }

  cities <- us.cities %>%
    filter(country.etc %in% sapply(temp_include_states, get_state), pop > city_pop) %>%
    rename(lon = long) %>%
    select(name, lon, lat, pop)

  cities_transformed <- usmap_transform(cities, input_names = c("lon", "lat"))

  fig_state <- plot_usmap(regions = "counties", data = county_data, values = "cz_label",
                          include = state_name, color = "grey50", linewidth = 0.2) +
    geom_sf(data = highlight_borders, fill = NA, color = "white",
            linewidth = 1, inherit.aes = FALSE) +
    geom_sf_text(data = cz_centroids, aes(label = cz_label),
                 size = 4, color = "black", inherit.aes = FALSE) +
    geom_sf(data = cities_transformed, fill = "yellow", color = "orange",
            aes(size = pop), shape = 23) +
    labs(title = paste0("CZ Map: ", get_state(state_name))) +
    theme(
      plot.title      = element_text(face = "bold", size = 13, hjust = 0.5),
      legend.position = "none"
    )

  print(fig_state)
  invisible(list(fig = fig_state))
}

#==============================================================================
# STEP 8: Fiscal Channel Decomposition (significant states)
# For highlighted driver CZs: decomposes spending change into IG vs local revenue
# Connects to regressions.qmd mechanism: does the wage→spending link run through
# local tax base (Amplification) or is it absorbed/inverted by IG transfers?
#==============================================================================

show_fiscal_decomposition <- function(cz_ids, state_code, decomp_bars = FALSE,
                                      add_wages = FALSE, first_diff = FALSE,
                                      wage_lag = 0, highlight_czs = NULL) {

  state_nm <- get_state(state_code)

  cz_lbl <- cz_names %>%
    mutate(cz_label = ifelse(!is.na(descriptor) & descriptor != "",
                             paste0(descriptor, " (CZ", cz_id, ")"),
                             paste0("CZ ", cz_id))) %>%
    select(cz_id, cz_label)

  # Facet order: ascending mean spending per pupil (lowest spender first)
  cz_spend_order <- df_ivs %>%
    filter(unit %in% cz_ids) %>%
    group_by(unit) %>%
    summarise(mean_spend_pp = mean(real_Elem_Educ_Total_Exp / Enrollment,
                                   na.rm = TRUE), .groups = "drop") %>%
    left_join(cz_lbl, by = c("unit" = "cz_id")) %>%
    arrange(mean_spend_pp) %>%
    pull(cz_label)

  # One row per highlighted CZ for facet border geom_rect
  highlight_border_data <- if (!is.null(highlight_czs)) {
    cz_lbl %>%
      filter(as.character(cz_id) %in% as.character(highlight_czs)) %>%
      mutate(cz_label = factor(cz_label, levels = cz_spend_order))
  } else {
    NULL
  }
  
  # Compute local revenue per pupil (Total Exp - IG Revenue), then log
  ts_data <- df_ivs %>%
    filter(unit %in% cz_ids) %>%
    left_join(cz_lbl, by = c("unit" = "cz_id")) %>%
    mutate(
      real_local_rev_pp     = (real_Elem_Educ_Total_Exp - real_Total_IG_Revenue) / Enrollment,
      log_real_local_rev_pp = log(pmax(real_local_rev_pp, 1))   # guard against negatives
    ) %>%
    select(year, unit, cz_label,
           `Wages`           = log_weighted_annual_avg_wkly_wage,
           `Total Spending`  = log_real_Elem_Educ_Total_Exp_pp,
           `IG Revenue`      = log_real_Total_IG_Revenue_pp,
           `Local Revenue`   = log_real_local_rev_pp) %>%
    pivot_longer(c(`Wages`, `Total Spending`, `IG Revenue`, `Local Revenue`),
                 names_to = "variable", values_to = "value") %>%
    mutate(variable = factor(variable,
                             levels = c("Wages", "Total Spending",
                                        "IG Revenue", "Local Revenue"))) %>%
    group_by(unit, variable) %>%
    mutate(value = value - value[year == min(year)]) %>%
    ungroup() %>%
    mutate(cz_label = factor(cz_label, levels = cz_spend_order))

  # Colour: Wages = black, Spending = dark blue, IG = orange, Local = green
  series_cols <- c(
    "Wages"          = "black",
    "Total Spending" = "#2166ac",
    "IG Revenue"     = "#d95f02",
    "Local Revenue"  = "#1b7837"
  )

  p <- ggplot(ts_data, aes(x = year, y = value,
                            color = variable, group = interaction(unit, variable))) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey55") +
    geom_line(linewidth = 1) +
    geom_point(size = 1.5) +
    scale_color_manual(values = series_cols, name = NULL) +
    facet_wrap(~cz_label, scales = "free_y", ncol = min(length(cz_ids), 3)) +
    labs(
      title    = paste0("Fiscal Channel Decomposition \u2014 ", state_nm),
      subtitle = paste0(
        "All series indexed to base year = 0 (log change). ",
        "If IG falls when wages rise \u2192 equalization channel. ",
        "If Local Revenue tracks wages \u2192 amplification via local tax base."
      ),
      x = "Year", y = "Log change from base year"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      plot.title       = element_text(face = "bold", size = 12),
      plot.subtitle    = element_text(size = 8.5, color = "grey30"),
      strip.text       = element_text(face = "bold"),
      strip.background = element_rect(fill = "grey95", color = NA),
      legend.position  = "bottom"
    ) +
    guides(color = guide_legend(nrow = 1)) +
    { if (!is.null(highlight_border_data))
        geom_rect(
          data        = highlight_border_data,
          aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
          fill        = NA, color = "black", linewidth = 0.9,
          inherit.aes = FALSE
        )
    }

  #print(p)

  # ── Optional: spending change decomposed into IG vs local revenue bars ─────
  if (decomp_bars) {

    # Work in real $ per pupil (levels) so bars are exactly additive:
    # Δspending_pp = Δig_pp + Δlocal_pp  (no approximation)
    decomp_base <- df_ivs %>%
      filter(unit %in% cz_ids) %>%
      left_join(cz_lbl, by = c("unit" = "cz_id")) %>%
      mutate(
        spending_pp = exp(log_real_Elem_Educ_Total_Exp_pp),
        ig_pp       = exp(log_real_Total_IG_Revenue_pp),
        local_pp    = spending_pp - ig_pp,
        cz_label    = factor(cz_label, levels = cz_spend_order)
      ) %>%
      arrange(unit, year)

    if (first_diff) {
      # Year-on-year changes: shows within-period co-movement
      decomp_data <- decomp_base %>%
        group_by(unit) %>%
        mutate(
          delta_spending = spending_pp - lag(spending_pp),
          delta_ig       = ig_pp       - lag(ig_pp),
          delta_local    = local_pp    - lag(local_pp)
        ) %>%
        filter(!is.na(delta_spending)) %>%
        ungroup()
    } else {
      # Cumulative change from base year
      decomp_data <- decomp_base %>%
        group_by(unit) %>%
        mutate(
          delta_spending = spending_pp - spending_pp[year == min(year)],
          delta_ig       = ig_pp       - ig_pp[year == min(year)],
          delta_local    = local_pp    - local_pp[year == min(year)]
        ) %>%
        ungroup()
    }

    # Long format for stacked bars; split positive/negative for correct stacking
    decomp_long <- decomp_data %>%
      pivot_longer(c(delta_ig, delta_local), names_to = "component", values_to = "delta") %>%
      mutate(component = factor(
        recode(component, "delta_ig" = "IG Revenue", "delta_local" = "Local Revenue"),
        levels = c("IG Revenue", "Local Revenue")
      ))

    comp_cols <- c("IG Revenue" = "#d95f02", "Local Revenue" = "#1b7837")

    # ── Wage–spending directional agreement (background shading) ─────────────
    # Compute year-on-year wage growth (with optional lag), join to spending
    # direction, then shade each year green (same direction) or red (opposite).
    # Fixed fill colors on geom_rect avoid any conflict with the bar fill scale.
    shade_data <- NULL
    if (add_wages) {
      wage_dir <- df_ivs %>%
        filter(unit %in% cz_ids) %>%
        left_join(cz_lbl, by = c("unit" = "cz_id")) %>%
        select(year, unit, cz_label, wage = weighted_annual_avg_wkly_wage) %>%
        arrange(unit, year) %>%
        group_by(unit) %>%
        mutate(wage_yoy = (wage - lag(wage)) / lag(wage) * 100) %>%
        filter(!is.na(wage_yoy)) %>%
        mutate(year = year + wage_lag) %>%   # shift forward to align with response
        ungroup()

      shade_data <- wage_dir %>%
        inner_join(
          decomp_data %>% select(unit, cz_label, year, delta_spending),
          by = c("unit", "cz_label", "year")
        ) %>%
        mutate(
          same_dir = sign(wage_yoy) == sign(delta_spending),
          # Overall correlation per CZ (annotated in facet)
          .by = unit
        ) %>%
        group_by(unit) %>%
        mutate(r_label = paste0("r = ", round(
                  cor(wage_yoy, delta_spending, use = "complete.obs"), 2))) %>%
        ungroup()
    }

    # ── Build plot ────────────────────────────────────────────────────────────
    p_decomp <- ggplot() +
      # Background shading: fixed fills (no aes → no scale conflict with bars)
      { if (!is.null(shade_data))
          list(
            geom_rect(data = shade_data %>% filter( same_dir),
                      aes(xmin = year - 0.5, xmax = year + 0.5),
                      ymin = -Inf, ymax = Inf,
                      fill = "#2ca25f", alpha = 0.18, inherit.aes = FALSE),
            geom_rect(data = shade_data %>% filter(!same_dir),
                      aes(xmin = year - 0.5, xmax = year + 0.5),
                      ymin = -Inf, ymax = Inf,
                      fill = "#de2d26", alpha = 0.18, inherit.aes = FALSE)
          )
      } +
      # Positive bar contributions stack upward
      geom_col(
        data     = decomp_long %>% filter(delta >= 0),
        aes(x = year, y = delta, fill = component),
        position = "stack", width = 0.75, alpha = 0.85
      ) +
      # Negative bar contributions stack downward
      geom_col(
        data     = decomp_long %>% filter(delta < 0),
        aes(x = year, y = delta, fill = component),
        position = "stack", width = 0.75, alpha = 0.85
      ) +
      # Total spending change line
      geom_line(data  = decomp_data,
                aes(x = year, y = delta_spending),
                color = "black", linewidth = 1.1) +
      geom_point(data = decomp_data,
                 aes(x = year, y = delta_spending),
                 color = "black", size = 1.8) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "grey55") +
      # Correlation annotation per facet
      { if (!is.null(shade_data))
          geom_text(
            data  = shade_data %>% select(cz_label, r_label) %>% distinct(),
            aes(label = r_label),
            x = Inf, y = Inf, hjust = 1.1, vjust = 1.5,
            size = 3.2, fontface = "bold", color = "grey30",
            inherit.aes = FALSE
          )
      } +
      scale_fill_manual(values = comp_cols, name = "Component") +
      { if (!is.null(highlight_border_data))
          geom_rect(
            data        = highlight_border_data,
            aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
            fill        = NA, color = "black", linewidth = 0.9,
            inherit.aes = FALSE
          )
      } +
      facet_wrap(~cz_label, scales = "free_y", ncol = min(length(cz_ids), 3)) +
      labs(
        title    = paste0("Spending Change Decomposition \u2014 ", state_nm),
        subtitle = paste0(
          "Bars: IG (orange) + Local Revenue (green) = total spending change (black line). ",
          if (!is.null(shade_data)) paste0(
            "Background: green = wages & spending moved same direction; ",
            " red = opposite. ",
            if (wage_lag > 0) paste0("Wage direction from t\u2212", wage_lag, ". ")
            else ""
          ) else "",
          "Annotation: Pearson r(wage growth, \u0394spending)."
        ),
        x = "Year",
        y = if (first_diff) "Year-on-year change (real $ per pupil)"
            else            "Change from base year (real $ per pupil)"
      ) +
      theme_minimal(base_size = 11) +
      theme(
        plot.title       = element_text(face = "bold", size = 12),
        plot.subtitle    = element_text(size = 8.5, color = "grey30"),
        strip.text       = element_text(face = "bold"),
        strip.background = element_rect(fill = "grey95", color = NA),
        legend.position  = "bottom"
      )

    print(p_decomp)
    invisible(list(ts = p, decomp = p_decomp))

  } else {
    invisible(p)
  }
}

#==============================================================================
# STEP 9: Null-Effect Diagnosis (non-significant states)
# Tests two competing explanations for a null IV result:
#   (A) Low wage variation across CZs → weak instrument → can't detect effect
#   (B) Equalization channel active → IG transfers offset wage changes →
#       no net spending effect despite wage variation
# Connects to regressions.qmd: states outside the significant-10 may be
# insulated or equalizing, not simply inert.
#==============================================================================

diagnose_no_effect <- function(state_code, exclude_czs = NULL) {

  state_nm <- get_state(state_code)

  # State CZ data (use the all-states leverage object)
  # cz_label is already present from cz_conditional_all_states
  state_czs <- cz_conditional_all_states %>%
    filter(state == state_code) %>%
    { if (!is.null(exclude_czs)) {
        filter(., !(cz_id %in% exclude_czs))
      } else {
        .
      }
    }

  # National reference: SD of wage growth across CZs, by state
  wage_sd_all <- cz_conditional_all_states %>%
   { if (!is.null(exclude_czs)) {
        filter(., !(cz_id %in% exclude_czs))
      } else {
        .
      }
    } %>%
    group_by(state) %>%
    summarize(wage_sd = sd(wage_growth_uncond, na.rm = TRUE), .groups = "drop")

  this_sd     <- wage_sd_all %>% filter(state == state_code) %>% pull(wage_sd)
  median_sd   <- median(wage_sd_all$wage_sd, na.rm = TRUE)
  cor_wage_ig <- cor(state_czs$wage_growth_uncond, state_czs$ig_revenue_growth,
                     use = "complete.obs")

  # ── Diagnosis text ────────────────────────────────────────────────────────
  cat("\n=================================================================\n")
  cat("NULL-EFFECT DIAGNOSIS:", toupper(state_nm), "\n")
  cat("=================================================================\n")
  cat(sprintf("Cross-CZ wage growth SD:      %.3f  (national median: %.3f)\n",
              this_sd, median_sd))
  cat(sprintf("Cor(wage growth, IG growth):  %.3f\n", cor_wage_ig))
  if (this_sd < 0.6 * median_sd) {
    cat(">> VERDICT: LOW WAGE VARIATION — instrument likely weak; null result reflects lack of signal.\n")
  } else if (cor_wage_ig < -0.25) {
    cat(">> VERDICT: EQUALIZATION CHANNEL ACTIVE — IG transfers offset wage gains; spending insulated.\n")
  } else if (abs(cor_wage_ig) < 0.15) {
    cat(">> VERDICT: INSULATION — spending buffered from wages; IG neither equalizes nor amplifies.\n")
  } else {
    cat(">> VERDICT: MIXED — check panels for CZ-level patterns.\n")
  }
  cat("=================================================================\n\n")

  # ── Panel A: Wage variation — this state vs all others ───────────────────
  all_czs <- cz_conditional_all_states %>%
    mutate(group = if_else(state == state_code, state_nm, "All other states")) %>%
    { if (!is.null(exclude_czs)) {
        filter(., !(cz_id %in% exclude_czs))
      } else {
        .
      }
    } 

  p_a <- ggplot(all_czs, aes(x = wage_growth_uncond, fill = group)) +
    geom_density(alpha = 0.55, linewidth = 0.4) +
    scale_fill_manual(values = c("All other states" = "grey70",
                                 setNames("#d7191c", state_nm)),
                      name = NULL) +
    #geom_vline(xintercept = 0, linetype = "dashed", color = "grey40") +
    labs(
      title    = "A.  Cross-CZ Wage Growth Distribution",
      subtitle = "Narrow distribution \u2192 weak instrument; limited scope for IV to detect an effect.",
      x = "CZ wage growth (log points)", y = "Density"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11),
          legend.position = "bottom")

  print(select(state_czs, contains("cz_label")))
  # ── Panel B: Wage growth vs. IG growth (equalization test) ───────────────
  p_b <- ggplot(state_czs, aes(x = wage_growth_uncond, y = ig_revenue_growth)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    #geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(color = "#2c7bb6", size = 2.5, alpha = 0.75) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.9) +
    geom_text_repel(aes(label = cz_label), size = 2.4, max.overlaps = 8,
                    segment.color = "grey60") +
    annotate("text", x = Inf, y = Inf,
             label = sprintf("r = %.2f", cor_wage_ig),
             hjust = 1.1, vjust = 1.5, size = 3.5, fontface = "bold") +
    labs(
      title    = "B.  Wage Growth vs. IG Revenue Growth (Equalization Test)",
      subtitle = "Negative slope \u2192 IG actively offsets wage gains (equalization). Flat \u2192 insulation.",
      x = "CZ wage growth (log points)", y = "CZ IG revenue growth (log points)"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11))

  # ── Panel C: Wage growth vs. total spending growth (confirms null) ────────
  cor_wage_spend <- cor(state_czs$wage_growth_uncond, state_czs$spending_growth_uncond,
                        use = "complete.obs")

  p_c <- ggplot(state_czs, aes(x = wage_growth_uncond, y = spending_growth_uncond)) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    #geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    geom_point(color = "grey50", size = 2.5, alpha = 0.75) +
    geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 0.9) +
    geom_text_repel(aes(label = cz_label), size = 2.4, max.overlaps = 8,
                    segment.color = "grey60") +
    annotate("text", x = Inf, y = Inf,
             label = sprintf("r = %.2f", cor_wage_spend),
             hjust = 1.1, vjust = 1.5, size = 3.5, fontface = "bold") +
    labs(
      title    = "C.  Wage Growth vs. Total Spending Growth",
      subtitle = "Confirms null: flat slope means wage variation does not translate to spending differences.",
      x = "CZ wage growth (log points)", y = "CZ spending growth (log points)"
    ) +
    theme_minimal(base_size = 11) +
    theme(plot.title = element_text(face = "bold", size = 11))

  fig <- (p_a | p_b) / p_c +
    plot_annotation(
      title    = paste0("Null-Effect Diagnosis \u2014 ", state_nm),
      subtitle = paste0(
        "Two explanations tested: (A) insufficient wage variation to identify an effect; ",
        "(B) equalization channel absorbs wage changes before they reach spending."
      ),
      theme = theme(
        plot.title    = element_text(face = "bold", size = 13),
        plot.subtitle = element_text(size = 9, color = "grey30", lineheight = 1.3)
      )
    )

  print(fig)
  invisible(fig)
}

