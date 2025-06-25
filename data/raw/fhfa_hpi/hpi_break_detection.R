# Break Detection on Property Prices
library(here)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(gets)
library(getspanel)
library(doParallel)
# Set unit id to fips or commuting zones
unit_id = "cz_id"
source(here("code/source_code/pull_data.R"))


# Property Prices
print("running isat...")

cl <- makeCluster(2)
registerDoParallel(cl)

# , "jsis", "fesis", "cfesis"
# Loop over each break type
mods <- foreach(ar_spec = c(0,1), .combine = rbind, .packages = c('tidyverse', 'getspanel')) %:% 
    foreach(end_year = c(2019, 2021), .combine = rbind) %dopar% {
      temp <- mines_cz %>% 
        filter(year <= end_year)
      # Create a list of arguments to pass
      args <- list(
        data = data.frame(temp),
        formula = as.formula("log_hpi ~ log_weighted_annual_avg_wkly_wage + 
                            l1_log_weighted_annual_avg_wkly_wage + 
                            l2_log_weighted_annual_avg_wkly_wage"),
        index = c("unit", "year"),
        effect = "twoways",
        csis = TRUE,
        t.pval = 0.01
      )
  
    # Add the current break_type with value TRUE
    #args[[break_type]] <- TRUE
    args[["ar"]] <- ar_spec
    
    # Call the function with the constructed arguments
    is <- do.call(isatpanel, args)
    model = tibble(source = "log_hpi ~ log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage", 
           p_val = 0.01, 
           is = list(is),
           break_spec = "csis",
           ar = ar_spec,
           time_span = paste0(as.character(min(temp$year), ":", max(temp$year))),
           max_year = end_year)
    }

print(nrow(mods))
stopCluster(cl)
saveRDS(mods, here("output/prop_price_breaks_csis.RDS"))
print('Saved and done!')

#temp <- readRDS(here("output/prop_price_breaks.RDS")) %>% slice(1) %>% pull(is) %>% first

# fun_list <- c(log_hpi ~ log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + log_real_gdp_total | unit + year,
#               
#               gr_hpi ~ gr_weighted_annual_avg_wkly_wage + l1_gr_weighted_annual_avg_wkly_wage + l2_gr_weighted_annual_avg_wkly_wage | unit + year,
#               
#               log_real_Elem_Educ_Total_Exp ~ log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + log_real_gdp_total + l1_log_real_gdp_total + l2_log_real_gdp_total | unit + year,
#               
#               diff_log_real_Elem_Educ_Total_Exp ~ gr_weighted_annual_avg_wkly_wage + l1_gr_weighted_annual_avg_wkly_wage + l2_gr_weighted_annual_avg_wkly_wage + diff_log_real_gdp_total + l1_diff_log_real_gdp_total + l2_diff_log_real_gdp_total | unit + year,
#               
#               log_real_Elem_Educ_Total_Exp_pp ~ log_weighted_annual_avg_wkly_wage + l1_log_weighted_annual_avg_wkly_wage + l2_log_weighted_annual_avg_wkly_wage + log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc | unit + year,
#               
#               diff_log_real_Elem_Educ_Total_Exp_pp ~ gr_weighted_annual_avg_wkly_wage + l1_gr_weighted_annual_avg_wkly_wage + l2_gr_weighted_annual_avg_wkly_wage + diff_log_real_gdp_total_pc + l1_diff_log_real_gdp_total_pc + l2_diff_log_real_gdp_total_pc | unit + year)
# 
# run_model(fun_list, mines_cz) %>% etable()
# 
# 
# # General-to-Specific model selection using fixest
# gets_fixest <- function(dep_var, indep_vars, data, fe = NULL, pval_cutoff = 0.1) {
#   remaining_vars <- indep_vars
#   
#   repeat {
#     # Build formula
#     rhs <- paste(remaining_vars, collapse = " + ")
#     formula_str <- as.formula(paste(dep_var, "~", rhs, if (!is.null(fe)) paste("|", fe) else ""))
#     
#     # Estimate model
#     model <- feols(formula_str, data = data)
#     coefs <- summary(model)$coeftable
#     
#     # Filter out only the coefficients from the explanatory variables
#     pvals <- coefs[, "Pr(>|t|)"]
#     pvals <- pvals[names(pvals) %in% remaining_vars]
#     
#     # Stop if all variables are below the cutoff or no variables remain
#     if (length(pvals) == 0 || all(pvals < pval_cutoff)) break
#     
#     # Remove the variable with the highest p-value
#     worst_var <- names(which.max(pvals))
#     remaining_vars <- setdiff(remaining_vars, worst_var)
#   }
#   
#   # Return the final reduced model
#   final_formula <- paste(dep_var, "~", paste(remaining_vars, collapse = " + "), if (!is.null(fe)) paste("|", fe) else "")
#   return(final_formula)
# }
# 
# # Example usage
# # Replace 'your_data' with your actual data frame name
# final_model <- gets_fixest(
#   dep_var = "log_real_Elem_Educ_Total_Exp_pp",
#   indep_vars = c("log_weighted_annual_avg_wkly_wage",
#                  "l1_log_weighted_annual_avg_wkly_wage",
#                  "l2_log_weighted_annual_avg_wkly_wage", 
#                  "log_real_gdp_total_pc", 
#                  "l1_log_real_gdp_total_pc",
#                  "l2_log_real_gdp_total_pc", 
#                  "log_real_Property_Tax_pp",
#                  #"log_real_Total_State_IG_Revenue_pp",
#                  #"log_real_Total_Fed_IG_Revenue_pp",
#                  "log_hpi"),  # replace with your actual variable names
#   data = mines_cz,
#   fe = "unit + year",        # optional fixed effects
#   pval_cutoff = 0.1         # significance threshold
# )
# 
# # View results
# # At a 10% significance level, we retain a 2-timeperiod lag of real_gdp_pc, contemporaneous wage (not likely), property_tax_pp, state_revenue_pp, hpi
# feols(as.formula(final_model), data = mines_cz) %>% summary()
# 
# 
# # I want to allow the coefficient to vary on the weekly wage treatment variable
# feols(as.formula(gsub("log_weighted_annual_avg_wkly_wage", "i(share_own_discrete, log_weighted_annual_avg_wkly_wage)", final_model)), data = mines_cz)
