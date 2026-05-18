################################################################################
################################################################################
##### RUNS CENTRAL ANALYSIS USING COUNTIES INSTEAD OF COMMUTING ZONES 

##### Runs independently from regressions.qmd to produce its own appendix file to 
##### avoid any conflicts between regressions.qmd and the following script objects
##### Produces Tables 1-2, 4-5, 8-9 and Figures 6-7, 9

################################################################################
################################################################################
appendix_file <- "appendix_fips_rob_check.tex" 
library(here)
source(here('code/source_code/export_appendix_support_funs.R'))
append_table_to_appendix("\\FloatBarrier")
append_table_to_appendix("\\section{County-level Sample}\\label{si:fips_rob_check}")
append_table_to_appendix("The following section executes the analysis presented in the main text using counties as panel units, rather than commuting zones. The section includes descriptive statistics, data coverage, descriptive regression results, baseline IV regressions, IV regressions with growth rate sub-samples, and a state-by-state estimation.")

library(tidyverse)       
library(fixest)          
library(here)            
library(plm)            
library(stargazer)      
library(kableExtra)      
library(ivreg)       
library(broom)          
library(ggrepel)    
library(RColorBrewer)
library(readxl)
library(viridis) 
library(conflicted)  
extrafont::loadfonts(quiet = TRUE)
source(here('code/source_code/useful_functions.R'))
source(here('code/source_code/dicts.R'))
source(here('code/reg_forms.R'))
conflict_prefer_all("dplyr", quiet = TRUE)
yes_contemp_ss = TRUE

source(here('code/source_code/fit_stats.R'))

setFixest_etable(fitstat = ~ . + ivf1 + ivf1.p + iv_fstat + iv_fstat_p + wh + wh.p + ivwaldall + ivwaldall.p + kpr, se.below = TRUE)

iv_fitstats = c("n", "r2", "wr2","iv_fstat", "iv_fstat_p", "wh", "wh.p", "ivwaldall", "ivwaldall.p", "ivf1", "ivf1.p") #"partial_f_wald", "partial_f_wald_p")#, "partial_f_effective") #"iv_fstat_marg", "iv_fstat_marg_p", "wh_manual", "wh_manual_p", "sargan_manual", "sargan_manual_p")

default_iv_note = c("", "Note that the R2 and Adjusted R2 values for second-stage regressions are irrelevant information here. I have not yet figured out how to suppress them from the regression tables but will do so for the final version.")

plot_annotation_theme = theme(
  text = ggplot2::element_text(
    family = "Latin Modern Roman"),
  plot.title = element_text(family = "LMRoman10-Bold",  # Use the exact bold font name
                            size = 16, 
                            face = "bold"),
  plot.subtitle = element_text(size = 12))

unit_id = "fips"
new_ss_calculation = FALSE
latex_tables = TRUE

data_cache <- here("output/cache/pull_data_cache_fips.RDS")
if (file.exists(data_cache)) {
  mines_fips <- readRDS(data_cache)
} else {
  source(here("code/source_code/pull_data.R"))
  mines_fips <- mines_cz
  rm(mines_cz)
  saveRDS(mines_fips, data_cache)
}

mean_df <- mines_fips %>% 
  group_by(year) %>%
  summarise(real_Total_Educ_Total_Exp = mean(real_Total_Educ_Total_Exp, na.rm = TRUE),
            real_Total_Educ_Total_Exp_pp = mean(real_Total_Educ_Total_Exp_pp, na.rm = TRUE),
            real_log_Total_Educ_Total_Exp = mean(log_real_Total_Educ_Total_Exp, na.rm = TRUE),
            real_log_Total_Educ_Total_Exp_pp = mean(log_real_Total_Educ_Total_Exp_pp, na.rm = TRUE))

stopifnot(is.pbalanced(mines_fips, index = c("fips", "year")))


# Create pdata for order of integration data:
pdata_mines_fips <- pdata.frame(arrange(mines_fips, unit, year), index = c("unit", "year"))
pdata_mines_fips_diff <- pdata.frame(arrange(filter(mines_fips, year != 2001), unit, year), index = c("unit", "year"))
log_transform = TRUE

# Function for testing stationarity
test_panel_stationarity <- function(pdata, var_name, exog = "intercept", test_type = "ips") {
  #print(var_name)
  if(var_name == "hpi"){
    pdata <- pdata %>% group_by(unit) %>% filter(!all(is.na(hpi))) %>% ungroup
  }
  result <- purtest(pdata[[var_name]],
                    test = test_type,
                    exo = exog,  # use "trend" if variables have trends
                    lags = "AIC",
                    pmax = 4)
  #print(result)
  
  return(list(
    statistic = result$statistic$statistic,
    p_value = result$statistic$p.value,
    is_stationary = result$statistic$p.value < 0.05
  ))
}

# Determine integration order
vars_to_test <- c("Enrollment", "pop_total", "real_Elem_Educ_Total_Exp_pp",
                  "real_Property_Tax_pp", "real_Total_IG_Revenue_pp",
                  "real_Total_State_IG_Revenue_pp", "real_gdp_total_pc", 
                  "real_gdp_priv_ind_pc")#, "hpi")
if(log_transform){
  vars_to_test <- paste0("log_", vars_to_test)
}

# Test levels
level_results <- lapply(vars_to_test, function(v) {
  test_panel_stationarity(pdata_mines_fips, v)
})
names(level_results) <- vars_to_test

integration_orders <- sapply(vars_to_test, function(v) {
  if (level_results[[v]]$is_stationary) {
    return(list(order = "I(0)", diff_p_value = ""))
  } else {
    # Test first difference
    diff_test <- test_panel_stationarity(pdata_mines_fips_diff, paste0("diff_", v), exo = "intercept")
    if (diff_test$is_stationary) {
      return(list(order = "I(1)", diff_p_value = round(diff_test$p_value, 4)))
    } else {
      # # Test second difference if needed
      # # Assuming you have second differences with prefix "diff2_"
      # diff2_test <- test_panel_stationarity(pdata_mines_fips_diff2, paste0("diff2_", v), exo = "intercept")
      # return(list(order = "I(2)", diff_p_value = round(diff2_test$p_value, 3)))
      return()
    }
  }
}, simplify = FALSE)

# Display results with cleaner structure
results_df <- tibble(
  Variable = names(integration_orders),
  `Order of Integration` = sapply(integration_orders, function(x) x$order),
  `I(0) test p-value` = sapply(level_results, function(x) ifelse(round(x$p_value, 4) == 0, "<0.0001",  round(x$p_value, 3))),
  `I(1) test p-value` = sapply(integration_orders, function(x) ifelse(x$diff_p_value == 0, "<0.0001", ""))
)

# Variables of interest for descriptive statistics
vars <- c("Property_Tax", "Total_IG_Revenue", "Total_Fed_IG_Revenue", "Total_State_IG_Revenue", "Total_Educ_Total_Exp", "Elem_Educ_Total_Exp") # Total_Revenu

tbl <- mines_fips %>%
  select(Enrollment, pop_total, real_Elem_Educ_Total_Exp_pp, real_Property_Tax_pp, real_Total_IG_Revenue_pp,# real_Total_Fed_IG_Revenue_pp, 
         real_Total_State_IG_Revenue_pp, 
         real_gdp_total_pc, real_gdp_priv_ind_pc, #real_gdp_o_g_mining_quarr_21_pc, 
         hpi, pct_black, pct_white, pct_hispanic) %>% 
  #real_Elem_Educ_Total_Exp, real_Property_Tax, real_Total_IG_Revenue, real_Total_Fed_IG_Revenue, 
  #real_Total_State_IG_Revenue, real_gdp_total, real_gdp_priv_ind, real_gdp_o_g_mining_quarr_21, total_active_n, total_active_prod) %>% 
  mutate(across(c(Enrollment, pop_total), ~./1000)) %>%  # gdp_govt, 
  data.frame() %>%
  stargazer(digits = 2, digits.extra = 3, 
            type = ifelse(latex_tables, "latex", "text"), covariate.labels = c(
              "Enrollment",
              "Population",
              "Elementary Expenditure per pupil",
              "Property Tax per pupil",
              "Intergovernmental (IG) Revenue per pupil", 
              #"Federal IG Revenue per pupil", 
              "State IG Revenue per pupil",
              "GDP per capita",
              "GDP pc - Private Industry",
              "House Price Index",
              "\\% Black",
              "\\% White", 
              "\\% Hispanic"),
            header = FALSE, label = "tbl_desc_stats_fips", title = "Descriptive Statistics")

append_table_to_appendix(tbl)
rm(tbl)

integration_table <- results_df %>% 
  mutate(Variable = case_when(Variable == "log_Enrollment" ~ "Enrollment",
                              Variable == "log_pop_total" ~ "Population",
                              Variable == "log_real_Elem_Educ_Total_Exp_pp" ~ "Elementary Expenditure per pupil",
                              Variable == "log_real_Property_Tax_pp" ~ "Property Tax per pupil",
                              Variable == "log_real_Total_IG_Revenue_pp" ~ "Intergovernmental (IG) Revenue per pupil", 
                              Variable == "log_real_Total_State_IG_Revenue_pp" ~ "State IG Revenue per pupil",
                              Variable == "log_real_gdp_total_pc" ~ "GDP per capita",
                              Variable == "log_real_gdp_priv_ind_pc" ~ "GDP pc - Private Industry")) %>% 
  kable(.,format = ifelse(latex_tables, "latex", "simple"),
        caption = "Order of Integration",
        booktabs = TRUE,  # This is key for stargazer-like lines
        align = c("l", "c", "c", "c"),  # left, center, center, center
        col.names = c("Variable", "Order of Integration", "I(0) test p-value", "I(1) test p-value"), label = "tbl_order_of_integration_fips") %>%
  footnote(general = "Order of integration determined using the Im-Pesaran-Shin (IPS) panel unit root test with intercept. Lag length selected via AIC with maximum of 4 lags. The null hypothesis is non-stationarity; rejection at the 5% level indicates stationarity. I(0) denotes stationarity in levels and I(1) denotes stationarity in first differences. All variables are log-transformed prior to testing to account for heteroskedasticity.",
           footnote_as_chunk = TRUE,
           threeparttable = TRUE)

integration_table

append_table_to_appendix(integration_table)
rm(integration_table, pdata_mines_fips, pdata_mines_fips_diff)

################################################################################
################################################################################
#################### DESCRIPTIVE REGRESSIONS
################################################################################
################################################################################
append_table_to_appendix("\\FloatBarrier")
append_table_to_appendix("\\subsection{Descriptive Regressions}")

baseline_sw <- "log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc, log_annual_avg_wkly_wage + l1_log_annual_avg_wkly_wage + l2_log_annual_avg_wkly_wage, log_hpi + l1_log_hpi + l2_log_hpi) + l1_log_real_Elem_Educ_Total_Exp_pp + log_real_Total_IG_Revenue_pp + log_Enrollment + pct_black + pct_hispanic"
# 
# baseline_sw_short <- "log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc, log_annual_avg_wkly_wage + l1_log_annual_avg_wkly_wage + l2_log_annual_avg_wkly_wage, log_hpi + l1_log_hpi + l2_log_hpi + l3_log_hpi + l1_log_real_Elem_Educ_Total_Exp_pp ) + log_real_Total_IG_Revenue_pp + log_Enrollment + pct_black + pct_hispanic"

baseline_full <- "log_real_Elem_Educ_Total_Exp_pp ~ log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + log_annual_avg_wkly_wage + l1_log_annual_avg_wkly_wage + l2_log_annual_avg_wkly_wage + log_hpi + l1_log_hpi + l2_log_hpi + l1_log_real_Elem_Educ_Total_Exp_pp + log_real_Total_IG_Revenue_pp + log_Enrollment + pct_black + pct_hispanic"

#baseline_gr_sw <- "diff_log_real_Elem_Educ_Total_Exp_pp ~ sw(diff_log_real_gdp_priv_ind_pc + l1_diff_log_real_gdp_priv_ind_pc + l2_diff_log_real_gdp_priv_ind_pc,  gr_annual_avg_wkly_wage + l1_gr_annual_avg_wkly_wage + l2_gr_annual_avg_wkly_wage, gr_hpi + l1_gr_hpi + l2_gr_hpi) + diff_log_real_Total_IG_Revenue_pp + diff_log_Enrollment + pct_black + pct_hispanic"

#baseline_gr_full <- "diff_log_real_Elem_Educ_Total_Exp_pp ~ diff_log_real_gdp_priv_ind_pc + l1_diff_log_real_gdp_priv_ind_pc + l2_diff_log_real_gdp_priv_ind_pc + gr_annual_avg_wkly_wage + l1_gr_annual_avg_wkly_wage + l2_gr_annual_avg_wkly_wage + gr_hpi + l1_gr_hpi + l2_gr_hpi + diff_log_real_Total_IG_Revenue_pp + diff_log_Enrollment + pct_black + pct_hispanic"

desc_levels_table <- c(c(run_model(paste0(baseline_sw, ' | unit + year'), mines_fips)),
                       c(run_model(paste0(baseline_full, ' | unit + year'), mines_fips))) %>% etable(adjustbox = "max width=\\textwidth", tex = latex_tables, caption = "Descriptive Results in Levels with County and Year FE", label = "tbl_desc_res_lev_fips")

append_table_to_appendix(desc_levels_table)

#desc_gr_table <- c(c(run_model(paste0(baseline_gr_sw, ' | unit + year'), mines_fips)),
 #                  c(run_model(paste0(baseline_gr_full, ' | unit + year'), mines_fips))) %>% etable(adjustbox = "max width=\\textwidth", tex = latex_tables, caption = "Descriptive Results in Growth Rates", label = "tbl_desc_res_gr_fips")

# Add to appendix
#append_table_to_appendix(desc_gr_table)

# Clean up
rm(desc_levels_table) #, desc_gr_table)
    
  
# STATE FE

m1 <- c(c(run_model(paste0(baseline_sw, ' | state + year'), mines_fips)),
        c(run_model(paste0(baseline_full, ' | state + year'), mines_fips))) %>% etable(adjustbox = "max width=\\textwidth", tex = latex_tables, caption = "Descriptive Results in Levels with State and Year FE", label = "tbl_desc_res_lev_sfe_fips")

append_table_to_appendix(m1)

# m2 <- c(c(run_model(paste0(baseline_gr_sw, ' | state + year'), mines_fips)),
#         c(run_model(paste0(baseline_gr_full, ' | state + year'), mines_fips))) %>% etable(adjustbox = "max width=\\textwidth", tex = latex_tables, caption = "Descriptive Results in Growth Rates", label = "tbl_desc_res_gr_sfe_fips")

#append_table_to_appendix(m2)

rm(m1) #, m2)

selected_twfe_models_state_share_interaction =
  c(log_real_Elem_Educ_Total_Exp_pp ~ sw(state_share*(log_annual_avg_wkly_wage + l1_log_annual_avg_wkly_wage + l2_log_annual_avg_wkly_wage), state_share*(log_hpi + l1_log_hpi + l2_log_hpi)) + log_real_Total_Fed_IG_Revenue_pp + log_Enrollment + pct_black + pct_hispanic | unit + year,
    log_real_Elem_Educ_Total_Exp_pp ~ sw(state_share*(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc), state_share*(log_annual_avg_wkly_wage + l1_log_annual_avg_wkly_wage + l2_log_annual_avg_wkly_wage), state_share*(log_hpi + l1_log_hpi + l2_log_hpi)) + log_real_Total_Fed_IG_Revenue_pp + log_Enrollment  + pct_black + pct_hispanic | unit + year)

m1 <- run_model(selected_twfe_models_state_share_interaction, mines_fips) %>% etable(adjustbox = "max width=\\textwidth", tex = latex_tables, order = c("GDP", "Wage", "House Price"), caption = "Descriptive Results with Funding Source Interaction Effects")

#append_table_to_appendix(m1)

rm(m1)
#run_model(selected_twfe_models_levs_state_share_interaction[1:2], mines_fips) %>% etable(tex = latex_tables)
#run_model(selected_twfe_models_levs_state_share_interaction[3:4], mines_fips) %>% etable(tex = latex_tables)


# Draw the functions needed to calculate the SS instruments from the following script.
source(here("data/raw/QCEW/industry_shares_cleaning.R"))

################################################################################
################################################################################
#################### COMPUTE SS INSTRUMENT
################################################################################
################################################################################

if(new_ss_calculation){
  ss_shares <- compute_shares(source = "QCEW", base_year = 2004, unit_id = "fips", flat = FALSE)

  coverage <- ss_shares %>% select(coverage_2digit_naics, coverage_3digit_naics) %>% distinct

  ss_temp <- compute_ss(ss_shares)

  saveRDS(ss_temp, here("code/ss_cache_manual/ss_temp_fips.RDS"))
  saveRDS(coverage, here("code/ss_cache_manual/coverage_ss_fips.RDS"))
}else{
  ss_temp <- readRDS(here("code/ss_cache_manual/ss_temp_fips.RDS"))
  coverage <- readRDS(here("code/ss_cache_manual/coverage_ss_fips.RDS"))
}

probs <- c(0.05, 0.25, 0.75, 0.95)
percentiles_coverage <- quantile(
  coverage$coverage_2digit_naics,
  probs = probs,
  na.rm = TRUE
)

# convert to data frame for plotting
percentile_df <- data.frame(
  x = percentiles_coverage,
  label = paste0(names(percentiles_coverage))
)

p <- coverage %>%
  ggplot() +
  geom_histogram(aes(x = coverage_3digit_naics, fill = "3-digit NAICS Codes"), alpha = 0.8, bins = 30) +
  geom_histogram(aes(x = coverage_2digit_naics, fill = "2-digit NAICS Codes"), alpha = 0.8, bins = 30) +
  geom_vline(data = percentile_df,
             aes(xintercept = x),
             linetype = "dashed", color = "black", linewidth= 0.3) +
  geom_text(data = percentile_df,
            aes(x = x, y = 80, label = label),
            vjust = -0.5, hjust = 1,
            size = 2.5) +
  labs(x = "% Coverage of County's Employed by NAICS sub-categorization",
       y = "No. Counties",
       title = str_wrap("Data Coverage of Industry-level Employment as Share of Total Reported Employed", width = str_wrap_title),
       subtitle = str_wrap("Data coverage is calculated as the fraction of total local employment accounted for in the industry-specific employment values. Percentage labels represent the proportion of counties (percentiles) falling below a coverage value using the 2-digit NAICS sectoral classification of industries.", width = str_wrap_subtitle),
       fill = "NAICS Specificity") +
  common_theme +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Pastel1")

p
append_figure_to_appendix(
  plot_obj = p,
  filename = "fig_data_coverage_fips.png",
  caption = "Data Coverage",
  label = "si_fig:data_coverage_fips",
  width = "\\textwidth")
rm(p)

mines_fips <- mines_fips %>% 
  select(
    unit, year, state, 
    share_own,
    log_real_Elem_Educ_Total_Exp_pp, l1_log_real_Elem_Educ_Total_Exp_pp, 
    log_real_Total_IG_Revenue_pp,
    log_real_gdp_priv_ind, 
    log_real_gdp_priv_ind_pc, 
    log_Enrollment, 
    pct_black, pct_hispanic, 
    log_pop_school_age, pct_white,
    var_log_Elem_Educ_Total_Exp_pp, 
    max_log_Elem_Educ_Total_Exp_pp, 
    min_log_Elem_Educ_Total_Exp_pp, 
    log_diff_min_max_Elem_Educ_Total_Exp_pp, 
    log_hpi, l1_log_hpi, l2_log_hpi,
    pct_private_school_primary_acs5, 
    pct_private_school_primary_acs1, 
    diff_log_real_Elem_Educ_Total_Exp_pp, 
    diff_log_real_Total_IG_Revenue_pp, 
    diff_log_real_gdp_priv_ind_pc,  
    diff_log_Enrollment, 
    #fd_pct_black, 
    #fd_pct_hispanic, 
    log_annual_avg_wkly_wage, 
    l1_log_annual_avg_wkly_wage,
    l2_log_annual_avg_wkly_wage,
    gr_annual_avg_wkly_wage, 
    log_real_Property_Tax_pp,
    real_gdp_priv_ind, real_gdp_total, pop_total,
    "real_gdp_priv_ind_pc", "real_gdp_total_pc",
    "log_real_gdp_total","log_pop_total", "log_real_gdp_total_pc", "diff_log_real_gdp_priv_ind",
    "diff_log_real_gdp_total","diff_log_pop_total", "diff_log_real_gdp_total_pc",
    natl_gr_annual_avg_wkly_wage, 
    state_gr_annual_avg_wkly_wage,
    natl_log_annual_avg_wkly_wage)
  
rm(coverage, czs, czs_w_names, 
   educ_coal, educ_coal_statetrends, 
   educ_gdp, educ_gdp_lags, 
   educ_gdp_statetrends, educ_source, 
   integration_orders, mean_df, results_df, 
   selected_base_models, selected_iv_models, 
   selected_twfe_models, selected_twfe_models_levs, 
   selected_twfe_models_levs_state_share_interaction, 
   selected_twfe_models_levs_state_trend, 
   selected_twfe_models_state_share_interaction, shares_flat, p1, p2, p3)

if(new_ss_calculation){
  temp_new <- shares_flat_filled %>%
    select(!contains("share")) %>%
    filter(year <= 2005) %>%
    group_by(unit) %>%
    # Creates a mean employment level across 2001-2005 - 5 year mean to deal with missing data
    summarise(across(!year, ~mean(., na.rm = TRUE))) %>%
    ungroup  %>%
    rename(fips = unit)

  if(unit_id == "cz_id"){
    czs_new <- czs %>%
      #select(-old_fips) %>%
      rename(old_fips = fips) %>%
      mutate(fips = case_when(!is.na(getfips[old_fips]) ~ getfips[old_fips],
                              TRUE ~ old_fips),
             fips = as.character(fips))

    missing_fips <- czs_new %>%
      pull(fips) %>%
      unique %>%
      setdiff(unique(temp_new$fips), .)

    if (length(missing_fips) > 0) {
      message("Warning: Some FIPS codes are missing from czs.")
    }

    temp <- temp_new %>%
      select(fips, matches("^annual_avg_emplvl_\\d{2}$"), annual_avg_emplvl_10_filled) %>%
      left_join(., fipss_new, by = "fips", multiple = "first") %>%
      rename("unit" = fips) %>%
      select(-c(fips, old_fips, fips_population, fips_1990)) %>%
      relocate(unit)

  }else if(unit_id == "fips"){
    temp <- temp_new %>%
      rename(unit = fips)
  }

  shift_share_filled <- temp %>%
    group_by(unit) %>%
    summarise(across(everything(), ~sum(., na.rm = TRUE))) %>%  # , total_annual_wages)
    #annual_avg_wkly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE)) %>%
    ungroup %>%
    mutate(across(contains("avg_emplvl"), ~./annual_avg_emplvl_10_filled, .names = "share_{.col}")) %>%
    select(unit, contains('share')) %>%
    mutate(year = 2001) %>%
    complete(unit, year = 2001:2022) %>%
    group_by(unit) %>%
    fill(everything(), .direction = "updown") %>%
    ungroup

  assert_that(nrow(shift_share_filled) == n_distinct(shift_share_filled$unit) * n_distinct(shift_share_filled$year))

  ss_temp_fill <- shift_share_filled %>%
    select(unit, year, contains("share")) %>%
    left_join(., natl_rates, by = "year", relationship = "many-to-one") %>%
    rename(!!unit_id := unit) %>%
    select(-ends_with("10"))

  ss_temp_filled <- compute_ss(ss_temp_fill)
  saveRDS(ss_temp_filled, here("code/ss_cache_manual/ss_temp_filled_fips.RDS"))

}else{
  ss_temp_filled <- readRDS(here("code/ss_cache_manual/ss_temp_filled_fips.RDS"))
}

ss_temp_old <- ss_temp
rm(ss_temp_old)
ss_temp <- ss_temp_filled
rm(ss_temp_filled)
rm(shares_flat_filled)

iv_lev_form <- "log_real_Elem_Educ_Total_Exp_pp ~ l1_log_real_Elem_Educ_Total_Exp_pp + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | "

iv_lev_form_var <- "var_log_Elem_Educ_Total_Exp_pp ~ l(var_log_Elem_Educ_Total_Exp_pp, 1) + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | "

iv_lev_form_max <- "max_log_Elem_Educ_Total_Exp_pp ~ l(max_log_Elem_Educ_Total_Exp_pp, 1) + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | "

iv_lev_form_min <- "min_log_Elem_Educ_Total_Exp_pp ~ l(min_log_Elem_Educ_Total_Exp_pp, 1) + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | "

iv_lev_form_diff_min_max <- "log_diff_min_max_Elem_Educ_Total_Exp_pp ~ l(log_diff_min_max_Elem_Educ_Total_Exp_pp, 1) + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | "

iv_lev_form_ig_rev <- "log_real_Total_IG_Revenue_pp ~ l(log_real_Total_IG_Revenue_pp, 1) + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | "

iv_lev_form_prop <- "log_hpi ~ l1_log_hpi + log_real_gdp_priv_ind_pc + pct_black + pct_hispanic | unit + year | "

iv_lev_form_prop_tax <- "log_real_Property_Tax_pp ~ l(log_real_Property_Tax_pp,1) + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | "

iv_lev_form_priv_school_acs5 <- "pct_private_school_primary_acs5 ~ l(pct_private_school_primary_acs5, 1) + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | "

iv_lev_form_priv_school_acs1 <- "pct_private_school_primary_acs1 ~ l(pct_private_school_primary_acs1, 1) + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | "

iv_gr_form <- "diff_log_real_Elem_Educ_Total_Exp_pp ~ diff_log_real_Total_IG_Revenue_pp + diff_log_real_gdp_priv_ind_pc + diff_log_Enrollment + fd_pct_black + fd_pct_hispanic | unit + year | "

if(yes_contemp_ss){
  
  ss_lev_gr_va <- "log_annual_avg_wkly_wage ~ l1_log_annual_avg_wkly_wage + l1_gdp_ss_2d + l2_gdp_ss_2d"
  
  ss_gr_gr_va <- "gr_annual_avg_wkly_wage ~ l1_gdp_ss_2d + l2_gdp_ss_2d"
  
  ss_lev_lev_va <- "log_annual_avg_wkly_wage ~ l1_log_annual_avg_wkly_wage + l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d"
  
  ss_gr_lev_va <- "gr_annual_avg_wkly_wage ~ l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d"
}else{
  
  ss_lev_gr_va <- "log_annual_avg_wkly_wage ~ l1_log_annual_avg_wkly_wage + l1_gdp_ss_2d + l2_gdp_ss_2d"
  
  ss_gr_gr_va <- "gr_annual_avg_wkly_wage ~ l1_gdp_ss_2d + l2_gdp_ss_2d"
  
  ss_lev_lev_va <- "log_annual_avg_wkly_wage ~ l1_log_annual_avg_wkly_wage + l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d"
  
  ss_gr_lev_va <- "gr_annual_avg_wkly_wage ~ l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d"
}

df_ivs <- left_join(mines_fips, rename(ss_temp, unit = fips), by = c("year","unit")) %>% 
  group_by(unit) %>% 
  arrange(year) %>% 
  mutate(l1_lev_ss_2d = lag(lev_ss_2d, 1),
         l2_lev_ss_2d = lag(lev_ss_2d, 2),
         l1_ss_2d = lag(ss_2d, 1),
         l2_ss_2d = lag(ss_2d, 2),
         l1_gdp_ss_2d = lag(gdp_ss_2d,1),
         l2_gdp_ss_2d = lag(gdp_ss_2d, 2),
         l1_lev_gdp_ss_2d = lag(lev_gdp_ss_2d, 1),
         l2_lev_gdp_ss_2d = lag(lev_gdp_ss_2d, 2),
         fd_pct_black = pct_black - lag(pct_black),
         fd_pct_hispanic = pct_hispanic - lag(pct_hispanic)) %>%
  ungroup %>% 
  # There were a few observations where the minimum and maximum were equal
  mutate(min_log_Elem_Educ_Total_Exp_pp = ifelse(min_log_Elem_Educ_Total_Exp_pp == max_log_Elem_Educ_Total_Exp_pp, NA, min_log_Elem_Educ_Total_Exp_pp), 
         max_log_Elem_Educ_Total_Exp_pp = ifelse(min_log_Elem_Educ_Total_Exp_pp == max_log_Elem_Educ_Total_Exp_pp, NA, max_log_Elem_Educ_Total_Exp_pp))

if(!file.exists(here("data/temp/df_ivs_fips.RDS"))){
  saveRDS(df_ivs, here("data/temp/df_ivs_fips.RDS"))
}
rm(ss_temp)

#df_ivs <- readRDS(here("data/temp/df_ivs_fips.RDS"))

################################################################################
################################################################################
#################### CROSS-SECTIONAL ROBUSTNESS CHECK
################################################################################
################################################################################

# Define specifications
csec_forms <- list(
  "Full w. Lags" = "log_real_Elem_Educ_Total_Exp_pp ~ l1_log_real_Elem_Educ_Total_Exp_pp + 
log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + 
pct_black + pct_hispanic | log_annual_avg_wkly_wage | 
l1_log_annual_avg_wkly_wage + lev_ss_2d + l1_lev_ss_2d + l2_lev_ss_2d",
  
  "Wage Lag Only" = "log_real_Elem_Educ_Total_Exp_pp ~ 
log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + 
pct_black + pct_hispanic | log_annual_avg_wkly_wage | 
l1_log_annual_avg_wkly_wage + lev_ss_2d",
  
  "SS Lags Only" = "log_real_Elem_Educ_Total_Exp_pp ~ 
log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + 
pct_black + pct_hispanic | log_annual_avg_wkly_wage | 
lev_ss_2d + l1_lev_ss_2d + l2_lev_ss_2d",
  
  "No Lags/Dynamics" = "log_real_Elem_Educ_Total_Exp_pp ~ 
log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + 
pct_black + pct_hispanic | log_annual_avg_wkly_wage | 
lev_ss_2d",
  
  "No Dynamics" = "log_real_Elem_Educ_Total_Exp_pp ~  l1_log_real_Elem_Educ_Total_Exp_pp + 
log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + 
pct_black + pct_hispanic | log_annual_avg_wkly_wage | 
lev_ss_2d"
)

# Function to run IV for one year and one specification
run_csec_iv <- function(year, formula, data) {
  df_year <- data %>% filter(year == !!year)
  
  tryCatch({
    model <- ivreg(as.formula(formula), data = df_year)
    
    # Extract diagnostics
    diag <- summary(model, diagnostics = TRUE)$diagnostics
    fstat <- diag["Weak instruments", "statistic"]
    
    # Extract wage coefficient
    wage_coef <- tidy(model) %>%
      filter(term == "log_annual_avg_wkly_wage")
    
    return(tibble(
      year = year,
      estimate = wage_coef$estimate,
      std.error = wage_coef$std.error,
      p.value = wage_coef$p.value,
      fstat = fstat,
      n_obs = nobs(model)
    ))
  }, error = function(e) {
    return(tibble(
      year = year,
      estimate = NA_real_,
      std.error = NA_real_,
      p.value = NA_real_,
      fstat = NA_real_,
      n_obs = NA_integer_
    ))
  })
}

# Run all specifications for all years
years <- (min(df_ivs$year) + 2):max(df_ivs$year)  # Start at 2003 for lags

results_all <- map_dfr(names(csec_forms), function(spec_name) {
  cat("Running specification:", spec_name, "\n")
  
  map_dfr(years, function(yr) {
    run_csec_iv(yr, csec_forms[[spec_name]], df_ivs)
  }) %>%
    mutate(specification = spec_name)
})

# Add panel estimate for comparison
panel_estimate <- tibble(
  year = years,
  estimate = 0.223,  # Your panel estimate
  std.error = 0.048,  # Your panel SE
  p.value = NA_real_,
  fstat = 7963.5,
  n_obs = NA_integer_,
  specification = "Panel (Main Spec)"
)

# Combine
results_combined <- bind_rows(results_all, panel_estimate)

# Save results
saveRDS(results_combined, here("output/cross_sectional_iv_results_fips.RDS"))

# Define colors for each specification
spec_colors <- c(
  "Full w. Lags" = "#E41A1C",
  "Wage Lag Only" = "#377EB8", 
  "SS Lags Only" = "#4DAF4A",
  "No Lags/Dynamics" = "#984EA3",
  "No Dynamics" = "#A65628",
  "Panel (Main Spec)" = "#FF7F00"
)

# Main plot
csec_comparison_plot <- results_combined %>%
  filter(!is.na(estimate) & !(specification %in% c("No Lags/Dynamics", "No Dynamics"))) %>%
  ggplot(aes(x = year, y = estimate, color = specification, fill = specification)) +
  
  # Zero line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  
  # # Recession shading
  # annotate("rect", xmin = 2007, xmax = 2009, ymin = -Inf, ymax = Inf, 
  #          fill = "gray80", alpha = 0.3) +
  
  # Confidence intervals (only for cross-sectional, not panel)
  geom_ribbon(
    data = results_combined %>% filter(!(specification %in% c("Panel (Main Spec)", "No Lags/Dynamics", "No Dynamics")), !is.na(estimate)),
    aes(ymin = estimate - 1.96*std.error, 
        ymax = estimate + 1.96*std.error),
    alpha = 0.15, color = NA
  ) +
  
  # Lines
  geom_line(size = 1) +
  
  # Points (size by F-stat for cross-sectional)
  geom_point(
    data = results_combined %>% filter(!(specification %in% c("Panel (Main Spec)", "No Lags/Dynamics", "No Dynamics"))),
    aes(size = fstat)
  ) +
  # geom_point(
  #   data = results_combined %>% filter(specification == "Panel (Main Spec)"),
  #   size = 2
  # ) +
  
  # Styling
  scale_color_manual(values = spec_colors) +
  scale_fill_manual(values = spec_colors) +
  scale_size_continuous(name = "First-stage F-stat", range = c(1.5, 4)) +
  
  labs(
    title = "Cross-Sectional IV Estimates: Comparing Specifications",
    subtitle = "Point size indicates first-stage F-statistic. Shaded area = 95% CI.",
    x = "Year",
    y = "Wage Elasticity (Effect of 10% wage increase on education spending)",
    color = "Specification",
    fill = "Specification",
    caption = "Panel estimate (orange line) uses full panel with unit and year fixed effects.\nCross-sectional estimates use only between-county variation within each year."
  ) +
  
  common_theme +
  theme(
    legend.position = "bottom",
    legend.box = "vertical",
    legend.margin = margin(t = 10)
  ) +
  
  guides(
    color = guide_legend(order = 1, nrow = 2),
    fill = guide_legend(order = 1, nrow = 2),
    size = guide_legend(order = 2)
  )

summary_table <- results_combined %>%
  group_by(specification) %>%
  summarise(
    Mean = mean(estimate, na.rm = TRUE),
    Median = median(estimate, na.rm = TRUE),
    SD = sd(estimate, na.rm = TRUE),
    Min = min(estimate, na.rm = TRUE),
    Max = max(estimate, na.rm = TRUE),
    `Mean F-stat` = round(mean(fstat, na.rm = TRUE), 2),
    `N Significant` = sum(p.value < 0.05, na.rm = TRUE),
    `N Years` = sum(!is.na(estimate))
  ) %>%
  mutate(`N Significant` = ifelse(specification == "Panel (Main Spec)", "", `N Significant`),
         `N Years` = ifelse(specification == "Panel (Main Spec)", "", `N Years`)) %>% 
  arrange(desc(Mean))

# Export table
tab_cross_sectional_summary <- summary_table %>%
  kable(
    format = "latex",
    digits = 3,
    booktabs = TRUE,
    caption = "Summary of Cross-Sectional IV Estimates by Specification"
  ) %>%
  kable_styling() 

#append_table_to_appendix(tab_cross_sectional_summary)

# Save figure
#append_figure_to_appendix(
#   plot_obj = csec_comparison_plot,
#   filename = "fig_cross_sectional_comparison_fips.png",
#   caption = "Comparison of cross-sectional IV estimates across specifications. Each line represents estimates from separate year-by-year regressions. Point size indicates first-stage F-statistic. The orange line shows the panel IV estimate from the main specification for comparison. Gray shaded area indicates the 2007-2009 recession period.",
#   label = "fig:csec_comparison_fips"
# )

    
################################################################################
################################################################################
#################### BASELINE (TABLES 4-6)
################################################################################
################################################################################
append_table_to_appendix("\\FloatBarrier")
append_table_to_appendix("\\subsection{Baseline Models}")

iv_model_2d_lev_gr <- feols(as.formula(paste0(iv_lev_form, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")

iv_model_2d_gr_gr <- feols(as.formula(paste0(iv_gr_form, ss_gr_gr_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")

iv_model_2d_gr_lev <- feols(as.formula(paste0(iv_gr_form, ss_gr_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")

iv_model_2d_gr_lev_lev_ed <- feols(as.formula(paste0(iv_lev_form, ss_gr_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")

iv_model_2d_lev_lev <- feols(as.formula(paste0(iv_lev_form, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")

# No AR
f2d_lev_lev_test <- "log_real_Elem_Educ_Total_Exp_pp ~  log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | log_annual_avg_wkly_wage ~ lev_gdp_ss_2d + l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d"
f2d_lev_lev_test2 <- "log_real_Elem_Educ_Total_Exp_pp ~ l1_log_real_Elem_Educ_Total_Exp_pp + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | log_annual_avg_wkly_wage ~ lev_gdp_ss_2d + l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d"
f2d_lev_lev_test3 <- "log_real_Elem_Educ_Total_Exp_pp ~ l1_log_real_Elem_Educ_Total_Exp_pp + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | log_annual_avg_wkly_wage ~ l1_log_annual_avg_wkly_wage + lev_gdp_ss_2d + l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d"
f2d_lev_lev_test4 <- "log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | log_annual_avg_wkly_wage ~ l1_log_annual_avg_wkly_wage + lev_gdp_ss_2d + l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d"
if(!yes_contemp_ss){
  f2d_lev_lev_test <- gsub(" lev_gdp_ss_2d + ", " ", f2d_lev_lev_test, fixed = TRUE)
  f2d_lev_lev_test2 <- gsub(" lev_gdp_ss_2d + ", " ", f2d_lev_lev_test2, fixed = TRUE)
  f2d_lev_lev_test3 <- gsub(" lev_gdp_ss_2d + ", " ", f2d_lev_lev_test3, fixed = TRUE)
  f2d_lev_lev_test4 <- gsub(" lev_gdp_ss_2d + ", " ", f2d_lev_lev_test4, fixed = TRUE)
}

iv_model_2d_lev_lev_test <- feols(as.formula(f2d_lev_lev_test), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
# SS AR
iv_model_2d_lev_lev_test2 <- feols(as.formula(f2d_lev_lev_test2), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
# Full AR
iv_model_2d_lev_lev_test3 <- feols(as.formula(f2d_lev_lev_test3), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
# FS AR
iv_model_2d_lev_lev_test4 <- feols(as.formula(f2d_lev_lev_test4), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")

tbl <- etable(iv_model_2d_lev_lev, iv_model_2d_lev_lev_test4, adjustbox = "width=0.95\\textheight", stage = 1:2,fitstat = iv_fitstats, tex = latex_tables, caption = "IV Estimation Using VA-based Shift-share instrument (l0, l1, l2) in Levels with county and year fixed effects and lags.", label = "tbl_va_ss_baseline_fips")

tbl
append_table_to_appendix(tbl, landscape = TRUE)
rm(tbl)

iv_model_2d_lev_lev_prop <- feols(as.formula(paste0(iv_lev_form_prop, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
iv_model_2d_lev_lev_prop_tax <-  feols(as.formula(paste0(iv_lev_form_prop_tax, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
iv_model_2d_lev_lev_priv_acs5 <- feols(as.formula(paste0(iv_lev_form_priv_school_acs5, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
iv_model_2d_lev_lev_priv_acs1 <- feols(as.formula(paste0(iv_lev_form_priv_school_acs1, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
   
tbl <- etable(iv_model_2d_lev_lev_prop, iv_model_2d_lev_lev_prop_tax, iv_model_2d_lev_lev_priv_acs5, iv_model_2d_lev_lev_priv_acs1, adjustbox = "max width=\\linewidth", stage = 1:2,fitstat = iv_fitstats, tex = latex_tables, caption = "IV Estimation Using VA-based shift-share instrument (l0, l1, l2) in Levels with county and year fixed effects and lags.", label = "tbl_va_ss_prop_priv_school_fips")

tbl
append_table_to_appendix(tbl, landscape = TRUE)
rm(tbl)

iv_model_2d_lev_lev_var <- feols(as.formula(paste0(iv_lev_form_var, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
iv_model_2d_lev_lev_min <- feols(as.formula(paste0(iv_lev_form_min, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
iv_model_2d_lev_lev_max <- feols(as.formula(paste0(iv_lev_form_max, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
iv_model_2d_lev_lev_diff_min_max <- feols(as.formula(paste0(iv_lev_form_diff_min_max, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")
iv_model_2d_lev_lev_ig_rev <- feols(as.formula(paste0(iv_lev_form_ig_rev, ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")

tbl <- etable(iv_model_2d_lev_lev_var, iv_model_2d_lev_lev_min, iv_model_2d_lev_lev_max, iv_model_2d_lev_lev_diff_min_max, adjustbox = "max width=\\linewidth", stage = 1:2,fitstat = iv_fitstats, tex = latex_tables, caption = "Within-County Outcomes via IV Estimation Using VA-based shift-share instrument.", label = "tbl_va_ss_baseline_within_fips")

tbl
append_table_to_appendix(tbl, landscape = TRUE)
rm(tbl)

rm(iv_model_2d_lev_lev_test, iv_model_2d_lev_lev_test2, iv_model_2d_lev_lev_test3, iv_model_2d_lev_lev_test4,
   iv_model_2d_lev_lev_prop, iv_model_2d_lev_lev_prop_tax, iv_model_2d_lev_lev_priv_acs5, iv_model_2d_lev_lev_priv_acs1,
   iv_model_2d_lev_lev_var, iv_model_2d_lev_lev_min, iv_model_2d_lev_lev_max, iv_model_2d_lev_lev_diff_min_max, iv_model_2d_lev_lev_ig_rev)
    
################################################################################
################################################################################
#################### IG EXPENDITURE (APPENDIX TABLE)
################################################################################
################################################################################

iv_model_2d_lev_lev_ig <- feols(as.formula(paste0("log_real_Total_IG_Revenue_pp  ~ l(log_real_Total_IG_Revenue_pp,1) + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year | ",  ss_lev_lev_va)), data = df_ivs, panel.id = c("unit", "year"), cluster = "unit")

ig_tbl <- etable(iv_model_2d_lev_lev_ig, adjustbox = "max width=\\linewidth", stage = 1:2,fitstat = iv_fitstats, tex = latex_tables, label = "tbl_va_ss_baseline_ig_exp_fips", caption = "Wages and Intergovernmental Revenue Per Pupil")
append_table_to_appendix(ig_tbl)
rm(ig_tbl)

################################################################################
################################################################################
#################### GROWING VS. DECLINING REGIONS (FIGS 6-7)
################################################################################
################################################################################
append_table_to_appendix("\\FloatBarrier")
append_table_to_appendix("\\subsection{Declining vs. Growing Regions}")

# -Creating a safe plotting function until state wage growth rates are incorporated
safe_plot <- function(expr, fail_label = "Plot failed", text_size = 4) {
  tryCatch(
    {
      p <- eval(substitute(expr), envir = parent.frame())
      if (!inherits(p, "ggplot")) stop("Expression did not return a ggplot object")
      # Force build so errors in scales/geoms are triggered here (and caught)
      ggplot_build(p)
      p
    },
    error = function(e) {
      ggplot() +
        geom_rect(aes(xmin = 0, xmax = 1, ymin = 0, ymax = 1),
                  fill = "white", color = "grey80") +
        annotate("text", x = 0.5, y = 0.55,
                 label = fail_label,
                 size = text_size, fontface = "bold", hjust = 0.5) +
        
        theme_void()
    }
  )
}


# Select variabls necessary for IVs without dragging all individual shift-share instrument variables
mines_fips_short <- df_ivs %>% select(names(mines_fips), 
                                    lev_ss_2d,
                                    l1_lev_ss_2d, 
                                    l2_lev_ss_2d,
                                    ss_2d,
                                    l1_ss_2d,
                                    l2_ss_2d,
                                    gdp_ss_2d,
                                    l1_gdp_ss_2d,
                                    l2_gdp_ss_2d,
                                    lev_gdp_ss_2d,
                                    l1_lev_gdp_ss_2d,
                                    l2_lev_gdp_ss_2d) %>% 
  select(-starts_with("var_"), -starts_with("max_"),
         -starts_with("min_"), -starts_with("diff_min_max_"))

state_lev <- mines_fips_short %>%
  group_by(state, year) %>%
  summarise(across(c(real_gdp_priv_ind, real_gdp_total, pop_total), ~sum(., na.rm = TRUE))) %>%
  ungroup %>%
  mutate(real_gdp_priv_ind_pc = real_gdp_priv_ind/pop_total,
         real_gdp_total_pc = real_gdp_total/pop_total,
         across(!c(state, year), ~log(. + 1), .names = "log_{.col}")) %>%
  group_by(state) %>%
  mutate(across(contains("log"), ~.- dplyr::lag(., 1), .names = "diff_{.col}")) %>%
  ungroup

natl_lev <- mines_fips_short %>%
  group_by(year) %>%
  summarise(across(c(real_gdp_priv_ind, real_gdp_total, pop_total), ~sum(., na.rm = TRUE))) %>%
  ungroup %>%
  mutate(real_gdp_priv_ind_pc = real_gdp_priv_ind/pop_total,
         real_gdp_total_pc = real_gdp_total/pop_total,
         across(!c(year), ~log(. + 1), .names = "log_{.col}"),
         across(contains("log"), ~.- dplyr::lag(., 1), .names = "diff_{.col}"))

growth_rates <- mines_fips_short %>%
  select(unit, names(state_lev)) %>%
  left_join(., state_lev, by = c("state", "year"), suffix = c("", "_state")) %>%
  left_join(., natl_lev, by = c("year"), suffix = c("", "_natl"))

## ---------------------------------------------------------
## 1. Orthogonalise state growth relative to national growth
## ---------------------------------------------------------
growth_rates_orthog <- growth_rates %>%
group_by(state) %>%
group_modify(~{
  # state-total residual
  m1 <- lm(diff_log_real_gdp_priv_ind_state ~ diff_log_real_gdp_priv_ind_natl,
           data = .x, na.action = na.exclude)
  .x$state_resid <- residuals(m1)
  
  # state-per-capita residual
  m2 <- lm(diff_log_real_gdp_priv_ind_pc_state ~ diff_log_real_gdp_priv_ind_pc_natl,
           data = .x, na.action = na.exclude)
  .x$state_resid_pc <- residuals(m2)
  
  .x
}) %>%
ungroup()

## ---------------------------------------------------------
## 2. Regression forms
## ---------------------------------------------------------
trend_forms <- list(
trend    = diff_log_real_gdp_priv_ind    ~ state_resid    + diff_log_real_gdp_priv_ind_natl,
trend_pc = diff_log_real_gdp_priv_ind_pc ~ state_resid_pc + diff_log_real_gdp_priv_ind_pc_natl
)

## ---------------------------------------------------------
## 3. Helper to extract coefficients safely
## ---------------------------------------------------------
extract_coefs <- function(formula_obj, data){
fit <- lm(formula_obj, data = data)
cf  <- coef(fit)

tibble(
  intercept = unname(cf["(Intercept)"]),
  beta_nat  = unname(cf[grepl("natl", names(cf))]),
  beta_state= unname(cf[grepl("state_resid", names(cf))])
)
}

## ---------------------------------------------------------
## 4. Run regressions by commuting zone
## ---------------------------------------------------------
fips_trends <- growth_rates_orthog %>%
group_by(unit) %>%
group_modify(~ extract_coefs(trend_forms$trend, .x)) %>%
ungroup() %>%
rename(trend = intercept)

fips_trends_pc <- growth_rates_orthog %>%
group_by(unit) %>%
group_modify(~ extract_coefs(trend_forms$trend_pc, .x)) %>%
ungroup() %>%
rename(trend_pc = intercept,
       beta_nat_pc = beta_nat,
       beta_state_pc = beta_state)

# join them
fips_trends <- fips_trends %>% left_join(fips_trends_pc, by = "unit")

## ---------------------------------------------------------
## 4. Percentiles for classification
## ---------------------------------------------------------
percentiles <- fips_trends %>%
summarize(
  p25trend     = quantile(trend, 0.25, na.rm = TRUE),
  p75trend     = quantile(trend, 0.75, na.rm = TRUE),
  p25trend_pc  = quantile(trend_pc, 0.25, na.rm = TRUE),
  p75trend_pc  = quantile(trend_pc, 0.75, na.rm = TRUE)
)

fips_trends <- fips_trends %>%
mutate(declining = trend < 0,
       declining_extreme = trend < percentiles$p25trend,
       growing_extreme   = trend > percentiles$p75trend,
       declining_pc = trend_pc < 0,
       declining_pc_extreme = trend_pc < percentiles$p25trend_pc,
       growing_pc_extreme   = trend_pc > percentiles$p75trend_pc)

## ---------------------------------------------------------
## 5. Histograms of intercepts
## ---------------------------------------------------------
my_color <- viridis_pal(option = "rocket")(6)

trend_hist <- fips_trends %>%
ggplot() +
geom_histogram(aes(x = trend), bins = 75, fill = my_color[2], alpha = 0.7) +
geom_vline(aes(xintercept = percentiles$p25trend), linetype = "dashed") +
geom_vline(aes(xintercept = percentiles$p75trend), linetype = "dashed") +
labs(x = "fips Intercept (trend)", title = "Distribution of County GDP Trend Coefficients", y = "Frequency") +
common_theme

trend_pc_hist <- fips_trends %>%
ggplot() +
geom_histogram(aes(x = trend_pc), bins = 75, fill = my_color[4], alpha = 0.7) +
geom_vline(aes(xintercept = percentiles$p25trend_pc), linetype = "dashed") +
geom_vline(aes(xintercept = percentiles$p75trend_pc), linetype = "dashed") +
labs(x = "County Intercept (trend_pc)", title = "Distribution of County GDPpc Trend Coefficients", y = "Frequency") +
common_theme

#trend_hist / trend_pc_hist

## ---------------------------------------------------------
## 6. Plot coefficients by state/region (like your version)
## ---------------------------------------------------------
fips_trends_plot <- fips_trends %>%
left_join(distinct(select(mines_fips_short, unit, state)), by = "unit")

fips_trends_plot$state = sapply(fips_trends_plot$state, get_state)
fips_trends_plot$region = sapply(fips_trends_plot$state, get_region)

n_states <- dplyr::n_distinct(fips_trends_plot$state)
tmp <- colorRampPalette(brewer.pal(9, "Oranges"))(n_states + 10)
myOranges <- tmp[-c(1:10)]
myOranges <- myOranges[1:n_states]

fips_states <- fips_trends_plot %>%
group_by(state) %>%
arrange(state, trend_pc) %>%
mutate(midpt = ifelse(row_number() == round((max(row_number()) - min(row_number()))/2),1, NA)) %>%
ungroup() %>%
mutate(midpt = row_number() * midpt) %>%
mutate(unit = factor(unit, levels = unit)) %>%
ggplot(aes(unit, trend_pc)) +
geom_segment(aes(x=unit ,xend=unit, y=0, yend=trend_pc, color = state)) +
geom_point(color="darkblue", size=0.5) +
coord_flip() +
geom_label(aes(midpt, 0.09,
               label = state),
           fill = NA,
           #family = "Special Elite",
           fontface = "bold",
           label.padding = unit(.2, "lines"),
           label.r = unit(.25, "lines"),
           label.size = .05,
           size = 3) +
labs(x = "Counties by State", y = "GDPpc Trend") +
common_theme + 
theme(axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      legend.position = "none") +
scale_color_manual(values = myOranges)



fips_trend <- fips_trends_plot %>%
arrange(trend_pc) %>%
mutate(unit = factor(unit, levels = unit)) %>%
ggplot(aes(unit, trend_pc)) +
geom_segment(aes(x=unit ,xend=unit, y=0, yend=trend_pc, color = unit)) +
geom_point(color="darkblue", size=0.5) +
coord_flip() +
labs(x = "Counties", y = "GDPpc Trend") +
common_theme +
theme(axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      legend.position = "none") +
scale_color_viridis(discrete = TRUE, option = "rocket", direction = -1)

#add_labels(fips_trend, fips_trends_plot, "trend_pc", 5) -> fips_trend

fips_regions <- fips_trends_plot %>%
group_by(region) %>%
arrange(region, trend_pc) %>%
mutate(midpt = ifelse(row_number() == round((max(row_number()) - min(row_number()))/2),1, NA)) %>%
ungroup %>%
mutate(midpt = row_number() * midpt) %>%
mutate(unit = factor(unit, levels = unit)) %>%
ggplot(aes(unit, trend_pc)) +
geom_segment(aes(x=unit ,xend=unit, y=0, yend=trend_pc, color = region)) +
geom_point(color="darkblue", size=0.5) +
coord_flip() +
labs(x = "Counties by Region", y = "GDPpc Trend") +
geom_label(aes(midpt, 0.09,
               label = region),
           fill = NA,
           #family = "Special Elite",
           fontface = "bold",
           label.padding = unit(.2, "lines"),
           label.r = unit(.25, "lines"),
           label.size = .05,
           size = 3) +
common_theme +
theme(axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      legend.position = "none") +
scale_color_brewer(palette = "Reds")

fips_regions + fips_states +  fips_trend + 
plot_annotation(
  title = "County GDP pc Growth Rates",
  subtitle = "Intercepts from regressions controlling for national growth and state-specific residual growth.", theme = plot_annotation_theme
) + 
common_theme

## ---------------------------------------------------------
## Scatter plot of betas
## ---------------------------------------------------------
scatter_betas <- fips_trends_plot %>%
ggplot(aes(x = beta_nat, y = beta_state, color = region)) +
geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
geom_point(alpha = 0.7) +
labs(
  x = expression(beta["nat"]~"(loading on national growth)"),
  y = expression(beta["state"]~"(loading on state-specific growth)"),
  title = "County GDPpc Growth Loadings",
  subtitle = "Coefficients from regressions on national growth and state-specific residuals",
  color = "Region"
) +
  scale_color_brewer(palette = "Set2") +
common_theme 

## ---------------------------------------------------------
## 9. Histograms of betas
## ---------------------------------------------------------
hist_nat <- fips_trends_plot %>%
ggplot(aes(x = beta_nat)) +
geom_histogram(fill = "steelblue", alpha = 0.7, bins = 50) +
labs(x = expression(beta["nat"]), y = "Count",
     title = "Distribution of beta_nat") + 
common_theme

hist_state <- fips_trends_plot %>%
ggplot(aes(x = beta_state)) +
geom_histogram(fill = "tomato", alpha = 0.7, bins = 50) +
labs(x = expression(beta["state"]), y = "Count",
     title = "Distribution of beta_state") + 
common_theme

scatter_betas / (hist_nat + hist_state)


# 0. Prepare County-level wage growth + national series
wage_growth_rates <- mines_fips_short %>%
select(year, unit, state, gr_annual_avg_wkly_wage, natl_gr_annual_avg_wkly_wage, state_gr_annual_avg_wkly_wage) 

growth_rates_wage_orthog <- wage_growth_rates %>%
group_by(state) %>%
group_modify(~{
  # fit state-level series on national series (na.action = na.exclude to preserve NAs)
  fm <- try(lm(state_gr_annual_avg_wkly_wage ~ natl_gr_annual_avg_wkly_wage, data = .x, na.action = na.exclude),
            silent = TRUE)
  if(inherits(fm, "try-error")){
    .x$state_resid <- NA_real_
  } else {
    .x$state_resid <- residuals(fm)
  }
  .x
}) %>%
ungroup()



wage_trend_formula <- gr_annual_avg_wkly_wage ~ state_resid + natl_gr_annual_avg_wkly_wage

extract_coefs_wage <- function(formula_obj, data){
fit <- try(lm(formula_obj, data = data), silent = TRUE)
if(inherits(fit, "try-error")){
  return(tibble(intercept = NA_real_, beta_nat = NA_real_, beta_state = NA_real_))
}

coefs <- coef(fit)                    # named numeric vector
intercept <- if("(Intercept)" %in% names(coefs)) coefs["(Intercept)"] else NA_real_
beta_nat   <- if(any(grepl("natl_gr", names(coefs)))) coefs[grep("natl_gr", names(coefs))[1]] else NA_real_
beta_state <- if(any(grepl("state_resid", names(coefs)))) coefs[grep("state_resid", names(coefs))[1]] else NA_real_

tibble(
  intercept = unname(intercept),
  beta_nat  = unname(beta_nat),
  beta_state = unname(beta_state)
)
}

# 6. Run the per-County regressions and extract intercept (wage_trend)
fips_wage_trends <- growth_rates_wage_orthog %>%
group_by(unit) %>%
group_modify(~ extract_coefs_wage(wage_trend_formula, .x)) %>%
ungroup() %>%   # .id returns the unit label from group_map
rename(wage_trend = intercept)

# 7. Percentiles and classification flags (same as your original)
percentiles <- fips_wage_trends %>%
summarize(
  p25trend = quantile(wage_trend, 0.25, na.rm = TRUE),
  p75trend = quantile(wage_trend, 0.75, na.rm = TRUE)
)

fips_wage_trends <- fips_wage_trends %>%
mutate(declining = wage_trend <= 0,
       declining_extreme = wage_trend <= percentiles$p25trend,
       growing_extreme = wage_trend > percentiles$p75trend)

# 8. Join back County metadata (state) and region mapping like you do downstream
fips_wage_trends_plot <- fips_wage_trends %>%
left_join(distinct(select(mines_fips_short, unit, state)), by = "unit")

# convert/clean state and region columns as in your working code
fips_wage_trends_plot$state  <- sapply(fips_wage_trends_plot$state, get_state)
fips_wage_trends_plot$region <- sapply(fips_wage_trends_plot$state, get_region)

my_color <- viridis_pal(option = "mako")(5)
trend_hist <- fips_wage_trends %>%
ggplot() +
geom_histogram(aes(x = wage_trend), bins = 75, fill = my_color[3], alpha = 0.7) +
geom_vline(aes(xintercept = percentiles$p25trend), linetype = "dashed") +
geom_vline(aes(xintercept = percentiles$p75trend), linetype = "dashed") +
labs(x = "County Wage Trend Coefficient", title = "Distribution of County Wage Trend Coefficients", y = "Frequency") +
annotate("text", x = percentiles$p25trend, y = 40, label = "25th Percentile", color = my_color[3], angle = 90, vjust = -0.5) +
annotate("text", x = percentiles$p75trend, y = 40, label = "75th Percentile", color = my_color[3], angle = 90, vjust = 1.5) +
common_theme

n_states <- dplyr::n_distinct(fips_wage_trends_plot$state)

# generate more colors than you need, then drop the first k lightest
tmp <- colorRampPalette(brewer.pal(9, "Blues"))(n_states + 10)  # oversample
myBlues <- tmp[-c(1:10)]   # drop the 2 lightest shades
myBlues <- myBlues[1:n_states]
# First, create a consistent ordering based on regions
region_order <- fips_wage_trends_plot %>%
group_by(region) %>%
summarise(median_trend = median(wage_trend, na.rm = TRUE)) %>%
arrange(median_trend) %>%
pull(region)

# Create state order that follows region order
state_region_order <- fips_wage_trends_plot %>%
mutate(region = factor(region, levels = region_order)) %>%
arrange(region, wage_trend) %>%
pull(state) %>%
unique()

# Now create the plots with consistent ordering

# LEFT PLOT: Regions (same as before)
fips_wage_regions <- fips_wage_trends_plot %>%
mutate(region = factor(region, levels = region_order)) %>%
group_by(region) %>%
arrange(region, wage_trend) %>%
mutate(midpt = ifelse(row_number() == round((max(row_number()) - min(row_number()))/2), 1, NA)) %>%
ungroup() %>%
mutate(
  midpt = row_number() * midpt,
  unit = factor(unit, levels = unique(unit))
) %>%
ggplot(aes(unit, wage_trend)) +
geom_segment(aes(x = unit, xend = unit, y = 0, yend = wage_trend, color = region)) +
geom_point(color = "darkblue", size = 0.5) +
geom_label(aes(midpt, 0.05, label = region),
           fill = NA,
           fontface = "bold",
           label.padding = unit(.2, "lines"),
           label.size = .09,
           size = 4) +
coord_flip() +
labs(x = "Counties by Region", y = "Wage Trend Coefficient") +
common_theme +
theme(axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10),
      legend.position = "none",
      plot.background = element_rect(fill = NA),
      panel.background = element_rect(fill = NA, color = NA),
      panel.border = element_rect(fill = NA, color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(size = 13,
                                  margin = margin(r = 10)),
      panel.grid.major = element_line(color = NA, linewidth = 0.5),
      legend.title = element_text(size = 9),
      plot.margin = margin(10, 25, 10, 25)) +
scale_color_brewer(palette = "Greens")

# MIDDLE PLOT: States (ordered by region)
fips_wage_states <- fips_wage_trends_plot %>%
mutate(
  region = factor(region, levels = region_order),
  state = factor(state, levels = state_region_order)
) %>%
arrange(region, state, wage_trend) %>%
mutate(unit_order = row_number()) %>%
group_by(state) %>%
mutate(midpt = ifelse(row_number() == round((max(row_number()) - min(row_number()))/2), 1, NA)) %>%
ungroup() %>%
mutate(
  midpt = unit_order * midpt,
  unit = factor(unit, levels = unique(unit))
) %>%
ggplot(aes(unit, wage_trend)) +
geom_segment(aes(x = unit, xend = unit, y = 0, yend = wage_trend, color = state)) +
geom_point(color = "darkblue", size = 0.5) +
geom_label(aes(midpt, 0.09, label = state),
           fill = NA,
           fontface = "bold",
           label.padding = unit(.2, "lines"),
           label.r = unit(.25, "lines"),
           label.size = .05,
           size = 3) +
coord_flip() +
labs(x = "Counties by State", y = "Wage Trend Coefficient") +
common_theme +
theme(axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10),
      legend.position = "none",
      plot.background = element_rect(fill = NA),
      panel.background = element_rect(fill = NA, color = NA),
      panel.border = element_rect(fill = NA, color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(size = 13,
                                  margin = margin(r = 10)),
      panel.grid.major = element_line(color = NA, linewidth = 0.5),
      legend.title = element_text(size = 9),
      plot.margin = margin(10, 25, 10, 25)) +
scale_color_manual(values = myBlues)

# RIGHT PLOT: All Countys (same ordering as states plot)
fips_wage_trend <- fips_wage_trends_plot %>%
arrange(wage_trend) %>%
mutate(unit = factor(unit, levels = unique(unit))) %>%
ggplot(aes(unit, wage_trend)) +
geom_segment(aes(x = unit, xend = unit, y = 0, yend = wage_trend, color = unit)) +
geom_point(color = "darkblue", size = 0.05) +
coord_flip() +
labs(x = "Counties", y = "Wage Trend Coefficient") +
common_theme +
theme(axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10),
      legend.position = "none",
      plot.background = element_rect(fill = NA),
      panel.background = element_rect(fill = NA, color = NA),
      panel.border = element_rect(fill = NA, color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(size = 13,
                                  margin = margin(r = 10)),
      panel.grid.major = element_line(color = NA, linewidth = 0.5),
      legend.title = element_text(size = 9),
      plot.margin = margin(10, 25, 10, 25)) +
scale_color_viridis(discrete = TRUE, option = "mako", direction = -1)

# Combine plots
fips_wage_regions + fips_wage_states + fips_wage_trend +
plot_annotation(
  title = "County Wage Growth Rate Controlling for National and State Level Trends",
  subtitle = "Calculated as idiosyncratic component of county growth rate, controlling for state and national fluctuations.",
  theme = plot_annotation_theme)

rm(fips_wage_regions)
rm(fips_wage_states)
rm(fips_wage_trend)

pct_1 <- fips_trends %>% left_join(fips_wage_trends, by = "unit") %>% ggplot(., aes(x = trend_pc, y = wage_trend)) +
geom_point(alpha = 0.2) +
geom_smooth(method = "lm", se = TRUE, color = "maroon", fill = "maroon") +
labs(
  x = "Real GDP pc Trend",
  y = "Wage Trend",
  title = "Relationship Between GDPpc and Wage Trends (per County)"
) +
theme_minimal() +
theme(axis.text.x = element_text(size = 14),
      axis.text.y = element_text(size = 14),
      title = element_text(size = 18))

pct_2 <- fips_trends %>%
left_join(fips_wage_trends, by = "unit") %>%
# Rank each variable into percentiles
mutate(
  gdp_pc_percentile = percent_rank(trend_pc) * 10,
  wage_percentile   = percent_rank(wage_trend) * 10
) %>%
ggplot(aes(x = gdp_pc_percentile, y = wage_percentile)) +
geom_point(alpha = 0.3, color = "steelblue") +
geom_smooth(method = "lm", color = "maroon", fill = "maroon", se = TRUE) +
geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray50") +
labs(
  x = "Percentile of Real GDP per Capita Trend",
  y = "Percentile of Wage Trend",
  title = "Percentile–Percentile Relationship Between GDPpc and Wage Trends"
) +
theme_minimal(base_size = 14) +
theme(
  axis.text = element_text(size = 14),
  title = element_text(size = 18)
)

pct_1/pct_2

temp <-  fips_wage_trends_plot %>% 
arrange(wage_trend) %>%
mutate(unit = factor(unit, levels = unit)) %>%
ggplot(aes(x = unit, y = wage_trend)) +
geom_segment(aes(xend = unit, y = 0, yend = wage_trend, color = unit)) +
geom_point(color = "darkblue", size = 0.6) +
# geom_text_repel(data = add_labels(fips_wage_trends_plot, "wage_trend", 5), aes(label = msa), direction = "y",
#                 nudge_y = 0.02, size = 3, segment.color = "grey60",
#                 max.overlaps = Inf) +
coord_flip() +
scale_x_discrete(limits = levels(fips_wage_trends_plot$unit)) +  # <- enforces the order
scale_color_viridis(discrete = TRUE, option = "mako", direction = -1) +
labs(x = "County", y = "Wage Trend Coefficient", title = "County Wage Growth Rate Controlling for National and State Level Trends", subtitle = "Zoom in on left-most plot from above to see outlier labels.") +
theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
      axis.text.x = element_text(size = 14), legend.position = "none")

#add_labels(temp, fips_wage_trends_plot, "wage_trend", 5)

# Example: safe scatter (will be caught if fips_wage_trends_plot or scale mismatches fail)
scatter_betas <- safe_plot({
fips_wage_trends_plot %>%
  ggplot(aes(x = beta_nat, y = beta_state, color = region)) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_point(alpha = 0.7) +
  labs(
    x = expression(beta["nat"]~"(loading on national growth)"),
    y = expression(beta["state"]~"(loading on state-specific growth)"),
    title = "County GDPpc Growth Loadings",
    subtitle = "Coefficients from regressions on national growth and state-specific residuals",
    color = "Region"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set2")
}, fail_label = "Expected error: \nMissing state wage growth rates so histogram fails.")
## ---------------------------------------------------------
## 9. Histograms of betas
## ---------------------------------------------------------


hist_nat <- safe_plot({fips_wage_trends_plot %>%
  ggplot(aes(x = beta_nat)) +
  geom_histogram(fill = "steelblue", alpha = 0.7, bins = 50) +
  labs(x = expression(beta["nat"]), y = "Count",
       title = expression("Distribution of"~beta["nat"])) +
  theme_minimal()}, fail_label = "hist_nat failed")

hist_state <- safe_plot({fips_wage_trends_plot %>%
  ggplot(aes(x = beta_state)) +
  geom_histogram(fill = "tomato", alpha = 0.7, bins = 50) +
  labs(x = expression(beta["state"]), y = "Count",
       title = expression("Distribution of "~beta["state"])) +
  theme_minimal()}, fail_label = "Expected error: \nMissing state wage growth rates so histogram fails.")


scatter_betas / (hist_nat + hist_state)

fips_trends_comb_plot <- fips_trends_plot %>% 
left_join(rename(fips_wage_trends_plot, beta_nat_wage = beta_nat, beta_state_wage = beta_state), by = c('unit', 'state', 'region'))

# Example: safe scatter (will be caught if fips_wage_trends_plot or scale mismatches fail)
scatter_betas <- fips_trends_comb_plot %>%
ggplot() +
geom_point(aes(x = beta_nat_pc, y = beta_state_pc, color = "GDP pc"), alpha = 0.7) +
geom_point(aes(x = beta_nat_wage, y = beta_state_wage, color = "Wage"), alpha = 0.7) +
geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
labs(
  x = expression(beta["nat"]~"(loading on national growth)"),
  y = expression(beta["state"]~"(loading on state-specific growth)"),
  title = "County GDPpc Growth Loadings",
  subtitle = "Coefficients from regressions on national growth and state-specific residuals",
  color = "Metric"
) +
common_theme + 
scale_colour_manual(values = c("cornflowerblue","mediumvioletred", "seagreen")) +
theme(legend.position = "top")


## ---------------------------------------------------------
## 9. Histograms of betas
## ---------------------------------------------------------
hist_nat <- fips_trends_comb_plot %>%
ggplot() +
geom_histogram(aes(x = beta_nat_pc, fill = "GDP pc"), alpha = 0.7, bins = 150) +
geom_histogram(aes(x = beta_nat_wage, fill = "Wage"), alpha = 0.7, bins = 150) +
#geom_histogram(aes(x = beta_nat_wage, fill = "GDP"), alpha = 0.7, bins = 50) +
labs(x = expression(beta["nat"]), y = "Count",
     title = expression("Distribution of"~beta["nat"]),
     fill = "Metric") +
common_theme + 
scale_colour_manual(values = c("cornflowerblue","mediumvioletred", "seagreen")) +
scale_fill_manual(values = c("cornflowerblue","mediumvioletred", "seagreen")) + 
theme(legend.position = "top")



hist_state <- fips_trends_comb_plot %>%
ggplot() +
geom_histogram(aes(x = beta_state_pc, fill = "GDP pc"), alpha = 0.7, bins = 150) +
geom_histogram(aes(x = beta_state_wage, fill = "Wage"), alpha = 0.7, bins = 150) +
#geom_histogram(aes(x = beta_state, fill = "GDP"), alpha = 0.7, bins = 50) +
labs(x = expression(beta["state"]), y = "Count",
     title = expression("Distribution of "~beta["state"]),
     fill = "Metric") +
common_theme + 
scale_colour_manual(values = c("cornflowerblue","mediumvioletred", "seagreen")) +
scale_fill_manual(values = c("cornflowerblue","mediumvioletred", "seagreen")) +
theme(legend.position = "top")

trend_coefs_hist <- fips_trends_comb_plot %>% 
pivot_longer(!c(unit, state, region)) %>% 
filter(name %in% c("wage_trend", "trend_pc")) %>% # , "trend"
mutate(name = case_when(name == 'wage_trend' ~ "Wage",
                        name == 'trend_pc' ~ "GDP pc")) %>% 
#name == 'trend' ~ "GDP")) %>% 
ggplot(aes(x = value)) +
geom_histogram(aes(fill = name), alpha = 0.4, bins = 250) +
geom_density(aes(color = name)) +
theme(legend.position = "top")  +
scale_colour_manual(values = c("GDP pc" = "cornflowerblue", "Wage" = "mediumvioletred")) + #, "GDP" = "mediumvioletred"))+
scale_fill_manual(values = c("GDP pc" = "cornflowerblue", "Wage" = "mediumvioletred")) + #, "GDP" = "mediumvioletred")) +
labs(title = "Histogram of Wage and GDPpc Growth Rates",
     x = "Growth Rate Value",
     y = "Count",
     color = "Metric",
     fill = "Metric") +
common_theme + 
theme(legend.title = element_text(size = 12, face = "italic")) + 
guides(color = guide_legend(
  title.position = "top",
  title.hjust = 0.5  # Center the title
))

p <- (trend_coefs_hist + hist_state) / (scatter_betas + hist_nat) + plot_annotation(title = "County Growth Rates and Jurisdictional Loadings", subtitle = "Each point or component unit of a distribution represents a single county.", theme = plot_annotation_theme)

p
#append_figure_to_appendix(p, "fig_growth_factor_loadings_fips.png", width = 10)
rm(p)

n_states <- dplyr::n_distinct(fips_trends_comb_plot $state)
tmp <- colorRampPalette(brewer.pal(9, "Oranges"))(n_states + 10)
myOranges <- tmp[-c(1:10)]
myOranges <- myOranges[1:n_states]

fips_states <- fips_trends_comb_plot  %>%
group_by(state) %>%
arrange(state, trend_pc) %>%
mutate(midpt = ifelse(row_number() == round((max(row_number()) - min(row_number()))/2),1, NA)) %>%
ungroup() %>%
mutate(midpt = row_number() * midpt) %>%
mutate(unit = factor(unit, levels = unit)) %>%
ggplot(aes(unit, trend_pc)) +
geom_segment(aes(x=unit ,xend=unit, y=0, yend=wage_trend), color = "lightgrey") +
geom_segment(aes(x=unit ,xend=unit, y=0, yend=trend_pc, color = state)) +
geom_point(color="darkblue", size=0.5) +
#geom_point(aes(x = unit ,wage_trend), color="green", size=0.5) +
coord_flip() +
geom_label(aes(midpt, 0.09,
               label = state),
           fill = NA,
           #family = "Special Elite",
           fontface = "bold",
           label.padding = unit(.2, "lines"),
           label.r = unit(.25, "lines"),
           label.size = .05,
           size = 3) +
labs(x = "Counties by State", y = "GDPpc Trend") +
common_theme + 
theme(axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10),
      legend.position = "none", 
      plot.background = element_rect(fill = NA),
      panel.background = element_rect(fill = NA, color = NA),
      panel.border = element_rect(fill = NA, color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(size = 13,
                                  margin = margin(r = 10)),
      panel.grid.major = element_line(color = NA, linewidth = 0.5),
      legend.title = element_text(size = 9),
      plot.margin = margin(10, 25, 10, 25)) +
scale_color_manual(values = myOranges)

fips_trend <- fips_trends_comb_plot  %>%
arrange(trend_pc) %>%
mutate(unit = factor(unit, levels = unit)) %>%
ggplot(aes(unit, trend_pc)) +
geom_segment(aes(x=unit ,xend=unit, y=0, yend=wage_trend), color = "lightgrey") +
geom_segment(aes(x=unit ,xend=unit, y=0, yend=trend_pc, color = unit)) +
geom_point(color="darkblue", size=0.5) +
coord_flip() +
labs(x = "Counties", y = "GDPpc Trend") +
common_theme + 
theme(axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10),
      legend.position = "none", 
      plot.background = element_rect(fill = NA),
      panel.background = element_rect(fill = NA, color = NA),
      panel.border = element_rect(fill = NA, color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(size = 13,
                                  margin = margin(r = 10)),
      panel.grid.major = element_line(color = NA, linewidth = 0.5),
      legend.title = element_text(size = 9),
      plot.margin = margin(10, 25, 10, 25)) +
scale_color_viridis(discrete = TRUE, option = "rocket", direction = -1)

#add_labels(fips_trend, fips_trends_plot, "trend_pc", 5) -> fips_trend

fips_regions <- fips_trends_comb_plot  %>%
group_by(region) %>%
arrange(region, trend_pc) %>%
mutate(midpt = ifelse(row_number() == round((max(row_number()) - min(row_number()))/2),1, NA)) %>%
ungroup %>%
mutate(midpt = row_number() * midpt) %>%
mutate(unit = factor(unit, levels = unit)) %>%
ggplot(aes(unit, trend_pc)) +
geom_segment(aes(x=unit ,xend=unit, y=0, yend=wage_trend), color = "lightgrey") +
geom_segment(aes(x=unit ,xend=unit, y=0, yend=trend_pc, color = region)) +
geom_point(color="darkblue", size=0.5) +
coord_flip() +
labs(x = "Counties by Region", y = "GDPpc Trend") +
geom_label(aes(midpt, 0.09,
               label = region),
           fill = NA,
           #family = "Special Elite",
           fontface = "bold",
           label.padding = unit(.2, "lines"),
           label.r = unit(.25, "lines"),
           label.size = .05,
           size = 3) +
common_theme + 
theme(axis.text.y = element_blank(),
      axis.text.x = element_text(size = 10),
      legend.position = "none", 
      plot.background = element_rect(fill = NA),
      panel.background = element_rect(fill = NA, color = NA),
      panel.border = element_rect(fill = NA, color = NA),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y = element_text(size = 13,
                                  margin = margin(r = 10)),
      panel.grid.major = element_line(color = NA, linewidth = 0.5),
      legend.title = element_text(size = 9),
      plot.margin = margin(10, 25, 10, 25)) +
scale_color_brewer(palette = "Reds")

p <- fips_regions + 
fips_states +  
fips_trend + 
plot_annotation(
  title = "County GDP pc and Wage Growth Rates",
  subtitle = str_wrap("County-level wage and GDP per capita growth rates are calculated using annual data from 2001-2021, controlling for national and state-level residual growth rates. The GDP per capita trends are represented in order by the colored bars/blue dots, grouped either by region (left), state (center), or overall (right). The grey bars represent the same county's wage growth rate.", str_wrap_subtitle),
  theme = plot_annotation_theme
)

p
append_figure_to_appendix(plot_obj = p, "fig_wage_trends_lollipop_fips.png",  
                          caption = "Wage and GDP PC Growth Rates by County", 
  label = "si_fig:fips_wage_trends_lollipop",
  width = "\\textwidth")
rm(p)

################################################################################
################################################################################
#################### PEARSON CORR GDP PC AND WAGES
################################################################################
################################################################################

# 1) compute unit-level correlations (if you already have this, skip this block)
unit_corr <- mines_fips_short %>%
group_by(unit) %>%
summarise(
  corr_gdp_wage = cor(diff_log_real_gdp_priv_ind_pc,
                      gr_annual_avg_wkly_wage,
                      use = "pairwise.complete.obs",
                      method = "pearson"),
  .groups = "drop"
)

# join state/region and keep one row per unit
unit_corr_plot <- unit_corr %>%
left_join(
  mines_fips_short %>% select(unit, state) %>% distinct(),
  by = "unit"
)

# 2) order units by state then by correlation (lowest -> highest within state)
unit_corr_plot <- unit_corr_plot %>%
arrange(state, corr_gdp_wage) %>%
mutate(unit_ord = factor(unit, levels = unique(unit)))    # preserve order

# 3) compute midpoint (y position) for each state block so we can label it
state_positions <- unit_corr_plot %>%
group_by(state) %>%
summarise(
  start = min(as.integer(unit_ord)),
  end   = max(as.integer(unit_ord)),
  mid   = (start + end) / 2,
  .groups = "drop"
)

unit_corr_plot$state = sapply(unit_corr_plot$state, get_state)
unit_corr_plot$region = sapply(unit_corr_plot$state, get_region)


gdp_wage_corr_plot <- ggplot(unit_corr_plot %>% arrange(corr_gdp_wage) %>%
                             mutate(unit = factor(unit, levels = unit)),
                           aes(x = unit, y = corr_gdp_wage, fill = corr_gdp_wage)) +
geom_col(show.legend = FALSE) +
geom_hline(aes(yintercept = 0)) + 
scale_fill_viridis_c(option = "cividis", limits = c(-1, 1)) +
coord_flip() +
labs(
  title = "County Correlation between GDPpc Growth and Wage Growth",
  x = "County (Unit)",
  y = "Correlation Coefficient"
) +
facet_wrap(~state, scales = "free") +
common_theme +
theme(
  panel.grid = element_blank(),   # remove grid lines
  panel.background = element_blank(), # remove gray panel background
  plot.background = element_blank(),  # remove outer gray background
  strip.background = element_blank()  # remove facet label background
)

# Save to appendix
#append_figure_to_appendix(
# plot_obj = gdp_wage_corr_plot,
# filename = "fig_gdp_wage_correlation_fips.png",
# caption = "County-level correlation between GDP per capita growth and wage growth, faceted by state. Each bar represents a single county, ordered by correlation strength within each state.",
# label = "fig:gdp_wage_corr_fips",
# width = "\\textwidth",
# fig_width = 10,    # Wider for faceted plot
# fig_height = 12    # Taller to accommodate many facets
# )

# Clean up
rm(unit_corr, unit_corr_plot, state_positions, gdp_wage_corr_plot)


################################################################################
################################################################################
#################### GROWING VS. DECLINING REGIONS REG TABLES (TABLES 8-9)
################################################################################
################################################################################
mines_fips_decl <- fips_trends %>%
select(unit, contains("trend"), contains("growing"), contains("declining")) %>%
left_join(mines_fips_short, ., by = "unit")

mines_fips_wage_decl <- fips_wage_trends %>%
select(unit, contains('trend'), contains('growing'), contains('declining')) %>%
left_join(mines_fips_short, ., by = "unit")

library(ggrepel)

# prepare data
plot_df <- mines_fips_wage_decl %>%
  select(year, unit, state, contains('extreme'), log_annual_avg_wkly_wage,
         gr_annual_avg_wkly_wage, natl_log_annual_avg_wkly_wage,
         natl_gr_annual_avg_wkly_wage, state_gr_annual_avg_wkly_wage) %>%
  group_by(unit) %>%
  arrange(unit, year) %>%
  mutate(
    first_obs = first(log_annual_avg_wkly_wage),
    trend_t0 = log_annual_avg_wkly_wage - first_obs
  ) %>%
  ungroup()

# choose which groups to highlight
# (example: growing_extreme == TRUE and declining_extreme == TRUE columns exist)
highlight_df <- plot_df %>%
  filter(growing_extreme == TRUE | declining_extreme == TRUE) %>% 
  mutate(extreme_grouping = case_when(growing_extreme ~ "Extreme Growth", 
                                        declining_extreme ~"Extreme Decline", 
                                        TRUE ~ NA))

# compute national trend (if available) — use the natl series (one value per year)
natl_df <- plot_df %>%
  select(year, natl_log_annual_avg_wkly_wage) %>%
  distinct() %>%
  arrange(year) %>%
  mutate(natl_trend = natl_log_annual_avg_wkly_wage - first(natl_log_annual_avg_wkly_wage))

# basic plot
p <- ggplot() +
  # 1) faint spaghetti for all Countys
  geom_line(
    data = filter(plot_df, growing_extreme != TRUE & declining_extreme != TRUE),
    aes(x = year, y = trend_t0, group = unit),
    color = "grey80",
    linetype = "dashed",
    linewidth = 0.5
  ) +
  # 2) overlay highlighted groups (growing / declining)
  geom_line(
    data = filter(plot_df, growing_extreme == TRUE),
    aes(x = year, y = trend_t0, group = unit),
    color = "#2b8cbe",   # blue
    linewidth = 0.25,
    alpha = 0.8
  ) +
  geom_line(
    data = filter(plot_df, declining_extreme == TRUE),
    aes(x = year, y = trend_t0, group = unit),
    color = "#ef3b2c",   # red
    linewidth = 0.25,
    alpha = 0.8
  ) +
  # 3) national reference line (bold)
  geom_line(
    data = natl_df,
    aes(x = year, y = natl_trend),
    color = "black",
    size = 1.0,
    linetype = "dashed"
  ) +
  # 4) add labels at the end for highlighted Countys (take last-year values)
  geom_text_repel(
    data = highlight_df %>% group_by(unit) %>% filter(year == max(year)),
    aes(x = year + 0.2, y = trend_t0, label = unit, color = as.factor(extreme_grouping)),
    size = 3,
    segment.size = 0.2,
    direction = "y",
    hjust = 0,
    show.legend = FALSE
  ) +
  # cosmetics
  scale_color_manual(values = c("Extreme Growth" = "#2b8cbe", "Extreme Decline" = "#ef3b2c")) + 
  labs(
    x = "Year",
    y = "Change in (log) wage since first observation in 2001",
    title = "County Wage Trends (Annual Average Weekly Wage compared to 2001)",
    subtitle = "All counties are displayed in faint grey.\nBlack dashed line represents the national trend\nRed (blue) lines represent observations that are in the 25th (75th) percentile of the distribution of\ncounty level growth rates as calculated in Section 3.2.1."
  ) +
  common_theme +

  scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01))

# widen plotting area to accommodate labels slightly to the right
p + coord_cartesian(xlim = c(min(plot_df$year), max(plot_df$year) + 2))

ggsave(here("output/wage_trends_plot_fips.png"), width = 12, height = 8)
decl <- filter(mines_fips_decl, declining_pc)
grow <- filter(mines_fips_decl, !declining_pc)

decl_extr <- filter(mines_fips_decl, declining_pc_extreme)
grow_extr <- filter(mines_fips_decl, growing_pc_extreme)

dfs <- list("All" = mines_fips_decl, "Hyper-Declining (GDP)" = decl_extr, "Declining (GDP)" = decl, "Growing (GDP)" = grow, "Hyper-Growing (GDP)" = grow_extr)


decl_wage <- filter(mines_fips_wage_decl, declining)
grow_wage <- filter(mines_fips_wage_decl, !declining)

decl_extr_wage <- filter(mines_fips_wage_decl, declining_extreme)
grow_extr_wage <- filter(mines_fips_wage_decl, growing_extreme)

dfs_wage <- list("All" = mines_fips_decl, "Hyper-Declining (Wage)" = decl_extr_wage, "Declining (Wage)" = decl_wage, "Growing (Wage)" = grow_wage, "Hyper-Growing (Wage)" = grow_extr_wage)

rm(decl_extr_wage, grow_extr_wage, decl_wage, grow_wage, decl, grow, decl_extr, grow_extr)
mods <- list()
#mods_fs <- list()
for(df in names(dfs)){
  # IV regression using shift-share instrument
  iv_model <- feols(as.formula(paste0(iv_lev_form, ss_lev_lev_va)),
                    data = dfs[[df]], panel.id = c("unit", "year"), cluster = "unit")
  mods[[df]] <- iv_model#$second_stage
  #mods_fs[[df]] <- iv_model$first_stage
  invisible(gc(verbose = FALSE))
}

mods_wage <- list()
#mods_wage_fs <- list()
for(df_wage in names(dfs_wage)){
  # IV regression using shift-share instrument
  iv_model <- feols(as.formula(paste0(iv_lev_form, ss_lev_lev_va)),
                    data = dfs_wage[[df_wage]], panel.id = c("unit", "year"), cluster = "unit")
  mods_wage[[df_wage]] <- iv_model#$second_stage
  #mods_wage_fs[[df_wage]] <- iv_model$first_stage
  invisible(gc(verbose = FALSE))
}


tbl <- etable(mods_wage, tex = latex_tables, headers = names(mods_wage), adjustbox = "max width=\\textwidth", fitstat = iv_fitstats, title = "Second-Stage: VA-based Shift-Share Instrument (l1) Applied to Declining Wage vs. Growing Wage Regions", label = "tbl_gdp_ss_wage_subsamples_fips")
append_table_to_appendix(tbl)
rm(tbl)

tbl <- etable(mods,   tex = latex_tables, adjustbox = "max width=\\textwidth", headers = names(mods), fitstat = iv_fitstats, title = "Second-Stage: VA-based Shift-Share Instrument (l1) Applied to Declining GDP vs. Growing GDP Regions", label = "tbl_gdp_ss_gdp_subsamples_fips")

append_table_to_appendix(tbl)
rm(tbl)

rm(mines_fips_decl, mines_fips_wage_decl, growth_rates_orthog, growth_rates, dfs_wage, mods, mods_wage, dfs)

invisible(gc(verbose = FALSE))


################################################################################
################################################################################
#################### STATE-BY-STATE (FIGURE 9)
################################################################################
################################################################################

append_table_to_appendix("\\FloatBarrier")
append_table_to_appendix("\\subsection{State-by-State Estimation}")

mines_fips_short %>%
  group_by(state) %>%
  summarise(n_fips = n_distinct(unit)) -> hist_states_fips

hist_states_fips$state_name = sapply(hist_states_fips$state, get_state)

outliers <- filter(hist_states_fips, n_fips > 100)
states_to_remove <- hist_states_fips %>% filter(n_fips < 5) %>% pull(state)

df_ivs_state <- df_ivs %>% filter(!(state %in% states_to_remove))
reg_states <- df_ivs_state %>%
  pull(state) %>%
  unique

if(!file.exists(here("data/temp/reg_states_fips.RDS"))){
  saveRDS(reg_states, here("data/temp/reg_states_fips.RDS"))
}

rm(df_ivs)
p <- ggplot(hist_states_fips, aes(x = n_fips)) +
  geom_histogram(bins = 25, fill = "dodgerblue4", color = "white", alpha = 0.8) +
  # add invisible points for label positioning
  geom_point(data = outliers, aes(y = 0), alpha = 0) +
  geom_text_repel(
    data = outliers,
    aes(x = n_fips, y = 1, label = state_name),
    nudge_y = 3.5,
    size = 3,
    color = "black"
  ) +
  common_theme +
  labs(
    x = "Number of Counties in State",
    y = "Number of States",
    title = "Distribution of Counties per State"
  )

p
append_figure_to_appendix(plot_obj = p, 
                          "fig_fips_dist_state_fips.png", 
                          caption = "Number of Counties per State",
                          label = "si_fig:fips_dist_by_state",
                         width = "\\textwidth")
rm(p)


extract_coef_info <- function(model, state_name, iv = FALSE, vars_of_interest_list) {
      # Get summary
      coef_summary <- summary(model)$coeftable
  if(iv){
      ftest_summary <- fitstat(model, "ivf1.stat")[[1]]
      ftest_p_summary <- fitstat(model, "ivf1.p")[[1]]
  }

  model_r2 <- r2(model, type = "wr2")
  # Filter for relevant variables
  coef_df <- as.data.frame(coef_summary) %>%
    rownames_to_column("variable") %>%
    filter(variable %in% vars_of_interest_list | grepl(paste0(vars_of_interest_list, collapse = "|"), variable)) %>%
    mutate(
      state = state_name,
      # Calculate 95% confidence intervals
      ci_lower = Estimate - 1.96 * `Std. Error`,
      ci_upper = Estimate + 1.96 * `Std. Error`,
      p_val = `Pr(>|t|)`,
      significance = case_when(
        p_val < 0.001 ~ "***",
        p_val < 0.01  ~ "**",
        p_val < 0.05  ~ "*",
        p_val < 0.1   ~ ".",
        TRUE               ~ ""
      ),
      r2_within = model_r2)
  
  if(iv){
    coef_df %>% 
      mutate(ftest = ftest_summary,
      ftest_p = ftest_p_summary) %>%
    select(state, variable, estimate = Estimate, ci_lower, ci_upper, p_val, significance, r2_within, ftest, ftest_p) %>% return(.)
  }else{
    coef_df %>% 
    select(state, variable, estimate = Estimate, ci_lower, ci_upper, p_val, significance, r2_within) %>% return(.)
  }
}


state_mods <- list()
for(k in reg_states){
  mod <- df_ivs_state %>%
    filter(state == k) %>%
    feols(as.formula("log_real_Elem_Educ_Total_Exp_pp ~ diff_log_real_Elem_Educ_Total_Exp_pp + log_annual_avg_wkly_wage + l1_log_annual_avg_wkly_wage + l2_log_annual_avg_wkly_wage + log_real_Total_IG_Revenue_pp + log_real_gdp_priv_ind_pc + log_Enrollment + pct_black + pct_hispanic | unit + year"),
    data = ., panel.id = c("unit", "year"), cluster = "unit")
  state_mods[[as.character(k)]] <- mod
}

# Apply to list of models
# Assuming: model_list is named by state names
results_df <- imap_dfr(state_mods, ~extract_coef_info(.x, .y, vars_of_interest_list = c("log_annual_avg_wkly_wage", "l1_log_annual_avg_wkly_wage", "l2_log_annual_avg_wkly_wage")))
results_df$state_name <- sapply(results_df$state, get_state)

results_df_cleaned_desc <- results_df %>%
  # Add lag labels
  mutate(label = case_when(
    variable == 'log_annual_avg_wkly_wage' ~ 'l0',
    variable == 'l1_log_annual_avg_wkly_wage' ~ 'l1',
    variable == 'l2_log_annual_avg_wkly_wage' ~ 'l2'
  )) %>%
  # filter(significance != "") %>%
  # Compute total estimate and max R²
  group_by(state_name) %>%
  mutate(
    total_estimate = sum(estimate, na.rm = TRUE),
    any_significant = any(significance != ""),
    r2_within_max = max(r2_within, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  # Order `state` by R²
  mutate(state_name = fct_reorder(state_name, total_estimate, .desc = TRUE)) %>%
  # Create numeric lag order (to control x-axis sorting within each state)
  mutate(
    lag_order = case_when(label == "l0" ~ 1,
                          label == "l1" ~ 2,
                          label == "l2" ~ 3),
    state_label = paste(state_name, label, sep = "_")
  ) %>%
  # Reorder state_label using combined (state, lag_order)
  arrange(state_name, lag_order) %>%
  mutate(state_label = factor(state_label, levels = unique(state_label)))


# Create axis labels: state names at 'l1' only, blanks otherwise
axis_labels <- ifelse(grepl("_l0$", levels(results_df_cleaned_desc$state_label)),
                      gsub("_l0$", "", levels(results_df_cleaned_desc$state_label)),
                      "")

rm(mines_fips, pct_2, pct_1, iv_model_2d_lev_gr, iv_model_2d_lev_lev, iv_model_2d_gr_lev_lev_ed, iv_model_2d_lev_lev_ig, iv_model_2d_gr_lev, iv_model_2d_gr_gr, scatter_betas, hist_state, hist_nat, trends_coefs_hist, trend_pc_hist)
f_stat_threshold = 12

sbs_iv_cache <- here("output/cache/state_by_state_iv_fips.RDS")
if (file.exists(sbs_iv_cache)) {
  state_mods_ss <- readRDS(sbs_iv_cache)
} else {
  state_mods_ss <- list()
  for(k in reg_states){
    mod <- df_ivs_state %>%
      filter(state == k) %>%
      feols(as.formula(paste0(iv_lev_form, ss_lev_lev_va)),
      data = ., panel.id = c("unit", "year"), cluster = "unit")
    state_mods_ss[[as.character(k)]] <- mod#$second_stage
    invisible(gc(verbose = FALSE))
  }
  saveRDS(state_mods_ss, sbs_iv_cache)
}

# Apply to list of models
# Assuming: model_list is named by state names
results_df <- imap_dfr(state_mods_ss, ~extract_coef_info(.x,.y, iv = TRUE, vars_of_interest_list = c("fit_log_annual_avg_wkly_wage", "fitted_endog")))
results_df$state_name <- sapply(results_df$state, get_state)

results_df_cleaned <- results_df %>%
  # Add lag labels
  mutate(label = case_when(
    variable == 'fit_log_annual_avg_wkly_wage' ~ 'HPI Effect'
  )) %>%
  #filter(significance != "") %>%
  # Compute total estimate and max R²
filter(r2_within > 0 & ftest >= f_stat_threshold & ftest_p < 0.05 & p_val < 0.1) %>%
  # Order `state` by R²
  mutate(state_name = fct_reorder(state_name, estimate, .desc = TRUE)) %>%
  # Reorder state_label using combined (state, lag_order)
  arrange(state_name) %>%
  mutate(state_label = factor(state_name, levels = unique(state_name)))

# Create axis labels: state names at 'l1' only, blanks otherwise
axis_labels <- levels(results_df_cleaned$state_label)
sig_states <- axis_labels

state_by_state_sig <- results_df_cleaned %>%
  # Plot
  ggplot(aes(x = state_label)) +
# Fill layer (with alpha by significance)
# geom_point(
#   aes(y = estimate, fill = ftest),
#    size = 4) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2, alpha = 0.5) +

geom_point(aes(y = estimate, size = ftest, color = r2_within), stroke = 0.25, alpha = 1) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  common_theme +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 90, hjust = 1)
  ) +
  labs(
    title = "Effect of 1% Increase in Wage (using VA SS Instrument) on Education Expenditure per Pupil",
    x = "States",
    y = "Coefficient Estimate",
    caption = paste0("Effect of 1% increase in Wage using l1 of VA SS Instrument on PP Education\n Expenditure controlling for enrollment, GDP, % black, % hispanic, and intergovernmental transfers.\n Within R^2 of state-level estimation reflected in color of point. \nDisplays only states whose second-stage coefficient is statistically significant at the 10% level and first-stage F statistic >= ", f_stat_threshold, " and p-value < 0.05"),
   color = "Within R2 of State-level Estimation",
    size = "F-test statistic value",
    alpha = "Statistical Significance"
  ) +
    scale_x_discrete(labels = axis_labels)  +
  scale_fill_distiller(direction = 1) #+
  # scale_shape_manual(
  # values = c(
  #   "***" = 21,
  #   "**"  = 22,
  #   "*"   = 23,
  #   "."   = 24#,
  #   #""    = 25  # non-significant
  # )
#)

ggsave(here("output/state_by_state_est_plot_fips.png"), width = 8, height = 7)
results_df_cleaned_iv <- results_df_cleaned
if(!file.exists(here("data/temp/results_df_cleaned_iv_fips.RDS"))){
  saveRDS(results_df_cleaned_iv, here("data/temp/results_df_cleaned_iv_fips.RDS"))
  saveRDS(sig_states, here("data/temp/sig_states_fips.RDS"))
}

if(!file.exists(here("data/temp/results_df_fips.RDS"))){
  saveRDS(results_df, here("data/temp/results_df_fips.RDS"))
}

testerr <- mines_fips_short %>% select(state, 
                                     share_own,  
                                     log_real_gdp_priv_ind_pc,
          log_real_gdp_priv_ind,
          log_real_Elem_Educ_Total_Exp_pp,
          log_Enrollment, 
          log_annual_avg_wkly_wage,
          log_pop_school_age,
          pct_black,
          pct_hispanic,
          pct_white,
         log_real_Property_Tax_pp) %>% group_by(state) %>% 
  summarise(across(everything(), ~mean(., na.rm = TRUE))) %>% ungroup
testerr$state_name <- sapply(testerr$state, get_state)

state_rankings <- testerr %>%
  pivot_longer(
    cols = -c(state, state_name),  # All columns except 'state'
    names_to = "variable",
    values_to = "value"
  ) %>%
  group_by(variable) %>%
  mutate(
    rank = rank(-value, ties.method = "first"),  # Higher values = better rank (rank 1)
    # Or use rank(value) if lower values should be rank 1
    in_analysis = state %in% results_df_cleaned_iv$state,  # TRUE/FALSE for coloring
    color_in_analysis = ifelse(in_analysis, state_name, NA)
  ) %>%
  ungroup() %>% 
  mutate(var_label = case_when(variable == 'log_real_gdp_priv_ind_pc' ~ "(log) Private Industry GDP per capita",
#variable == 'diff_log_real_gdp_priv_ind_pc' ~ '(GR) Private Industry GDP per capita',
#variable ==  'diff_log_real_gdp_priv_ind' ~ '(GR) Private Industry GDP',
#variable == 'log_real_gdp_priv_ind' ~ "(log) Private Industry GDP",
variable == 'log_annual_avg_wkly_wage' ~ '(log) Annual Avg Weekly Wage',
#variable == 'gr_annual_avg_wkly_wage' ~ '(GR) Annual Avg Weekly Wage',
#variable == 'log_Enrollment' ~ '(log) Enrollment',
variable == 'log_pop_school_age' ~ '(log) School-age Population',
variable == 'share_own' ~ 'Share of Exp from Local Sources',
variable == "log_real_Elem_Educ_Total_Exp_pp" ~ "Education Expenditure per pupil",
variable == 'log_real_Property_Tax_pp' ~ "(log) Property Taxes Collected per pupil",
variable ==  "pct_black" ~ "% Black",
variable ==  "pct_hispanic" ~ "% Hispanic",
variable ==  "pct_white" ~ "% White"))

state_rankings_plot <- ggplot(filter(state_rankings, !is.na(var_label)), aes(x = var_label, y = rank)) +
  geom_point(aes(color = color_in_analysis), size = 2, alpha = 0.7) +
  geom_text(aes(label = color_in_analysis, color = color_in_analysis),
    size = 4,
    hjust = -0.2,  # Offset text to the right of points
    check_overlap = TRUE  # Prevents label overlap
  ) +
  scale_y_reverse() +  # Rank 1 at top
  # scale_color_manual(
  #   #values = c("TRUE" = "steelblue", "FALSE" = "gray60"),
  #   #labels = c("TRUE" = "In Analysis", "FALSE" = "Not in Analysis"),
  #   #name = "Included in IV Analysis"
  # ) +
  labs(
    x = "Indicator",
    y = "Rank",
    subtitle = str_wrap("All 35 states included in the state-by-state estimation are ranked by the population-weighted mean across counties within the state. Each variable is ranked from highest (top) to lowest (bottom) value.", str_wrap_subtitle),
    title = "State Rankings Across Socioeconomic Indicators"
  ) +
  common_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "none"
  )

p <- state_by_state_sig / state_rankings_plot + 
  plot_layout(heights = unit(c(1, 2), c('null', 'null')))

p
append_figure_to_appendix(
  plot_obj = p,
  filename = "fig_state_estimates_rankings_fips.png",
  caption = "State-by-State Estimation with County Units",
  label = "si_fig:state_by_state_rankings_fips",
  width = "\\textwidth"
)
rm(p)


################################################################################
################################################################################
#################### POLICIES (FIGURE 9)
################################################################################
################################################################################
#append_table_to_appendix("\\FloatBarrier")
#append_table_to_appendix("\\subsection{Political Economy and Fiscal Transmission Channel}")

df_dist_all_sum <- readRDS(here("data/temp/df_dist_all_sum.RDS"))
ss_variance <- df_ivs_state %>%
  group_by(state, year) %>%
  summarize(sd_cross_fips = sd(lev_gdp_ss_2d, na.rm = TRUE), n_fips = n(), .groups = "drop") %>%
  group_by(state) %>%
  summarize(mean_sd_cross_fips = mean(sd_cross_fips, na.rm = TRUE),
            mean_n_fips       = mean(n_fips), .groups = "drop") %>%
  left_join(results_df %>% select(state, ftest, estimate, p_val) %>%
              group_by(state) %>% slice(1), by = "state") %>%
  mutate(
    state_name = sapply(state, get_state),
    sig_state  = state_name %in% sig_states
  ) %>%
  arrange(desc(mean_sd_cross_fips))

grade_fill_colours <- c("A" = "#1D9E75",
                        "B" = "#7FB539",
                        "C" = "#9A9A9A",  # grey
                        "D" = "#D85A30",
                        "F" = "#E24B4A")

state_order <- ss_variance %>%
  arrange(mean_sd_cross_fips) %>%
  pull(state_name)

saveRDS(state_order, here("data/temp/state_order_fips.RDS"))

df_grades_over_time <- readRDS(here("data/temp/df_grades_over_time.RDS"))

# Figure out a sensible x-range for the grade strip.
# Place the strip in the rightmost portion of the plot so it doesn't
# overlap the bars. Adjust `strip_start` and `strip_width` to taste.
y_max        <- max(ss_variance$mean_sd_cross_fips, na.rm = TRUE)
strip_start  <- y_max * 1.02                        # just past longest bar
strip_width  <- y_max * 0.25                        # how wide the strip is
year_range   <- range(df_grades_over_time$source_year)

# Width of each tile on the y-axis (in data units).
# The spacing between consecutive years is strip_width / (n_years - 1),
# so set tile_width to match that for a continuous bar with no gaps.
n_years    <- length(unique(df_grades_over_time$source_year))
tile_width <- strip_width / (n_years - 1)

fig_ss_var_1 <- ggplot(
  ss_variance %>%
    left_join(df_dist_all_sum, by = "state_name") %>% 
    mutate(state_name = factor(state_name, levels = state_order)),
  aes(x = state_name, y = mean_sd_cross_fips, fill = sig_state)
) +
  geom_col(alpha = 0.85) +
  coord_flip(clip = "off") +
  scale_fill_manual(
    values = c("TRUE" = "#1a7a3c", "FALSE" = "grey70"),
    labels = c("TRUE" = "Included in main IV results",
               "FALSE" = "Not significant"),
    name   = NULL
  ) +
  # Per-year grade strip as shaded tiles
  # ggnewscale::new_scale_fill() +
  # geom_tile(
  #   data = df_grades_over_time,
  #   aes(x = state_name, y = axis_placement, fill = grade),
  #   width = 0.8, height = tile_width,
  #   inherit.aes = FALSE
  # ) +
  # scale_fill_manual(values = grade_fill_colours, name = "Grade") +
  # scale_y_continuous(
  #   expand = expansion(mult = c(0.01, 0.05)),
  #   limits = c(0, strip_start + strip_width * 1.05)
  # ) +
  geom_text(aes(y = 0.01, hjust = 0, label = fiscal_mech, color = fiscal_mech_color), size = 2.5, fontface = "bold", family = "LMRoman10-Bold") +
  scale_color_manual(values = c(                                       # ← second color scale
    "Yes" = "black",
    "No"  = "white"
  ), name = "Broken Fiscal Channel") +
  labs(
    title    = "Cross-County Variation in Shift-Share Instrument by State",
    subtitle = str_wrap("Mean within-year SD of the shift-share instrument across counties.
       Higher values indicate more heterogeneous industry exposure across
       counties within the same state. The color of the bar displays whether the state exhibits a statistically significant (green) non-zero relationship between wages and public education expenditure per pupil. The text label represents the presence of a legislative or constitutional redistribution mechanism, and the policy's name, if applicable. If a policy was implemented between 2001-2021 (during panel window), the year is displayed along with the policy name.", str_wrap_subtitle), 
    x = NULL, y = "Mean cross-county SD of shift-share instrument",
    caption = "SD computed across counties within each state-year, then averaged."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title       = element_text(face = "bold", size = 13),
    plot.subtitle    = element_text(size = 11, colour = "grey30"),
    legend.position  = "none",
    panel.grid.minor = element_blank(),
    plot.margin      = margin(5, 30, 5, 5)
  ) + common_theme


#append_figure_to_appendix(fig_ss_var_1, "fig_policy_wage_variation_fips.png", height = 9)
rm(fig_ss_var_1)

fig_ss_var_2 <- ss_variance %>%
  left_join(df_dist_all_sum, by = "state_name") %>% 
  left_join(
    select(filter(state_rankings, variable == "share_own"), state_name, value, rank, var_label),
    by = "state_name"
  ) %>%
  mutate(
    state_type = case_when(
      sig_state == TRUE  & fiscal_mech_color == "Yes" ~ "Sig + Fiscal mech",
      sig_state == TRUE  & fiscal_mech_color == "No"  ~ "Sig + No mech",
      sig_state == FALSE & fiscal_mech_color == "Yes" ~ "Not sig + Fiscal mech",
      sig_state == FALSE & fiscal_mech_color == "No"  ~ "Not sig + No mech"
    )
  ) %>%
  ggplot(aes(x = value, y = mean_sd_cross_fips)) + 
  
  # Quadrant shading
  annotate("rect", xmin = -Inf, xmax = 0.376, ymin = 0.753, ymax = Inf,
           fill = "steelblue", alpha = 0.05) +
  annotate("rect", xmin = 0.376, xmax = Inf, ymin = 0.753, ymax = Inf,
           fill = "red", alpha = 0.05) +
  annotate("rect", xmin = -Inf, xmax = 0.376, ymin = -Inf, ymax = 0.753,
           fill = "grey", alpha = 0.05) +
  annotate("rect", xmin = 0.376, xmax = Inf, ymin = -Inf, ymax = 0.753,
           fill = "orange", alpha = 0.05) +
  geom_hline(yintercept = 0.753, linetype = "dashed", color = "grey40", linewidth = 0.4) +
  geom_vline(xintercept = 0.376,  linetype = "dashed", color = "grey40", linewidth = 0.4) +
  
  geom_point(
    aes(color = state_type, shape = state_type),   # ← both mapped to state_type
    size = 3, stroke = 0.8
  ) +
  scale_color_manual(
    values = c(
      "Sig + Fiscal mech"     = "darkorange",
      "Sig + No mech"         = "red",
      "Not sig + Fiscal mech" = "#5b8db8",
      "Not sig + No mech"     = "grey60"
    ),
    labels = c(
      "Sig + Fiscal mech"     = "Significant + Redist. Policy",
      "Sig + No mech"         = "Significant + No Redist. Policy",
      "Not sig + Fiscal mech" = "Not significant + Redist. Policy",
      "Not sig + No mech"     = "Not significant + No Redist. Policy"
    ),
    name = "Point Label"       # ← must match shape scale name
  ) +
  scale_shape_manual(
    values = c(
      "Sig + Fiscal mech"     = 4,    # ×
      "Sig + No mech"         = 16,   # circle
      "Not sig + Fiscal mech" = 4,    # ×
      "Not sig + No mech"     = 16    # circle
    ),
    labels = c(
      "Sig + Fiscal mech"     = "Significant + Redist. Policy",
      "Sig + No mech"         = "Significant + No Redist. Policy",
      "Not sig + Fiscal mech" = "Not significant + Redist. Policy",
      "Not sig + No mech"     = "Not significant + No Redist. Policy"
    ),
    name = "Point Label"       # ← must match color scale name
  ) +
  
  ggrepel::geom_text_repel(
    aes(label = state_name, color = state_type),
    size = 2.8, max.overlaps = 20,
    family = "LMRoman10-Bold",
    show.legend = FALSE
  ) +
  
  annotate("text", x = -Inf, y = Inf,  hjust = 0, vjust = 1.5,
           label = "High Exposure\nWeak Fiscal Channel",   size = 3, family = "LMRoman10-Bold") +
  annotate("text", x = Inf,  y = Inf,  hjust = 1, vjust = 1.5,
           label = "High Exposure\nStrong Fiscal Channel", size = 3, family = "LMRoman10-Bold") +
  annotate("text", x = -Inf, y = -Inf, hjust = 0, vjust = -0.5,
           label = "Low Exposure\nWeak Fiscal Channel",    size = 3, family = "LMRoman10-Bold") +
  annotate("text", x = Inf,  y = -Inf, hjust = 1, vjust = -0.5,
           label = "Low Exposure\nStrong Fiscal Channel",  size = 3, family = "LMRoman10-Bold") +
  
  labs(
    x       = "Share of Education Expenditure from Local Sources",
    y       = "Mean cross-county SD of shift-share instrument",
    title   = "Reliance on Local Tax Base vs. Instrument Variation by State",
    caption = "Dashed lines show median values. Color and shape = IV significance + Presence of Redistributive Policy."
  ) +
  theme_minimal(base_size = 11) +
  scale_x_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.15))) +
  common_theme +
  theme(legend.position = "bottom",
        panel.grid.major = element_blank()) +
  guides(color = guide_legend(ncol = 2),  
         shape = guide_legend(ncol = 2))  

fig_ss_var_2
# append_figure_to_appendix(
#   plot_obj = fig_ss_var_2,
#   filename = "fig_policy_instrument_strength_fips.png",
#   caption = "Reliance on Local Tax Base vs. Instrument Strength",
#   label = "si_fig:fipg_policy_instrument_strenght_fips",
#   width = "\\textwidth")
rm(fig_ss_var_2)

rm(list = ls())

# Remove appendix header from local file
tex_content <- readLines(here("output/tex", appendix_file))
tex_content <- tex_content[tex_content != "\\appendix"]
writeLines(tex_content, here("output/tex", appendix_file))

# Remove appendix header from dropbox file
tex_content <- readLines(file.path(DROPBOX_PAPER_PATH, "si_docs", appendix_file))
tex_content <- tex_content[tex_content != "\\appendix"]
writeLines(tex_content, file.path(DROPBOX_PAPER_PATH, "si_docs", appendix_file))



