# Pulls the final version of the data from school_dist_cleaning.Rmd
# Additional datasets incorporated below
# To be reincorporated into school_dist_cleaning.Rmd before submission

if(unit_id == "cz_id"){
  print(paste0("Running analysis on CZs (", unit_id, ")."))
  mines_cz <- readRDS(here("data/out/cz_dataset.RDS")) %>% 
    mutate(unit = get(unit_id)) %>% 
    rename(state = main_state)
  # 
  # ss <- readRDS(here("data/temp/shift_shares_cz_base_01_05_11.RDS")) %>% 
  #   mutate(unit = get(unit_id))
  
  ## Wage Data (QCEW)
  
  wages <- readRDS(here("data/raw/QCEW/QCEW_wage_stats_cz.RDS")) %>% 
    mutate(across(!c(cz_id, year), ~log(.), .names = "log_{.col}")) %>% 
    group_by(cz_id) %>% 
    arrange(year) %>% 
    mutate(across(contains("log"), ~. - lag(., 1), .names = gsub("log","", "gr_{.col}")),
           across(!c(year), list(l1 = ~dplyr::lag(., 1), l2 = ~dplyr::lag(., 2)), .names = "{.fn}_{.col}")) %>% 
    rename(unit = cz_id) %>%
    rename_with(~ str_replace(., "gr_log_", "gr_"), contains("gr_log_")) %>% 
    ungroup %>% 
    arrange(unit)
  
  mines_cz <- mines_cz %>% 
    left_join(., wages, by = c("unit", "year"))
  
  ## Property Price Data (FHFA)
  source(here('data/raw/fhfa_hpi/cleaning_fhfa.R'))
  mines_cz <- mines_cz %>% 
    left_join(., rename(df_cz, unit = cz_id), by = c("unit", "year")) %>%
    mutate(state_share = real_Total_State_IG_Revenue_pp/real_Total_Educ_Total_Exp_pp)
  
}else if(unit_id == "fips"){
  print(paste0("Running analysis on counties (", unit_id, ")."))
  mines_cz <- readRDS(here("data/out/regression_data_complete_fips.RDS")) %>% 
    mutate(unit = get(unit_id)) %>% 
    group_by(unit) %>%
    arrange(unit, year) %>% 
    mutate(across(c(starts_with("log_real_gdp_total") | starts_with("log_real_gdp_priv_ind")), ~.- dplyr::lag(., 1), .names = "diff_{.col}")) %>% 
    ungroup 
  
  # ss <- readRDS(here("data/temp/shift_shares_base_01_05_11.RDS")) %>% 
  #   mutate(unit = get(unit_id)) 
}else{
  stop("You need to specify fips or cz_id as the panel unit.")}