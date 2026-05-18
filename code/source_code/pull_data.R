# Pulls the final version of the data from school_dist_cleaning.Rmd
# Additional datasets incorporated below
# To be reincorporated into school_dist_cleaning.Rmd before submission

# CAREFUL WITH ANY LAGS OR DIFFS OF EXPENDITURE/REVENUE ITEMS RECORDED IN YEAR 2000 (INCLUDES L2 ITEMS IN 2001) AS THESE RELY ON ZERO VALUES IN 1999 and 1998!!! These are not included in the main analysis, but saved here as a warning for future replicators. 
if(unit_id == "cz_id"){
  print(paste0("Running analysis on CZs (", unit_id, ")."))
  mines_cz <- readRDS(here("data/out/cz_dataset_corrected_lags_2.RDS")) %>% 
  #mines_cz <- readRDS(here("data/out/cz_dataset.RDS")) %>% 
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
  
  rm(wages)
  
  ## Property Price Data (FHFA)
  source(here('data/raw/fhfa_hpi/cleaning_fhfa.R'))
  mines_cz <- mines_cz %>% 
    left_join(., rename(df_cz, unit = cz_id), by = c("unit", "year")) %>%
    mutate(state_share = real_Total_State_IG_Revenue_pp/real_Total_Educ_Total_Exp_pp) %>% 
    arrange(cz_id, year) %>% 
    group_by(cz_id) %>% 
    mutate(l1_log_real_Elem_Educ_Total_Exp_pp = lag(log_real_Elem_Educ_Total_Exp_pp),
           l1_diff_log_real_Elem_Educ_Total_Exp_pp = lag(diff_log_real_Elem_Educ_Total_Exp_pp),
           l1_log_real_Total_IG_Revenue_pp = lag(log_real_Total_IG_Revenue_pp)) %>% 
    ungroup
  
  rm(df_cz)
  rm(df)
  
  race_data <- readRDS(here("data/raw/race_controls/data_race_seer_selected_cz.RDS")) %>% 
    rename(pct_hispanic = pct_pop_hispanic)
  
  mines_cz_race <- mines_cz %>% 
    left_join(., race_data, by = c("cz_id", "year"))
  
  stopifnot(mines_cz %>% filter(!(cz_id %in% race_data$cz_id)) %>% nrow(.) == 0)

  stopifnot(mines_cz_race %>% 
              select(-c("race_pop_total", "pop_hispanic", "pct_hispanic", "pct_white", "pct_black", "pct_ai_an", "pct_asian_pac", "pop_race_white", "pop_race_black", "pop_race_ai_an", "pop_race_asian_pac")) %>% 
              identical(mines_cz))
  
  mines_cz <- mines_cz_race
  
  state_natl_wages <- readRDS(here("data/raw/QCEW/wage_growth_rate_data_state_natl.RDS"))
  stopifnot(setdiff(mines_cz$state, state_natl_wages$state) == 0)
  
  mines_cz_state_wages <- left_join(mines_cz, state_natl_wages, by = c("year", "state"))
  
  stopifnot(mines_cz_state_wages %>% summarise(across(all_of(names(state_natl_wages)), ~sum(is.na(.)))) %>% rowSums == 0)
  
  stopifnot(mines_cz_state_wages %>% 
              select(-names(state_natl_wages)[which(!(names(state_natl_wages) %in% c("year", "state")))]) %>% 
              identical(mines_cz))
  
  mines_cz <- mines_cz_state_wages
  
  rm(state_natl_wages)
  rm(mines_cz_state_wages)
  
  priv_school_data <- readRDS(here("data/raw/priv_school_enrolment/complete_priv_school_enrolment_cz.RDS")) %>% 
    select(cz_id, year, pct_private_school_primary_acs5, pct_private_school_primary_acs1)
  
  mines_cz_priv_school <- mines_cz %>% 
    left_join(., priv_school_data, by = c("cz_id", "year"))

  stopifnot(mines_cz %>% filter(!(cz_id %in% priv_school_data$cz_id)) %>% nrow(.) == 0)
  
  stopifnot(mines_cz_priv_school %>% 
              select(-c('pct_private_school_primary_acs5', 'pct_private_school_primary_acs1')) %>% 
              identical(mines_cz))
  
  mines_cz <- mines_cz_priv_school
  
  within_cz_vars <- readRDS(here("data/out/within_vars_cz.RDS")) %>% 
    select(-n_school_districts)
  
  stopifnot(mines_cz %>% filter(!(cz_id %in% within_cz_vars$cz_id)) %>% nrow(.) == 0)
  stopifnot(mines_cz %>% group_by(cz_id, year) %>% arrange(cz_id, year) %>% identical(., arrange(group_by(mines_cz, cz_id, year), cz_id, year)))
  
  mines_cz_within <- mines_cz %>% 
    left_join(within_cz_vars, by = c("cz_id", "year"))
  
  stopifnot(mines_cz_within %>%
    select(-c(var_Elem_Educ_Total_Exp_pp, min_Elem_Educ_Total_Exp_pp, max_Elem_Educ_Total_Exp_pp, diff_min_max_Elem_Educ_Total_Exp_pp, 
              var_Total_Revenue_pp, min_Total_Revenue_pp, max_Total_Revenue_pp, diff_min_max_Total_Revenue_pp, 
              var_Total_Rev_Own_Sources_pp, min_Total_Rev_Own_Sources_pp, max_Total_Rev_Own_Sources_pp, diff_min_max_Total_Rev_Own_Sources_pp, 
              var_Total_IG_Revenue_pp, min_Total_IG_Revenue_pp, max_Total_IG_Revenue_pp, diff_min_max_Total_IG_Revenue_pp),
             -contains("var_log"), -contains("min_log"), -contains("max_log"), -contains("diff_min_max")) %>% 
    identical(mines_cz))
  
  mines_cz <- mines_cz_within %>% 
    filter(year >= 2001)
  
}else if(unit_id == "fips"){
  
  print(paste0("Running analysis on counties (", unit_id, ")."))
  mines_cz <- readRDS(here("data/out/regression_data_complete_fips.RDS")) %>% 
    mutate(unit = get(unit_id)) %>% 
    group_by(unit) %>%
    arrange(unit, year) %>% 
    mutate(across(c(starts_with("log_real_gdp_total") | starts_with("log_real_gdp_priv_ind")), ~.- dplyr::lag(., 1), .names = "diff_{.col}")) %>% 
    ungroup 
  
  ## Wage Data (QCEW)
  
  wages <- readRDS(here("data/raw/QCEW/QCEW_wage_stats_fips.RDS")) %>% 
    mutate(across(!c(fips, year), ~log(.), .names = "log_{.col}")) %>% 
    group_by(fips) %>% 
    arrange(year) %>% 
    mutate(across(contains("log"), ~. - lag(., 1), .names = gsub("log","", "gr_{.col}")),
           across(!c(year), list(l1 = ~dplyr::lag(., 1), l2 = ~dplyr::lag(., 2)), .names = "{.fn}_{.col}")) %>% 
    rename(unit = fips) %>%
    rename_with(~ str_replace(., "gr_log_", "gr_"), contains("gr_log_")) %>% 
    ungroup %>% 
    arrange(unit)
  
  mines_cz_wages <- mines_cz %>% 
    left_join(., wages, by = c("unit", "year"))
  
  stopifnot(mines_cz_wages %>% 
              select(-c(names(wages)[which(!(names(wages) %in% c("unit", "year")))])) %>% 
              identical(mines_cz))
  
  mines_cz <- mines_cz_wages
  rm(mines_cz_wages)
  rm(wages)

  
  ## Property Price Data (FHFA)
  data_hpi <- readRDS(here('data/raw/fhfa_hpi/hpi_fips.RDS'))
  mines_cz_hpi <- mines_cz %>% 
    left_join(., rename(data_hpi, unit = fips), by = c("unit", "year")) #%>%
    #mutate(state_share = real_Total_State_IG_Revenue_pp/real_Total_Educ_Total_Exp_pp) %>% 
   # arrange(cz_id, year) %>% 
   # group_by(cz_id) %>% 
   # mutate(l1_log_real_Elem_Educ_Total_Exp_pp = lag(log_real_Elem_Educ_Total_Exp_pp),
          # l1_diff_log_real_Elem_Educ_Total_Exp_pp = lag(diff_log_real_Elem_Educ_Total_Exp_pp)) %>% 
   # ungroup

  stopifnot(mines_cz_hpi %>% 
              select(-c( "hpi_annual_change_pct", "hpi", "hpi_1990_base", "hpi_2000_base", "log_hpi", "log_hpi_1990_base","log_hpi_2000_base",
                         "l1_log_hpi", "l2_log_hpi", "l3_log_hpi", "l4_log_hpi", "l5_log_hpi",  "gr_hpi", "l1_gr_hpi", "l2_gr_hpi", "l3_gr_hpi","l4_gr_hpi", "l_gr_hpi")) %>% 
              identical(mines_cz))

  mines_cz <- mines_cz_hpi
  rm(data_hpi)
  rm(mines_cz_hpi)
  
  race_data <- readRDS(here("data/raw/race_controls/data_race_seer.rds")) %>% 
    mutate(fips = ifelse(fips == "46113", "46102", fips)) %>% 
    filter(fips %in% mines_cz$fips & year >= 2001) %>% 
    mutate(across(c(pct_black, pct_ai_an, pct_asian_pac), ~ifelse(is.na(.), 0, .))) %>% 
    complete(year, fips) %>% 
    group_by(fips) %>% 
    arrange(year) %>% 
    fill(state_postal, state_fips, race_pop_total, pop_hispanic, pct_hispanic, pct_white, pct_black, pct_ai_an, pct_asian_pac, pop_race_white, pop_race_black, pop_race_ai_an, pop_race_asian_pac, .direction = "downup") %>% 
    ungroup 

  # race_data %>% saveRDS(here("data/raw/race_controls/data_race_seer_selected_fips.RDS"))
  
  stopifnot(mines_cz %>% filter(!(fips %in% race_data$fips)) %>% nrow(.) == 0)
  
  mines_cz_race <- mines_cz %>% 
    left_join(., race_data, by = c("year", "fips"))
  
  stopifnot(mines_cz_race %>% 
              select(-c("state_postal", "state_fips", "race_pop_total", "pop_hispanic", "pct_hispanic", "pct_white", "pct_black", "pct_ai_an", "pct_asian_pac", "pop_race_white", "pop_race_black", "pop_race_ai_an", "pop_race_asian_pac")) %>% 
              identical(mines_cz))
  
  mines_cz <- mines_cz_race
  
  # State and national wages
  state_natl_wages <- readRDS(here("data/raw/QCEW/wage_growth_rate_data_state_natl.RDS"))
  stopifnot(setdiff(mines_cz$state, state_natl_wages$state) == 0)
  
  mines_cz_state_wages <- left_join(mines_cz, state_natl_wages, by = c("year", "state"))
  
  stopifnot(mines_cz_state_wages %>% summarise(across(all_of(names(state_natl_wages)), ~sum(is.na(.)))) %>% rowSums == 0)
  
  stopifnot(mines_cz_state_wages %>% 
              select(-names(state_natl_wages)[which(!(names(state_natl_wages) %in% c("year", "state")))]) %>% 
              identical(mines_cz))
  
  mines_cz <- mines_cz_state_wages
  
  rm(state_natl_wages)
  rm(mines_cz_state_wages)
  
  priv_school_data <- readRDS(here("data/raw/priv_school_enrolment/complete_priv_school_enrolment_fips.RDS")) %>% 
    select(fips, year, pct_private_school_primary_acs5, pct_private_school_primary_acs1)
  
  mines_cz_priv_school <- mines_cz %>% 
    left_join(., priv_school_data, by = c("fips", "year"))
  
  stopifnot(mines_cz %>% filter(!(fips %in% priv_school_data$fips)) %>% nrow(.) == 0)
  
  stopifnot(mines_cz_priv_school %>% 
              select(-c('pct_private_school_primary_acs5', 'pct_private_school_primary_acs1')) %>% 
              identical(mines_cz))
  
  mines_cz <- mines_cz_priv_school
  
  within_cz_vars <- readRDS(here("data/out/within_vars_fips.RDS")) %>% 
    select(-n_school_districts)
  
  stopifnot(mines_cz %>% filter(!(fips %in% within_cz_vars$fips)) %>% nrow(.) == 0)
  stopifnot(mines_cz %>% group_by(fips, year) %>% arrange(fips, year) %>% identical(., arrange(group_by(mines_cz, fips, year), fips, year)))
  
  mines_cz_within <- mines_cz %>% 
    left_join(within_cz_vars, by = c("fips", "year"))
  
  stopifnot(mines_cz_within %>%
              select(-c(var_Elem_Educ_Total_Exp_pp, min_Elem_Educ_Total_Exp_pp, max_Elem_Educ_Total_Exp_pp, diff_min_max_Elem_Educ_Total_Exp_pp, 
                        var_Total_Revenue_pp, min_Total_Revenue_pp, max_Total_Revenue_pp, diff_min_max_Total_Revenue_pp, 
                        var_Total_Rev_Own_Sources_pp, min_Total_Rev_Own_Sources_pp, max_Total_Rev_Own_Sources_pp, diff_min_max_Total_Rev_Own_Sources_pp, 
                        var_Total_IG_Revenue_pp, min_Total_IG_Revenue_pp, max_Total_IG_Revenue_pp, diff_min_max_Total_IG_Revenue_pp),
                     -contains("var_log"), -contains("min_log"), -contains("max_log"), -contains("diff_min_max")) %>% 
              identical(mines_cz))
  
  mines_cz <- mines_cz_within %>% 
    mutate(state_share = real_Total_State_IG_Revenue_pp/real_Total_Educ_Total_Exp_pp) %>% 
    filter(year >= 2001)
  
  # ss <- readRDS(here("data/temp/shift_shares_base_01_05_11.RDS")) %>% 
  #   mutate(unit = get(unit_id)) 
}else{
  stop("You need to specify fips or cz_id as the panel unit.")}


rm(race_data)
rm(mines_cz_race)
rm(priv_school_data)
rm(mines_cz_priv_school)
rm(within_cz_vars)
rm(mines_cz_within)


