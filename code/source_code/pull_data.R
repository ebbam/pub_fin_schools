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
    mutate(state_share = real_Total_State_IG_Revenue_pp/real_Total_Educ_Total_Exp_pp) %>% 
    arrange(cz_id, year) %>% 
    group_by(cz_id) %>% 
    mutate(l1_log_real_Elem_Educ_Total_Exp_pp = lag(log_real_Elem_Educ_Total_Exp_pp),
           l1_diff_log_real_Elem_Educ_Total_Exp_pp = lag(diff_log_real_Elem_Educ_Total_Exp_pp)) %>% 
    ungroup
  
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
  
}else if(unit_id == "fips"){
  print(paste0("Running analysis on counties (", unit_id, ")."))
  mines_cz <- readRDS(here("data/out/regression_data_complete_fips.RDS")) %>% 
    mutate(unit = get(unit_id)) %>% 
    group_by(unit) %>%
    arrange(unit, year) %>% 
    mutate(across(c(starts_with("log_real_gdp_total") | starts_with("log_real_gdp_priv_ind")), ~.- dplyr::lag(., 1), .names = "diff_{.col}")) %>% 
    ungroup 
  
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
  
  # ss <- readRDS(here("data/temp/shift_shares_base_01_05_11.RDS")) %>% 
  #   mutate(unit = get(unit_id)) 
}else{
  stop("You need to specify fips or cz_id as the panel unit.")}
