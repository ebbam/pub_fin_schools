library(here)
library(tidyverse)
library(readxl)
library(usmap)
library(conflicted)
library(zoo)
conflict_prefer_all("dplyr", quiet = TRUE)
source(here("code/source_code/dicts.R"))
source(here("code/source_code/useful_functions.R"))

cz = TRUE

# Source: https://www.bls.gov/cew/downloadable-data-files.htm.  - CSVs Single Files: Annual Averages

# Industry codes provided by QCEW BLS: https://www.bls.gov/cew/classifications/industry/industry-titles.htm
ind_codes <- read_excel(here("data/raw/QCEW/industry-titles.xlsx"))

# df_list_filtered <- list()
# for(y in 1990:2022){
#   print(y)
#   df_list_filtered[[as.character(y)]] <- read.csv(here(paste0("data/raw/QCEW/", y, ".annual.singlefile.csv"))) %>%
#     tibble %>%
#     mutate(year = y) %>%
#     left_join(., ind_codes, by = "industry_code") %>%
#     filter(keep == 1)
# }

#saveRDS(df_list_filtered, here("data/temp/qcew_compiled_raw_ff.RDS"))

temp_test <- readRDS(here("data/temp/qcew_compiled_raw_ff.RDS"))

temp <- temp_test %>% 
  do.call("rbind", .) %>% 
  filter(substr(area_fips, 3,5) != "000" & substr(area_fips, 3,5) != "999") %>%  
  mutate(fips_state = substr(area_fips, 1,2)) %>% 
  rename(fips = area_fips) %>% 
  filter(!(fips_state %in% c("72","78", "C1", "C2", "C3", "C4", "CS"))) %>% 
  mutate(fips = ifelse(fips %in% names(getfips), unname(getfips[fips]), fips)) %>% 
  filter(!(fips %in% c("51560", "51515")) & !(fips == "46113" & year <= 2015)) %>% 
  mutate(fips = ifelse(fips == "46113", "46102", fips))

# National rates
natl_rates <- temp_test %>% 
  do.call("rbind", .) %>% 
  filter(area_fips == "US000" & industry_code %in% c('10', '21', '2121', '211') & agglvl_code %in% c(10, 14, 15, 16)) %>% 
  group_by(year, industry_code) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  ungroup %>% 
  pivot_wider(id_cols = c(year), names_from = industry_code, values_from = c(annual_avg_emplvl, total_annual_wages)) %>% 
  rename_with(~str_c("natl_", .), annual_avg_emplvl_10:total_annual_wages_2121) %>% 
  # Creates first-difference or change and lag for employment and wage levels
  mutate(across(natl_annual_avg_emplvl_10:natl_total_annual_wages_2121, .fns = list(l1 = ~lag(.x), fd = ~.x - lag(.x)), .names = "{.fn}_{.col}")) %>% 
  # Creates lag of first-difference or change for employment and wage levels
  mutate(across(contains("fd"), ~ lag(.x), .names = "l1_{.col}")) 

#saveRDS(natl_rates, here("data/temp/natl_rates.RDS"))

rm(temp_test)

if(!cz){
  shift_share <- temp %>% 
    filter(industry_code %in% c('10', '21', '2121', '211') & agglvl_code %in% c(70, 74, 76, 75) & disclosure_code != "N") %>%
    group_by(fips, year, industry_code, fips_state) %>% 
    summarise(across(where(is.numeric), sum)) %>% 
    ungroup %>% 
    pivot_wider(id_cols = c(fips, year, fips_state), names_from = industry_code, values_from = c(annual_avg_emplvl, total_annual_wages)) %>%
    # Balance the panel
    complete(fips, year) %>%
    group_by(fips) %>% 
    mutate(across(where(is.numeric), ~ na.approx(., maxgap = 7, na.rm = FALSE))) %>% #filter(!all(is.na(annual_avg_emplvl_2121))) %>% ggplot(aes(x = year, y = annual_avg_emplvl_2121, color = fips)) + geom_line() + facet_wrap(~fips_state)
    ungroup %>%
    # Calculates share of employment and wage per industry in each county 
    mutate(share_emp_coal = annual_avg_emplvl_2121/annual_avg_emplvl_10,
           share_emp_extraction = annual_avg_emplvl_21/annual_avg_emplvl_10,
           share_emp_ff = (annual_avg_emplvl_2121 + annual_avg_emplvl_211)/annual_avg_emplvl_10,
           share_emp_oil_gas =  annual_avg_emplvl_211/annual_avg_emplvl_10,
           share_wage_coal = total_annual_wages_2121/total_annual_wages_10,
           share_wage_extraction = total_annual_wages_21/total_annual_wages_10,
           share_wage_ff = (total_annual_wages_2121 + total_annual_wages_211)/total_annual_wages_10,
           share_wage_oil_gas =  total_annual_wages_211/total_annual_wages_10) %>% 
    group_by(fips) %>% 
    mutate(across(annual_avg_emplvl_10:total_annual_wages_2121,  .fns = list(l1 = ~lag(.x), fd = ~.x - lag(.x)), .names = "{.fn}_{.col}")) %>% ungroup
  
  shift_share %>% group_by(fips) %>% filter(!all(is.na(share_emp_coal))) %>% ggplot(aes(x = year, y = share_emp_coal, color = fips)) + geom_line() + facet_wrap(~fips_state)
  
  #saveRDS(shift_share, here("data/temp/shift_shares.RDS"))

  
  # Creates a shift-share measure with a baseline of 2001
  # tester_2001 <- shift_share %>% 
  #   left_join(natl_rates, by = "year") %>% 
  #   group_by(fips) %>% 
  #   mutate(ss_emp_coal = share_emp_coal[year == 2001]*fd_natl_annual_avg_emplvl_2121,
  #          ss_emp_extraction = share_emp_extraction[year == 2001]*fd_natl_annual_avg_emplvl_21,
  #          ss_emp_ff = share_emp_ff[year == 2001]*(fd_natl_annual_avg_emplvl_2121 + fd_natl_annual_avg_emplvl_211),
  #          ss_emp_oil_gas = share_emp_oil_gas[year == 2001]*fd_natl_annual_avg_emplvl_211,
  #          ss_wage_coal = share_wage_coal[year == 2001]*fd_natl_total_annual_wages_2121,
  #          ss_wage_extraction = share_wage_extraction[year == 2001]*fd_natl_total_annual_wages_21,
  #          ss_wage_ff = share_wage_ff[year == 2001]*(fd_natl_total_annual_wages_2121 + fd_natl_total_annual_wages_211),
  #          ss_wage_oil_gas = share_wage_oil_gas[year == 2001]*fd_natl_total_annual_wages_211) %>% 
  #   
  #   ungroup %>% 
  #   select(-fips_state) %>% saveRDS(here('data/temp/shift_shares_2001_base.RDS'))
    
  # Creates a shift-share measure with a baseline of 2001 (first time period in series), 2006 (local peak prior to global peak in national FF employment), 2011 (peak national FF employment)
  full <- shift_share %>% 
    left_join(natl_rates, by = "year") %>% 
    select(-fips_state)
  for(years in c(2001, 2005, 2011)){
    print(years)
    full <- full %>% 
      group_by(fips) %>% 
      mutate("ss_emp_coal_{years}" := share_emp_coal[year == years]*fd_natl_annual_avg_emplvl_2121,
             "ss_emp_extraction_{years}" := share_emp_extraction[year == years]*fd_natl_annual_avg_emplvl_21,
             "ss_emp_ff_{years}" :=  share_emp_ff[year == years]*(fd_natl_annual_avg_emplvl_2121 + fd_natl_annual_avg_emplvl_211),
             "ss_emp_oil_gas_{years}" :=  share_emp_oil_gas[year == years]*fd_natl_annual_avg_emplvl_211,
             "ss_wage_coal_{years}" := share_wage_coal[year == years]*fd_natl_total_annual_wages_2121,
             "ss_wage_extraction_{years}"  := share_wage_extraction[year == years]*fd_natl_total_annual_wages_21,
             "ss_wage_ff_{years}" :=  share_wage_ff[year == years]*(fd_natl_total_annual_wages_2121 + fd_natl_total_annual_wages_211),
             "ss_wage_oil_gas_{years}" :=  share_wage_oil_gas[year == years]*fd_natl_total_annual_wages_211) %>% 
      ungroup %>% select(fips, year, contains(as.character(years))) %>% left_join(full, ., by = c("fips", "year"))
  }
  
  #full %>% saveRDS(here("data/temp/shift_shares_base_01_05_11.RDS"))
  
  
  # Testing for missing values
  shift_share %>% 
    select(fips, year, annual_avg_emplvl_10, total_annual_wages_10) %>% 
    filter(!complete.cases(.)) %>% 
    filter(substr(fips, 1,2) != "02") 
  
  
  states_rep <- full %>% select(fips, year, contains("ss_emp_coal")) %>% 
    group_by(fips) %>% filter(!any(is.na(ss_emp_coal_2001)) | !any(is.na(ss_emp_coal_2005)) | !any(is.na(ss_emp_coal_2011)))  %>% 
    ungroup %>% filter(!complete.cases(.)) %>% group_by(fips) %>% 
    summarise(across(ss_emp_coal_2001:ss_emp_coal_2011, mean)) %>% pull(fips) %>% unique
  
  full %>% filter(fips %in% states_rep) %>% select(fips, year, share_emp_coal) %>% print(n = 1078)
  
  
  full %>% select(fips, year, contains("ss_emp_coal")) %>% 
    group_by(fips) %>% filter(!any(is.na(ss_emp_coal_2001)) | !any(is.na(ss_emp_coal_2005)) | !any(is.na(ss_emp_coal_2011))) %>% 
    pivot_longer(cols = !c(fips, year), values_to = "ss", names_to = "base_year") %>% 
    filter(base_year == "ss_emp_coal_2001") %>% 
    ggplot(aes(x = year, y = ss, color = base_year)) + geom_line() + facet_wrap(~fips, scales = "free")
  
  
  full %>% select(fips, year, contains("ss_emp_coal")) %>% 
    group_by(fips) %>% filter(!any(is.na(ss_emp_coal_2001)) | !any(is.na(ss_emp_coal_2005)) | !any(is.na(ss_emp_coal_2011))) %>% 
    ungroup %>% summarise(across(ss_emp_coal_2001:ss_emp_coal_2011, ~sum(is.na(.))))
  
  
  full %>% select(fips, year, contains("ss_emp_oil_gas")) %>% 
    group_by(fips) %>% filter(!any(is.na(ss_emp_oil_gas_2001)) | !any(is.na(ss_emp_oil_gas_2005)) | !any(is.na(ss_emp_oil_gas_2011))) %>% ungroup %>% slice(1:1100) %>% 
    pivot_longer(cols = !c(fips, year), values_to = "ss", names_to = "base_year") %>%
    ggplot(aes(x = year, y = ss, color = base_year)) + geom_line() + facet_wrap(~fips, scales = "free")
  
  full %>% select(fips, year, contains("ss_emp_oil_gas")) %>% 
    group_by(fips) %>% filter(!any(is.na(ss_emp_oil_gas_2001)) | !any(is.na(ss_emp_oil_gas_2005)) | !any(is.na(ss_emp_oil_gas_2011))) %>% 
    ungroup %>% summarise(across(ss_emp_oil_gas_2001:ss_emp_oil_gas_2011, ~sum(is.na(.))/nrow(full)))

}

################### Create CZ dataset
source(here("code/source_code/cz_cleaning.R"))

czs <- czs %>% 
  rename(old_fips = fips) %>% 
  mutate(fips = case_when(!is.na(getfips[old_fips]) ~ getfips[old_fips],
                          TRUE ~ old_fips),
         cz_id = as.character(cz_id))

czs %>% 
  pull(fips) %>% 
  unique %>% 
  setdiff(unique(temp$fips), .)

temp_cz <- temp %>% 
  left_join(., czs, by = "fips", multiple = "first") %>% 
  filter(agglvl_code %in% c(70, 74, 76, 75) & disclosure_code != "N") %>% 
  group_by(cz_id, year, industry_code) %>% 
  # summarise(across(contains("total"), ~sum(., na.rm = TRUE)),
  #           across(contains("avg"), ~mean(., na.rm = TRUE)))
  summarise(across(c(annual_avg_emplvl, total_annual_wages), ~sum(., na.rm = TRUE))) %>% 
  ungroup
  

shift_share_cz <- temp_cz %>% 
  filter(industry_code %in% c('10', '21', '2121', '211')) %>%
  group_by(cz_id, year, industry_code) %>% 
  summarise(across(where(is.numeric), sum)) %>% 
  ungroup %>% 
  pivot_wider(id_cols = c(cz_id, year), names_from = industry_code, values_from = c(annual_avg_emplvl, total_annual_wages)) %>%
  # Balance the panel
  complete(cz_id, year) %>%
  group_by(cz_id) %>% 
  mutate(across(where(is.numeric), ~ na.approx(., maxgap = 7, na.rm = FALSE))) %>% #filter(!all(is.na(annual_avg_emplvl_2121))) %>% ggplot(aes(x = year, y = annual_avg_emplvl_2121, color = fips)) + geom_line() + facet_wrap(~fips_state)
  ungroup %>%
  # Calculates share of employment and wage per industry in each county 
  mutate(share_emp_coal = annual_avg_emplvl_2121/annual_avg_emplvl_10,
         share_emp_extraction = annual_avg_emplvl_21/annual_avg_emplvl_10,
         share_emp_ff = (annual_avg_emplvl_2121 + annual_avg_emplvl_211)/annual_avg_emplvl_10,
         share_emp_oil_gas =  annual_avg_emplvl_211/annual_avg_emplvl_10,
         share_wage_coal = total_annual_wages_2121/total_annual_wages_10,
         share_wage_extraction = total_annual_wages_21/total_annual_wages_10,
         share_wage_ff = (total_annual_wages_2121 + total_annual_wages_211)/total_annual_wages_10,
         share_wage_oil_gas =  total_annual_wages_211/total_annual_wages_10) %>% 
  group_by(cz_id) %>% 
  mutate(across(annual_avg_emplvl_10:total_annual_wages_2121,  .fns = list(l1 = ~lag(.x), fd = ~.x - lag(.x)), .names = "{.fn}_{.col}")) %>% ungroup


# Creates a shift-share measure with a baseline of 2001 (first time period in series), 2006 (local peak prior to global peak in national FF employment), 2011 (peak national FF employment)
full_cz <- shift_share_cz %>% 
  left_join(natl_rates, by = "year")

for(years in c(2001, 2005, 2011)){
  print(years)
  full_cz <- full_cz %>% 
    group_by(cz_id) %>% 
    mutate("ss_emp_coal_{years}" := share_emp_coal[year == years]*fd_natl_annual_avg_emplvl_2121,
           "ss_emp_extraction_{years}" := share_emp_extraction[year == years]*fd_natl_annual_avg_emplvl_21,
           "ss_emp_ff_{years}" :=  share_emp_ff[year == years]*(fd_natl_annual_avg_emplvl_2121 + fd_natl_annual_avg_emplvl_211),
           "ss_emp_oil_gas_{years}" :=  share_emp_oil_gas[year == years]*fd_natl_annual_avg_emplvl_211,
           "ss_wage_coal_{years}" := share_wage_coal[year == years]*fd_natl_total_annual_wages_2121,
           "ss_wage_extraction_{years}"  := share_wage_extraction[year == years]*fd_natl_total_annual_wages_21,
           "ss_wage_ff_{years}" :=  share_wage_ff[year == years]*(fd_natl_total_annual_wages_2121 + fd_natl_total_annual_wages_211),
           "ss_wage_oil_gas_{years}" :=  share_wage_oil_gas[year == years]*fd_natl_total_annual_wages_211) %>% 
    ungroup %>% select(cz_id, year, contains(as.character(years))) %>% left_join(full_cz, ., by = c("cz_id", "year"))
}

full_cz %>% select(cz_id, year, contains("ss_emp_coal")) %>% 
  group_by(cz_id) %>% filter(!any(is.na(ss_emp_coal_2001)) | !any(is.na(ss_emp_coal_2005)) | !any(is.na(ss_emp_coal_2011))) %>% 
  pivot_longer(cols = !c(cz_id, year), values_to = "ss", names_to = "base_year") %>% 
  filter(base_year == "ss_emp_coal_2001") %>% 
  ggplot(aes(x = year, y = ss, color = base_year)) + geom_line() + facet_wrap(~cz_id, scales = "free")


full_cz %>% select(cz_id, year, contains("ss_emp_coal")) %>% 
  group_by(cz_id) %>% filter(!any(is.na(ss_emp_coal_2001)) | !any(is.na(ss_emp_coal_2005)) | !any(is.na(ss_emp_coal_2011))) %>% 
  ungroup %>% summarise(across(ss_emp_coal_2001:ss_emp_coal_2011, ~sum(is.na(.))))


full_cz %>% select(cz_id, year, contains("ss_emp_oil_gas")) %>% 
  group_by(cz_id) %>% filter(!any(is.na(ss_emp_oil_gas_2001)) | !any(is.na(ss_emp_oil_gas_2005)) | !any(is.na(ss_emp_oil_gas_2011))) %>% ungroup %>% slice(1:1100) %>% 
  pivot_longer(cols = !c(cz_id, year), values_to = "ss", names_to = "base_year") %>%
  ggplot(aes(x = year, y = ss, color = base_year)) + geom_line() + facet_wrap(~cz_id, scales = "free")


full_cz %>% select(cz_id, year, contains("ss_emp_oil_gas")) %>% 
  group_by(cz_id) %>% filter(!any(is.na(ss_emp_oil_gas_2001)) | !any(is.na(ss_emp_oil_gas_2005)) | !any(is.na(ss_emp_oil_gas_2011))) %>% 
  ungroup %>% summarise(across(ss_emp_oil_gas_2001:ss_emp_oil_gas_2011, ~sum(is.na(.))/nrow(full_cz)))

#saveRDS(full_cz, here("data/temp/shift_shares_cz_base_01_05_11.RDS"))
