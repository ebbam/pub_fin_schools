### Cleaning local wage statistics from QCEW
# Source: https://www.bls.gov/cew/downloadable-data-files.htm.  - CSVs Single Files: Annual Averages

library(here)
library(tidyverse)
library(readxl)
library(usmap)
library(conflicted)
library(zoo)
library(patchwork)
library(tidyquant)
library(assertthat)
conflict_prefer_all("dplyr", quiet = TRUE)
source(here("code/source_code/dicts.R"))
source(here("code/source_code/useful_functions.R"))

source(here("code/source_code/cz_cleaning.R"))

new_data = FALSE

################################################################################
################################################################################
###################### Wage Statistics #########################################
################################################################################
################################################################################
vars_base <- c("annual_avg_estabs", "annual_avg_emplvl", "total_annual_wages", "taxable_annual_wages", "annual_avg_wkly_wage", "avg_annual_pay")

fips_stats <- tibble()
for(yr in 1990:2022){
  print(yr)
  temp_test <- read.csv(here(paste0("data/raw/QCEW/", yr, ".annual.singlefile.csv"))) %>%
    tibble %>% 
    filter(industry_code == "10" & agglvl_code == "70") %>% 
    select(area_fips, year, annual_avg_estabs, annual_avg_emplvl, total_annual_wages, taxable_annual_wages, annual_avg_wkly_wage, avg_annual_pay)
  
  assert_that(nrow(temp_test) == n_groups(group_by(temp_test, area_fips)))
  
  fips_stats <- rbind(fips_stats, temp_test)
}

fips_stats <- fips_stats %>% 
  # Filters out national and missing area codes
  filter(substr(area_fips, 3,5) != "000" & substr(area_fips, 3,5) != "999" & !grepl("US", area_fips)) %>%
  mutate(fips_state = substr(area_fips, 1,2)) %>%
  rename(fips = area_fips) %>%
  # Rules our PR, Samoa, etc
  filter(!(fips_state %in% c("72","78", "C1", "C2", "C3", "C4", "CS"))) %>%
  # Mutates fips that are often mislabelled
  mutate(fips = ifelse(fips %in% names(getfips), unname(getfips[fips]), fips)) %>%
  filter(!(fips %in% c("51560", "51515")) & !(fips == "46113" & year <= 2015)) %>%
  mutate(fips = ifelse(fips == "46113", "46102", fips)) %>% 
  select(-fips_state)

################################################################################
################################################################################
################################# FIPS #########################################
################################################################################
################################################################################
if(new_data){
  saveRDS(fips_stats, here("data/raw/QCEW/QCEW_wage_stats_fips.RDS"))
  print("saved county fips file.")
}
################################################################################
################################################################################
################################# CZ ###########################################
################################################################################
################################################################################

czs_new <- czs %>% 
  rename(old_fips = fips) %>% 
  mutate(fips = case_when(!is.na(getfips[old_fips]) ~ getfips[old_fips],
                          TRUE ~ old_fips),
         cz_id = as.character(cz_id))

missing_fips <- czs_new %>% 
  pull(fips) %>% 
  unique %>% 
  setdiff(unique(fips_stats$fips), .) 

if (length(missing_fips) > 0) {
  message("Warning: Some FIPS codes are missing from czs.")
}

cz_stats <-  fips_stats %>% 
  left_join(., czs_new, by = "fips", multiple = "first") %>% 
  rename("unit" = cz_id) %>% 
  select(-fips) %>%
  group_by(unit, year) %>% 
    mutate(weight = annual_avg_emplvl/sum(annual_avg_emplvl, na.rm = TRUE)) %>% 
    summarise(across(c(annual_avg_estabs, annual_avg_emplvl, total_annual_wages, taxable_annual_wages), ~sum(., na.rm = TRUE), .names = "{.col}_total"),
              across(c(annual_avg_wkly_wage, avg_annual_pay), ~mean(., na.rm = TRUE), .names = "nonweighted_{.col}"),
              across(c(annual_avg_wkly_wage, avg_annual_pay), ~weighted.mean(., weight, na.rm = TRUE), .names = "weighted_{.col}")) %>% ungroup %>% 
  rename(cz_id = unit)


if(new_data){
  saveRDS(cz_stats, here("data/raw/QCEW/QCEW_wage_stats_cz.RDS"))
  print("saved CZ file.")
}

################################################################################
################################################################################
###################### STATE AND NATIONAL ######################################
################################################################################
################################################################################

state_stats <- tibble()
for(yr in 1990:2022){
  print(yr)
  temp_test <- read.csv(here(paste0("data/raw/QCEW/", yr, ".annual.singlefile.csv"))) %>%
    tibble %>% 
    filter(substr(area_fips, 3,7) == "000" & own_code == 0) %>% 
    select(area_fips, year, annual_avg_estabs, annual_avg_emplvl, total_annual_wages, taxable_annual_wages, annual_avg_wkly_wage, avg_annual_pay)
  
  assert_that(nrow(temp_test) == n_groups(group_by(temp_test, area_fips)))
  
  state_stats <- rbind(state_stats, temp_test)
}

state_stats <- state_stats %>% 
  select(-taxable_annual_wages) %>% 
  mutate(state = substr(area_fips, 1, 2),
         across(!c(state, year, area_fips), ~log(.), .names = "log_{.col}")) %>% 
  select(-area_fips) %>% 
  group_by(state) %>% 
  arrange(state, year) %>% 
  mutate(across(contains("log"), ~. - lag(., 1), .names = gsub("log","", "gr_{.col}")),
         across(!c(year), list(l1 = ~dplyr::lag(., 1), l2 = ~dplyr::lag(., 2)), .names = "{.fn}_{.col}")) %>% 
  rename_with(~ str_replace(., "gr_log_", "gr_"), contains("gr_log_")) %>% 
  ungroup 

state_stats_us <- state_stats %>% 
  filter(state == "US") %>% 
  select(-state) %>% 
  rename_with(~paste0("natl_", .), !c("year"))

stopifnot(nrow(state_stats_us) == n_groups(group_by(state_stats_us, year)))

state_stats_df <- state_stats %>% 
  filter(state != "US") %>% 
  rename_with(~paste0("state_", .), !c("year", "state"))

stopifnot(nrow(state_stats_df) == n_groups(group_by(state_stats_df, state, year)))

state_stats_df_full <- left_join(state_stats_df, state_stats_us, by = 'year') %>% 
  relocate(year, state)

stopifnot(nrow(state_stats_df_full) == n_groups(group_by(state_stats_df_full, state, year)))

state_stats_df_full %>% saveRDS(here("data/raw/QCEW/wage_growth_rate_data_state_natl.RDS"))


