# Aggregating commuting zone level wages by industry for industry-by-industry estimation
# Source: https://www.bls.gov/cew/downloadable-data-files.htm.  - CSVs Single Files: Annual Averages

library(here)
library(tidyverse)
library(readxl)
library(usmap)
library(conflicted)
library(zoo)
library(patchwork)
library(ggrepel)
library(tidyquant)
library(assertthat)
conflict_prefer_all("dplyr", quiet = TRUE)
source(here("code/source_code/dicts.R"))
source(here("code/source_code/useful_functions.R"))

testing = FALSE
new_flat = FALSE


# Industry codes provided by QCEW BLS: https://www.bls.gov/cew/classifications/industry/industry-titles.htm
ind_codes <- read_excel(here("data/raw/QCEW/industry-titles.xlsx"))
source(here("code/source_code/cz_cleaning.R"))

unit_id = "cz_id"
tot_file <- tibble()
for(base_year in 2001:2022){
  print(base_year)
  # Import county-level data from yr
  # Format for QCEW files
  temp_test <- read.csv(here(paste0("data/raw/QCEW/", base_year, ".annual.singlefile.csv"))) %>%
    tibble
  
  # This section cleans the base_file
  temp <- temp_test %>%
    # Filters industry codes that have 4-digits or more
    filter(nchar(industry_code) <= 3 | grepl("-", industry_code)) %>% 
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
    select(-fips_state) %>% 
    # Removes 72 because it is associated only with the industry code distinction between 101 (Goods-producing) and 102 (service-providing) : https://www.bls.gov/cew/classifications/industry/industry-titles.htm
    # Removes 73 becuase it refers to super sectors, not our relevant sectors : https://www.bls.gov/cew/classifications/aggregation/agg-level-titles.htm
    filter(agglvl_code %in% c(70, 71, 74, 75) & disclosure_code != "N") %>% 
    # We keep 71 for now because in a few rare cases, agglvl_code ' 70 is not reported and we need to replace with the sum of 71's values 
    # even though "70" represents the total covered: https://www.bls.gov/cew/classifications/ownerships/ownership-titles.htm
    mutate(industry_code = gsub("-", "_", industry_code))
  
  rm(temp_test)
  
  if(unit_id == "cz_id"){
    czs_new <- czs %>% 
      #select(-old_fips) %>% 
      rename(old_fips = fips) %>% 
      mutate(fips = case_when(!is.na(getfips[old_fips]) ~ getfips[old_fips],
                              TRUE ~ old_fips),
             cz_id = as.character(cz_id))
    
    missing_fips <- czs_new %>% 
      pull(fips) %>% 
      unique %>% 
      setdiff(unique(temp$fips), .) 
    
    if (length(missing_fips) > 0) {
      message("Warning: Some FIPS codes are missing from czs.")
    }
    
    temp <- temp %>% 
      left_join(., czs_new, by = "fips", multiple = "first") %>% 
      rename("unit" = cz_id) %>% 
      select(-fips)
    
  }else if(unit_id == "fips"){
    temp <- temp %>% 
      rename(unit = fips)
  }
  
  
  # NEED TO TEST THAT INDUSRTY CODE 10 IS THE ONLY ONE THAT HAS MULTIPLE AGGREGATION LEVELS
  ind_test <- temp %>% 
    select(industry_code, agglvl_code) %>% 
    distinct %>% 
    group_by(industry_code) %>% 
    # Counts all unique agglvl_codes for each industry_code
    summarise(n = n()) %>% 
    # Filters the industry_codes that have multiple agglvl_codes
    filter(n != 1) %>% 
    pull(industry_code) %>% 
    unique
  
  # Tests to make sure that this list is either empty or contains only "10"
  stopifnot(length(ind_test) == 0 || all(ind_test == "10"))
  
  temp_long_wage <- temp %>% 
    group_by(unit, year, industry_code, agglvl_code) %>% 
    summarise(across(c(annual_avg_emplvl), ~sum(., na.rm = TRUE)),
              across(c(annual_avg_wkly_wage), ~mean(., na.rm = TRUE))) %>%  # , total_annual_wages)
    #annual_avg_wkly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE)) %>% 
    ungroup %>% 
    group_by(unit, year, industry_code) %>% 
    summarise(across(c(annual_avg_emplvl, annual_avg_wkly_wage), ~max(., na.rm = TRUE))) %>%
    mutate(across(c(annual_avg_emplvl, annual_avg_wkly_wage), ~ifelse(. == -Inf, NA, .))) %>% 
    ungroup %>% 
    filter(!industry_code %in% c(99,999)) %>% 
    pivot_wider(id_cols = c(unit, year), 
                values_from = c(annual_avg_emplvl, annual_avg_wkly_wage), names_from = industry_code)
  
  rm(temp)
  
  tot_file <- bind_rows(tot_file, temp_long_wage) 
}

saveRDS(tot_file, here('data/raw/QCEW/cz_ind_wage_panel.RDS'))
  