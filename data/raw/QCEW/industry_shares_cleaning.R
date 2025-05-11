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

testing = FALSE

# Source: https://www.bls.gov/cew/downloadable-data-files.htm.  - CSVs Single Files: Annual Averages

# Industry codes provided by QCEW BLS: https://www.bls.gov/cew/classifications/industry/industry-titles.htm
ind_codes <- read_excel(here("data/raw/QCEW/industry-titles.xlsx"))
source(here("code/source_code/cz_cleaning.R"))

# df_list_natl <- list()
# for(y in 2009:2022){
#   print(y)
#   rows <- nrow(df_list_natl)
#   df_list_natl <- read.csv(here(paste0("data/raw/QCEW/", y, ".annual.singlefile.csv"))) %>%
#     tibble %>%
#     filter(area_fips == "US000") %>% 
#     mutate(year = y) %>%
#     left_join(., ind_codes, by = "industry_code") %>%
#     filter(nchar(industry_code) <= 3) %>% 
#     rbind(df_list_natl, .)
#   print(nrow(df_list_natl) - rows)
#   
#   #assign(paste0("qcew", as.character(y)), temp)
# }

# previously saved ..."99_08.RDS"
#saveRDS(df_list_natl, here("data/raw/QCEW/natl_all_industries_09_22.RDS"))

################################################################################
################################################################################
# Taken from qcew_shift_share
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
# temp_test <- readRDS(here("data/temp/qcew_compiled_raw_ff.RDS"))

# temp <- temp_test %>% 
#   do.call("rbind", .) %>% 
#   filter(substr(area_fips, 3,5) != "000" & substr(area_fips, 3,5) != "999") %>%  
#   mutate(fips_state = substr(area_fips, 1,2)) %>% 
#   rename(fips = area_fips) %>% 
#   filter(!(fips_state %in% c("72","78", "C1", "C2", "C3", "C4", "CS"))) %>% 
#   mutate(fips = ifelse(fips %in% names(getfips), unname(getfips[fips]), fips)) %>% 
#   filter(!(fips %in% c("51560", "51515")) & !(fips == "46113" & year <= 2015)) %>% 
#   mutate(fips = ifelse(fips == "46113", "46102", fips))
# 


# # National rates
# natl_rates <- temp_test %>% 
#   do.call("rbind", .) %>% 
#   filter(area_fips == "US000" & industry_code %in% c('10', '21', '2121', '211') & agglvl_code %in% c(10, 14, 15, 16)) %>% 
#   group_by(year, industry_code) %>% 
#   summarise(across(where(is.numeric), sum)) %>% 
#   ungroup %>% 
#   pivot_wider(id_cols = c(year), names_from = industry_code, values_from = c(annual_avg_emplvl, total_annual_wages)) %>% 
#   rename_with(~str_c("natl_", .), annual_avg_emplvl_10:total_annual_wages_2121) %>% 
#   # Creates first-difference or change and lag for employment and wage levels
#   mutate(across(natl_annual_avg_emplvl_10:natl_total_annual_wages_2121, .fns = list(l1 = ~lag(.x), fd = ~.x - lag(.x)), .names = "{.fn}_{.col}")) %>% 
#   # Creates lag of first-difference or change for employment and wage levels
#   mutate(across(contains("fd"), ~ lag(.x), .names = "l1_{.col}")) 
# 
# #saveRDS(natl_rates, here("data/temp/natl_rates.RDS"))

# rm(temp_test)

################################################################################
################################################################################
################### National ###################################################
################################################################################
################################################################################
df_list_natl <- readRDS(here('data/raw/QCEW/natl_all_industries_99_08.RDS')) %>%
  rbind(readRDS(here("data/raw/QCEW/natl_all_industries_09_22.RDS")))

# National rates
natl_rates_plot <- df_list_natl %>% 
  group_by(year, industry_code) %>%
  summarise(across(c(annual_avg_emplvl, total_annual_wages), ~sum(., na.rm = TRUE)),
            annual_avg_wkly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE)) %>%
  ungroup %>%
  rename_with(~str_c("natl_", .), annual_avg_emplvl:annual_avg_wkly_wage) %>% 
  arrange(industry_code, year) %>% 
  # Creates first-difference or change and lag for employment and wage levels
  group_by(industry_code) %>% 
  mutate(across(natl_annual_avg_emplvl:natl_annual_avg_wkly_wage, ~log(.) - lag(log(.),1), .names = "gr_{.col}")) %>% 
  ungroup

natl_rates <- natl_rates_plot %>% 
  pivot_wider(id_cols = c(year), names_from = industry_code, values_from = natl_annual_avg_emplvl:gr_natl_annual_avg_wkly_wage)

natl_rates_plot %>% 
  pivot_longer(!c(year, industry_code)) %>% 
  mutate(name = gsub("natl_annual_avg_emplvl", "Average Employment Level", gsub("natl_total_annual_wages", "Total Wages", gsub("natl_annual_avg_wkly_wage", "Avg. Weekly Wage", gsub("gr_", "Growth Rate ", name))))) %>% 
  filter(industry_code != 10) %>% 
  ggplot(aes(x = year, y = value, color = industry_code)) + 
  geom_point() + 
  facet_wrap(~name, scales = "free_y") +
  theme(legend.position = "none") + 
  labs(title = "National Wage and Employment (Levels & Growth Rates by Industry)")
#saveRDS(natl_rates, here("data/temp/natl_rates.RDS"))

################################################################################
################################################################################
################# Shift Shares Transformation Function #########################
################################################################################
################################################################################

compute_ss <- function(source = NULL, base_year, industry_codes, unit_id = "fips"){
  if(source == "QCEW"){
    # Import base year for Bartik instrument - 2001
    # Format for QCEW files
    temp_test <- read.csv(here(paste0("data/raw/QCEW/", base_year, ".annual.singlefile.csv"))) %>%
      tibble
    print(paste0("Downloaded QCEW data for ", base_year, "."))
  }
  
  # This section cleans the base_file
  temp <- temp_test %>%
    # Filters industry codes that have 4-digits or more
    filter(nchar(industry_code) <= 3) %>% 
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
    # Removes 
    filter(agglvl_code %in% c(70, 74, 75) & disclosure_code != "N") 
  
  if(unit_id == "cz_id"){
    czs <- czs %>% 
      rename(old_fips = fips) %>% 
      mutate(fips = case_when(!is.na(getfips[old_fips]) ~ getfips[old_fips],
                              TRUE ~ old_fips),
             cz_id = as.character(cz_id))
    
    missing_fips <- czs %>% 
      pull(fips) %>% 
      unique %>% 
      setdiff(unique(temp$fips), .) 
    
    if (length(missing_fips) > 0) {
      message("Warning: Some FIPS codes are missing from czs.")
    }
    
    temp <- temp %>% 
      left_join(., czs, by = "fips", multiple = "first") %>% 
      rename("unit" = cz_id) %>% 
      select(-fips)
    
  }else if(unit_id == "fips"){
    temp <- temp %>% 
      rename(unit = fips)
  }
  
  temp <- temp %>% 
    group_by(unit, year, industry_code) %>% 
    summarise(across(c(annual_avg_emplvl, total_annual_wages), ~sum(., na.rm = TRUE)),
              annual_avg_wkly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE)) %>% 
    ungroup %>% 
    filter(!industry_code %in% c(99,999))
  
  rm(temp_test)
  
  print("Cleaned temp file.")
  
  shift_share <- temp %>% 
   # left_join(natl_rates, by = "year") %>% 
    pivot_wider(id_cols = c(unit, year), names_from = industry_code, values_from = c(annual_avg_emplvl, total_annual_wages)) %>%
    # Calculates share of employment and wage per industry in each county 
    mutate(across(contains("avg_emplvl"), ~./annual_avg_emplvl_10, .names = "share_{.col}"),
           across(contains("total_annual_wages"), ~./total_annual_wages_10, .names = "share_{.col}")) 
  
  # 2-digit coverage ratios
  coverage_2digit_naics <- shift_share %>% 
    select(year, unit, matches("^share_annual_avg_emplvl_\\d{2}$")) %>% 
    rowwise() %>% mutate(coverage_2digit_naics = sum(c_across(!c(year, unit)), na.rm = TRUE) - 1) %>%
    ungroup() %>% 
    select(year, unit, coverage_2digit_naics)
  
  # 3-digit coverage ratios
  coverage_3digit_naics <- shift_share %>% 
    select(year, unit, matches("^share_annual_avg_emplvl_\\d{3}$")) %>% 
    rowwise() %>% mutate(coverage_3digit_naics = sum(c_across(!c(year, unit)), na.rm = TRUE)) %>%
    ungroup() %>% 
    select(year, unit, coverage_3digit_naics)
  
  shift_share <- shift_share %>% 
    left_join(., coverage_2digit_naics, by = c("unit", "year")) %>% 
    left_join(., coverage_3digit_naics, by = c("unit", "year")) %>% 
    complete(unit, year = 2001:2022) %>% 
    group_by(unit) %>%
    fill(everything(), .direction = "updown") %>% 
    ungroup
  
  print("Created employment share values.")

  assert_that(nrow(shift_share) == n_distinct(shift_share$unit) * n_distinct(shift_share$year))
  
  # Creates a shift-share measure with a baseline of 2001 (first time period in series), 2006 (local peak prior to global peak in national employment), 2011 (peak national employment)
  full <- shift_share %>% 
    left_join(., natl_rates, by = "year", relationship = "many-to-one") %>% 
    rename(!!unit_id := unit)
  
  print("Appended national shock variables.")
  
  return(full)
}

################################################################################
################### PLOTTING FUNCTION ##########################################
################################################################################
plot_ss <- function(dat, code, shock_var, unit_id, ind_codes = NULL){
  # Need to call them dynamically using the code variable
  share_col <- sym(paste0("share_annual_avg_emplvl_", code))
  shock_col <- sym(paste0(shock_var, code))
  # name of SS variable
  ss_name <- sym(paste0("ss_", code))
  if(!is.null(ind_codes)){
    industry_name <- ind_codes %>% 
       filter(industry_code == code) %>% 
       pull(industry_title)
  }else{
    industry_name <- NULL
    print('Consider providing an industry code list for accurate title.')}
  
  colors = c("fips" = "darkorchid4", "cz_id" = "tomato4")
  
  dat <- dat %>% 
    select(year, !!unit_id, !!share_col, !!shock_col) %>% 
    mutate(!!ss_name := !!share_col*!!shock_col) 
  
  dat %>% 
    ggplot() +
    geom_line(aes(x = year, y = !!ss_name, group = !!sym(unit_id)), color = colors[unit_id]) +
    theme(legend.position = "none") +
    labs(title = industry_name, subtitle = paste0("Unit: ", toupper(gsub("_id", "", unit_id))), x = "Year", y= "Industry-Specific Shift-Share Value") + 
    theme_minimal() + theme(legend.position = "none")
  
  return(dat)
}

################################################################################
################### FIPS #######################################################
################################################################################
if(testing){
  
full_fips <- compute_ss(source = "QCEW", base_year = 2001, unit_id = "fips")

plot_ss(full_fips, "51", "gr_natl_annual_avg_wkly_wage_", "fips", ind_codes)

################################################################################
################### TESTING CZs ################################################
################################################################################

full_cz <- compute_ss(source = "QCEW", base_year = 2001, unit_id = "cz_id")

plot_ss(full_cz, "51", "gr_natl_annual_avg_wkly_wage_", "cz_id", ind_codes)

################################################################################
}
