# Race statistics at county-level from National Institutes of Health - National Cancer Institute - Surveillance, Epidemiology, and End Results Program
# Available via API call

# Source: https://seer.cancer.gov/popdata/download.html
library(tidyverse)
library(here)
library(httr)
library(tidycensus)
library(rvest)
library(janitor)

# SEER Data

# 1) Download SEER county population file (20-age groups, expanded races 1990+)
#    Confirm the exact filename on the SEER page; example URL below (adjust if needed).
# url <- "https://seer.cancer.gov/popdata/yr1990_2023.20ages/us.1990_2023.20ages.adjusted.txt.gz"
# out_gz <- "us.1990_2023.20ages.adjusted.txt.gz"
# 
# if (!file.exists(out_gz)) {
#   download.file(url, out_gz, mode = "wb")
# }

# 2) Read fixed-width file using SEER data dictionary layout (26 bytes per record)
# From SEER data dictionary the layout (start col, length) corresponds to:
# Year (4), StatePostal(2), StateFIPS(2), CountyFIPS(3), filler(2),
# Race(1), Origin(1), Sex(1), Age(2), Population(8)  -> totals 26 bytes
fwf_widths <- c(4, 2, 2, 3, 2, 1, 1, 1, 2, 8)
col_names   <- c("year", "state_postal", "state_fips", "county_fips_raw",
                 "filler", "race_code", "origin_code", "sex_code", "age_code",
                 "population")

# # read_fwf can read from gz directly
# #conn <- gzfile(out_gz, open = "rt")
# raw_df <- read_fwf(out_gz,
#                    fwf_widths(fwf_widths, col_names),
#                    col_types = cols("n", "c", "c", "c", "c", "n", "n", "n", "n", "n")) %>% 
#   data.frame()
# #close(conn)

# nrows = 2500000
# i = 0
# nrow_full_df = 0
# max_year = 1
# while(max_year < 2023){
#   raw_df <- read_fwf(here("data/raw/race_controls/us.1990_2023.20ages.adjusted.txt"),
#                      fwf_widths(fwf_widths, col_names),
#                      col_types = cols("n", "c", "c", "c", "c", "n", "n", "n", "n", "n"), 
#                      skip = nrow_full_df - 20,
#                      n_max = 2500000)
#   
#   max_year_cut <- raw_df %>% pull(year) %>% max(.)
#   if(max_year_cut == 2023){
#     max_year <- 2023
#   }else{
#     max_year <- max_year_cut - 1
#   }
# 
#   raw_df %>% 
#     filter(year <= max_year) -> temp
#   
#   nrow_full_df <- nrow_full_df + nrow(temp)
#   #print(paste0("Max year read in: ", max_year))
#   #print(paste0("Max year kept: ", max_year_cut))
#   #print(paste0("Nrows of master :", nrow_full_df))
#   print(paste0("Year range: ", min(temp$year), "-", max(temp$year)))
#   
# }



convert_race_data <- function(raw_df){
  df <- raw_df %>%
    mutate(
      fips = paste0(state_fips, county_fips_raw),
      state = state_postal) %>%
    select(year, state_postal, state_fips, county_fips_raw, fips, race_code, origin_code, sex_code, age_code, population) 
  
  rm(raw_df)
  
  stopifnot(n_groups(df %>% group_by(year, fips, race_code, state_postal, state_fips)) == n_groups(df %>% group_by(year, fips, race_code)))
  stopifnot(n_groups(df %>% group_by(year, fips, origin_code, state_postal, state_fips)) == n_groups(df %>% group_by(year, fips, origin_code)))
  
  df_pop_hisp <- df %>% 
    group_by(year, fips, state_postal, state_fips) %>% 
    mutate(race_pop_total = sum(population, na.rm = TRUE),
           pop_hispanic = sum(population*origin_code, na.rm = TRUE)) %>% 
    ungroup
  
  stopifnot(n_groups(df_pop_hisp %>% group_by(year, fips, race_code, state_postal, state_fips, race_pop_total, pop_hispanic)) == n_groups(df_pop_hisp %>% group_by(year, fips, race_code)))
  
  df_sum <- df_pop_hisp %>% 
    group_by(year, fips, race_code, state_postal, state_fips, race_pop_total, pop_hispanic) %>% 
    summarise(pop_race = sum(population, na.rm = TRUE)) %>% 
    ungroup
  
  # 4) Map race/origin codes to labels (1990+ expanded races)
  # According to the data dictionary (1990+):
  # race_code: 1=White, 2=Black, 3=American Indian/Alaska Native, 4=Asian or Pacific Islander
  # origin_code: 0 = Non-Hispanic, 1 = Hispanic
  
  df <- df_sum  %>%
    mutate(
      race = case_when(
        race_code == 1 ~ "white",
        race_code == 2 ~ "black",
        race_code == 3 ~ "ai_an",     # american indian / alaska native
        race_code == 4 ~ "asian_pac",
        TRUE           ~ "other"
      )
    )  %>% 
    mutate(across(c(pop_hispanic, pop_race), ~ ./race_pop_total, .names = gsub("pop", "", "pct_{.col}"))) %>% 
    rename_with(~ sub("pop_", "", .x), starts_with("pct_pop_")) %>% 
    pivot_wider(
      id_cols = all_of(setdiff(names(.), c('race_code', 'pop_race', 'race', 'pct_race'))),
      names_from = race,
      values_from = c(pct_race, pop_race),
      names_glue = "{.value}_{race}",
      values_fill = NA      # or use 0 if you prefer
    ) %>% 
    rename_with(~gsub("pct_race", "pct", .x), contains("pct_race"))
  
  stopifnot(df %>% mutate(test = pct_white + pct_black + pct_ai_an + pct_asian_pac) %>% filter(test < 0.99 | test > 1.01) %>% nrow(.) == 0)
  
  return(df)
  
}


# 1990-1999
test <- read_fwf(here("data/raw/race_controls/us.1990_2023.20ages.adjusted.txt"),
                 fwf_widths(fwf_widths, col_names),
                 col_types = cols("n", "c", "c", "c", "c", "n", "n", "n", "n", "n"), 
                 skip = 0,
                 n_max = 5000000)

max_year <- max(test$year)

test %>% 
  filter(year < max_year) -> raw_90_99

read_rows_1 <- nrow(raw_90_99)

convert_race_data(raw_90_99) -> df_90_99

rm(raw_90_99)
rm(test)

# 2000-2009

test <- read_fwf(here("data/raw/race_controls/us.1990_2023.20ages.adjusted.txt"),
                 fwf_widths(fwf_widths, col_names),
                 col_types = cols("n", "c", "c", "c", "c", "n", "n", "n", "n", "n"), 
                 # small buffer just in case...
                 skip = read_rows_1-10,
                 n_max = 6500000)

max_year <- max(test$year)

test %>% 
  filter(year < max_year & year > max(df_90_99$year)) -> raw_00_09

stopifnot(max(raw_00_09$year) == 2009 & min(raw_00_09$year) == 2000)

read_rows_2 <- nrow(raw_00_09)

convert_race_data(raw_00_09) -> df_00_09

rm(raw_00_09)
rm(test)


# 2010-2019

test <- read_fwf(here("data/raw/race_controls/us.1990_2023.20ages.adjusted.txt"),
                 fwf_widths(fwf_widths, col_names),
                 col_types = cols("n", "c", "c", "c", "c", "n", "n", "n", "n", "n"), 
                 # small buffer just in case...
                 skip = read_rows_1 + read_rows_2 - 10,
                 n_max = 7500000)

max_year <- max(test$year)

test %>% 
  filter(year < max_year & year > max(df_00_09$year)) -> raw_10_19

stopifnot(max(raw_10_19$year) == 2019 & min(raw_10_19$year) == 2010)

read_rows_3 <- nrow(raw_10_19)

convert_race_data(raw_10_19) -> df_10_19

rm(raw_10_19)
rm(test)

# 2020-2023

test <- read_fwf(here("data/raw/race_controls/us.1990_2023.20ages.adjusted.txt"),
                 fwf_widths(fwf_widths, col_names),
                 col_types = cols("n", "c", "c", "c", "c", "n", "n", "n", "n", "n"), 
                 # small buffer just in case...
                 skip = read_rows_1 + read_rows_2 + read_rows_3 - 10)

max_year <- max(test$year)

test %>% 
  filter(year > max(df_10_19$year)) -> raw_20_23

stopifnot(max(raw_20_23$year) == 2023 & min(raw_20_23$year) == 2020)

convert_race_data(raw_20_23) -> df_20_23

rm(raw_20_23)
rm(test)

rbind(df_90_99, 
      df_00_09,
      df_10_19, 
      df_20_23) -> full_df

test <- readRDS(here("data/raw/race_controls/data_race_seer.rds")) 
                
full_df %>% select(names(test)) %>% identical(test)

full_df %>% saveRDS(here("data/raw/race_controls/data_race_seer.rds"))



# Commuting Zones
source(here('code/source_code/cz_cleaning.R'))
source(here("code/source_code/useful_functions.R"))


# Created in pull_data.R
race_data <- readRDS(here("data/raw/race_controls/data_race_seer_selected_fips.RDS"))

czs_new <- czs %>% 
  rename(old_fips = fips) %>% 
  mutate(fips = case_when(!is.na(getfips[old_fips]) ~ getfips[old_fips],
                          TRUE ~ old_fips),
         cz_id = as.character(cz_id))

missing_fips <- czs_new %>% 
  pull(fips) %>% 
  unique %>% 
  setdiff(unique(race_data$fips), .) 

if (length(missing_fips) > 0) {
  message("Warning: Some FIPS codes are missing from czs.")
}

race_data_cz <-  race_data %>% 
  left_join(., czs_new, by = "fips") %>%
  select(-fips) %>%
  group_by(cz_id, year) %>% 
  summarise(across(c(race_pop_total, pop_hispanic, pop_race_white, pop_race_black, pop_race_ai_an, pop_race_asian_pac), ~sum(., na.rm = TRUE))) %>% 
    ungroup %>% 
    mutate(across(c(pop_hispanic, pop_race_white, pop_race_black, pop_race_ai_an, pop_race_asian_pac), ~ ./race_pop_total, .names = "pct_{.col}")) %>% 
  rename_with(~gsub("pct_pop_race_", "pct_", .), contains("pct_pop_race_"))

stopifnot(race_data_cz %>% mutate(test = pct_white + pct_black + pct_ai_an + pct_asian_pac) %>% filter(test < 0.99 | test > 1.01) %>% nrow(.) == 0)

race_data_cz %>% saveRDS(here("data/raw/race_controls/data_race_seer_selected_cz.RDS"))
  
# Census option:


# file <- read_html(here("data/raw/race_cps_acs/census_api_acs_5yr.html"))
# tables <- html_nodes(file, "table")
# data <- html_table(tables)[[1]] %>% 
#   clean_names
# 
# 
# race_vars <- data %>% 
#   filter(grepl("Estimate!!Percent!!RACE AND HISPANIC OR LATINO ORIGIN BY EDUCATIONAL ATTAINMENT!!", label))
# 
# priv_school <- data %>% 
#   filter(grepl("Estimate!!Percent in private school!!", label))
# 
# state_codes <- fips_codes %>% 
#   # includes only 50 states and DC 
#   filter(as.numeric(state_code) <= 56) %>% 
#   pull(state_code) %>% 
#   unique
# 
# vars_short <- vars %>% 
#   filter(Indicator.Variable %in% c("EmpS", "EmpSpv", "HirA", "Sep", "EarnS")) %>% 
#   # filter(grepl("Earn|Sep|Emp|Hir", Indicator.Variable) &
#   #        !grepl("Beg", Indicator.Variable)) %>% tibble
#   pull(Indicator.Variable) %>% 
#   paste0(., collapse = ",")
# 
# #### Downloads all data for all counties
# race_df <- tibble()
# for(yr in 2000:2001){
#   print(yr)
#   for(st in state_codes){
#     print(st)
#     dat <- tibble()
#     url <- paste0("https://api.census.gov/data/timeseries/qwi/se?get=industry,",
#                   vars_short,
#                   "&year=", as.character(yr),
#                   "&quarter=1,2,3,4&ind_level=S&for=county:*&in=state:", st,
#                   "&key=0b9039bd3038272ab0afe94e28911dcb9b9b7d43")
#     try(dat <- read.csv(url, header = TRUE))
#     naics_df <- rbind(naics_df, dat)
#   }
# }
# 
# # percent in private school
# # race
# 
# file <- read_html(here("data/raw/race_cps_acs/census_api_acs_demog_housing_estimates_key.html"))
# tables <- html_nodes(file, "table")
# data <- html_table(tables)[[1]] %>% 
#   clean_names
# 
# race_vars <- data %>% 
#   filter(grepl("Percent!!RACE!!", label, fixed = TRUE)) %>% 
#     filter(str_count(label, fixed("!")) == 8) %>% 
#   mutate(label = tolower(gsub(" ", "_", gsub("!!", "_", label)))) 
# 
# race_vars_list <- race_vars %>% pull(name)
# 
# race_df <- tibble()
# names_tmp <- list()
# for(yr in 2009:2023){
#   print(yr)
#   tmp <- read.csv(paste0("https://api.census.gov/data/", yr, "/acs/acs5/profile/variables?get=NAME,", paste0(race_vars_list, collapse = ","), "&for=county:*&descriptive=false&key=0b9039bd3038272ab0afe94e28911dcb9b9b7d43")) %>% 
#     tibble %>% 
#     mutate(year = yr) %>% 
#     clean_names() %>% 
#     rename_with(~gsub("fips", "", .))
#   # 
#   # print(tmp)
#   # names_prev <- names_tmp
#   # names_tmp <- names(tmp)
#   # print(setdiff(names_tmp, names_prev))
#   
#   race_df <- rbind(race_df, tmp)
#   gc()
# }
# 
# 
# race_df %>% 
#   rename(county_name = x_name) %>% 
#   mutate(county_name = gsub("[", "", county_name, fixed = TRUE),
#          county = gsub("]", "", county, fixed = TRUE),
#          state = as.character(state),
#          state = ifelse(nchar(state) == 1, paste0("0", state), state), 
#          fips = paste0(state, county)) %>% 
#   pull(fips)
#   

#race_df %>% saveRDS(here("county_race_data.RDS"))

