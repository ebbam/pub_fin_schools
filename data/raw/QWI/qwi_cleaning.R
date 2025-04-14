# Quarterly Workforce Indicators
# https://www.census.gov/data/developers/data-sets/qwi.html
# Available via API call

# https://www.census.gov/data/developers/data-sets/qwi.html

library(tidyverse)
library(here)
library(httr)
library(tidycensus)

vars <- read.csv(here('data/raw/QWI/variables_qwi.csv'), sep = ";") %>% 
  # Excludes data on replacement hires
  filter(Use == 1) #%>% 
  
vars_all <- vars %>% 
  pull(Indicator.Variable) %>% 
  paste0(., collapse = ",")

state_codes <- fips_codes %>% 
  # includes only 50 states and DC 
  filter(as.numeric(state_code) <= 56) %>% 
  pull(state_code) %>% 
  unique

# #### Downloads all data for all counties
# full_df <- tibble()
# for(yr in 2000:2025){
#   print(yr)
#   for(st in state_codes){
#     print(st)
#     dat <- tibble()
#     url <- paste0("https://api.census.gov/data/timeseries/qwi/se?get=", 
#                   vars_all, 
#                   "&year=", as.character(yr), 
#                   "&quarter=1,2,3,4&for=county:*&in=state:", st,
#                   "&key=0b9039bd3038272ab0afe94e28911dcb9b9b7d43")
#     try(dat <- read.csv(url, header = TRUE))
#     full_df <- rbind(full_df, dat)
#   }
# }
# 
# data <- full_df %>% 
#   tibble %>%
#   mutate(X..Emp = gsub("[", "", X..Emp, fixed = TRUE),
#          county. = gsub("]", "", county., fixed = TRUE), 
#          across(X..Emp:Payroll, ~as.numeric(.)),
#          state = ifelse(nchar(state) == 1, paste0("0", as.character(state)), as.character(state)),
#          fips = paste0(state, county.)) %>% 
#   select(-Payroll) %>%
#   rename(county = county.,
#          Emp = X..Emp) %>% 
#   relocate(year, fips, state, county, quarter) 
# 
# data %>% saveRDS(here("data/raw/QWI/qwi_all_2000-2024.RDS"))
#   

vars_short <- vars %>% 
 filter(Indicator.Variable %in% c("EmpS", "EmpSpv", "HirA", "Sep", "EarnS")) %>% 
  # filter(grepl("Earn|Sep|Emp|Hir", Indicator.Variable) &
  #        !grepl("Beg", Indicator.Variable)) %>% tibble
  pull(Indicator.Variable) %>% 
  paste0(., collapse = ",")

#### Downloads all data for all counties
naics_df <- tibble()
for(yr in 2000:2001){
  print(yr)
  for(st in state_codes){
    print(st)
    dat <- tibble()
    url <- paste0("https://api.census.gov/data/timeseries/qwi/se?get=industry,",
                  vars_short,
                  "&year=", as.character(yr),
                  "&quarter=1,2,3,4&ind_level=S&for=county:*&in=state:", st,
                  "&key=0b9039bd3038272ab0afe94e28911dcb9b9b7d43")
    try(dat <- read.csv(url, header = TRUE))
    naics_df <- rbind(naics_df, dat)
  }
}

