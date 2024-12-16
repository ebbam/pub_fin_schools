# ACS Property Values
library(here)
library(tidyverse)
library(fixest)
library(readxl)
library(plm)
library(multcomp)
conflicted::conflict_prefer_all("dplyr", quiet = TRUE)


## Census API Key
census_key = "0b9039bd3038272ab0afe94e28911dcb9b9b7d43"

## Controls
#ACS API:
  
#  Geographical level: 050
#% Bachelors degree or higher: DP02_0068PE

#DP02_0062PE	Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate (includes equivalency)
#DP02_0067PE	Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate or higher
#DP03_0092E	Estimate!!INCOME AND BENEFITS (IN 2022 INFLATION-ADJUSTED DOLLARS)!!Median earnings for workers (dollars)

#Information about missing data in ACS: https://www.census.gov/programs-surveys/acs/guidance/estimates.html
```{r}
# Educational attainment
api_list = list("median_earnings" = "DP03_0092E",	#Estimate!!INCOME AND BENEFITS (IN 2022 INFLATION-ADJUSTED DOLLARS)!!Median earnings for workers (dollars)
                "ed_25_over_bachelors_or_higher" = "DP02_0068PE",
                "ed_25_over_hse" = "DP02_0062PE",	#Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate (includes equivalency)
                "ed_25_over_hse_or_higher" = "DP02_0067PE"	#Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate or higher 
)

control_temp <- tibble()
for(yr in 2019:2009){
  url <- paste0("https://api.census.gov/data/", as.character(yr),"/acs/acs5/profile?get=", 
                paste0(api_list, collapse = ","),"&for=county:*&key=",census_key)
  # }else{
  #   url <- paste0("https://api.census.gov/data/", as.character(yr),"/acs/acs1/profile?get=DP03_0092E&for=county:*&key=",census_key)
  # }
  temp <- read.csv(url, header = TRUE, colClasses = "character") %>% 
    tibble %>% 
    rename("median_earnings" = "X..DP03_0092E",	#Estimate!!INCOME AND BENEFITS (IN 2022 INFLATION-ADJUSTED DOLLARS)!!Median earnings for workers (dollars)
           "ed_25_over_bachelors_or_higher" = "DP02_0068PE",
           "ed_25_over_hse" = "DP02_0062PE",	#Percent!!EDUCATIONAL ATTAINMENT!!Population 25 years and over!!High school graduate (includes equivalency)
           "ed_25_over_hse_or_higher" = "DP02_0067PE",
           county = county.) %>%
    mutate(median_earnings = as.numeric(gsub("[", "", median_earnings, fixed = TRUE)),
           across(c(ed_25_over_bachelors_or_higher, ed_25_over_hse, ed_25_over_hse_or_higher), ~as.numeric(.)),
           county = gsub("]", "", county, fixed = TRUE),
           fips_cb = paste0(state, county),
           year = yr) %>%
    select(-c(X, state, county))
  control_temp <- rbind(control_temp, temp)
}