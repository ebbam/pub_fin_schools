### Commuting Zones Cleaning

# Official source of Commuting Zone codes - fips crosswalk (USDA ERS): https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/
# See also discussion here: https://osf.io/8vsh2
# PSU keep record of comparable US labour market geographies: https://sites.psu.edu/psucz/
# David Dorn's website with references and crosswalks: https://www.ddorn.net/data.htm

library(here)
library(readxl)
library(tidyverse)
library(conflicted)
conflict_prefer_all("dplyr", quiet = TRUE)

# Commuting zones
czs <- read_xls(here('data/out/cz00_eqv_v1.xls')) %>%
  select(FIPS, `Commuting Zone ID, 2000`, `Commuting Zone Population 2000`, `Commuting Zone ID, 1990`) %>% 
  rename(fips = 1, 
         cz_id= 2,
         cz_population = 3,
         cz_id_1990 = 4) %>% 
  mutate(fips = ifelse(fips == "46113", "46102", fips),
         cz_id = as.character(cz_id))

# CZs with full years
gen_czs_years <- function(df, cz_conversion){
  min_year <- min(df$year) 
  max_year <- max(df$year)
  czs_years <- czs %>% 
    mutate(year = min_year) %>% 
    select(fips, cz_id, year) %>% 
    complete(fips, year = min_year:max_year) %>% 
    group_by(fips) %>% 
    fill(cz_id, .direction = "updown") %>% 
    ungroup %>% 
    select(cz_id, fips, year) 
  return(czs_years)
}

# Function that converts commuting zones to fips codes for plotting via usmap
cz_to_map <- function(df, czs_years, vars = NULL){
  if(!is.null(vars)){
    print("Returning dataframe with selected vars.")
    temp <- mines_restr %>% 
      select(cz_id, year, all_of(vars)) %>% 
      left_join(czs_years, ., by = c("cz_id", "year")) 

  }else{
    print("Returning dataframe with all vars.")
    temp <- czs_years %>% 
      left_join(., mines_restr, by = c("cz_id", "year"))
  }
  return(temp)
}
