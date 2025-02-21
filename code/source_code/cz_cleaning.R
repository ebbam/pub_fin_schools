### Commuting Zones Cleaning

# Official source of Commuting Zone codes - fips crosswalk (USDA ERS): https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/
# See also discussion here: https://osf.io/8vsh2
# PSU keep record of comparable US labour market geographies: https://sites.psu.edu/psucz/
# David Dorn's website with references and crosswalks: https://www.ddorn.net/data.htm

library(here)
library(readxl)
library(tidyverse)
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


