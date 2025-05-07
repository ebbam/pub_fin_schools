# Cleaning the FHFA Price Index
library(readxl)
library(here)
library(tidyverse)
library(janitor)
source(here("code/source_code/cz_cleaning.R"))
source(here("code/source_code/useful_functions.R"))
czs <- czs %>% 
  rename(old_fips = fips) %>% 
  mutate(fips = case_when(!is.na(getfips[old_fips]) ~ getfips[old_fips],
                          TRUE ~ old_fips),
         cz_id = as.character(cz_id))

# hpi_at_bdl_county is taken from the "Additional Data" section https://www.fhfa.gov/data/hpi/datasets?tab=additional-data ("Counties (Developmental Index; Not Seasonally Adjusted")
# County map: https://www.fhfa.gov/data/dashboard/fhfa-hpi-county-map
# Definition: The FHFA HPI® is a comprehensive collection of publicly available house price indexes that measure changes in single-family home values based on data that extend back to the mid-1970s from all 50 states and over 400 American cities.
# The FHFA HPI® is a broad measure of the movement of single-family house prices. The FHFA HPI® is a weighted, repeat-sales index, meaning that it measures average price changes in repeat sales or refinancings on the same properties. This information is obtained by reviewing repeat mortgage transactions on single-family properties whose mortgages have been purchased or securitized by Fannie Mae or Freddie Mac since January 1975.
# The FHFA HPI® serves as a timely, accurate indicator of house price trends at various geographic levels. Because of the breadth of the sample, it provides more information than is available in other house price indexes. It also provides housing economists with an improved analytical tool that is useful for estimating changes in the rates of mortgage defaults, prepayments and housing affordability in specific geographic areas.
df <- read_xlsx(here('data/raw/fhfa_hpi/hpi_at_bdl_county.xlsx'), skip = 5) %>% 
  clean_names() %>% 
  rename(fips = fips_code,
         county_name = county, 
         hpi_annual_change_pct = annual_change_percent,
         hpi_1990_base = hpi_with_1990_base,
         hpi_2000_base = hpi_with_2000_base) %>%  
  mutate(fips = ifelse(fips %in% names(getfips), unname(getfips[fips]), fips)) %>%
  filter(!(fips %in% c("51560", "51515")) & !(fips == "46113" & year <= 2015)) %>%
  mutate(fips = ifelse(fips == "46113", "46102", fips)) %>% 
  group_by(fips, year) %>% 
  summarise(across(c(hpi_annual_change_pct, hpi, hpi_1990_base, hpi_2000_base), ~mean(., na.rm = TRUE))) %>% 
  ungroup %>% 
  mutate(across(c(hpi, hpi_1990_base, hpi_2000_base), ~log(.), .names = "log_{.col}")) 

missing_fips <- czs %>% 
  pull(fips) %>% 
  unique %>% 
  setdiff(unique(df$fips), .) 

df_cz <- df %>% 
  left_join(., czs, by = "fips", multiple = "first") %>% 
  filter(!is.na(cz_id)) %>% 
  group_by(year, cz_id) %>% 
  summarise(across(c(hpi_annual_change_pct, hpi, hpi_1990_base, hpi_2000_base), ~mean(., na.rm = TRUE))) %>% 
  ungroup %>% 
  mutate(across(c(hpi, hpi_1990_base, hpi_2000_base), ~log(.), .names = "log_{.col}")) 
  
df %>% 
  pivot_longer(!c(fips, year)) %>% 
  filter(name %in% c("hpi_annual_change_pct", "hpi", "log_hpi")) %>% 
  mutate(label = case_when(name == "hpi_annual_change_pct" ~ "Growth Rate (Annual Pct. Change)", 
                           name == "hpi" ~ "House Price Index", 
                           name == "log_hpi" ~ "(log) House Price Index",
                           TRUE ~ NA)) %>% 
  ggplot(aes(x = year, y = value, group = fips, color = label)) + 
  geom_jitter() + 
  facet_wrap(~label, scales = "free") +
  theme_minimal() + 
  labs(title = "House Price Index (levels, log-levels, and growth rates)", subtitle = "Unit: Counties", x = "Year", y = "Value") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")

df_cz %>% 
  pivot_longer(!c(cz_id, year)) %>% 
  filter(name %in% c("hpi_annual_change_pct", "hpi", "log_hpi")) %>% 
  mutate(label = case_when(name == "hpi_annual_change_pct" ~ "Growth Rate (Annual Pct. Change)", 
                           name == "hpi" ~ "House Price Index", 
                           name == "log_hpi" ~ "(log) House Price Index",
                           TRUE ~ NA)) %>% 
  ggplot(aes(x = year, y = value, group = cz_id, color = label)) + 
  geom_jitter() + 
  facet_wrap(~name, scales = "free") + theme_minimal() + 
  labs(title = "House Price Indices (levels, log-levels, and growth rates)", subtitle = "Unit: Commuting Zones", x = "Year", y = "Value") + 
  scale_color_brewer(palette = "Dark2") +
  theme(legend.position = "none")

# hpi_master.csv is taken from "Master HPI Data" https://www.fhfa.gov/data/hpi/datasets?tab=master-hpi-data
# With accompanying data dictionary