### County Characteristics Cleaning ###
## This work is refined from scoping recorded in 'code/source_code/scoping/Willamette_Scoping.Rmd"

library(here)
library(tidyverse)
library(janitor)
library(gridExtra)
library(readr)
library(data.table)
library(readxl)

source(here('code/source_code/dicts.R'))
# County Economy Stats
# Source: https://apps.bea.gov/regional/downloadzip.cfm

## CAINC4: Personal Income and Employment by Major Component by County (1969-2021)
# All values except 5 are included in CAINC30

cainc4 <- read.csv(here("data/raw/BEA_county_econ_stats/CAINC4/CAINC4__ALL_AREAS_1969_2021.csv"), 
                   colClasses = c("GeoFIPS" = "character"), nrows = 73600) %>% 
  tibble %>% 
  mutate(fips = trimws(GeoFIPS)) %>% 
  filter(substr(fips, 3,5) != "000") %>%
  # These values are present in CAINC1 - have verified that they are identical!
  mutate(desc_unif = case_when(substr(Description, 1,1) != " " ~ paste0("a_inc_", trimws(Description)), 
                               substr(Description, 1,1) == " " ~ paste0("b_inc_", trimws(Description)),
                               TRUE ~ Description),  
         desc_unif = gsub(",", "",gsub(":", "", gsub(",", "", gsub("[0-9]/", "", gsub("[()]", "", gsub(" ", "_", desc_unif)))))),
         desc_unif = case_when(Description == "Personal income (thousands of dollars) " ~ "a_personal_inc",
                               Description == "Population (persons) 1/" ~ "population",
                               Description == "Per capita personal income (dollars) 2/" ~ "personal_inc_pc",
                               Description == "Total employment " ~ "a_empl_Total_employment",
                               Description == " Wage and salary employment " ~ "b_empl_Wage_and_salary_employment",
                               Description == " Proprietors employment " ~ "b_empl_Proprietors_employment" ,
                               Description == "Personal income (thousands of dollars) " ~ "a_Personal_income_k",
                               TRUE ~ desc_unif),
         desc_unif = ifelse(Unit == "Thousands of dollars", paste0(desc_unif, "_k"), desc_unif)) %>% 
  select(-c(Region, TableName, IndustryClassification, GeoName, LineCode, GeoFIPS, Description, Unit)) %>% 
  relocate(fips, desc_unif) %>% 
  # There are two observations for b_inc_Employer_contributions_for_government_social_insurance_k
  distinct %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value", names_prefix = "X") %>% 
  pivot_wider(id_cols = c("fips", "year"), values_from = value, names_from = desc_unif) %>%
  mutate(across(!fips, ~as.numeric(.x)))

cainc4_names <- cainc4 %>% 
  names 



## CAINC5N: Personal Income by Major Component of Industry and Earnings by NAICS Industry (2001-2021)
# Isolates variables present in CAINC4 - to be excluded from CAINC5N
# They have been confirmed IDENTICAL via testing!
cainc4_cats <- read.csv(here("data/raw/BEA_county_econ_stats/CAINC4/CAINC4__ALL_AREAS_1969_2021.csv"), 
                        colClasses = c("GeoFIPS" = "character"), nrows = 73600) %>% 
  tibble %>% pull(Description) %>% unique %>% 
  lapply(., function(x) trimws(gsub("[()]", "", gsub("[0-9]/", "", x)))) %>% paste0(., collapse = "|")


cainc5n <- read.csv(here("data/raw/BEA_county_econ_stats/CAINC5N/CAINC5N__ALL_AREAS_2001_2021.csv"), 
                    colClasses = c("GeoFIPS" = "character"), nrows = 416318) %>%
  tibble %>% 
  mutate(fips = trimws(GeoFIPS)) %>% 
  filter(substr(fips, 3,5) != "000") %>% 
  # Removes values that are already present in CAINC4
  filter(!grepl(cainc4_cats, trimws(gsub("[()]", "", Description)))) %>%
  mutate(IndustryClassification = case_when(Description == "  Government and government enterprises " ~ "99",
                                            Description == "   Federal civilian " ~ "991",
                                            Description == "   Military "   ~ "992",
                                            Description == "   State and local " ~ "993",
                                            Description == "    State government " ~ "9931",
                                            Description == "    Local government " ~ "9932",
                                            Description == " Nonfarm earnings " ~ "113-99",
                                            TRUE ~ IndustryClassification),
         Description = gsub(",", "", gsub("[()]", "", gsub(" ", "_", trimws(gsub("[0-9]/", "", Description))))),
         IndustryClassification = gsub("-", "_", gsub(",", "__", IndustryClassification)),
         # IndustryClassificaition has an ordering of the NAICS codes and how they are nested in the dataframe
         # All values in thousands of dollars
         desc_unif = paste0("x", IndustryClassification, "_", Description, "_inc_k")) %>%
  select(-c(TableName, Region, GeoName, GeoFIPS, LineCode, Description, Unit, IndustryClassification)) %>% 
  relocate(fips, desc_unif) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value", names_prefix = "X") %>% 
  pivot_wider(id_cols = c("fips", "year"), values_from = value, names_from = desc_unif) %>%
  mutate(across(!fips, ~as.numeric(.x)))

cainc5_names <- cainc5n %>% names



## CAINC6N: Compensation of Employees by NAICS Industry (2001-2021)
cainc6n <- read.csv(here("data/raw/BEA_county_econ_stats/CAINC6N/CAINC6N__ALL_AREAS_2001_2021.csv"), 
                    colClasses = c("GeoFIPS" = "character"), nrows = 378182) %>%
  tibble %>% 
  mutate(fips = trimws(GeoFIPS)) %>% 
  filter(substr(fips, 3,5) != "000") %>% 
  select(-c(TableName, GeoFIPS, Region, GeoName, LineCode)) %>% 
  relocate(fips) %>% 
  # Following are reported in CAINC4 on a longer time horizon 1969 - 2021, therefore removed here
  filter(!Description %in% c(" Wages and salaries " , 
                             " Supplements to wages and salaries ",   
                             "  Employer contributions for employee pension and insurance funds 2/",
                             "  Employer contributions for government social insurance ")) %>% 
  # Compensation of employees = wages&salaries + Supplements to wages and salaries - "Compensation of employees total is not present in CAINC4
  mutate(IndustryClassification = case_when(Description == "  Government and government enterprises " ~ "99",
                                            Description == "   Federal civilian " ~ "991",
                                            Description == "   Military "   ~ "992",
                                            Description == "   State and local " ~ "993",
                                            Description == "    State government " ~ "9931",
                                            Description == "    Local government " ~ "9932",
                                            Description == " Nonfarm compensation " ~ "113-99",
                                            TRUE ~ IndustryClassification),
         Description = gsub(",", "", gsub("[()]", "", gsub(" ", "_", trimws(gsub("[0-9]/", "", Description))))),
         IndustryClassification = gsub("-", "_", gsub(",", "__", IndustryClassification)), 
         desc_unif = ifelse(Description %in% c("Compensation_of_employees_thousands_of_dollars", 
                                               "Average_compensation_per_job_dollars"), Description, paste0("x", IndustryClassification, "_comp_", Description, "_k"))) %>% 
  select(fips, desc_unif, X2001:X2021) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value", names_prefix = "X") %>% 
  pivot_wider(id_cols = c("fips", "year"), values_from = value, names_from = desc_unif) %>%
  mutate(across(!fips, ~as.numeric(.x)))


cainc6_names <- cainc6n %>% names

## CAEMP25N: Total FT and PT Employment by NAICS Industry (2001-2021)
caemp25n <- read.csv(here("data/raw/BEA_county_econ_stats/CAEMP25N/CAEMP25N__ALL_AREAS_2001_2021.csv"), 
                     nrows = 104874, colClasses = c("GeoFIPS" = "character")) %>% 
  tibble %>% 
  mutate(fips = trimws(GeoFIPS)) %>% 
  # Filtered out of this dataset because it is included on a longer time horizon in CAINC4 (1969-2021)
  filter(!(Description %in% c("Total employment (number of jobs) ",
                              " Wage and salary employment ",
                              " Proprietors employment "))) %>%
  # Filter out national and state-wide totals
  filter(substr(fips, 3,5) != "000") %>% 
  mutate(desc_unif = case_when(substr(Description, 1,4) == "    " ~ paste0("e_empl_", trimws(Description)), 
                               substr(Description, 1,3) == "   " ~ paste0("d_empl_", trimws(Description)), 
                               substr(Description, 1,2) == "  " ~ paste0("c_empl_", trimws(Description)), 
                               substr(Description, 1, 1) == " " ~ paste0("b_empl_", trimws(Description)),  
                               substr(Description, 1, 1) != " " ~ paste0("a_empl_", trimws(Description))),
         desc_unif = gsub(",", "", gsub("2/", "", gsub("[()]", "", gsub(" ", "_", desc_unif))))) %>% 
  relocate(fips, desc_unif) %>% 
  # Removing unnecessary columns and columns that only have one unique value (TableName, Unit)
  select(-c(TableName, Description, GeoFIPS, Unit, IndustryClassification, LineCode, Region, GeoName)) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "n_jobs", names_prefix = "X") %>% 
  pivot_wider(id_cols = c("fips", "year"), values_from = n_jobs, names_from = desc_unif) %>%
  mutate(across(!fips, ~as.numeric(.x)))

caemp25_names <- caemp25n %>% names

## CAINC30: Economic Profile by County (1969-2021) (MERGED!)
cainc30 <- read.csv(here("data/raw/BEA_county_econ_stats/CAINC30/CAINC30__ALL_AREAS_1969_2021.csv"), colClasses = c("GeoFIPS" = "character"), nrows = 99200) %>%
  tibble %>% 
  mutate(fips = trimws(GeoFIPS)) %>% 
  # Filter out national and state-wide totals
  filter(substr(fips, 3,5) != "000") %>% 
  select(fips, Description, X1969:X2021) %>% 
  mutate(Description = trimws(gsub("(number of jobs)", "", gsub("[0-9]/", "", Description), fixed = TRUE))) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value", names_prefix = "X") %>% 
  pivot_wider(id_cols = c("fips", "year"), values_from = value, names_from = Description) %>%
  mutate(across(!fips, ~as.numeric(.x))) %>% 
  clean_names %>% 
  rename(pinc_k = personal_income_thousands_of_dollars) %>% 
  mutate(pinc_k_pc = pinc_k/population_persons)

cainc30_names <- cainc30 %>% select(-c(fips, year)) %>% names

# Controls saved for merging in analysis.Rmd
#cainc30 %>% saveRDS(here('data/temp/cainc30_controls.RDS'))

cats_controls <- readRDS(here("data/temp/pubexp_1970_2020.RDS")) %>% 
  mutate(fips = ifelse(fips %in% names(getfips), unname(getfips[fips]), fips)) %>% 
  left_join(., cainc30, by = c('fips', 'year'))

#cats_controls %>% select(-names(cainc30)) %>% identical(select(readRDS(here("data/temp/pubexp_1970_2020.RDS")), -c(fips, year)))

# Testing "balanced" panels
cats_controls %>% nrow == 51*3058*19

#cats_controls %>% saveRDS(here("data/temp/pubexp_1970_2020_controls.RDS"))

tots_controls <- readRDS(here("data/temp/pubexp_1970_2020_totals.RDS")) %>% 
  mutate(fips = ifelse(fips %in% names(getfips), unname(getfips[fips]), fips)) %>% 
  left_join(., cainc30, by = c('fips', 'year'))

#tots_controls %>% select(-names(cainc30)) %>% identical(select(readRDS(here("data/temp/pubexp_1970_2020_totals.RDS")), -c(fips, year)))

# Testing "balanced" panels
tots_controls %>% nrow == 51*3058*22

#tots_controls %>% saveRDS(here("data/temp/pubexp_1970_2020_totals_controls.RDS"))


### Testing presence of CAINC4N in CAINC30

cainc4_test <- read.csv(here("data/raw/BEA_county_econ_stats/CAINC4/CAINC4__ALL_AREAS_1969_2021.csv"), 
                        colClasses = c("GeoFIPS" = "character"), nrows = 73600) %>% 
  tibble %>% 
  mutate(fips = trimws(GeoFIPS)) %>% 
  filter(substr(fips, 3,5) != "000") %>%
  # These values are present in CAINC1 - have verified that they are identical!
  mutate(Description = trimws(gsub("(dollars)", "", 
                                   gsub("Less: ", "",
                                        gsub("Plus: ", "",
                                             gsub("Equals: ", "", 
                                                  gsub("[0-9]/", "", Description)))), fixed = TRUE))) %>% 
  select(-c(Region, TableName, IndustryClassification, GeoName, LineCode, GeoFIPS, Unit)) %>% 
  relocate(fips, Description) %>% 
  # There are two observations for b_inc_Employer_contributions_for_government_social_insurance_k
  distinct %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value", names_prefix = "X") %>% 
  pivot_wider(id_cols = c("fips", "year"), values_from = value, names_from = Description) %>%
  mutate(across(!fips, ~as.numeric(.x)))

cainc4_names <- cainc4_test %>% select(-c(fips, year)) %>% names

for(j in intersect(cainc4_names, cainc30_names)){
  stopifnot(cainc4_test %>%  
              select(fips, year, eval(j)) %>% 
              identical(select(cainc30, fips, year, eval(j))) == TRUE)
}

# Categories in cainc4 that are missing from cainc30:
setdiff(cainc4_names, cainc30_names)
# "Nonfarm personal income",
# "Farm income", 
# "Contributions for government social insurance", 
# "Employee and self-employed contributions for government social insurance", 
# "Adjustment for residence"



## CAINC35: Personal Current Transfer Receipts (1969-2021)
cainc35 <- read.csv(here("data/raw/BEA_county_econ_stats/CAINC35/CAINC35__ALL_AREAS_1969_2021.csv"), colClasses = c("GeoFIPS" = "character"), nrows = 73600) %>%
  tibble %>% 
  mutate(fips = trimws(GeoFIPS)) %>% 
  # Filter out national and state-wide totals
  filter(substr(fips, 3,5) != "000")

cainc35 %>% select(Description) %>% distinct %>% print(n = 31)


## (EXCLUDED) CAINC45: Farm Income & Expenses (1969-2021)
# EXCLUDED because deemed unrelated to topic for now

cainc45 <- read.csv(here("data/raw/BEA_county_econ_stats/CAINC45/CAINC45__ALL_AREAS_1969_2021.csv"), colClasses = c("GeoFIPS" = "character"), nrows = 92800) %>% 
  tibble %>% 
  mutate(fips = trimws(GeoFIPS)) %>% 
  # Filter out national and state-wide totals
  filter(substr(fips, 3,5) != "000")

cainc45 %>% select(Description) %>% distinct %>% print(n = 31)



## CAINC91: Gross Flow of Earnings (1990-2021)
cainc91 <- read.csv(here("data/raw/BEA_county_econ_stats/CAINC91/CAINC91__ALL_AREAS_1990_2021.csv"), colClasses = c("GeoFIPS" = "character"), nrows = 9357) %>% 
  tibble %>% 
  mutate(fips = trimws(GeoFIPS)) %>% 
  # Filter out national and state-wide totals
  filter(substr(fips, 3,5) != "000") %>% 
  select(-c(TableName, LineCode, IndustryClassification, Unit, Region, GeoFIPS, GeoName)) %>% 
  relocate(fips, Description) %>% 
  # Inflows of earnings - outflow of earnings = adjustment for residence
  # all in thousands of dollars
  mutate(Description = gsub(" ", "_", trimws(Description))) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value", names_prefix = "X") %>% 
  pivot_wider(id_cols = c("fips", "year"), values_from = value, names_from = Description) %>%
  mutate(across(!fips, ~as.numeric(.x)))


# County GDP and Real GDP
#Source: BEA https://apps.bea.gov/regional/downloadzip.cfm

gdp <- read.csv(here("data/raw/BEA_county_econ_stats/CAGDP2/CAGDP2__ALL_AREAS_2001_2021.csv"), colClasses = c("GeoFIPS" = "character"), nrows = 108052) %>% 
  tibble %>% 
  mutate(fips = trimws(GeoFIPS)) %>% 
  # Filter out national and state-wide totals
  # LineCode 1 corresponds to total GDP
  # LineCode 2 Private industry total
  # LineCode 6 Mining, quarrying, and oil & gas extraction
  # LineCode 83 is Government and government enterprises
  filter(substr(fips, 3,5) != "000" & LineCode %in% c(1, 2, 6, 83)) %>%
  select(-c(Unit, IndustryClassification, TableName, Region, GeoFIPS, GeoName, LineCode)) %>% 
  relocate(fips, Description) %>% 
  mutate(Description = case_when(Description == "All industry total " ~ "total", 
                                 Description == " Private industries " ~ "priv_ind", 
                                 Description == "  Mining, quarrying, and oil and gas extraction " ~ "o_g_mining_quarr_21", 
                                 Description == "Government and government enterprises "  ~ "govt")) %>%
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value", names_prefix = "X") %>% 
  pivot_wider(id_cols = c("fips", "year"), values_from = value, names_from = Description, names_glue = "gdp_{Description}") %>%
  mutate(across(!fips, ~as.numeric(.x)))


full_gdp <- read.csv(here("data/raw/BEA_county_econ_stats/CAGDP9/CAGDP9__ALL_AREAS_2001_2021.csv"), colClasses = c("GeoFIPS" = "character"), nrows = 108052) %>% 
  tibble %>%
  mutate(fips = trimws(GeoFIPS)) %>%
  # Filter out national and state-wide totals
  # LineCode 1 corresponds to total GDP
  # LineCode 2 Private industry total
  # LineCode 6 Mining, quarrying, and oil & gas extraction
  # LineCode 83 is Government and government enterprises
  filter(substr(fips, 3,5) != "000" & LineCode %in% c(1, 2, 6, 83)) %>% 
  select(-c(Unit, IndustryClassification, TableName, Region, GeoFIPS, GeoName, LineCode)) %>% 
  relocate(fips, Description) %>%
  mutate(Description = case_when(Description == "All industry total " ~ "total", 
                                 Description == " Private industries " ~ "priv_ind", 
                                 Description == "  Mining, quarrying, and oil and gas extraction " ~ "o_g_mining_quarr_21", 
                                 Description == "Government and government enterprises "  ~ "govt")) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value", names_prefix = "X") %>% 
  pivot_wider(id_cols = c("fips", "year"), values_from = value, names_from = Description, names_glue = "real_gdp_{Description}") %>%
  mutate(across(!fips, ~as.numeric(.x))) %>% 
  left_join(., gdp, by = c("fips", "year"))


#full_gdp %>% saveRDS(here("data/temp/gdp_controls.RDS"))
