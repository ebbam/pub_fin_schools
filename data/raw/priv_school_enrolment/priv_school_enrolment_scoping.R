# ACS Property Values
library(here)
library(tidyverse)
library(fixest)
library(readxl)
library(plm)
library(janitor)
library(zoo)
library(multcomp)
source(here('code/source_code/dicts.R'))
source(here('code/source_code/useful_functions.R'))
conflicted::conflict_prefer_all("dplyr", quiet = TRUE)


## Census API Key
census_key = "0b9039bd3038272ab0afe94e28911dcb9b9b7d43"
acs = "acs5"

## Controls
#ACS API:

# Information about missing data in ACS: https://www.census.gov/programs-surveys/acs/guidance/estimates.html
# Information about ACS1 variables: https://api.census.gov/data/2024/acs/acs1/variables.html - used below to identify variable names to call
# Information about ACS5 variables: https://api.census.gov/data/2024/acs/acs5/variables.html - used below to identify variable names to call

library(rvest)
# Reading dcoumentation on variables in the ACS one-year estimates
acs_vars_html_url <- read_html(paste0("https://api.census.gov/data/2024/acs/", acs,"/variables.html"))
acs_vars_raw <- html_table(
  acs_vars_html_url,
  header = NA,
  trim = TRUE,
  dec = ".",
  na.strings = "NA",
  convert = TRUE)[[1]] %>% 
  rename("check" = 9) %>% 
  tibble

if(acs == "acs1"){
  
  
  # Select variables in "School Enrollment by Type of School by Age for the Population 3 Years and Over" universe
  acs_vars_all <- acs_vars_raw %>% 
    filter(substr(Name, 1, 6) == "C14003")  %>% 
    select(Name, Label, Attributes) %>% 
    separate(Label, "!!", into = c("Value", "value_universe", "enrollment", "age_group"), extra = "merge") %>% 
    separate(Attributes, ",\n                ", into = c("estimate_annotation", "margin_of_error", "margin_of_error_annotation"), extra = "merge") %>% 
    mutate(enrollment = ifelse(is.na(enrollment), "students", tolower(gsub(" ", "_", gsub(":", "", gsub("Enrolled ", "", enrollment))))),
           age_group = ifelse(is.na(age_group), "total", gsub("_years", "", gsub(" ", "_", age_group)))) %>% 
    rename(estimate = Name) %>% 
    select(-c("Value", "value_universe")) %>% 
    pivot_longer(cols = c('estimate', 'estimate_annotation', 'margin_of_error', 'margin_of_error_annotation')) 
  
  # Selecting both estimate and margin of error from the API call - ignoring annotation for now
  acs_vars_labels <- acs_vars_all %>% 
    filter(!grepl("annotation", name)) %>% 
    mutate(var_name = paste0(enrollment, "_", age_group)) %>% 
    select(-c(enrollment, age_group)) %>% 
    # Removes students over high school age to minimise size of API call
    filter(!grepl("20_to_24|25_to_34|35_and_over", var_name)) %>% 
    mutate(clean_name = ifelse(name == "margin_of_error", paste0(var_name, "_", name), var_name))
  
  name_map <- setNames(acs_vars_labels$value, acs_vars_labels$clean_name)
  
  acs_vars_selected <- acs_vars_labels$value
  stopifnot(identical(length(acs_vars_selected), n_distinct(acs_vars_selected)))
  
}else if(acs == "acs5"){
  group_var = "B14002"
  
  acs_vars_all <- acs_vars_raw %>% 
    filter(substr(Name, 1, 6) == "B14002") %>% 
    select(Name, Label, Attributes) %>% 
    separate(Label, "!!", into = c("Value", "value_universe", "sex", "enrolled_bool", "enrolled_level", "school_type"), extra = "merge") %>% 
    separate(Attributes, ",\n                ", into = c("estimate_annotation", "margin_of_error", "margin_of_error_annotation"), extra = "merge") %>% 
    filter(!is.na(enrolled_level)) %>% 
    select(Name, sex, enrolled_level, school_type, estimate_annotation, margin_of_error, margin_of_error_annotation) %>% 
    mutate(enrolled_level = gsub(", | |:", "_", gsub("Enrolled in ", "", enrolled_level)),
           sex = paste0(trimws(tolower(gsub(":", "", sex))), "_"),
           school_type = ifelse(is.na(school_type), "total", tolower(gsub(" ", "_", school_type))),
           var_name = paste0(sex, enrolled_level, school_type)) %>% 
    rename(estimate = Name) %>% 
    select(-c("sex", "enrolled_level", "school_type")) %>% 
    pivot_longer(cols = c('estimate', 'estimate_annotation', 'margin_of_error', 'margin_of_error_annotation')) 
  
  # Selecting both estimate and margin of error from the API call - ignoring annotation for now
  acs_vars_labels <- acs_vars_all %>% 
    filter(!grepl("annotation", name)) %>% 
    #select(-c(enrollment, age_group)) %>% 
    # Removes students in nursery school or over high school age to minimise size of API call
    filter(!grepl("nursery|graduate", var_name)) %>%
    mutate(clean_name = ifelse(name == "margin_of_error", paste0(var_name, "_", name), var_name))
  
  name_map <- setNames(acs_vars_labels$value, acs_vars_labels$clean_name)
  
  acs_vars_selected <- acs_vars_labels$value
  stopifnot(identical(length(acs_vars_selected), n_distinct(acs_vars_selected)))
  
}


# Go through all years for each county, ACS 1-year estimates only available from 2005 and 2020 does not exist due to covid
priv_temp <- tibble()
for(yr in 2005:2024){
  if(yr == 2020 | (yr < 2009 & acs == "acs5")){
    next
  }
  print(yr)
  
  url <- paste0("https://api.census.gov/data/" , as.character(yr),"/acs/", acs,"?get=",
                paste0(acs_vars_selected, collapse = ","), "&for=county:*&key=", census_key)

  temp <- read.csv(url, header = TRUE, colClasses = "character") %>% 
    tibble %>% 
    mutate(year = yr)
  
  priv_temp <- rbind(priv_temp, temp)
}

if(acs == "acs1"){
  new <- priv_temp %>% 
    rename("C14003_001E" = "X..C14003_001E", 
           "county" = `county.`) %>% 
    mutate(C14003_001E = gsub("[", "", C14003_001E, fixed = TRUE),
           county = gsub("]", "", county, fixed = TRUE)) %>% 
    mutate(fips = paste0(state, county)) %>% 
    relocate("state", "county", "fips", "X", "year") %>% 
    select(-X) %>% 
    mutate(across(contains("C14003"), ~as.numeric(.))) %>% 
    rename(any_of(name_map)) %>% 
    filter(state != "72") %>% 
    # Total for each age group
    mutate(total_students_3_and_4 = in_public_school_3_and_4 + in_private_school_3_and_4,
           total_students_5_to_9 = in_public_school_5_to_9 + in_private_school_5_to_9,
           total_students_10_to_14 = in_public_school_10_to_14 + in_private_school_10_to_14,
           total_students_15_to_17 = in_public_school_15_to_17 + in_private_school_15_to_17,
           total_students_18_and_19 = in_public_school_18_and_19 + in_private_school_18_and_19,
           # Total for primary school
           in_private_school_primary = in_private_school_5_to_9 + in_private_school_10_to_14,
           total_students_primary = total_students_5_to_9 + total_students_10_to_14,
           not_enrolled_primary = not_enrolled_in_school_5_to_9 + not_enrolled_in_school_10_to_14,
           # Total for secondary school
           in_private_school_secondary = in_private_school_15_to_17 + in_private_school_18_and_19,
           total_students_secondary = total_students_15_to_17 + total_students_18_and_19,
           not_enrolled_secondary = not_enrolled_in_school_15_to_17 + not_enrolled_in_school_18_and_19,
           # Pct private school for each age group 
           #pct_private_school_3_and_4 = in_private_school_3_and_4/total_students_3_and_4, 
           pct_private_school_5_to_9 = in_private_school_5_to_9/total_students_5_to_9,
           pct_private_school_10_to_14 = in_private_school_10_to_14/total_students_10_to_14,
           pct_private_school_15_to_17 = in_private_school_15_to_17/total_students_15_to_17,
           pct_private_school_18_and_19 = in_private_school_18_and_19/total_students_18_and_19,
           # Pct private school for primary school
           pct_private_school_primary = in_private_school_primary/total_students_primary,
           pct_private_school_primary_incl_nonenrolled = in_private_school_primary/(total_students_primary + not_enrolled_primary),
           
           # Pct private school for secondary school
           pct_private_school_secondary = in_private_school_secondary/total_students_secondary,
           # Pct not enrolled for each age group 
           pct_not_enrolled_3_and_4 = not_enrolled_in_school_3_and_4/(total_students_3_and_4 + not_enrolled_in_school_3_and_4), 
           pct_not_enrolled_5_to_9 = not_enrolled_in_school_5_to_9/(total_students_5_to_9 + not_enrolled_in_school_5_to_9),
           pct_not_enrolled_10_to_14 = not_enrolled_in_school_10_to_14/(total_students_10_to_14 + not_enrolled_in_school_10_to_14),
           pct_not_enrolled_15_to_17 = not_enrolled_in_school_15_to_17/(total_students_15_to_17 + not_enrolled_in_school_15_to_17),
           pct_not_enrolled_18_and_19 = not_enrolled_in_school_18_and_19/(total_students_18_and_19 + not_enrolled_in_school_18_and_19),
           pct_not_enrolled_total_primary = (not_enrolled_primary)/(total_students_primary + not_enrolled_primary))
}else if(acs == "acs5"){
  test <- priv_temp %>% 
    rename("B14002_007E" = "X..B14002_007E", 
           "county" = `county.`) %>% 
    mutate(B14002_007E = gsub("[", "", B14002_007E, fixed = TRUE),
           county = gsub("]", "", county, fixed = TRUE)) %>% 
    mutate(fips = paste0(state, county)) %>% 
    relocate("state", "county", "fips", "X", "year") %>% 
    select(-X) %>% 
    mutate(across(contains("B14002"), ~as.numeric(.))) %>% 
    rename(any_of(name_map)) %>% 
    filter(state != "72") #%>% 
  
  suffixes <- test %>% select(-c(state, county, fips, year)) %>% names %>%
      map_chr(., ~gsub("male_|female_|_margin_of_error", "", .x)) %>% unique
  
  # Pattern to apply
    for(suffix in suffixes){
      test <- test %>% 
        mutate(!!paste0("all_", suffix) := !!sym(paste0("male_", suffix)) + !!sym(paste0("female_", suffix)))
  }

  new <- test %>% 
    mutate(total_students_primary = all_kindergarten_total + all_grade_1_to_grade_4_total + all_grade_5_to_grade_8_total,
           total_students_primary_private = all_kindergarten_private_school + all_grade_1_to_grade_4_private_school + all_grade_5_to_grade_8_private_school,
           total_students_primary_public = all_kindergarten_public_school + all_grade_1_to_grade_4_public_school + all_grade_5_to_grade_8_public_school,
           pct_private_school_primary = total_students_primary_private/total_students_primary,
           fips = ifelse(fips == "46113", "46102", fips))
}

#age_groups <- c("total", "3_and_4", "5_to_9", "10_to_14", "15_to_17", "18_and_19")
 
new %>% 
  ggplot(aes(y = pct_private_school_primary, x = fips, color = state)) + 
  geom_jitter() + 
  geom_smooth(method = "lm") + 
  theme(legend.position = "none") + 
  labs(x = "County", y = "% of Primary School Students Enrolled in Private School by County",
       title = "% of Primary School Students Enrolled in Private School by County") + 
  common_theme

saveRDS(new, here(paste0("data/raw/priv_school_enrolment/", acs, "_priv_school_enrolment_temp_fips.RDS")))

if(acs == "acs1"){
  new %>% 
    ggplot(aes(x = abs(pct_private_school_primary - pct_private_school_primary_incl_nonenrolled))) + 
    geom_histogram(fill = "slateblue", alpha = 0.7, bins = 50) + 
    theme(legend.position = "none") + 
    labs(y = "Count", x = "PP Difference in % of Primary School Students when Including Non-enrolled students",
         title = str_wrap("Percentage Point Difference of Primary School Students in Private School when Including Non-enrolled students", 60)) + 
    common_theme
  
  new %>% 
    ggplot(aes(y = abs(pct_private_school_primary - pct_private_school_primary_incl_nonenrolled), x = as.numeric(fips), color = state)) + 
    geom_jitter() + 
    theme(legend.position = "none") + 
    labs(x = "County", y = "PP Difference in % of Primary School Students when Including Non-enrolled Students", title = str_wrap("Percentage Point Difference of Primary School Students in Private School when Including Non-enrolled students by State", 60)) + 
    common_theme
  
  new %>% 
    ggplot(aes(y = pct_not_enrolled_total_primary, x = fips, color = state)) + 
    geom_jitter() + 
    geom_smooth(method = "lm") + 
    theme(legend.position = "none") + 
    labs(x = "County", y = "% of Primary School-Age Students Not Enrolled in School by County",
         title = "% of Primary School-Age Students Not Enrolled in School by County") + 
    common_theme
}


new %>% 
  ggplot(aes(y = pct_private_school_primary, x = year)) + 
  geom_jitter() + 
  geom_smooth() + 
  labs(x = "County", y = "% of Primary School Students Enrolled in Private School by Year") + 
  common_theme

###############################################################
###############################################################
#### Checking correct FIPS codes 
###############################################################

# FIPS checker
fips_ref <- readRDS(here("data/out/regression_data_complete_fips.RDS"))

# Exclude states not in ref_fips
# Note that Broomfield county is missing and a few NY counties are also missing in the reference file...not sure that should be the case...
fips_ref %>% arrange(state) %>% pull(state) %>% unique -> states_pres

new <- readRDS(here(paste0("data/raw/priv_school_enrolment/", acs, "_priv_school_enrolment_temp_fips.RDS"))) %>% 
  filter(state %in% states_pres) 

if(acs == "acs1"){
  # Check that all FIPS codes are present except those 5 from NY and Broomfield which I am aware of and have noted to take care of at a later date
  stopifnot(length(setdiff(new$fips, c(fips_ref$fips, "36005", "36047", "36061", "36081", "36085", "08014"))) == 0)
}else if(acs == "acs5"){
  # Check that all FIPS from main text are available in the dataset! remarkably no issues.....
  stopifnot(length(setdiff(fips_ref$fips,new$fips)) == 0)
}

new <- new %>% filter(fips %in% fips_ref$fips)

###############################################################
#### Complete missing years filling up to 2 continuous NA values
###############################################################

new <- new %>% 
  complete(fips, year = min(new$year):2024) %>% 
  select(-state) %>% 
  #select(fips, year, total_students_primary, not_enrolled_primary, contains("pct")) %>% 
  group_by(fips) %>% 
  arrange(fips, year) %>% 
  mutate(na_count_pre_fill = rowSums(is.na(across(starts_with("pct_")))), 
         across(contains('pct'), ~na.approx(., na.rm = FALSE, maxgap = 2)),
         na_count_post_fill = rowSums(is.na(across(starts_with("pct_"))))) %>% 
  group_by(fips) %>% 
  # Counts how many rows have 
  mutate(missing_years = sum(na_count_post_fill > 0)) %>% 
  ungroup

###############################################################
#### Check how many missing years - summarise FIPS and year coverage
###############################################################
new %>% 
  arrange(-missing_years) %>% 
  mutate(factor_fips = fct_reorder(factor(fips), missing_years)) %>% 
  group_by(factor_fips) %>% 
  summarise(missing_years = unique(missing_years)) %>% 
    ggplot() + 
    geom_col(aes(x = factor_fips, y = missing_years), fill = "steelblue") + 
  geom_hline(aes(yintercept = 5), linetype = "dashed", color = "slateblue") +
  labs(x = "County", y = "Number of Missing Values",
       title = "Degree of Missingness in Private School Enrolment Data",
       subtitle = "We remove any counties that have more than 5 missing values.") +
  common_theme
  
###############################################################
#### Save FIPS File
###############################################################

new %>% 
  filter(missing_years <= 5) %>% 
  saveRDS(here(paste0("data/raw/priv_school_enrolment/", acs, "_priv_school_enrolment_final_fips_long.RDS")))

readRDS(here(paste0("data/raw/priv_school_enrolment/", acs, "_priv_school_enrolment_final_fips_long.RDS"))) %>% 
  select(fips, year, contains('pct')) %>% 
  saveRDS(here(paste0("data/raw/priv_school_enrolment/", acs, "_priv_school_enrolment_final_fips.RDS")))
  
fips_file <- readRDS(here(paste0("data/raw/priv_school_enrolment/", acs, "_priv_school_enrolment_final_fips.RDS")))

library(stargazer)
if(acs == "acs1"){
  fips_file %>% 
    mutate(state = substr(fips, 1,2)) %>% 
    select(fips, state, pct_private_school_primary, pct_private_school_primary_incl_nonenrolled) %>% 
    mutate(across(starts_with("pct_"), ~ .x * 100)) %>%   # convert to percentages
    data.frame(.) %>% 
    stargazer(digits = 1, digits.extra = 3, 
              title = "% of Primary School Age Students (5-14) Enrolled in Private School",
              type = "text", covariate.labels = c("Baseline", "Including Children Not Enrolled in School"),
              header = FALSE, label = "tbl_desc_stats")
}

new_w_state <- fips_file %>% 
  mutate(state = substr(fips, 1, 2),
         State = sapply(state, get_state))

new_w_state %>% 
  group_by(State) %>% 
  summarise(
    Mean   = mean(pct_private_school_primary, na.rm = TRUE) * 100,
    SD     = sd(pct_private_school_primary, na.rm = TRUE) * 100,
    Min    = min(pct_private_school_primary, na.rm = TRUE) * 100,
    Max    = max(pct_private_school_primary, na.rm = TRUE) * 100,
    `N Counties`     = n_distinct(fips)
  ) %>% 
  data.frame(.) %>% 
  arrange(desc(Mean)) %>% 
  stargazer(summary = FALSE, digits = 2, digits.extra = 3,
            title = "% of Primary School (K-8) Enrolled in Private School, by State",
            type = "text", rownames = FALSE,
            header = FALSE, label = "tbl_desc_stats_state")

###############################################################
###############################################################
###############################################################

description_string_general <- c("The data are drawn from the American Community Survey (ACS) one-year estimates, accessed via the Census Bureau API for all US counties across the years 2005 to 2024, excluding 2020 which was not released due to COVID-19. ", 
                                "Specifically, the script pulls variables from the 'School Enrollment by Type of School by Age for the Population 3 Years and Over' universe (table C14003), retaining enrollment figures by school type (public, private, and not enrolled) for age groups up to 19 years old. ", 
                                "Puerto Rico (state FIPS code 72) is excluded from the raw download. ",
                                "The resulting county-year panel is then restricted to FIPS codes present in the main dataset dataset, with six known exceptions — five New York City borough counties (36005, 36047, 36061, 36081, 36085) and Broomfield County, Colorado (08014) — which are dropped. ",
                                "To address missingness, the panel is first completed to a balanced 2005–2024 structure and then linear interpolation is applied to fill gaps of up to two consecutive missing years. ",
                                "Counties with more than five remaining missing years after interpolation are dropped from the final dataset. ",
                                "Note that one-year ACS estiamtes are only available for counties with populations greater than 65,000 people. Therefore the sample size of counties is considerably smaller than the full sample explored in the main econometric specifications of this work. ")

description_string_calculation <- "The percentage of primary school students enrolled in private school is calculated as the number of students 5-14 years old enrolled in private school divided by the total number of students 5-14 years old enrolled in public or private school."

description_string_fips <- paste0(paste0(description_string_general, collapse = ""),
                                  description_string_calculation,
                                  "This yields a complete panel dataset of ", 
                                  n_distinct(new_w_state$fips), 
                                  " counties across ", 
                                  n_distinct(new_w_state$state), 
                                  " states.")

print(description_string_fips)

###############################################################
#### Save CZ File
###############################################################
source(here('code/source_code/cz_cleaning.R'))
new_cz <- readRDS(here(paste0("data/raw/priv_school_enrolment/", acs, "_priv_school_enrolment_final_fips_long.RDS")))

czs_new <- czs %>% 
  rename(old_fips = fips) %>% 
  mutate(fips = case_when(!is.na(getfips[old_fips]) ~ getfips[old_fips],
                          TRUE ~ old_fips),
         cz_id = as.character(cz_id))

missing_fips <- czs_new %>% 
  pull(fips) %>% 
  unique %>% 
  setdiff(unique(new_cz$fips), .) 

if (length(missing_fips) > 0) {
  message("Warning: Some FIPS codes are missing from czs.")
}


if(acs == "acs1"){
  priv_school_cz <- new_cz %>% 
    left_join(., czs_new, by = "fips") %>%
    select(cz_id, year, students_total, total_students_primary, not_enrolled_primary, in_private_school_primary)  %>% 
    group_by(cz_id, year) %>% 
    summarise(across(everything(), ~sum(., rm.na = TRUE))) %>% 
    ungroup %>% 
    mutate(pct_private_school_primary = in_private_school_primary/total_students_primary,
           pct_private_school_primary_incl_nonenrolled = in_private_school_primary/(total_students_primary + not_enrolled_primary))
  
  priv_school_cz %>% 
    #mutate(state = substr(fips, 1,2)) %>% 
    select(cz_id, pct_private_school_primary, pct_private_school_primary_incl_nonenrolled) %>% 
    mutate(across(starts_with("pct_"), ~ .x * 100)) %>%   # convert to percentages
    data.frame(.) %>% 
    stargazer(digits = 1, digits.extra = 3, 
              title = "% of Primary School Age Students (5-14) Enrolled in Private School",
              type = "text", covariate.labels = c("Baseline", "Including Children Not Enrolled in School"),
              header = FALSE, label = "tbl_desc_stats_state_priv_school_cz")
  
}else if(acs == "acs5"){
  priv_school_cz <- new_cz %>% 
    left_join(., czs_new, by = "fips") %>%
    select(cz_id, year, total_students_primary, total_students_primary_private, total_students_primary_public) %>% 
    group_by(cz_id, year) %>% 
    summarise(across(everything(), ~sum(., rm.na = TRUE))) %>% 
    ungroup %>% 
    mutate(pct_private_school_primary = total_students_primary_private/total_students_primary)
  
  priv_school_cz %>% 
    #mutate(state = substr(fips, 1,2)) %>% 
    select(cz_id, pct_private_school_primary) %>% 
    mutate(across(starts_with("pct_"), ~ .x * 100)) %>%   # convert to percentages
    data.frame(.) %>% 
    stargazer(digits = 1, digits.extra = 3, 
              title = "% of Primary School Students (K-8) Enrolled in Private School",
              type = "text", covariate.labels = c("Baseline"),
              header = FALSE, label = "tbl_desc_stats_state_priv_school_cz")
}

saveRDS(priv_school_cz, here(paste0("data/raw/priv_school_enrolment/", acs, "_priv_school_enrolment_final_cz.RDS")))

# Reference cz dataset
refs_cz <- readRDS(here("data/out/cz_dataset.RDS")) %>% 
  rename(state = main_state) %>% 
  select(cz_id, state) %>% 
  distinct

new_w_state <- priv_school_cz %>% 
  left_join(., refs_cz, by = "cz_id") %>% 
  mutate(State = sapply(state, get_state))

stopifnot(new_w_state %>% filter(is.na(state)) %>% nrow(.) == 0)
stopifnot(new_w_state %>% filter(is.na(State)) %>% nrow(.) == 0)

new_w_state %>% 
  group_by(State) %>% 
  summarise(
    Mean   = mean(pct_private_school_primary, na.rm = TRUE) * 100,
    SD     = sd(pct_private_school_primary, na.rm = TRUE) * 100,
    Min    = min(pct_private_school_primary, na.rm = TRUE) * 100,
    Max    = max(pct_private_school_primary, na.rm = TRUE) * 100,
    `N CZs`     = n_distinct(cz_id)
  ) %>% 
  data.frame(.) %>% 
  arrange(desc(Mean)) %>% 
  stargazer(summary = FALSE, digits = 2, digits.extra = 3,
            title = "% of Primary School Age Students (5-14) Enrolled in Private School, by State",
            type = "text", rownames = FALSE,
            header = FALSE, label = "tbl_desc_stats_state_priv_school_cz")


###############################################################
###############################################################
###############################################################

description_string_cz <- paste0(paste0(description_string_general, collapse = ""),
                                "Counties were assigned to commuting zones using the crosswalk available from the USDA ERS (https://www.ers.usda.gov/data-products/commuting-zones-and-labor-market-areas/).",
                                description_string_calculation,
                                  "This yields a complete panel dataset of ", 
                                  n_distinct(new_w_state$cz_id), 
                                  " commuting zones across ", 
                                  n_distinct(new_w_state$state), 
                                  " states.")
print(description_string_cz)


# #######################################
# if(acs == "acs1"){
#   test_strings <- c("priv_school_enrolment_temp_fips.RDS", "priv_school_enrolment_final_fips_long.RDS", "priv_school_enrolment_final_fips.RDS")
#   for(test in test_strings){
#     print(all.equal(arrange(readRDS(here(paste0('data/raw/priv_school_enrolment/', acs, "_", test))), fips, year),
#               arrange(readRDS(here(paste0('data/raw/priv_school_enrolment/', test))), fips, year)))
#   }
# }

###############################################################
#### Combine in one FIPS dataset
###############################################################

readRDS(here("data/raw/priv_school_enrolment/acs5_priv_school_enrolment_final_fips.RDS")) %>% 
  rename(pct_private_school_primary_acs5 = pct_private_school_primary) %>% 
  full_join(., select(readRDS(here("data/raw/priv_school_enrolment/acs1_priv_school_enrolment_final_fips.RDS")), fips, year, pct_private_school_primary), by = c('fips', 'year')) %>% 
  rename(pct_private_school_primary_acs1 = pct_private_school_primary)  %>%
  complete(fips, year) %>% 
  saveRDS(., here("data/raw/priv_school_enrolment/complete_priv_school_enrolment_fips.RDS"))

readRDS(here("data/raw/priv_school_enrolment/acs5_priv_school_enrolment_final_cz.RDS")) %>% 
  rename(pct_private_school_primary_acs5 = pct_private_school_primary) %>% 
  full_join(., select(readRDS(here("data/raw/priv_school_enrolment/acs1_priv_school_enrolment_final_cz.RDS")), cz_id, year, pct_private_school_primary), by = c('cz_id', 'year')) %>% 
  rename(pct_private_school_primary_acs1 = pct_private_school_primary)  %>%
  complete(cz_id, year) %>% 
  saveRDS(., here("data/raw/priv_school_enrolment/complete_priv_school_enrolment_cz.RDS"))

