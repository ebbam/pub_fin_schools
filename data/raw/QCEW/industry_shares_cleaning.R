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

df_natl_va <- readRDS(here("data/raw/bea_va/va_for_ss.rds")) %>% 
  select(year, log_real_VA, diff_log_real_VA, ind_code) %>% 
  mutate(ind_code = gsub("-", "_", ind_code)) %>% 
  rename(gr_real_VA = diff_log_real_VA) %>% 
  pivot_wider(id_cols = year, names_from = c(ind_code), values_from = c(log_real_VA, gr_real_VA)) 
  
# National rates
natl_rates_plot_short <- df_list_natl %>% 
  filter(!(industry_code %in% c("99", "999"))) %>% 
  group_by(year, industry_code, agglvl_code) %>%
  summarise(across(c(annual_avg_emplvl, total_annual_wages), ~sum(., na.rm = TRUE)),
            annual_avg_wkly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE)) %>%
  ungroup %>%
  rename_with(~str_c("natl_", .), annual_avg_emplvl:annual_avg_wkly_wage) %>% 
  arrange(industry_code, agglvl_code, year) %>% 
  # Creates first-difference or change and lag for employment and wage levels
  group_by(industry_code, agglvl_code) %>% 
  mutate(across(natl_annual_avg_emplvl:natl_annual_avg_wkly_wage, list(gr = ~log(.) - lag(log(.),1), log = ~log(.)), .names = "{.fn}_{.col}")) %>% 
  ungroup %>% 
  mutate(classification = case_when(agglvl_code %in% c("10", "11", "12", "94", "95") ~ "A. Overall", 
                                    agglvl_code == "14" ~ "B. Disaggregated (2-digit)",
                                    agglvl_code == "15" ~ "C. Disaggregated (3-digit)",
                                    TRUE ~ NA))

# # I need to fill in the industry codes that are "grouped" and therefore separate in df_list_natl
# "Manufacturing" = "31-33",
# "Retail Trade" = "44-45",
# "Transportation and Warehousing" = "48-49",
inds_missing <- df_list_natl %>% 
  mutate(industry_code = case_when(substr(industry_code, 1,1) == "3" ~ "31_33", 
          substr(industry_code, 1,2) %in% c("44", "45") ~ "44_45",
         substr(industry_code, 1,2) %in% c("48", "49") ~ "48_49",
  TRUE ~ NA)) %>% 
    filter(!is.na(industry_code)) %>% 
    # Exact same lines as above when creating natl_rates_plot
    group_by(year, industry_code, agglvl_code) %>%
    summarise(across(c(annual_avg_emplvl, total_annual_wages), ~sum(., na.rm = TRUE)),
              annual_avg_wkly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE)) %>%
    ungroup %>% 
    rename_with(~str_c("natl_", .), annual_avg_emplvl:annual_avg_wkly_wage) %>% 
    arrange(industry_code, agglvl_code, year) %>% 
    # Creates first-difference or change and lag for employment and wage levels
    group_by(industry_code, agglvl_code) %>% 
    mutate(across(natl_annual_avg_emplvl:natl_annual_avg_wkly_wage, list(gr = ~log(.) - lag(log(.),1), log = ~log(.)), .names = "{.fn}_{.col}")) %>% 
    ungroup %>% 
    mutate(agglvl_code ="14",
           classification = "B. Disaggregated (2-digit)")

identical(names(inds_missing), names(natl_rates_plot_short))

natl_rates_plot <- natl_rates_plot_short %>% 
  rbind(inds_missing)

natl_rates <- natl_rates_plot %>% 
  filter(!(agglvl_code %in% c(11, 12, 94, 95))) %>%
  pivot_wider(id_cols = year, names_from = c(industry_code), values_from = natl_annual_avg_emplvl:log_natl_annual_avg_wkly_wage) %>% 
  left_join(df_natl_va, ., by = "year")

p1 <- natl_rates_plot %>% 
  pivot_longer(!c(year, industry_code, agglvl_code, classification)) %>% 
  mutate(name = gsub("natl_annual_avg_emplvl", "Average Employment Level", gsub("natl_total_annual_wages", "Total Wages", gsub("natl_annual_avg_wkly_wage", "Avg. Weekly Wage", gsub("gr_", "Growth Rate ", name))))) %>% 
  filter(industry_code != 10) %>% 
  ggplot(aes(x = year, y = value, color = industry_code)) + 
  geom_point() + 
  facet_wrap(~name, scales = "free_y") +
  theme_minimal()+
  theme(legend.position = "none") + 
  labs(title = "National Wage and Employment (Levels & Growth Rates by Industry)") 

p2 <- natl_rates_plot %>% 
  select(-contains("emp")) %>% 
  pivot_longer(!c(year, industry_code, agglvl_code, classification)) %>% 
  mutate(name = gsub("natl_total_annual_wages", "Total Wages", gsub("natl_annual_avg_wkly_wage", "Avg. Weekly Wage", gsub("gr_", "Growth Rate ", name))),
         type = case_when(grepl("Growth Rate", name) ~ "Growth Rate",
                          TRUE ~ "Level")) %>% 
  filter(type == "Level") %>% 
  ggplot(aes(x = year, y = value, color= interaction(industry_code, agglvl_code))) +
  geom_line() + 
  facet_wrap(~  name + classification, scales = "free_y") +
  theme_minimal() +
  theme(legend.position = "none")

plot_data <- natl_rates_plot %>% 
  group_by(industry_code, agglvl_code, classification) %>% 
  summarise(across(gr_natl_annual_avg_wkly_wage, ~mean(., na.rm = TRUE))) %>% 
  pivot_longer(!c(industry_code, agglvl_code, classification)) %>% 
  mutate(label = paste(industry_code, agglvl_code, sep = ", ")) %>% 
  group_by(classification) %>%
  mutate(
    Q1 = quantile(value, 0.25, na.rm = TRUE),
    Q3 = quantile(value, 0.75, na.rm = TRUE),
    is_outlier = ((value < 0.9*Q1 | value > Q3*1.1) & !(classification == "C. Disaggregated (3-digit)")) | ((value < 0.75*Q1 | value > Q3*1.25) & classification == "C. Disaggregated (3-digit)"),
    label = case_when(name == "gr_natl_annual_avg_wkly_wage" ~ "Annual Avg Weekly Wage: Mean Growth Rate (2000-2021) by Industry",
                      TRUE ~ NA)
  ) %>%
  ungroup() %>% 
  left_join(., ind_codes, by = 'industry_code')
  
p3 <- plot_data %>% 
  filter(classification != "A. Overall") %>% 
  ggplot(., aes(x = classification, y = value, fill = classification)) +
  geom_boxplot(outlier.shape = NA) +  # suppress default outlier points
  geom_jitter(color = "black", size = 0.4, alpha = 0.9) +
  geom_text_repel(
    data = filter(plot_data, is_outlier),
    aes(label = industry_title),
    size = 2.5, max.overlaps = 30
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_grid(~label, scales = "free_y") +  # if needed, adjust `name` usage
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Industry",
       #fill = "Level of aggregation",
       x = "Level of Industrial Aggregation") + 
  theme_minimal() +
  theme(legend.position = "none") 

print(p1)
print(p2)
print(p3)
  
#saveRDS(natl_rates, here("data/temp/natl_rates.RDS"))

################################################################################
################################################################################
################# Shift Shares Overall Transformation Function #################
################################################################################
################################################################################

compute_shares <- function(source = NULL, base_year, industry_codes, unit_id = "fips", flat = FALSE){
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
    
  temp_long <- temp %>% 
    group_by(unit, year, industry_code, agglvl_code) %>% 
    summarise(across(c(annual_avg_emplvl), ~sum(., na.rm = TRUE))) %>%  # , total_annual_wages)
              #annual_avg_wkly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE)) %>% 
    ungroup 
  
  temp_short <- temp_long %>% 
    group_by(unit, year, industry_code) %>% 
    summarise(annual_avg_emplvl = max(annual_avg_emplvl, na.rm = TRUE)) %>%
    mutate(annual_avg_emplvl = ifelse(annual_avg_emplvl == -Inf, NA, annual_avg_emplvl)) %>% 
    ungroup 
    
  temp <- temp_short %>% 
    filter(!industry_code %in% c(99,999))
  
  rm(temp_test)
  
  print("Cleaned temp file.")
  
  shift_share <- temp %>% 
    # left_join(natl_rates, by = "year") %>% 
    pivot_wider(id_cols = c(unit, year), names_from = industry_code, values_from = c(annual_avg_emplvl), names_glue = "annual_avg_emplvl_{industry_code}") %>%
    # Calculates share of employment and wage per industry in each county 
    mutate(across(contains("avg_emplvl"), ~./annual_avg_emplvl_10, .names = "share_{.col}"))
          # across(contains("total_annual_wages"), ~./total_annual_wages_10, .names = "share_{.col}")) 
  
  # 2-digit coverage ratios
  coverage_2digit_naics <- shift_share %>% 
    select(year, unit, matches("^share_annual_avg_emplvl_\\d{2}(?:_\\d{2})*$")) %>% 
    rowwise() %>% mutate(coverage_2digit_naics = sum(c_across(!c(year, unit)), na.rm = TRUE) - 1) %>%
    ungroup() %>% 
    select(year, unit, coverage_2digit_naics)
  
  # 3-digit coverage ratios
  coverage_3digit_naics <- shift_share %>% 
    select(year, unit, matches("^share_annual_avg_emplvl_\\d{3}$")) %>% 
    rowwise() %>% mutate(coverage_3digit_naics = sum(c_across(!c(year, unit)), na.rm = TRUE)) %>%
    ungroup() %>% 
    select(year, unit, coverage_3digit_naics)
  
  shift_share_flat <- shift_share %>% 
    left_join(., coverage_2digit_naics, by = c("unit", "year")) %>% 
    left_join(., coverage_3digit_naics, by = c("unit", "year"))
  
  if(!flat){
    shift_share <- shift_share_flat %>% 
      complete(unit, year = 2001:2022) %>% 
      group_by(unit) %>%
      fill(everything(), .direction = "updown") %>% 
      ungroup
    
    print("Created employment share values.")
    
    assert_that(nrow(shift_share) == n_distinct(shift_share$unit) * n_distinct(shift_share$year))
    
    # Creates a shift-share measure with a baseline of 2001 (first time period in series), 2006 (local peak prior to global peak in national employment), 2011 (peak national employment)
    full <- shift_share %>% 
      select(unit, year, contains("share"), contains("coverage")) %>% 
      left_join(., natl_rates, by = "year", relationship = "many-to-one") %>% 
      rename(!!unit_id := unit) %>% 
      select(-ends_with("10"))
    
    print("Appended national shock variables.")
    return(full)
  }else{
    return(shift_share_flat)
    }
}


compute_ss <- function(dat, change = "gr"){
  
  # Get all share and shock column names
  share_cols <- names(dat)[str_starts(names(dat), "share_annual_avg_emplvl_")]
  shock_cols <- names(dat)[str_starts(names(dat), "gr_natl_annual_avg_wkly_wage_")]
  lev_shock_cols <- names(dat)[str_starts(names(dat), "log_natl_annual_avg_wkly_wage_")]
  gdp_shock_cols <- names(dat)[str_starts(names(dat), "gr_real_VA_")]
  lev_gdp_shock_cols <- names(dat)[str_starts(names(dat), "log_real_VA_")]
  
  
  # Extract suffixes
  share_suffixes <- str_remove(share_cols, "^share_annual_avg_emplvl_")
  shock_suffixes <- str_remove(shock_cols, "^gr_natl_annual_avg_wkly_wage_")
  lev_shock_suffixes <- str_remove(lev_shock_cols, "^log_natl_annual_avg_wkly_wage_")
  gdp_shock_suffixes <- str_remove(gdp_shock_cols, "^gr_real_VA_")
  lev_gdp_shock_suffixes <- str_remove(lev_gdp_shock_cols, "^log_real_VA_")
  
  # Find matched suffixes
  # Identical to lev_shock_suffixes so keep as is
  matched_suffixes <- intersect(share_suffixes, shock_suffixes)
  
  # Loop through matched suffixes to compute shift-share terms
  for (suffix in matched_suffixes) {
    share_var <- sym(paste0("share_annual_avg_emplvl_", suffix))
    shock_var <- sym(paste0("gr_natl_annual_avg_wkly_wage_", suffix))
    lev_shock_var <- sym(paste0("log_natl_annual_avg_wkly_wage_", suffix))
    ss_var <- sym(paste0("ss_", suffix))
    lev_ss_var <- sym(paste0("lev_ss_", suffix))
    
    dat <- dat %>%
      mutate(!!ss_var := !!share_var * !!shock_var,
             !!lev_ss_var := !!share_var * !!lev_shock_var)
  }
  
  # Identify all newly created ss_ columns
  ss_cols <- names(dat)[str_starts(names(dat), "ss_")]
  lev_ss_cols <- names(dat)[str_starts(names(dat), "lev_ss_")]
  
  # Separate by 2-digit and 3-digit suffixes
  ss_2digit <- ss_cols[str_detect(ss_cols, "^ss_\\d{2}(?:_\\d{2})*$")]
  ss_3digit <- ss_cols[str_detect(ss_cols, "^ss_\\d{3}$")]
  
  # Separate by 2-digit and 3-digit suffixes
  lev_ss_2digit <- lev_ss_cols[str_detect(lev_ss_cols, "^lev_ss_\\d{2}(?:_\\d{2})*$")]
  lev_ss_3digit <- lev_ss_cols[str_detect(lev_ss_cols, "^lev_ss_\\d{3}$")]
  
  # Add two new columns: row-wise sum for 2-digit and 3-digit suffixes
  dat <- dat %>%
    rowwise() %>%
    mutate(
      ss_2d = sum(c_across(all_of(ss_2digit)), na.rm = TRUE),
      ss_3d = sum(c_across(all_of(ss_3digit)), na.rm = TRUE),
      lev_ss_2d = sum(c_across(all_of(lev_ss_2digit)), na.rm = TRUE),
      lev_ss_3d = sum(c_across(all_of(lev_ss_3digit)), na.rm = TRUE)
    ) %>%
    ungroup()
  
  # Find matched suffixes for gdp
  # Identical to lev_shock_suffixes so keep as is
  matched_suffixes <- intersect(share_suffixes, gdp_shock_suffixes)
  identical(matched_suffixes, intersect(share_suffixes, lev_gdp_shock_suffixes))
  
  # Loop through matched suffixes to compute shift-share terms
  for (suffix in matched_suffixes) {
    share_var <- sym(paste0("share_annual_avg_emplvl_", suffix))
    shock_var <- sym(paste0("gr_real_VA_", suffix))
    lev_shock_var <- sym(paste0("log_real_VA_", suffix))
    ss_var <- sym(paste0("gdp_ss_", suffix))
    lev_ss_var <- sym(paste0("lev_gdp_ss_", suffix))
    
    dat <- dat %>%
      mutate(!!ss_var := !!share_var * !!shock_var,
             !!lev_ss_var := !!share_var * !!lev_shock_var)
  }
  # Identify all newly created ss_ columns
  ss_cols <- names(dat)[str_starts(names(dat), "gdp_ss_")]
  lev_ss_cols <- names(dat)[str_starts(names(dat), "lev_gdp_ss_")]
  
  # Separate by 2-digit and 3-digit suffixes
  ss_2digit <- ss_cols[str_detect(ss_cols, "^gdp_ss_\\d{2}(?:_\\d{2})*$")]
  # Separate by 2-digit and 3-digit suffixes
  lev_ss_2digit <- lev_ss_cols[str_detect(lev_ss_cols, "^lev_gdp_ss_\\d{2}(?:_\\d{2})*$")]
  
  # Add two new columns: row-wise sum for 2-digit and 3-digit suffixes
  dat <- dat %>%
    rowwise() %>%
    mutate(
      gdp_ss_2d = sum(c_across(all_of(ss_2digit)), na.rm = TRUE),
      lev_gdp_ss_2d = sum(c_across(all_of(lev_ss_2digit)), na.rm = TRUE)) %>%
    ungroup()
  return(dat)
}

# Create "Flat shares" meaning a specific emplevel for each year
if(new_flat){
  shares_flat <- tibble() 
  for(yr in 1995:2022){
    print(yr)
    shares_flat <- compute_shares(source = "QCEW", base_year = yr, unit_id = "fips", flat = TRUE) %>% 
      bind_rows(shares_flat)
    gc()
  }
  saveRDS(shares_flat, here("data/raw/QCEW/shares_flat.RDS"))
  
  # Fills missing 2-digit NAICS codes with lower-level values if available
  d2_cats <- shares_flat %>% 
    select(matches("^annual_avg_emplvl_\\d{2}$"), -annual_avg_emplvl_10) %>% 
    names
  
  # I've checked and 0 values are legitimate (ie. should not be NA) while NA values should remain NA values
  shares_flat2 <- shares_flat %>% 
    select(unit, year, annual_avg_emplvl_10)
  
  for(k in d2_cats){
    print(k)
    shares_flat2 <- shares_flat %>% 
      select(unit, year, starts_with(k)) %>% 
      rowwise() %>% mutate(emp_fill = sum(c_across(!c(unit, year, eval(k))), na.rm = TRUE)) %>%
      ungroup() %>% 
      mutate(!!k := ifelse(is.na(.data[[k]]) | emp_fill >= .data[[k]], emp_fill, .data[[k]])) %>%
      select(-emp_fill) %>% 
      left_join(shares_flat2, ., by = c("unit", "year"))
  }
  
  saveRDS(shares_flat2, here("data/raw/QCEW/shares_flat_2digit_filled.RDS"))
  
  shares_flat_filled <- shares_flat2 %>% 
    select(year, unit, matches("^annual_avg_emplvl_\\d{2}(?:_\\d{2})*$")) %>% 
    rowwise() %>% 
    mutate(annual_avg_emplvl_10_filled = sum(c_across(matches("^annual_avg_emplvl_\\d{2}(?:_\\d{2})*$") & !matches("annual_avg_emplvl_10")), na.rm = TRUE)) %>% 
    ungroup %>% 
    mutate(annual_avg_emplvl_10_filled = ifelse(annual_avg_emplvl_10_filled > annual_avg_emplvl_10, annual_avg_emplvl_10_filled, annual_avg_emplvl_10)) %>% 
    select(year, unit, annual_avg_emplvl_10_filled) %>% 
    left_join(shares_flat2, ., by = c("unit", "year")) %>% 
    mutate(across(contains("avg_emplvl"), ~./annual_avg_emplvl_10_filled, .names = "share_{.col}")) 
  
  saveRDS(shares_flat_filled, here("data/raw/QCEW/shares_flat_2digit_filled_corrected_total.RDS"))
  
  coverage <- shares_flat_filled %>% 
    select(year, unit, matches("^share_annual_avg_emplvl_\\d{2}(?:_\\d{2})*$")) %>% 
    rowwise() %>% mutate(coverage_2digit_naics = sum(c_across(!c(year, unit)), na.rm = TRUE) - 1) %>%
    ungroup() %>% 
    select(year, unit, coverage_2digit_naics)
  
  coverage <- shares_flat_filled %>% 
    select(year, unit, matches("^share_annual_avg_emplvl_\\d{3}$")) %>% 
    rowwise() %>% mutate(coverage_3digit_naics = sum(c_across(!c(year, unit)), na.rm = TRUE)) %>%
    ungroup() %>% 
    select(year, unit, coverage_3digit_naics) %>% 
    left_join(coverage, ., by = c("year", "unit"))
  
  saveRDS(coverage, here("data/raw/QCEW/NAICS_share_coverage.RDS"))
  
}else{
 shares_flat <- readRDS(here("data/raw/QCEW/shares_flat.RDS"))
 shares_flat_filled <- readRDS(here("data/raw/QCEW/shares_flat_2digit_filled_corrected_total.RDS"))
 coverage <- readRDS(here("data/raw/QCEW/NAICS_share_coverage.RDS"))
}

################################################################################
################################################################################
################# Shift Shares Transformation Function #########################
################################################################################
################################################################################

compute_ss_old <- function(source = NULL, base_year, industry_codes, unit_id = "fips"){
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
plot_ss_old <- function(dat, code, shock_var, unit_id, ind_codes = NULL){
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
  
  dat_plot <- dat %>% 
    ggplot() +
    geom_line(aes(x = year, y = !!ss_name, group = !!sym(unit_id)), color = colors[unit_id]) +
    theme(legend.position = "none") +
    labs(title = industry_name, subtitle = paste0("Unit: ", toupper(gsub("_id", "", unit_id))), x = "Year", y= "Industry-Specific Shift-Share Value") + 
    theme_minimal() + theme(legend.position = "none")
  
  print(dat_plot)
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
