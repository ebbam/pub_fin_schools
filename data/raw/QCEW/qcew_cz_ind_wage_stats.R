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
source(here("code/source_code/useful_functions.R"))
source(here("code/source_code/cz_cleaning.R"))

new_data = FALSE

if(new_data){
  total_df <- tibble()
  # 1998-2011 first go
  for(yr in 1998:2011){
    print(yr)
    base <- read.csv(here(paste0("data/raw/QCEW/", yr, ".annual.singlefile.csv"))) %>%
      tibble %>% 
      mutate(across(c('own_code', 'annual_avg_emplvl', 'annual_avg_wkly_wage', 'agglvl_code', 'size_code'), ~as.numeric(.)))
    
    print(unique(base$disclosure_code))
    # Check that variable format mutate above did not create NA values for our identifying codes
    stopifnot(base %>% summarise(across(c('own_code', 'agglvl_code', 'annual_avg_emplvl', 'annual_avg_wkly_wage', 'size_code'), ~sum(is.na(.)))) %>% rowSums(.) == 0)
    # CHECKING THAT DISCLOSURE_CODE CAN BE DESELECTED OR FILTERED OUT - WE OPT FOR FILTERING OUT
    stopifnot(base %>% filter(disclosure_code == "N") %>% select(disclosure_code, annual_avg_wkly_wage, annual_avg_emplvl) %>% distinct %>% nrow() <= 1)
    stopifnot(base %>% filter(disclosure_code == "N") %>% select(disclosure_code, annual_avg_wkly_wage, annual_avg_emplvl) %>% distinct %>% select(-disclosure_code) %>% rowSums(na.rm = TRUE) == 0)
    
    # select (area_fips, own_code, industry_code, agglvl_code, size_code, year, qtr, disclosure_code, annual_avg_wkly_wage)
    base_clean <- base %>% 
      filter(disclosure_code != "N" | is.na(disclosure_code)) %>% 
      select(area_fips, own_code, industry_code, agglvl_code, size_code, year, qtr, disclosure_code, annual_avg_wkly_wage, annual_avg_emplvl) %>% 
      # assert that n_distinct of size_code, year, qtr all == 1 
      select(where(function(x) n_distinct(x) > 1), year) %>% 
      filter(industry_code != 10) %>% 
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
      # Removes 73 because it refers to super sectors, not our relevant sectors : https://www.bls.gov/cew/classifications/aggregation/agg-level-titles.htm
      filter(agglvl_code %in% c(74, 75, 76, 77, 78)) %>% 
      # We keep 71 for now because in a few rare cases, agglvl_code ' 70 is not reported and we need to replace with the sum of 71's values 
      # even though "70" represents the total covered: https://www.bls.gov/cew/classifications/ownerships/ownership-titles.htm
      mutate(industry_code = gsub("-", "_", industry_code)) %>% 
      mutate(naics_group = ifelse(nchar(industry_code) == 2 | grepl("_", industry_code), TRUE, FALSE),
             naics_group_code = ifelse(naics_group, industry_code, substr(industry_code, 1, 2)),
             naics_group_code = case_when(naics_group_code %in% c(48,49) ~ "48_49",
                                          naics_group_code %in% c(31, 32, 33) ~ "31_33",
                                          naics_group_code %in% c(44, 45) ~ "44_45",
                                          TRUE ~ naics_group_code)) %>%
      filter(annual_avg_wkly_wage != 0)
    
    stopifnot(base_clean %>% mutate(naics_group = ifelse(nchar(industry_code) == 2 | grepl("_", industry_code), TRUE, FALSE)) %>% identical(mutate(base_clean, naics_group = ifelse(agglvl_code == 74, TRUE, FALSE))))
    
    # sector-decomposed data does not exist with an associated ownership code of 0. so we always need to deal with different ownership levels. 
    # We reconcile this in two ways. 
    # First, we take the max reported wage. 
    # Second, we take the weighted mean by employment level if an employment level is reported. 
    
    # ACTUALLY, CODE 74 IS WHAT WE WANT BECAUSE IT REPRESENTS BY SECTOR
    # Here we select those observations that have a well-defined value at agglvl_code = 74
    easy <- base_clean %>% 
      group_by(fips, naics_group_code) %>% 
      filter(any(agglvl_code == 74)) %>% 
      ungroup 
    
    easy_final <- easy %>% 
      filter(agglvl_code == 74) %>% 
      group_by(fips, industry_code) %>% 
      summarise(annual_avg_wkly_wage_max = max(annual_avg_wkly_wage),
                annual_avg_wkly_wage_mean = mean(annual_avg_wkly_wage, na.rm = TRUE),
                annual_avg_wkly_wage_wtd_mean = weighted.mean(annual_avg_wkly_wage, annual_avg_emplvl, na.rm = TRUE)) %>% 
      mutate(annual_avg_wkly_wage_wtd_mean = ifelse(is.nan(annual_avg_wkly_wage_wtd_mean), annual_avg_wkly_wage_mean, annual_avg_wkly_wage_wtd_mean)) %>% ungroup
    
    
    # Assert that all industry_code-fips pairs are unique
    stopifnot(easy_final %>% group_by(industry_code, fips) %>% n_groups == nrow(easy_final))
    stopifnot(easy_final %>% group_by(industry_code, fips) %>% n_groups == nrow(easy_final))
    stopifnot(easy_final %>% summarise(across(everything(), ~sum(is.nan(.)))) %>% rowSums(.) == 0)
    
    to_fix <- base_clean %>% 
      group_by(fips, naics_group_code) %>% 
      filter(all(agglvl_code != 74)) %>% 
      ungroup 
    
    stopifnot(nrow(easy) + nrow(to_fix) == nrow(base_clean))
    
    to_fix_final <- to_fix %>%
      # check that all levels exist? they dont...sometimes they contain 4D but not 3D
      group_by(naics_group_code, fips) %>% 
      # Therefore, we take the value reported at the lowest-level of NAICS code
      filter(agglvl_code == min(agglvl_code)) %>% 
      summarise(annual_avg_wkly_wage_max = max(annual_avg_wkly_wage),
                annual_avg_wkly_wage_mean = mean(annual_avg_wkly_wage, na.rm = TRUE),
                annual_avg_wkly_wage_wtd_mean = weighted.mean(annual_avg_wkly_wage, annual_avg_emplvl, na.rm = TRUE)) %>% 
      # This handles the case in which 0 employment is reported in which case we simply take the mean
      mutate(annual_avg_wkly_wage_wtd_mean = ifelse(is.nan(annual_avg_wkly_wage_wtd_mean), annual_avg_wkly_wage_mean, annual_avg_wkly_wage_wtd_mean)) %>% 
      ungroup %>% 
      rename(industry_code = naics_group_code) %>% 
      relocate(fips, industry_code)
    
    stopifnot(to_fix_final %>% group_by(industry_code, fips) %>% n_groups == nrow(to_fix_final))
    stopifnot(to_fix_final %>% group_by(industry_code, fips) %>% n_groups == nrow(to_fix_final))
    stopifnot(to_fix_final %>% summarise(across(everything(), ~sum(is.nan(.)))) %>% rowSums(.) == 0)
    
    # Assert that all variable names are the same
    stopifnot(identical(names(easy_final), names(to_fix_final)))
    # Assert that no groups in easy_final are in to_fix_final
    stopifnot(nrow(anti_join(to_fix_final, easy_final, by = c("fips", "industry_code"))) == nrow(to_fix_final))
    stopifnot(nrow(anti_join(easy_final, to_fix_final, by = c("fips", "industry_code"))) == nrow(easy_final))
    
    final <- rbind(easy_final, to_fix_final) %>% 
      mutate(year = yr)
    
    total_df <- rbind(final, total_df)
    
    rm(base)
    rm(base_clean)
    rm(easy_final)
    rm(easy)
    rm(to_fix_final)
    rm(to_fix)
    gc()
  }
  
  # Saving 1998-2011 as exceeded local memory limits
  # saveRDS(total_df, here('data/raw/QCEW/cz_wage_stats_ts_fips_1998_2011.RDS'))
  # Saving 2012-2022 as exceeded local memory limits
  # saveRDS(total_df, here('data/raw/QCEW/cz_wage_stats_ts_fips_2012_2022.RDS'))
  
}else{

}

test <- rbind(readRDS(here('data/raw/QCEW/cz_wage_stats_ts_fips_1998_2011.RDS')), 
              readRDS(here('data/raw/QCEW/cz_wage_stats_ts_fips_2012_2022.RDS'))) %>% 
  filter(industry_code != 99) %>% 
  pivot_longer(cols = !c(fips, industry_code, year), values_to = "annual_avg_wkly_wage_measure", names_to = "metric") %>% 
  pivot_wider(id_cols = c(fips, year, metric), values_from = annual_avg_wkly_wage_measure, names_from = industry_code, names_glue = "{.value}_{.name}") %>% 
  complete(year, fips, metric)


for(metric_val in unique(test$metric)){
  
  t1_full <- test %>% 
    group_by(fips, metric) %>% 
    arrange(metric, fips, year) %>% 
    mutate(across(contains('annual_avg_wkly_wage'), ~na.approx(., na.rm = FALSE, maxgap = 5))) %>% 
    ungroup 
    
  t1 <- t1_full %>% filter(metric == metric_val)
  
  t2 <- test %>% 
    filter(metric == metric_val) %>% 
    group_by(fips) %>% 
    arrange(metric, fips, year) %>% 
    mutate(across(contains('annual_avg_wkly_wage'), ~na.approx(., na.rm = FALSE, maxgap = 5))) %>% 
    ungroup
  
  stopifnot(identical(t1, t2))
}

rm(t1)
rm(t2)

t1_full %>% 
  filter(metric == "annual_avg_wkly_wage_wtd_mean") %>% 
  select(-metric) %>% 
  arrange(fips, year) %>% 
  pivot_longer(cols = !c(fips, year)) %>% 
  ggplot(aes(x = year, y = value, color = fips)) + 
  geom_point() + 
  facet_wrap(~name) + 
  theme(legend.position = "none") 

library(tidyverse)
library(zoo)

# Prepare data
df_long <- test %>% 
  filter(metric == "annual_avg_wkly_wage_wtd_mean" & year > 2000) %>% 
  select(-metric) %>% 
  arrange(fips, year) %>% 
  pivot_longer(cols = !c(fips, year), names_to = "industry", values_to = "wage")

# Create interpolated version
df_interpolated <- t1_full %>%
  filter(metric == "annual_avg_wkly_wage_wtd_mean" & year > 2000) %>% 
  select(-metric) %>% 
  arrange(fips, year) %>% 
  pivot_longer(cols = !c(fips, year), names_to = "industry", values_to = "wage_interpolated")

# 1. Summary Statistics
missingness_summary <- bind_rows(
  df_long %>%
    summarise(
      Version = "Raw Data",
      `Total Obs` = n(),
      `Missing` = sum(is.na(wage)),
      `% Missing` = round(100 * Missing / `Total Obs`, 2)
    ),
  df_interpolated %>%
    summarise(
      Version = "After Interpolation",
      `Total Obs` = n(),
      `Missing` = sum(is.na(wage_interpolated)),
      `% Missing` = round(100 * Missing / `Total Obs`, 2)
    )
)

print(missingness_summary)

# 2. Time Series Comparison Plot
missingness_comparison <- bind_rows(
  df_long %>%
    group_by(year) %>%
    summarise(pct_missing = 100 * sum(is.na(wage)) / n()) %>%
    mutate(version = "Raw Data"),
  
  df_interpolated %>%
    group_by(year) %>%
    summarise(pct_missing = 100 * sum(is.na(wage_interpolated)) / n()) %>%
    mutate(version = "After Interpolation")
)

p1 <- ggplot(missingness_comparison, aes(x = year, y = pct_missing, color = version)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(
    title = "Missing Wage Data Over Time",
    subtitle = "Comparison of raw data vs. after linear interpolation",
    x = "Year",
    y = "Percent Missing (%)",
    color = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_color_manual(values = c("Raw Data" = "#E74C3C", 
                                "After Interpolation" = "#3498DB"))

print(p1)

# 3. Bar Chart - Before/After Comparison
comparison_data <- tibble(
  Category = c("Raw Data", "After Interpolation"),
  Missing = c(
    sum(is.na(df_long$wage)),
    sum(is.na(df_interpolated$wage_interpolated))
  ),
  Total = nrow(df_long)
) %>%
  mutate(
    Present = Total - Missing,
    pct_missing = 100 * Missing / Total,
    pct_present = 100 * Present / Total
  )

p2 <- comparison_data %>%
  select(Category, Missing, Present) %>%
  pivot_longer(cols = c(Missing, Present), names_to = "Status", values_to = "Count") %>%
  ggplot(aes(x = Category, y = Count, fill = Status)) +
  geom_col(position = "stack") +
  geom_text(aes(label = scales::comma(Count)), 
            position = position_stack(vjust = 0.5),
            color = "white", fontface = "bold", size = 5) +
  labs(
    title = "Impact of Linear Interpolation on Data Completeness",
    y = "Number of Observations",
    x = ""
  ) +
  scale_fill_manual(values = c("Missing" = "#E74C3C", "Present" = "#2ECC71")) +
  theme_minimal() +
  theme(legend.position = "bottom")

print(p2)

# 4. Missingness by Industry
missingness_by_industry <- bind_rows(
  df_long %>%
    group_by(industry) %>%
    summarise(pct_missing = 100 * sum(is.na(wage)) / n()) %>%
    mutate(version = "Raw Data"),
  
  df_interpolated %>%
    group_by(industry) %>%
    summarise(pct_missing = 100 * sum(is.na(wage_interpolated)) / n()) %>%
    mutate(version = "After Interpolation")
)

p3 <- ggplot(missingness_by_industry, aes(x = reorder(industry, pct_missing), 
                                          y = pct_missing, fill = version)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(
    title = "Missing Data by Industry",
    x = "Industry Code",
    y = "Percent Missing (%)",
    fill = ""
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("Raw Data" = "#E74C3C", 
                               "After Interpolation" = "#3498DB"))

print(p3)

# 5. Faceted plot by industry over time (raw data only)
p4 <- df_long %>%
  group_by(year, industry) %>%
  summarise(pct_missing = 100 * sum(is.na(wage)) / n(), .groups = "drop") %>%
  ggplot(aes(x = year, y = pct_missing)) +
  geom_line() +
  geom_point() +
  facet_wrap(~industry) +
  labs(
    title = "Missing Wage Data by Industry Over Time",
    x = "Year",
    y = "Percent Missing (%)"
  ) +
  theme_minimal() +
  theme(strip.text = element_text(size = 8))

print(p4)

# 6. Save interpolated data back to wide format if needed
t1_full_interpolated <- df_interpolated %>%
  select(fips, year, industry, wage_interpolated) %>%
  pivot_wider(names_from = industry, values_from = wage_interpolated) %>%
  mutate(metric = "annual_avg_wkly_wage_wtd_mean") %>%
  relocate(metric, .before = everything())


# CZs
unit_id = "cz_id"
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
    setdiff(unique(t1_full$fips), .) 
  
  if (length(missing_fips) > 0) {
    message("Warning: Some FIPS codes are missing from czs.")
    print(missing_fips)
  }
  
  temp <- t1_full %>% 
    left_join(., czs_new, by = "fips", multiple = "first") %>% 
    rename("unit" = cz_id) #%>% 
    #select(-fips)
  
}else if(unit_id == "fips"){
  temp <- temp %>% 
    rename(unit = fips)
}

mines_cz_for_lf <- readRDS(here("data/out/regression_data_complete_fips.RDS")) %>% select(fips, year, pop_total)

# All fips in mines_cz are in temp
setdiff(mines_cz_for_lf$fips, temp$fips)

temp_pop <- temp %>% filter(year %in% mines_cz_for_lf$year & fips %in% mines_cz_for_lf$fips) %>% left_join(mines_cz_for_lf, by = c('fips', 'year'))
stopifnot(temp_pop %>% filter(is.na(pop_total)) %>% pull(fips) %>% n_distinct == 0)
stopifnot(temp_pop %>% group_by(fips, year, metric) %>% n_groups == nrow(temp_pop))

temp_wages_czs <- temp_pop %>% 
  group_by(unit, year, metric) %>% 
  summarise(across(contains('annual_avg_wkly_wage'), ~weighted.mean(., pop_total, na.rm = TRUE))) %>% 
  ungroup

grouped_temp <- temp_wages_czs %>% group_split(metric) 
if(new_data){
  for(el in grouped_temp){
    print(el)
    title <- el %>% pull(metric) %>% unique %>% gsub("annual_avg_wkly_wage_","",.) %>% unlist
    print(title)
   saveRDS(el, here(paste0("data/raw/QCEW/cz_wage_stats_ts_", title ,".RDS")))
  }
}


