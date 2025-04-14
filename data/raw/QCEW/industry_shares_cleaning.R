library(here)
library(tidyverse)
library(readxl)
library(usmap)
library(conflicted)
library(zoo)
library(patchwork)
library(tidyquant)
conflict_prefer_all("dplyr", quiet = TRUE)
source(here("code/source_code/dicts.R"))
source(here("code/source_code/useful_functions.R"))

cz = TRUE

# Source: https://www.bls.gov/cew/downloadable-data-files.htm.  - CSVs Single Files: Annual Averages

# Industry codes provided by QCEW BLS: https://www.bls.gov/cew/classifications/industry/industry-titles.htm
ind_codes <- read_excel(here("data/raw/QCEW/industry-titles.xlsx"))

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
# temp_test <- readRDS(here("data/temp/qcew_compiled_raw_ff.RDS"))
df_list_natl <- readRDS(here('data/raw/QCEW/natl_all_industries_99_08.RDS')) %>%
  rbind(readRDS(here("data/raw/QCEW/natl_all_industries_09_22.RDS")))

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


################################################################################
################################################################################
################### National ###################################################
################################################################################
################################################################################
# National rates
natl_rates_plot <- df_list_natl %>% 
  group_by(year, industry_code) %>%
  summarise(across(c(annual_avg_emplvl, total_annual_wages), ~sum(., na.rm = TRUE)),
            annual_avg_wkly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE)) %>%
  ungroup %>%
  rename_with(~str_c("natl_", .), annual_avg_emplvl:annual_avg_wkly_wage) %>% 
  arrange(industry_code, year) %>% 
  # Creates first-difference or change and lag for employment and wage levels
  group_by(industry_code) %>% 
  mutate(across(natl_annual_avg_emplvl:natl_annual_avg_wkly_wage, ~log(.) - lag(log(.),1), .names = "gr_{.col}")) %>% 
  ungroup

natl_rates <- natl_rates_plot %>% 
  pivot_wider(id_cols = c(year), names_from = industry_code, values_from = natl_annual_avg_emplvl:gr_natl_annual_avg_wkly_wage)

natl_rates_plot %>% 
  ggplot(aes(x = year, y = gr_natl_annual_avg_wkly_wage, color = industry_code)) + 
  geom_point() + 
  #geom_ma(ma_fun = SMA, n = 5) +
  theme(legend.position = "none")
#saveRDS(natl_rates, here("data/temp/natl_rates.RDS"))

# Import base year for Bartik instrument - 2001
temp_test <- read.csv(here(paste0("data/raw/QCEW/2001.annual.singlefile.csv"))) %>%
  tibble

temp <- temp_test %>%
  filter(nchar(industry_code) <= 3) %>% 
  filter(substr(area_fips, 3,5) != "000" & substr(area_fips, 3,5) != "999" & !grepl("US", area_fips)) %>%
  mutate(fips_state = substr(area_fips, 1,2)) %>%
  rename(fips = area_fips) %>%
  filter(!(fips_state %in% c("72","78", "C1", "C2", "C3", "C4", "CS"))) %>%
  mutate(fips = ifelse(fips %in% names(getfips), unname(getfips[fips]), fips)) %>%
  filter(!(fips %in% c("51560", "51515")) & !(fips == "46113" & year <= 2015)) %>%
  mutate(fips = ifelse(fips == "46113", "46102", fips)) %>% 
  select(-fips_state) %>% 
  filter(agglvl_code %in% c(70, 74, 76, 75) & disclosure_code != "N") %>% 
  group_by(fips, year, industry_code) %>% 
  summarise(across(c(annual_avg_emplvl, total_annual_wages), ~sum(., na.rm = TRUE)),
            annual_avg_wkly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE)) %>% 
  ungroup 

shift_share_fips <- temp %>% 
  left_join(natl_rates, by = "year") %>% 
  pivot_wider(id_cols = c(fips, year), names_from = industry_code, values_from = c(annual_avg_emplvl, total_annual_wages)) %>%
  group_by(fips) %>% 
  mutate(across(where(is.numeric), ~ na.approx(., maxgap = 3, na.rm = FALSE))) %>% #filter(!all(is.na(annual_avg_emplvl_2121))) %>% ggplot(aes(x = year, y = annual_avg_emplvl_2121, color = fips)) + geom_line() + facet_wrap(~fips_state)
  ungroup %>%
  # Calculates share of employment and wage per industry in each county 
  mutate(across(contains("avg_emplvl"), ~./annual_avg_emplvl_10, .names = "share_{.col}"),
         across(contains("total_annual_wages"), ~./total_annual_wages_10, .names = "share_{.col}")) %>% 
  complete(fips, year = 2001:2022) %>% 
  group_by(fips) %>%
  fill(everything(), .direction = "down") %>% 
  ungroup

# Creates a shift-share measure with a baseline of 2001 (first time period in series), 2006 (local peak prior to global peak in national FF employment), 2011 (peak national FF employment)
full_fips <- shift_share_fips %>% 
  left_join(., natl_rates, by = "year", relationship = "many-to-one")


temp_plot_fips <- full_fips %>% 
  select(year, fips, share_annual_avg_emplvl_51, gr_natl_annual_avg_wkly_wage_51,
         share_annual_avg_emplvl_54, gr_natl_annual_avg_wkly_wage_54,
         share_annual_avg_emplvl_11, gr_natl_annual_avg_wkly_wage_11,
         share_annual_avg_emplvl_21, gr_natl_annual_avg_wkly_wage_21,
         share_annual_avg_emplvl_23, gr_natl_annual_avg_wkly_wage_23,
         share_annual_avg_emplvl_321, gr_natl_annual_avg_wkly_wage_321) %>% 
  mutate(ss_51 = share_annual_avg_emplvl_51*gr_natl_annual_avg_wkly_wage_51, 
         ss_11 = share_annual_avg_emplvl_11*gr_natl_annual_avg_wkly_wage_11,
         ss_54 = share_annual_avg_emplvl_54*gr_natl_annual_avg_wkly_wage_54,
         ss_21 = share_annual_avg_emplvl_21*gr_natl_annual_avg_wkly_wage_21,
         ss_23 = share_annual_avg_emplvl_23*gr_natl_annual_avg_wkly_wage_23,
         ss_321 = share_annual_avg_emplvl_321*gr_natl_annual_avg_wkly_wage_321)

p1 <- temp_plot_fips %>% 
  ggplot() +
  geom_line(aes(x = year, y = ss_51, color = fips))+
  theme(legend.position = "none") +
  ylim(-0.002, 0.018) + 
  labs(title = "Information Sector Wage Growth")

p2 <- temp_plot_fips %>% 
  ggplot() +
  geom_line(aes(x = year, y = ss_54, color = fips))+
  theme(legend.position = "none") +
  ylim(-0.002, 0.018) + 
  labs(title = "Prof, Sci, Tech Services Sector Wage Growth")

p3 <- temp_plot_fips %>% 
  ggplot() +
  geom_line(aes(x = year, y = ss_321, color = fips))+
  theme(legend.position = "none") +
  ylim(-0.002, 0.018) + 
  labs(title = "Manufacturing Services Sector Wage Growth")

p4 <- temp_plot_fips %>% 
  ggplot() +
  geom_line(aes(x = year, y = ss_11, color = fips))+
  theme(legend.position = "none") +
  ylim(-0.002, 0.018) + 
  labs(title = "Ag, Forest, Fishing, Hunting Sector Wage Growth")

p5 <- temp_plot_fips %>% 
  ggplot() +
  geom_line(aes(x = year, y = ss_21, color = fips))+
  theme(legend.position = "none") +
  ylim(-0.002, 0.018) + 
  labs(title = "Mining & Extraction Sector Wage Growth")

p6<- temp_plot_fips %>% 
  ggplot() +
  geom_line(aes(x = year, y = ss_23, color = fips))+
  theme(legend.position = "none") +
  ylim(-0.002, 0.018) + 
  labs(title = "Construction Sector Wage Growth")


(p1 + p2 + p3) / (p4 + p5 + p6)


rm(temp_test)

################################################################################
################################################################################
################### CZs ########################################################
################################################################################
################################################################################
source(here("code/source_code/cz_cleaning.R"))
if(cz){
  czs <- czs %>% 
    rename(old_fips = fips) %>% 
    mutate(fips = case_when(!is.na(getfips[old_fips]) ~ getfips[old_fips],
                            TRUE ~ old_fips),
           cz_id = as.character(cz_id))
  
  czs %>% 
    pull(fips) %>% 
    unique %>% 
    setdiff(unique(temp$fips), .)
  
  temp_cz <- temp %>% 
    left_join(., czs, by = "fips", multiple = "first") %>% 
    filter(agglvl_code %in% c(70, 74, 76, 75) & disclosure_code != "N") %>% 
    group_by(cz_id, year, industry_code) %>% 
    # summarise(across(contains("total"), ~sum(., na.rm = TRUE)),
    #           across(contains("avg"), ~mean(., na.rm = TRUE)))
    summarise(across(c(annual_avg_emplvl, total_annual_wages), ~sum(., na.rm = TRUE)),
              annual_avg_wkly_wage = mean(annual_avg_wkly_wage, na.rm = TRUE)) %>% 
    ungroup
  
  
  shift_share_cz <- temp_cz %>% 
    pivot_wider(id_cols = c(cz_id, year), names_from = industry_code, values_from = c(annual_avg_emplvl, total_annual_wages)) %>%
    group_by(cz_id) %>% 
    mutate(across(where(is.numeric), ~ na.approx(., maxgap = 3, na.rm = FALSE))) %>% #filter(!all(is.na(annual_avg_emplvl_2121))) %>% ggplot(aes(x = year, y = annual_avg_emplvl_2121, color = fips)) + geom_line() + facet_wrap(~fips_state)
    ungroup %>%
    # Calculates share of employment and wage per industry in each county 
    mutate(across(contains("avg_emplvl"), ~./annual_avg_emplvl_10, .names = "share_{.col}"),
           across(contains("total_annual_wages"), ~./total_annual_wages_10, .names = "share_{.col}")) %>% 
    complete(cz_id, year = 2001:2022) %>% 
    group_by(cz_id) %>%
    fill(everything(), .direction = "down") %>% 
    ungroup
    
  
  # Creates a shift-share measure with a baseline of 2001 (first time period in series), 2006 (local peak prior to global peak in national FF employment), 2011 (peak national FF employment)
  full_cz <- shift_share_cz %>% 
    left_join(., natl_rates, by = "year", relationship = "many-to-one")
  
  
  temp_plot <- full_cz %>% 
    select(year, cz_id, share_annual_avg_emplvl_51, gr_natl_annual_avg_wkly_wage_51,
           share_annual_avg_emplvl_54, gr_natl_annual_avg_wkly_wage_54,
           share_annual_avg_emplvl_11, gr_natl_annual_avg_wkly_wage_11,
           share_annual_avg_emplvl_21, gr_natl_annual_avg_wkly_wage_21,
           share_annual_avg_emplvl_23, gr_natl_annual_avg_wkly_wage_23,
           share_annual_avg_emplvl_321, gr_natl_annual_avg_wkly_wage_321) %>% 
    mutate(ss_51 = share_annual_avg_emplvl_51*gr_natl_annual_avg_wkly_wage_51, 
           ss_11 = share_annual_avg_emplvl_11*gr_natl_annual_avg_wkly_wage_11,
           ss_54 = share_annual_avg_emplvl_54*gr_natl_annual_avg_wkly_wage_54,
           ss_21 = share_annual_avg_emplvl_21*gr_natl_annual_avg_wkly_wage_21,
           ss_23 = share_annual_avg_emplvl_23*gr_natl_annual_avg_wkly_wage_23,
           ss_321 = share_annual_avg_emplvl_321*gr_natl_annual_avg_wkly_wage_321)
    
    p1 <- temp_plot %>% 
      ggplot() +
      geom_line(aes(x = year, y = ss_51, color = cz_id))+
      theme(legend.position = "none") +
      ylim(-0.002, 0.018) + 
      labs(title = "Information Sector Wage Growth")
    
    p2 <- temp_plot %>% 
      ggplot() +
      geom_line(aes(x = year, y = ss_54, color = cz_id))+
      theme(legend.position = "none") +
      ylim(-0.002, 0.018) + 
      labs(title = "Prof, Sci, Tech Services Sector Wage Growth")
    
    p3 <- temp_plot %>% 
      ggplot() +
      geom_line(aes(x = year, y = ss_321, color = cz_id))+
      theme(legend.position = "none") +
      ylim(-0.002, 0.018) + 
      labs(title = "Manufacturing Services Sector Wage Growth")
    
    p4 <- temp_plot %>% 
      ggplot() +
      geom_line(aes(x = year, y = ss_11, color = cz_id))+
      theme(legend.position = "none") +
      ylim(-0.002, 0.018) + 
      labs(title = "Ag, Forest, Fishing, Hunting Sector Wage Growth")
  
    p5 <- temp_plot %>% 
      ggplot() +
      geom_line(aes(x = year, y = ss_21, color = cz_id))+
      theme(legend.position = "none") +
      ylim(-0.002, 0.018) + 
      labs(title = "Mining & Extraction Sector Wage Growth")
    
    p6<- temp_plot %>% 
      ggplot() +
      geom_line(aes(x = year, y = ss_23, color = cz_id))+
      theme(legend.position = "none") +
      ylim(-0.002, 0.018) + 
      labs(title = "Construction Sector Wage Growth")
    
  
    (p1 + p2 + p3) / (p4 + p5 + p6)
  
  
  for(years in c(2001, 2005, 2011)){
    print(years)
    full_cz <- full_cz %>%
      group_by(cz_id) %>%
      mutate(
        #share_emp_coal = annual_avg_emplvl_2121/annual_avg_emplvl_10,
             share_emp_extraction = annual_avg_emplvl_21/annual_avg_emplvl_10,
             #share_emp_ff = (annual_avg_emplvl_2121 + annual_avg_emplvl_211)/annual_avg_emplvl_10,
             share_emp_oil_gas =  annual_avg_emplvl_211/annual_avg_emplvl_10,
             #share_wage_coal = total_annual_wages_2121/total_annual_wages_10,
             share_wage_extraction = total_annual_wages_21/total_annual_wages_10,
             #share_wage_ff = (total_annual_wages_2121 + total_annual_wages_211)/total_annual_wages_10,
             share_wage_oil_gas =  total_annual_wages_211/total_annual_wages_10) %>% 
      mutate(across(c(natl_annual_avg_emplvl_21, natl_annual_avg_emplvl_211, natl_total_annual_wages_21, natl_total_annual_wages_211),
                    .fns = list(l1 = ~lag(.x), fd = ~.x - lag(.x)), .names = "{.fn}_{.col}"),
        #"ss_emp_coal_{years}" := share_emp_coal[year == years]*fd_natl_annual_avg_emplvl_2121,
             "ss_emp_extraction_{years}" := share_emp_extraction[year == years]*fd_natl_annual_avg_emplvl_21,
             #"ss_emp_ff_{years}" :=  share_emp_ff[year == years]*(fd_natl_annual_avg_emplvl_2121 + fd_natl_annual_avg_emplvl_211),
             "ss_emp_oil_gas_{years}" :=  share_emp_oil_gas[year == years]*fd_natl_annual_avg_emplvl_211,
           #  "ss_wage_coal_{years}" := share_wage_coal[year == years]*fd_natl_total_annual_wages_2121,
             "ss_wage_extraction_{years}"  := share_wage_extraction[year == years]*fd_natl_total_annual_wages_21,
             #"ss_wage_ff_{years}" :=  share_wage_ff[year == years]*(fd_natl_total_annual_wages_2121 + fd_natl_total_annual_wages_211),
             "ss_wage_oil_gas_{years}" :=  share_wage_oil_gas[year == years]*fd_natl_total_annual_wages_211) %>%
      ungroup %>% select(cz_id, year, contains(as.character(years))) %>% left_join(full_cz, ., by = c("cz_id", "year"))
  }
  
  # full_cz %>% select(cz_id, year, contains("ss_emp_coal")) %>% 
  #   group_by(cz_id) %>% filter(!any(is.na(ss_emp_coal_2001)) | !any(is.na(ss_emp_coal_2005)) | !any(is.na(ss_emp_coal_2011))) %>% 
  #   pivot_longer(cols = !c(cz_id, year), values_to = "ss", names_to = "base_year") %>% 
  #   filter(base_year == "ss_emp_coal_2001") %>% 
  #   ggplot(aes(x = year, y = ss, color = base_year)) + geom_line() + facet_wrap(~cz_id, scales = "free")
  
  
  # full_cz %>% select(cz_id, year, contains("ss_emp_coal")) %>% 
  #   group_by(cz_id) %>% filter(!any(is.na(ss_emp_coal_2001)) | !any(is.na(ss_emp_coal_2005)) | !any(is.na(ss_emp_coal_2011))) %>% 
  #   ungroup %>% summarise(across(ss_emp_coal_2001:ss_emp_coal_2011, ~sum(is.na(.))))
  
    full_cz %>% select(cz_id, year, contains("ss_emp_extraction")) %>% 
      group_by(cz_id) %>% 
      filter(any(!is.na(ss_emp_extraction_2001)) | any(!is.na(ss_emp_extraction_2005)) | any(!is.na(ss_emp_extraction_2011))) %>% 
      pivot_longer(cols = !c(cz_id, year), values_to = "ss", names_to = "base_year") %>%
      ggplot(aes(x = year, y = ss, group = cz_id)) + geom_line(colour = "pink") + facet_wrap(~base_year) + 
      theme(legend.position = "none")+
      theme_minimal()
    
    full_cz %>% select(cz_id, year, contains("ss_emp_extraction")) %>% 
      group_by(cz_id) %>% 
      filter(any(!is.na(ss_emp_extraction_2001)) | any(!is.na(ss_emp_extraction_2005)) | any(!is.na(ss_emp_extraction_2011))) %>% 
      ungroup %>%
      summarise(across(ss_emp_extraction_2001:ss_emp_extraction_2011, ~sum(is.na(.))/nrow(full_cz)))
    
  full_cz %>% select(cz_id, year, contains("ss_emp_oil_gas")) %>% 
    group_by(cz_id) %>% 
    filter(any(!is.na(ss_emp_oil_gas_2001)) | any(!is.na(ss_emp_oil_gas_2005)) | any(!is.na(ss_emp_oil_gas_2011))) %>% 
    pivot_longer(cols = !c(cz_id, year), values_to = "ss", names_to = "base_year") %>%
    ggplot(aes(x = year, y = ss, group = cz_id)) + 
    geom_line(colour = "lightblue") + 
    facet_wrap(~base_year) + 
    theme(legend.position = "none") +
    theme_minimal()
  
  full_cz %>% select(cz_id, year, contains("ss_emp_oil_gas")) %>% 
    group_by(cz_id) %>% 
    filter(any(!is.na(ss_emp_oil_gas_2001)) | any(!is.na(ss_emp_oil_gas_2005)) | any(!is.na(ss_emp_oil_gas_2011))) %>% 
    ungroup %>% summarise(across(ss_emp_oil_gas_2001:ss_emp_oil_gas_2011, ~sum(is.na(.))/nrow(full_cz)))
  
  #saveRDS(full_cz, here("data/temp/shift_shares_cz_base_01_05_11.RDS"))
}