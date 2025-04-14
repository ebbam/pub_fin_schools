
# County economic diversity index
#https://www.chmura.com/download-diversity-index-county

econ_div <- read_excel(here('data/raw/Chmura_Diversity-Index-County_final.xlsx')) %>% 
  select(-region_description) %>% 
  rename(fips = region_code) %>% 
  pivot_longer(cols = !c(fips), names_to = "year", values_to = "econ_div", names_transform = ~as.numeric(sub("diversity_index_", "", .))) %>% 
  complete(fips, year = 2000:2022) %>% 
  group_by(fips) %>% 
  fill(econ_div, .direction = "updown") %>% 
  ungroup %>% 
  mutate(fips = ifelse(fips %in% names(getfips), unname(getfips[fips]), fips)) %>% 
  mutate(fips = ifelse(fips == "15009", "15901", fips)) %>%
  group_by(fips, year) %>% 
  summarise(across(where(is.numeric), mean)) %>% 
  ungroup %>% 
  mutate(global_mean = mean(econ_div)) %>% 
  group_by(fips) %>% 
  # checked that no non-zero or one values exist
  mutate(local_mean = mean(unique(econ_div)), 
         high_econ_div = ifelse(local_mean >= global_mean, 1, 0), 
         low_econ_div = ifelse(local_mean < global_mean, 1, 0)) %>% 
  ungroup %>% 
  select(-global_mean) %>% 
  rename(mean_econ_div = local_mean)

# saveRDS(econ_div, here('data/temp/econ_div_controls.RDS'))
