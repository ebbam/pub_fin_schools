## Copy of rob_checks.R in coal_usa project

library(tidyverse)
library(readxl)
library(here)
library(openxlsx)
library(gdata)
library(janitor)
library(tidycensus)
library(zoo)
library(splm)
library(spdep)
library(modelsummary)
library(kableExtra)
library(conflicted)
library(fixest)
library(plm)
conflict_prefer_all("dplyr", quiet = TRUE)

# Test function ####

test_complete <- function(df){
  ifelse(nrow(df) == nrow(complete(df, fips, year)), "Complete fipsxyear", "INCOMPLETE FIPSXYEAR") %>% print
  df %>% filter(!complete.cases(.)) %>% pull(year) %>% unique %>% paste0(., collapse = "; ") %>% paste0("Incomplete years: ", .)
  
}

# DATA CLEANING ###

# Clean production data
clean_prod <- function(df){
  return(
    df %>% tibble %>% 
    clean_names %>% 
    select(mine_state, mine_county, mine_type, mine_status, operation_type, union_code, production_short_tons, average_employees, labor_hours) %>% 
    mutate(across(production_short_tons:labor_hours, ~as.numeric(gsub(",", "", ., fixed = TRUE))),
           active = ifelse(mine_status %in% c("Active","Active, men working, not producing"), "active", "inactive")) %>% 
    filter(!(operation_type %in% c("Preparation Plant", ""))) %>%
    group_by(mine_state, mine_county, mine_type, active) %>% 
    summarise(n = n(), prod = sum(production_short_tons, na.rm = TRUE)/1000, avg_emp = sum(average_employees, na.rm = TRUE), labour_hours = sum(labor_hours, na.rm = TRUE), n_unionised = sum(!is.na(union_code))) %>% 
    ungroup %>% 
    mutate(mine_type = tolower(mine_type)) %>% 
    filter(mine_type != "refuse") %>% 
    arrange(mine_county, mine_state, active) %>% 
    pivot_wider(id_cols = c(mine_county, mine_state), names_from = c(mine_type, active), names_glue = "{mine_type}_{active}_{.value}", values_from = c(n, prod, avg_emp, labour_hours, n_unionised)) %>% 
    mutate(across(surface_active_n:underground_inactive_n_unionised, ~replace(., is.na(.), 0)),
           total_active_n = surface_active_n + underground_active_n,
           total_active_prod = surface_active_prod + underground_active_prod,
           surface_n = rowSums(across(c(surface_active_n, surface_inactive_n)), na.rm = TRUE),
           surface_prod = surface_active_prod + surface_inactive_prod,
           underground_n = underground_active_n + underground_inactive_n,
           underground_prod = underground_active_prod + underground_inactive_prod,
           total_n = surface_n + underground_n,
           total_prod = surface_prod + underground_prod,
           across(surface_active_n:total_prod, ~round(.))) %>%
    rename(county = mine_county,
           state = mine_state) %>%
    select(state, county, underground_active_n, underground_active_prod, surface_active_n, surface_active_prod, total_active_n, total_active_prod, surface_n, surface_prod, underground_n, underground_prod, total_n, total_prod) %>%
    mutate(state = gsub(" \\(Anthracite\\)| \\(Bituminous\\)| \\(Northern\\)| \\(Southern\\)| \\(East\\)| \\(West\\)", "", state)) %>%
    arrange(state, county) %>%
    mutate(year = yr,
           county = case_when(state == "Louisiana" ~ paste0(county, " Parish"),
                     TRUE ~ paste0(county, " County"))) %>%
    filter(state != "Alaska") %>%
    relocate(year)
    )
}

full_prod <- tibble()
for(yr in 2001:2020){
  print(yr)
  if(yr == 2012){
    full_prod <- read.xls(here("data/temp/mines_data/coalpublic2012.xls"), skip = 2) %>%
      clean_prod %>% 
      rbind(full_prod, .)
  }else{
    url <- paste0("https://www.eia.gov/coal/data/public/xls/coalpublic", as.character(yr), ".xls")
    full_prod <- read.xls(url, skip = 2) %>%
    clean_prod %>% 
    rbind(full_prod, .)}
}

rm(fips_codes)
fips_codes <- fips_codes %>% 
  mutate(fips = paste0(state_code, county_code)) %>%
  select(state_name, county, fips) %>%
  rename(state = state_name) %>%
  mutate(county = case_when(county == "DeKalb County" ~ "De Kalb County",
                            county == "McCreary County" ~ "Mccreary County",
                            county == "McDowell County" ~ "Mcdowell County",
                            county == "McDonough County" ~ "Mcdonough County",
                            county == "McLean County" ~ "Mclean County",
                            county == "McKinley County" ~ "Mckinley County",
                   TRUE ~ county))

full_prod <- full_prod %>% 
  mutate(county = case_when(county == "Bighorn County" ~ "Big Horn County",
                            county == "Athans County" ~ "Athens County",
                            county == "Clairborne County" ~ "Claiborne County",
                            TRUE ~ county),
         state = case_when(county == "Monongalia County" ~ "West Virginia",
                           TRUE ~ state)) %>%
  group_by(year, state, county) %>% 
  summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
  ungroup %>%
  left_join(., fips_codes, by = c("state", "county")) %>% 
  mutate(fips = ifelse(fips == "51195", "51955", fips)) %>% 
  complete(fips, year) %>% 
  mutate(across(underground_active_n:last_col(), ~ifelse(is.na(.), 0, .))) %>% 
  group_by(fips) %>% 
  fill(c(state, county), .direction = "updown") %>% 
  mutate(across(c("total_active_prod", "total_prod"), .fns = list(diff = ~c(0,diff(.))), .names = "{.fn}_{.col}")) %>%
  mutate(across(contains("diff"), .fns = list(l1 = ~lag(.x, 1), l2 = ~lag(.x, 2)), .names = "{.fn}_{.col}")) %>% 
  ungroup

test_complete(full_prod)

# Original
# full_prod <- full_prod %>% 
#   mutate(county = case_when(county == "Bighorn County" ~ "Big Horn County",
#                             county == "Athans County" ~ "Athens County",
#                             county == "Clairborne County" ~ "Claiborne County",
#                             TRUE ~ county),
#          state = case_when(county == "Monongalia County" ~ "West Virginia",
#                            TRUE ~ state)) %>%
#   group_by(year, state, county) %>% 
#   summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>% 
#   ungroup %>%
#   left_join(., fips_codes, by = c("state", "county")) %>% 
#   mutate(fips = ifelse(fips == "51195", "51955", fips)) %>% 
#   filter(year <= 2019) %>% 
#   complete(fips, year) %>% 
#   group_by(fips) %>% 
#   mutate(across(c("total_active_prod", "total_prod"), .fns = list(diff = ~c(0,diff(.))), .names = "{.fn}_{.col}")) %>%
#   mutate(across(contains("diff"), .fns = list(l1 = ~lag(.x, 1), l2 = ~lag(.x, 2)), .names = "{.fn}_{.col}")) %>% 
#   ungroup

cap <- read.csv(here('data/temp/mines_data/production_coal/Productive_capacity.csv'), skip = 4) %>% 
  tibble %>%
  slice(-1) %>% 
  select(-source.key, -units) %>% 
  rename(state = description) %>% 
  mutate(state = gsub("Total : ", "", state),
         across(X2001:X2022, as.numeric)) %>%
  filter(!(state %in% c("Total", "Middle Atlantic", "East North Central", 
                        "West North Central", "South Atlantic", "East South Central", 
                        "West South Central", "Pacific Contiguous", "Pacific Noncontiguous", "Mountain"))) %>% 
  pivot_longer(cols = !state, names_to = "year", values_to = "prod_cap_state", names_transform = list(year = ~as.numeric(gsub("X","", .)))) %>% 
  mutate(prod_cap_state = ifelse(is.na(prod_cap_state), 0, round(prod_cap_state/1000))) %>% 
  arrange(state) %>% filter(state != "Alaska") %>%
  group_by(state) %>% 
  mutate(diff_prod_cap_state = lag(prod_cap_state) - prod_cap_state) %>% 
  ungroup 

# Original 
# cap <- read.csv(here('data/temp/mines_data/production_coal/Productive_capacity.csv'), skip = 4) %>% 
#   tibble %>%
#   slice(-1) %>% 
#   select(-source.key, -units) %>% 
#   rename(state = description) %>% 
#   mutate(state = gsub("Total : ", "", state),
#          across(X2001:X2022, as.numeric)) %>%
#   filter(!(state %in% c("Total", "Middle Atlantic", "East North Central", 
#                         "West North Central", "South Atlantic", "East South Central", 
#                         "West South Central", "Pacific Contiguous", "Pacific Noncontiguous", "Mountain"))) %>% 
#   pivot_longer(cols = !state, names_to = "year", values_to = "prod_cap_state", names_transform = list(year = ~as.numeric(gsub("X","", .)))) %>% 
#   mutate(prod_cap_state = round(prod_cap_state/1000)) %>% arrange(state) %>% filter(state != "Alaska") %>% 
#   group_by(state) %>% 
#   mutate(diff_prod_cap_state = lag(prod_cap_state) - prod_cap_state) %>% 
#   ungroup

prod_cap <- full_prod %>% 
  group_by(year, state) %>%
  mutate(total_state_prod = sum(total_prod, na.rm = TRUE)) %>% 
  ungroup %>% 
  # Check whether the numerator is ever greater than the state_prod: never happens
 # filter(total_state_prod < total_prod)
  mutate(prop_prod = ifelse(total_state_prod == 0 & total_prod == 0, 0, total_prod/total_state_prod)) %>% 
  # Checked whether produces any NA values
  #filter(is.nan(prop_prod) | is.na(prop_prod))
  select(year, state, fips, total_prod, prop_prod, total_state_prod) %>%
  group_by(fips) %>% 
  fill(state, .direction = "updown") %>% 
  ungroup %>% 
  left_join(., cap, by = c("state", "year")) %>% 
  mutate(prod_cap = prop_prod*prod_cap_state,
         diff_prod_cap = prop_prod*diff_prod_cap_state) %>%
  #complete(fips, year) %>% 
  group_by(fips) %>% 
  mutate(across(c("prod_cap", "diff_prod_cap"), .fns = list(l1 = ~lag(.x, 1), l2 = ~lag(.x, 2)), .names = "{.fn}_{.col}")) %>% 
  ungroup %>% 
  select(year, fips, prop_prod, total_state_prod, prod_cap_state, diff_prod_cap_state, prod_cap, diff_prod_cap, contains("l1"), contains("l2"))

test_complete(prod_cap)

# Original
# prod_cap <- full_prod %>% 
#   group_by(year, state) %>%
#   mutate(total_state_prod = sum(total_prod, na.rm = TRUE)) %>% 
#   ungroup %>% 
#   mutate(prop_prod = total_prod/total_state_prod) %>% 
#   select(year, state, fips, total_prod, prop_prod, total_state_prod) %>%
#   group_by(fips) %>% 
#   fill(state, .direction = "updown") %>% 
#   ungroup %>% 
#   left_join(., cap, by = c("state", "year")) %>% 
#   mutate(prod_cap = prop_prod*prod_cap_state,
#          diff_prod_cap = prop_prod*diff_prod_cap_state) %>% 
#   complete(fips, year) %>% 
#   group_by(fips) %>% 
#   mutate(across(c("prod_cap", "diff_prod_cap"), .fns = list(l1 = ~lag(.x, 1), l2 = ~lag(.x, 2)), .names = "{.fn}_{.col}")) %>% 
#   ungroup %>% 
#   select(year, fips, prop_prod, total_state_prod, prod_cap_state, diff_prod_cap_state, prod_cap, diff_prod_cap, contains("l1"), contains("l2"))

# RUNS ###
# Dataset of relevant variables for analysis
allcomp_complete <- #read_excel(here("data/temp/mines_data/allcomp_final.xlsx")) %>% 
  left_join(full_prod, prod_cap, by = c("fips", "year")) %>%
  #left_join(., prod_cap, by = c("fips", "year")) %>% 
  mutate(across(diff_total_active_prod:l2_diff_total_prod, ~ifelse(is.na(.x), 0, .x)/1000),
         across(c(diff_prod_cap, l1_diff_prod_cap, l2_diff_prod_cap), ~ifelse(is.na(.x), 0, .x)/1000))
  # mutate(active_prod_diff = as.logical(abs(mines_diff))*prod_diff,
  #       active_lag_prod_diff = as.logical(abs(lag_diff))*lag_prod_diff,
  #       active_lag_prod_diff2 = as.logical(abs(lag_diff2))*lag_prod_diff) %>% 
  # mutate(across(c(active_prod_diff, active_lag_prod_diff, active_lag_prod_diff2), ~ifelse(is.na(.x), 0, .x)/1000000)) %>% 
  # mutate(diff_prod_cap2 = ifelse(sign(mines_diff) == sign(diff_prod_cap_state), diff_prod_cap_state*prop_prod, 0)) %>% 
  #group_by(fips) %>% names
  #mutate(across(c("diff_prod_cap2"), .fns = list(l1 = ~lag(.x, 1), l2 = ~lag(.x, 2)), .names = "{.fn}_{.col}"),
  #       across(c(diff_prod_cap2, l1_diff_prod_cap2, l2_diff_prod_cap2), ~ifelse(is.na(.x), 0, .x)/1000)) %>% 
  #ungroup

test_complete(allcomp_complete)

# Shares a version with minimal missing values
allcomp_complete %>% saveRDS(here("data/temp/mines_data/allcomp_cap_prod_complete.RDS"))

# allcomp %>% saveRDS(here("data/allcomp_cap_prod.RDS"))

# 
# # Unmodified lookup table to adjust FIPS codes between various standards
# bea_fips_mods <- read_excel(here("data/FIPSModificationsVA.xlsx"), skip = 1, col_types = c("text", "text","text","text"))
# 
# # Create subset of coal counties (CC) only (with active mines at some point)
# cclist <- unique(allcomp$fips[which(allcomp$active_mines != 0)])
# allcomp_cc <- subset(allcomp, fips %in% cclist)
# 
# # Dataset including the "type" of county as found in the Typology work (Typology Work_New.Rmd)
# # Merged with CC dataset
# cc_cluster <- read_excel(here("data/cc_clusters_251.xlsx"))
# allcomp_cc_types <- merge(allcomp_cc, cc_cluster, by = "fips", all.x = TRUE)
# 
# # Load required functions and dictionaries
# source("useful_functions.R")
# source("dicts.R")
# source("ref_lists.R")
# 
# # Create adjacency matrix for spatial model
# ##############################################################
# # Import adjacency matrix from US Census Bureau: 
# # https://www.census.gov/programs-surveys/geography/library/reference/county-adjacency-file.html
# xmat1 <- read.csv("data/county_adjacency.txt", sep="\t", stringsAsFactors = FALSE, header = FALSE)
# 
# # Retain only columns with FIPS codes of each county (V2) and its neighbors (V4)
# xmat1 <- xmat1[,c("V2","V4")]
# 
# # Replace NA values in V2 such that each row is a pair of neighbors
# xmat1$V2 <- na.locf(xmat1$V2)
# 
# # Convert from integer to character fips code for appropriate matching
# xmat1$V2 <- lapply(xmat1$V2, function(x) sapply(x, fips_format))
# xmat1$V4 <- lapply(xmat1$V4, function(x) sapply(x, fips_format))
# 
# # Create lookup table to convert VA FIPS codes
# getfips <- bea_fips_mods$`BEA FIPS`
# names(getfips) <- bea_fips_mods$FIPS
# 
# xmat1[xmat1 == "46113"] <- "46102"
# xmat1[xmat1 == "51515"] <- "51019"
# xmat1$V2 <- lapply(xmat1$V2, function(x) ifelse(x %in% bea_fips_mods$FIPS, getfips[x], x))
# xmat1$V4 <- lapply(xmat1$V4, function(x) ifelse(x %in% bea_fips_mods$FIPS, getfips[x], x))
# 
# # Removes Alaska, Hawaii, DC, PR, Guam, American Samoa
# xmat1 <- subset(xmat1, substr(V2, 1, 2) != "60" & 
#                   substr(V2, 1, 2) != "66" &
#                   substr(V2, 1, 2) != "69" &
#                   substr(V2, 1, 2) != "72" &
#                   substr(V2, 1, 2) != "78" &
#                   substr(V2, 1, 2) != "15" &
#                   substr(V2, 1, 2) != "02")
# 
# LA_list <- c(LA_list_missing, "11001")
# xmat1 <- subset(xmat1, !(V2 %in% LA_list) & !(V4 %in% LA_list))
# 
# length(unique(xmat1$V2))
# #as.list(unique(xmat1$V2[which(!(xmat1$V2 %in% allcomp$fips))]))
# 
# # Unnest list elements for easier manipulation
# xmat1 <- unnest(xmat1)
# # Create a list of each unique FIPS code for matching
# fipslist <- unique(xmat1$V2)
# 
# # Subset dataframe to only include FIPS present in the distance matrix
# allcomp_nomissing <- subset(allcomp, fips %in% fipslist)
# #allcomp_nomissing <- subset(allcomp_nomissing, fips != "51770")
# allcomp_full <- allcomp_nomissing %>%
#   arrange(fips) %>%
#   select(fips,year,everything())
# 
# xmatfips <- unique(xmat1$V2)
# 
# # Create an empty (all cells = 0) n x n distance matrix with n = no of FIPS codes/counties
# fmat <- matrix(data=0, nrow = length(xmatfips), ncol = length(xmatfips), dimnames = list(xmatfips,xmatfips))
# 
# # Iterate through xmatfips, populating matrix with 1 if two counties are neighbors
# # Census bureau lists counties as neighbors of itself so the loop will fill cell 
# # with 0 for row-pair that lists a county as a neighbor of itself
# for(i in 1:nrow(xmat1)){
#   a = as.character(xmat1$V2[i])
#   b = as.character(xmat1$V4[i])
#   if(identical(a,b)){fmat[a,b] <- 0}else{fmat[a,b] <- 1}
# }
# 
# # Row standardises the distance matrix
# fmat <- fmat/rowSums(fmat)
# sum(is.na(fmat))
# dim(fmat)
# 
# # Creates listw object for use in splm package
# fmatlw <- mat2listw(fmat, style = "W")
# 
# # Also a test for errors
# testfmatcdg <- listw2dgCMatrix(fmatlw)
# 
# # Sanity checks
# ### Ensure panel is balanced
# is.pbalanced(allcomp_full)
# ### No NAs
# sum(is.na(fmatlw))
# 
# 
# FE_diffuer <- as.formula("diff_uer ~ mines_diff + lag_diff + lag_diff2 + diff_log_realgdp_pc | fips + year")
# FE_prod_orig <- as.formula("diff_uer ~ prod_diff + lag_prod_diff + lag_prod_diff2 + diff_log_realgdp_pc | fips + year")
# #FE_prod_active_orig <- as.formula("diff_uer ~ active_prod_diff + active_lag_prod_diff + active_lag_prod_diff2 + diff_log_realgdp_pc | fips + year")
# #FE_prod_new <- as.formula("diff_uer ~ diff_total_prod + l1_diff_total_prod + l2_diff_total_prod + diff_log_realgdp_pc | fips + year")
# #FE_prod_active_new <- as.formula("diff_uer ~ diff_total_active_prod + l1_diff_total_active_prod + l2_diff_total_active_prod + diff_log_realgdp_pc | fips + year")
# FE_prod_cap <- as.formula("diff_uer ~ diff_prod_cap + l1_diff_prod_cap + l2_diff_prod_cap + diff_log_realgdp_pc | fips + year")
# FE_prod_cap2 <- as.formula("diff_uer ~ diff_prod_cap2 + l1_diff_prod_cap2 + l2_diff_prod_cap2 + diff_log_realgdp_pc | fips + year")
# 
# #FE_diff_cap <- as.formula("diff_uer ~ cap_diff + lag_cap_diff + lag_cap_diff2 + diff_log_realgdp_pc | fips + year")
# main <- feols(FE_diffuer, allcomp, se = 'twoway')
# main_prod_orig <- feols(FE_prod_orig, allcomp, se = 'twoway')
# #main_prod_active_orig <- feols(FE_prod_active_orig, allcomp, se = 'twoway')
# #main_prod_new <- feols(FE_prod_new, allcomp, se= "twoway")
# #main_prod_active_new <- feols(FE_prod_active_new, allcomp, se= "twoway")
# main_cap <- feols(FE_prod_cap, allcomp, se = 'twoway')
# main_cap2 <- feols(FE_prod_cap2, allcomp, se = 'twoway')
# modelsummary(list(main, main_prod_orig, main_cap, main_cap2), stars = TRUE)
# 
# 
# allcomp_full$diff_log_realgdp_pc[which(allcomp_full$fips == "08014" & allcomp_full$year == 2002)] <- 0
# allcomp_full$diff_log_realgdp[which(allcomp_full$fips == "08014" & allcomp_full$year == 2002)] <- 0
# allcomp_full$diff_log_pop[which(allcomp_full$fips == "08014" & allcomp_full$year == 2002)] <- 0
# 
# 
# fmdiff_prod <- diff_uer ~ prod_diff + lag_prod_diff + lag_prod_diff2 + diff_log_realgdp_pc
# fmdiff_prodcap <- diff_uer ~ diff_prod_cap + l1_diff_prod_cap + l2_diff_prod_cap + diff_log_realgdp_pc
# fmdiff_prodcap2 <- diff_uer ~ diff_prod_cap2 + l1_diff_prod_cap2 + l2_diff_prod_cap2 + diff_log_realgdp_pc
# 
# sp_err_lag_prod <- spml(fmdiff_prod, data = allcomp_full, index = NULL,  listw = fmatlw, 
#                          lag = TRUE, na.action = na.fail, spatial.error = "b",
#                          model = "within", effect = "twoways", quiet = FALSE)
# 
# sp_err_lag_cap <- spml(fmdiff_prodcap, data = allcomp_full, index = NULL,  listw = fmatlw, 
#                         lag = TRUE, na.action = na.fail, spatial.error = "b",
#                         model = "within", effect = "twoways", quiet = FALSE)
# 
# sp_err_lag_cap2 <- spml(fmdiff_prodcap2, data = allcomp_full, index = NULL,  listw = fmatlw, 
#                        lag = TRUE, na.action = na.fail, spatial.error = "b",
#                        model = "within", effect = "twoways", quiet = FALSE)
# 
# allcomp_pdf <- pdata.frame(allcomp_full, index = c("fips", "year"))
# allcomp_pdf$sl_prod_diff <- slag(allcomp_pdf$prod_diff, fmatlw)
# allcomp_pdf$sl_lag_prod_diff <- slag(allcomp_pdf$lag_prod_diff, fmatlw)
# allcomp_pdf$sl_lag_prod_diff2 <- slag(allcomp_pdf$lag_prod_diff2, fmatlw)
# 
# allcomp_pdf$sl_diff_prod_cap <- slag(allcomp_pdf$diff_prod_cap, fmatlw)
# allcomp_pdf$sl_l1_diff_prod_cap <- slag(allcomp_pdf$l1_diff_prod_cap, fmatlw)
# allcomp_pdf$sl_l2_diff_prod_cap<- slag(allcomp_pdf$l2_diff_prod_cap, fmatlw)
# 
# allcomp_pdf$sl_diff_prod_cap2 <- slag(allcomp_pdf$diff_prod_cap2, fmatlw)
# allcomp_pdf$sl_l1_diff_prod_cap2 <- slag(allcomp_pdf$l1_diff_prod_cap2, fmatlw)
# allcomp_pdf$sl_l2_diff_prod_cap2 <- slag(allcomp_pdf$l2_diff_prod_cap2, fmatlw)
# 
# fmdiff_prod_spl <-  diff_uer ~ prod_diff + lag_prod_diff + lag_prod_diff2 + diff_log_realgdp_pc + sl_prod_diff + sl_lag_prod_diff + sl_lag_prod_diff2
# fmdiff_cap_spl <-  diff_uer ~ diff_prod_cap + l1_diff_prod_cap + l2_diff_prod_cap + diff_log_realgdp_pc + sl_diff_prod_cap + sl_l1_diff_prod_cap + sl_l2_diff_prod_cap
# fmdiff_cap2_spl2 <-  diff_uer ~ diff_prod_cap2 + l1_diff_prod_cap2 + l2_diff_prod_cap2 + diff_log_realgdp_pc + sl_diff_prod_cap2 + sl_l1_diff_prod_cap2 + sl_l2_diff_prod_cap2
# 
# 
# 
# # run model
# durbin_error_prod <- spml(fmdiff_prod_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, spatial.error = "b")
# summary(durbin_error_prod)
# # durbin_lag_prod <- spml(fmdiff_prod_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, lag = TRUE)
# # summary(durbin_lag_prod)
# 
# durbin_error_cap <- spml(fmdiff_cap_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, spatial.error = "b")
# summary(durbin_error_cap)
# # durbin_lag_cap <- spml(fmdiff_cap_spl, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, lag = TRUE)
# # summary(durbin_lag_cap)
# 
# durbin_error_cap2 <- spml(fmdiff_cap2_spl2, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, spatial.error = "b")
# summary(durbin_error_cap2)
# # durbin_lag_cap2 <- spml(fmdiff_cap2_spl2, data = allcomp_pdf, listw = fmatlw, model="within", effect = "twoways", quiet = FALSE, lag = TRUE)
# # summary(durbin_lag_cap2)
# 
# sparse.W <- listw2dgCMatrix(fmatlw)
# time <- length(unique(allcomp_full$year))
# s.lwcounties <- kronecker(Matrix::Diagonal(time), sparse.W)
# trMatc <- spatialreg::trW(s.lwcounties, type="mult")
# #implag <- impacts(sp_lag_prod, tr = trMatc, R = 200)
# imperrlag_prod <- impacts(sp_err_lag_prod, tr = trMatc, R = 200)
# imperrlag_cap <- impacts(sp_err_lag_cap, tr = trMatc, R = 200)
# imperrlag_cap2 <- impacts(sp_err_lag_cap2, tr = trMatc, R = 200)
# 
# summary(imperrlag_prod, zstats=TRUE, short=TRUE)
# summary(imperrlag_cap, zstats=TRUE, short=TRUE)
# summary(imperrlag_cap2, zstats=TRUE, short=TRUE)
# summary(durbin_error_prod)
# summary(durbin_error_cap)
# summary(durbin_error_cap2)
# 
# # Spatial model summary results
# # sums_prod <- summary(imperrlag_prod, zstats=TRUE, short=TRUE)
# # sums_cap <- summary(imperrlag_cap, zstats=TRUE, short=TRUE)
# # sums_cap2 <- summary(imperrlag_cap2, zstats=TRUE, short=TRUE)
# 
# 
# # Spatial Durbin Summary
# # Spatial error model output table
# 
# sum_tbl <- function(mods, var_names, durbin){
#   if(durbin){
#     temp <- summary(mods) %>% .$CoefTable %>% as.data.frame
#     var_names <- c("Spatial Error Parameter", var_names)
#     }else{
#       temp <- summary(mods) %>% .$coeftable %>% as.data.frame
#     }
#   names(temp)[4] <- "p_values"
#   
#   temp1 = temp %>% 
#     mutate_at("p_values", function(x) ifelse(x <= 0.001, "***",
#                                              ifelse(x > 0.001 & x <= 0.01, "**", 
#                                                     ifelse(x > 0.01 & x <= 0.05, "*", 
#                                                            ifelse(x > 0.05 & x <= 0.1, ".",""))))) %>%
#     mutate_if(is.numeric, round, 3) %>% 
#     mutate(Estimate = paste0(Estimate, p_values), var = var_names, se = paste0("(",`Std. Error`,")")) %>%
#     select(1,5,6) %>% 
#     pivot_longer(!var, names_to = "measure", values_to = "estimate")
#   
#   return(temp1)
#   
# }
# 
# 
# names_prod <- c("Production_{i,t}", "Production_{i,t-1}", "Production_{i,t-2}", "(log) Real GDPPC")
# 
# names_cap <- c("Capacity_{i,t}", "Capacity_{i,t-1}", "Capacity_{i,t-2}", "(log) Real GDPPC")
# 
# names_cap2 <- c("Capacity*_{i,t}", "Capacity*_{i,t-1}", "Capacity*_{i,t-2}", "(log) Real GDPPC")
# 
# names_durbin_prod <- c("Production_{i,t}", "Production_{i,t-1}", "Production_{i,t-2}", "(log) Real GDPPC", "Production_{-i,t}", "Production_{-i,t-1}", "Production_{-i,t-2}")
# 
# names_durbin_cap <- c("Capacity_{i,t}", "Capacity_{i,t-1}", "Capacity_{i,t-2}", "(log) Real GDPPC", "Capacity_{-i,t}", "Capacity_{-i,t-1}", "Capacity_{-i,t-2}")
# 
# names_durbin_cap2 <- c("Capacity*_{i,t}", "Capacity*_{i,t-1}", "Capacity*_{i,t-2}", "(log) Real GDPPC", "Capacity*_{-i,t}", "Capacity*_{-i,t-1}", "Capacity*_{-i,t-2}")
# 
# # sum_tbl(durbin_error_prod, names_durbin_prod, TRUE) %>% 
# #   left_join(sum_tbl(main_prod_orig, names_prod, FALSE), by = c("var", "measure")) %>% 
# #   mutate(var = ifelse(row_number()%%2, var, "")) %>%
# #   select(-measure) %>% 
# #   kable(., booktabs=TRUE, format = "latex") %>%
# #     kable_styling(position="center")
# # 
# # sum_tbl(durbin_error_cap, names_durbin_cap, TRUE) %>% 
# #   left_join(sum_tbl(main_cap, names_cap, FALSE), by = c("var", "measure")) %>% 
# #   mutate(var = ifelse(row_number()%%2, var, "")) %>%
# #   select(-measure) %>% 
# #   kable(., booktabs=TRUE, format = "latex") %>%
# #   kable_styling(position="center")
# # 
# # sum_tbl(durbin_error_cap2, names_durbin_cap2, TRUE) %>% 
# #   left_join(sum_tbl(main_cap2, names_cap2, FALSE), by = c("var", "measure")) %>% 
# #   mutate(var = ifelse(row_number()%%2, var, "")) %>%
# #   select(-measure) %>% 
# #   kable(., booktabs=TRUE, format = "latex") %>%
# #   kable_styling(position="center")
# 
# 
# # SARAR Results
# sarar_tbl <-cbind(spat_output_rob(imperrlag_prod), 
#                     spat_output_rob(imperrlag_cap), 
#                     spat_output_rob(imperrlag_cap2))
# 
# sarar_tbl[1:5] %>% 
#   tibble %>% 
#   mutate(var = case_when(var == "prod_diff" ~ "Production_{i,t}",
#                          var == "lag_prod_diff" ~ "Production_{i,t-1}",
#                          var == "lag_prod_diff2" ~ "Production_{i,t-2}",
#                          var == "diff_log_realgdp_pc" ~ "(log) Real GDPPC")) %>% 
#   left_join(sum_tbl(durbin_error_prod, names_durbin_prod, TRUE), ., by = c("var", "measure")) %>% 
#   left_join(., sum_tbl(main_prod_orig, names_prod, FALSE), by = c("var", "measure")) %>% 
#   relocate(var, measure, estimate.y, estimate.x, Direct, Indirect, Total) %>% 
#   mutate(var = ifelse(row_number()%%2, var, "")) %>% 
#   select(-measure) %>% 
#   kable(., booktabs=TRUE, format = "latex") %>% 
#   kable_styling(position="center")
# 
# sarar_tbl[6:10] %>% 
#   tibble %>% 
#   mutate(var = case_when(var == "diff_prod_cap" ~ "Capacity_{i,t}",
#                          var == "l1_diff_prod_cap" ~ "Capacity_{i,t-1}",
#                          var == "l2_diff_prod_cap" ~ "Capacity_{i,t-2}",
#                          var == "diff_log_realgdp_pc" ~ "(log) Real GDPPC")) %>% 
#   left_join(sum_tbl(durbin_error_cap, names_durbin_cap, TRUE), ., by = c("var", "measure")) %>% 
#   left_join(., sum_tbl(main_cap, names_cap, FALSE), by = c("var", "measure")) %>% 
#   relocate(var, measure, estimate.y, estimate.x, Direct, Indirect, Total) %>% 
#   mutate(var = ifelse(row_number()%%2, var, "")) %>% 
#   select(-measure) %>% 
#   kable(., booktabs=TRUE, format = "latex") %>% 
#   kable_styling(position="center")
# 
# sarar_tbl[11:15] %>% 
#   tibble %>% 
#   mutate(var = case_when(var == "diff_prod_cap2" ~ "Capacity*_{i,t}",
#                          var == "l1_diff_prod_cap2" ~ "Capacity*_{i,t-1}",
#                          var == "l2_diff_prod_cap2" ~ "Capacity*_{i,t-2}",
#                          var == "diff_log_realgdp_pc" ~ "(log) Real GDPPC")) %>% 
#   left_join(sum_tbl(durbin_error_cap2, names_durbin_cap2, TRUE), ., by = c("var", "measure")) %>% 
#   left_join(sum_tbl(main_cap2, names_cap2, FALSE), by = c("var", "measure")) %>% 
#   relocate(var, measure, estimate.y, estimate.x, Direct, Indirect, Total) %>% 
#   mutate(var = ifelse(row_number()%%2, var, "")) %>% 
#   select(-measure) %>% 
#   kable(., booktabs=TRUE, format = "latex") %>% 
#   kable_styling(position="center")
# 
# # PRODUCTIVE CAPACITY
# 
# # test <- read.xls('https://www.eia.gov/coal/annual/xls/table11.xls', skip = 3) %>%
# #   tibble %>%
# #   slice(-c(28:31)) %>%
# #   rename(state = Coal.Producing,
# #          X2022 = Total,
# #          X2021 = Total.1) %>%
# #   select(state, contains("x")) %>%
# #   filter(!grepl("      ", state) & state != "State") %>%
# #   mutate(state = gsub(" Total", "", state),
# #          across(!state, ~as.numeric(gsub(",", "", .)))) %>%
# #     pivot_longer(cols = !state, names_to = "year", values_to = "prod_cap_state", names_transform = list(year = ~as.numeric(gsub("X","", .)))) %>%
# #   arrange(year, state)
# 
# 
#   
#   # Conclusion: active production versus total production are not that different...likely mislabeled things...
#   # ggplot() +
#   # geom_line(aes(x = year, y = total_state_prod)) +
#   # geom_line(aes(x = year, y = total_active_state_prod), color = "blue") + 
#   # facet_wrap(~state, scales  = "free")

  
