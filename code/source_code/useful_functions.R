##############################################################

# Add labels to plots with commuting zones
add_labels <- function(plot, df, var, ns){ # plot,
  tmp <- df %>% 
    left_join(cz_labels, by = "unit") %>% 
    arrange(get(var)) %>% 
    slice(c(1:ns, (n() - (ns-1)):n()))
  
  plot +
    geom_text_repel(data = tmp, aes(label = msa), direction = "y",
                    nudge_y = 0.02, size = 3, segment.color = "grey60",
                    max.overlaps = Inf)
  
}


library(tidycensus)
get_state <- function(st){
  fips_codes %>% filter(state_code == st) %>% pull(state) %>% unique %>% return(.)
}

states <- data.frame(state.abb, state.region)
get_region <- function(st){
  states %>% filter(state.abb == st) %>% pull(state.region) %>% unique %>% return(.)
}

get_code <- function(st_abb){
  fips_codes %>% filter(state == st_abb) %>% pull(state_code) %>% unique %>% return(.)
}

#vec_get_state = Vectorize(get_state)

## Function to identify FIPS code

fips_format <- function(fipscode){
  if (is.na(fipscode)) return("00000")
  fips_c = as.character(fipscode)
  test_chars = nchar(fips_c)
  if (is.null(fipscode) | is.na(fipscode)) {
    return (nchar(trunc(fipscode)))
  }
  else if (test_chars < 4) {
    return (fips_c)
  }
  else if (test_chars < 5) {
    return (paste0(0, fips_c, sep = ""))
  }
  else {
    return (fips_c)
  }
}


create_cols <- function(model){
  pval <- model %>% pvalue() %>% 
    lapply(., function(z) ifelse(z <= 0.001, 4,
                                 ifelse(z > 0.001 & z <= 0.01, 3, 
                                        ifelse(z > 0.01 & z <= 0.05, 2, 
                                               ifelse(z > 0.05 & z <= 0.1, 1,0)))))
  pval <- as.matrix(as.numeric(pval[1:3]))
  iden <- matrix(nrow = 3, ncol = 3, data = 0)
  diag(iden) <- unlist(lapply(coef(model)[1:3], sign))
  return(-1*(t(pval) %*% iden))
}


df_va <- function(df){
  fips_pre <- as.list(unique(df$fips))
  for(a in 1:length(fips_pre)){
    f = fips_pre[a]
    if(f %in% getfips){
      newfips = names(getfips[which(getfips == f)])
      if(length(newfips) != 0){
        for(b in 1:length(newfips)){
          copy <- subset(df, fips == f)
          copy$fips <- newfips[b]
          df <- rbind(df, copy)
        }
      }else{
      }
      df <- subset(df, fips != f)
    }
  }
  return(df)
}



# Initialise function for specific size of matrix
c_mat <- function(var) {matrix(var, 18, 3072)}

# Function to mute outliers for mapping plot_us_map - 
# takes any values outside 3 standard deviations from the mean and replaces with mean + 3*sd
mute_outliers <- function(df, var_outliers, sds = 3){
  temp <- df %>% 
    mutate(sd = sd(get(var_outliers), na.rm = TRUE), 
           mean = mean(get(var_outliers), na.rm = TRUE),
           var_muted = case_when(get(var_outliers) > mean + sds*sd ~ mean + sds*sd,
                                 get(var_outliers) < mean - sds*sd ~ mean - sds*sd,
                                 TRUE ~ get(var_outliers)))
  return(temp)
}


# # Function to convert standard fips codes to BEA fips codes
# # FIPS modifications - inconsistent FIPS codes between BEA sources and others
# # the following creates a lookup process for replacing standard FIPS codes with their BEA equivalents
## Taken from coal project: coal_usa/replication_code/data/FIPSModificationsVA.xlsx
bea_fips <- read_excel(here("data/out/FIPSModificationsVA.xlsx"), skip = 1, col_types = c("text", "text","text","text"))
getfips <- bea_fips$`BEA FIPS`
names(getfips) <- bea_fips$FIPS
