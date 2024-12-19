run_model <- function(mods, df){
  res <- list()
  for(k in mods){
    res <- append(res, list(feols(as.formula(k), data = df, se = "cluster", cluster = "cz_id", panel.id = ~cz_id+year)))
  }
  return(res)
}

# Property Tax ~ gdp

prop_taxes = c(log_Property_Tax ~ log_gdp_total + sw0(l(log_gdp_total, 1:4)) | cz_id + year, 
               log_Property_Tax_pp ~ log_gdp_total_pc + sw0(l(log_gdp_total_pc, 1:4)) | cz_id + year)


# Property Tax ~ gdp
educ_source = c(log_Elem_Educ_Total_Exp_pp ~ sw(log_Total_Rev_Own_Sources_pp + log_Total_IG_Revenue_pp,
                                                                      log_Property_Tax_pp,
                                                                      log_Property_Tax_pp + log_Total_IG_Revenue_pp,
                                                                      log_Property_Tax_pp + log_Total_Fed_IG_Revenue_pp + log_Total_State_IG_Revenue_pp) | cz_id + year,
                log_Elem_Educ_Total_Exp ~ sw(log_Property_Tax,
                                                                      log_Property_Tax + log_Total_IG_Revenue,
                                                                      log_Property_Tax + log_Total_Fed_IG_Revenue + log_Total_State_IG_Revenue,
                                                                      log_Total_Rev_Own_Sources + log_Total_IG_Revenue) | cz_id + year)



educ_gdp = c(log_Elem_Educ_Total_Exp_pp ~ sw(log_gdp_total_pc,
                                             log_gdp_priv_ind_pc, 
                                             log_gdp_o_g_mining_quarr_21_pc) | cz_id + year)

educ_gdp_statetrends = c(log_Elem_Educ_Total_Exp_pp ~ sw(log_gdp_total_pc,
                                       log_gdp_total_pc + l(log_Elem_Educ_Total_Exp_pp, 1),
                                       log_gdp_priv_ind_pc, 
                                       log_gdp_priv_ind_pc + l(log_Elem_Educ_Total_Exp_pp, 1)) + time:as.factor(main_state) | cz_id + year)

# W. state time trends
educ_gdp_lags = c(log_Elem_Educ_Total_Exp_pp ~ log_gdp_total_pc + l1_log_gdp_total_pc + l2_log_gdp_total_pc + sw0(time:as.factor(main_state)) | cz_id + year, 
                  log_Elem_Educ_Total_Exp_pp ~  log_gdp_priv_ind_pc + l1_log_gdp_priv_ind_pc + l2_log_gdp_priv_ind_pc + sw0(time:as.factor(main_state)) | cz_id + year,
                  log_Elem_Educ_Total_Exp_pp ~  log_gdp_o_g_mining_quarr_21_pc + l1_log_gdp_o_g_mining_quarr_21_pc + l2_log_gdp_o_g_mining_quarr_21_pc + sw0(time:as.factor(main_state)) | cz_id + year)

educ_coal = c(log_Elem_Educ_Total_Exp_pp ~ sw(log_gdp_priv_ind_pc + l1_log_gdp_priv_ind_pc + l2_log_gdp_priv_ind_pc + l(total_active_n, 0:2),
                                                                           log_gdp_total_pc + l1_log_gdp_total_pc + l2_log_gdp_total_pc + l(total_active_n, 0:2)) | cz_id + year,
    log_Elem_Educ_Total_Exp_pp ~ sw(log_gdp_priv_ind_pc + l1_log_gdp_priv_ind_pc + l2_log_gdp_priv_ind_pc + l(total_active_prod, 0:2),
                                                                           log_gdp_total_pc + l1_log_gdp_total_pc + l2_log_gdp_total_pc + l(total_active_prod, 0:2)) | cz_id + year)

educ_coal_statetrends = c(log_Elem_Educ_Total_Exp_pp ~ sw(log_gdp_priv_ind_pc + l1_log_gdp_priv_ind_pc + l2_log_gdp_priv_ind_pc + l(total_active_n, 0:2),
                                                                           log_gdp_total_pc + l1_log_gdp_total_pc + l2_log_gdp_total_pc + l(total_active_n, 0:2)) + time:as.factor(main_state) | cz_id + year,
     log_Elem_Educ_Total_Exp_pp ~ sw(log_gdp_priv_ind_pc + l1_log_gdp_priv_ind_pc + l2_log_gdp_priv_ind_pc + l(total_active_prod, 0:2),
                                                                           log_gdp_total_pc + l1_log_gdp_total_pc + l2_log_gdp_total_pc + l(total_active_prod, 0:2)) + time:as.factor(main_state) | cz_id + year)
######### IV Approaches ########

                # Using total active production as an instrument for per capita property tax
iv_prod_tax = c(log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_Property_Tax_pp ~ total_active_prod,
                # Using the lag of total active production as an instrument for per capita property tax
                log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_Property_Tax_pp ~ l(total_active_prod,1))
                # Using total active mines as an instrument for per capita property tax
iv_coaln_tax = c(log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_Property_Tax_pp ~ total_active_n,
                 # Using lag of total active mines as an instrument for per capita property tax
                 log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_Property_Tax_pp ~ l(total_active_n,1))


# Using total active production as an instrument for per capita property tax
iv_prod_gdp = c(log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_gdp_total_pc ~ total_active_prod,
                # Using the lag of total active production as an instrument for per capita property tax
                log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_gdp_total_pc ~ l(total_active_prod,1))
# Using total active mines as an instrument for per capita property tax
iv_coaln_gdp = c(log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_gdp_total_pc ~ total_active_n,
                 # Using lag of total active mines as an instrument for per capita property tax
                 log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_gdp_total_pc ~ l(total_active_n,1))

selected_base_models = c(log_Property_Tax_pp ~ log_gdp_priv_ind_pc + l1_log_gdp_priv_ind_pc + l2_log_gdp_priv_ind_pc | cz_id + year,
                   # log_Elem_Educ_Total_Exp_pp ~ log_Property_Tax_pp + log_Total_Fed_IG_Revenue_pp + log_Total_State_IG_Revenue_pp | cz_id + year,
                    log_Elem_Educ_Total_Exp_pp ~ log_gdp_priv_ind_pc + l1_log_gdp_priv_ind_pc + l2_log_gdp_priv_ind_pc | cz_id + year,
                   log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp + log_gdp_priv_ind_pc + l1_log_gdp_priv_ind_pc + l2_log_gdp_priv_ind_pc | cz_id + year,
                   log_Property_Tax_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp + l(total_active_prod, 0:1) | cz_id + year)

selected_iv_models = c(log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_Property_Tax_pp ~ total_active_prod,
                    # Using the lag of total active production as an instrument for per capita property tax
                    log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_Property_Tax_pp ~ l(total_active_prod,1),
                    log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_gdp_total_pc ~ total_active_prod,
                    # Using the lag of total active production as an instrument for per capita property tax
                    log_Elem_Educ_Total_Exp_pp ~ log_Total_State_IG_Revenue_pp + log_Total_Fed_IG_Revenue_pp | cz_id + year | log_gdp_total_pc ~ l(total_active_prod,1))

