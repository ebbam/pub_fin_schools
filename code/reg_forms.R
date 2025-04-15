run_model <- function(mods, df){
  res <- list()
  for(k in mods){
    res <- append(res, list(feols(as.formula(k), data = df, se = "cluster", cluster = "unit", panel.id = ~unit+year)))
  }
  return(res)
}

run_model_fips <- function(mods, df){
  res <- list()
  for(k in mods){
    res <- append(res, list(feols(as.formula(k), data = df, se = "cluster", cluster = "fips", panel.id = ~fips+year)))
  }
  return(res)
}

# Property Tax ~ gdp

prop_taxes = c(log_real_Property_Tax ~ log_real_gdp_total + sw0(l(log_real_gdp_total, 1:4)) | unit + year, 
               log_real_Property_Tax_pp ~ log_real_gdp_total_pc + sw0(l(log_real_gdp_total_pc, 1:4)) | unit + year)


# Property Tax ~ gdp
educ_source = c(log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_Total_Rev_Own_Sources_pp + log_real_Total_IG_Revenue_pp,
                                                                      log_real_Property_Tax_pp,
                                                                      log_real_Property_Tax_pp + log_real_Total_IG_Revenue_pp,
                                                                      log_real_Property_Tax_pp + log_real_Total_Fed_IG_Revenue_pp + log_real_Total_State_IG_Revenue_pp) | unit + year,
                log_real_Elem_Educ_Total_Exp ~ sw(log_real_Property_Tax,
                                                                      log_real_Property_Tax + log_real_Total_IG_Revenue,
                                                                      log_real_Property_Tax + log_real_Total_Fed_IG_Revenue + log_real_Total_State_IG_Revenue,
                                                                      log_real_Total_Rev_Own_Sources + log_real_Total_IG_Revenue) | unit + year)



educ_gdp = c(log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_total_pc,
                                             log_real_gdp_priv_ind_pc, 
                                             log_real_gdp_o_g_mining_quarr_21_pc) | unit + year)

educ_gdp_statetrends = c(log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_total_pc,
                                       log_real_gdp_total_pc + l(log_real_Elem_Educ_Total_Exp_pp, 1),
                                       log_real_gdp_priv_ind_pc, 
                                       log_real_gdp_priv_ind_pc + l(log_real_Elem_Educ_Total_Exp_pp, 1)) + time:as.factor(state) | unit + year)

# W. state time trends
educ_gdp_lags = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc + sw0(time:as.factor(state)) | unit + year, 
                  log_real_Elem_Educ_Total_Exp_pp ~  log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + sw0(time:as.factor(state)) | unit + year,
                  log_real_Elem_Educ_Total_Exp_pp ~  log_real_gdp_o_g_mining_quarr_21_pc + l1_log_real_gdp_o_g_mining_quarr_21_pc + l2_log_real_gdp_o_g_mining_quarr_21_pc + sw0(time:as.factor(state)) | unit + year)

educ_coal = c(log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + l(total_active_n, 0:2),
                                                                           log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc + l(total_active_n, 0:2)) | unit + year,
    log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + l(total_active_prod, 0:2),
                                                                           log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc + l(total_active_prod, 0:2)) | unit + year)

educ_coal_statetrends = c(log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + l(total_active_n, 0:2),
                                                                           log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc + l(total_active_n, 0:2)) + time:as.factor(state) | unit + year,
     log_real_Elem_Educ_Total_Exp_pp ~ sw(log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc + l(total_active_prod, 0:2),
                                                                           log_real_gdp_total_pc + l1_log_real_gdp_total_pc + l2_log_real_gdp_total_pc + l(total_active_prod, 0:2)) + time:as.factor(state) | unit + year)
######### IV Approaches ########

                # Using total active production as an instrument for per capita property tax
iv_prod_tax = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ total_active_prod,
                # Using the lag of total active production as an instrument for per capita property tax
                log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ l(total_active_prod,1))
                # Using total active mines as an instrument for per capita property tax
iv_coaln_tax = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ total_active_n,
                 # Using lag of total active mines as an instrument for per capita property tax
                 log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ l(total_active_n,1))


# Using total active production as an instrument for per capita property tax
iv_prod_gdp = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ total_active_prod,
                # Using the lag of total active production as an instrument for per capita property tax
                log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ l(total_active_prod,1))
# Using total active mines as an instrument for per capita property tax
iv_coaln_gdp = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ total_active_n,
                 # Using lag of total active mines as an instrument for per capita property tax
                 log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ l(total_active_n,1))

selected_base_models = c(log_real_Property_Tax_pp ~ log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc | unit + year,
                   # log_real_Elem_Educ_Total_Exp_pp ~ log_real_Property_Tax_pp + log_real_Total_Fed_IG_Revenue_pp + log_real_Total_State_IG_Revenue_pp | unit + year,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc | unit + year,
                   log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp + log_real_gdp_priv_ind_pc + l1_log_real_gdp_priv_ind_pc + l2_log_real_gdp_priv_ind_pc | unit + year,
                   log_real_Property_Tax_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp + l(total_active_prod, 0:1) | unit + year)

selected_iv_models = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ total_active_prod,
                    # Using the lag of total active production as an instrument for per capita property tax
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ l(total_active_prod,1),
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ total_active_prod,
                    # Using the lag of total active production as an instrument for per capita property tax
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ l(total_active_prod,1))


solar_iv_models = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_area,
                       log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_cap_ac,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_cap_dc,
                       log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_area,
                       log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_cap_ac,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_cap_dc)

solar_iv_models_pc = c(log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_area_pc,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_cap_ac_pc,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_Property_Tax_pp ~ pv_p_cap_dc_pc,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_area_pc,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_cap_ac_pc,
                    log_real_Elem_Educ_Total_Exp_pp ~ log_real_Total_State_IG_Revenue_pp + log_real_Total_Fed_IG_Revenue_pp | unit + year | log_real_gdp_total_pc ~ pv_p_cap_dc_pc)

