source(here('code/source_code/useful_functions.R'))
source(here('code/source_code/dicts.R'))
# Get the significant states from your state-by-state analysis
sig_states <- unique(results_df_cleaned_iv$state)

# Function to calculate conditional CZ leverage (accounting for controls)
get_conditional_cz_leverage <- function(state_code) {
  
  state_data <- df_ivs %>% filter(state == state_code)
  
  # Step 1: Regress spending on controls (without wage)
  spending_on_controls <- feols(
    log_real_Elem_Educ_Total_Exp_pp ~ 
      l1_log_real_Elem_Educ_Total_Exp_pp +
      log_real_Total_IG_Revenue_pp +
      log_real_gdp_priv_ind_pc +
      log_Enrollment + 
      pct_black + 
      pct_hispanic | 
      year,
    data = state_data
  )

  wage_on_controls <- feols(
    log_weighted_annual_avg_wkly_wage ~ 
    l1_log_weighted_annual_avg_wkly_wage + 
    lev_ss_2d + 
    l1_lev_ss_2d + l2_lev_ss_2d + 
    l1_log_real_Elem_Educ_Total_Exp_pp +
    log_real_Total_IG_Revenue_pp +
    log_real_gdp_priv_ind_pc +
    log_Enrollment + 
    pct_black + 
    pct_hispanic | 
    year,
    data = state_data
  )
  
  # Step 2: Get residuals for spending (NOT for wages - we use raw wage growth)
  state_data_resid <- state_data %>%
    filter(!is.na(log_real_Elem_Educ_Total_Exp_pp),
           !is.na(log_weighted_annual_avg_wkly_wage),
           !is.na(l1_log_real_Elem_Educ_Total_Exp_pp),
           !is.na(log_real_Total_IG_Revenue_pp),
           !is.na(log_real_gdp_priv_ind_pc),
           !is.na(log_Enrollment),
           !is.na(pct_black),
           !is.na(pct_hispanic))
  
  # Get residuals - spending NOT explained by controls
  spending_resid <- resid(spending_on_controls)
  wage_fitted <- fitted(wage_on_controls)  # For diagnostics, not used in main score
  
  # Create a clean dataset with residuals
  state_data_with_resid <- state_data_resid %>%
    mutate(
      spending_resid = as.numeric(spending_resid)#,
      #wage_fitted = as.numeric(wage_fitted)
    )
  
  # Step 3: Calculate CZ-level growth
  cz_conditional <- state_data_with_resid %>%
    arrange(unit, year) %>%
    group_by(unit) %>%
    summarize(
      cz_name = unique(cz_id),  # FIXED: use cz_name not cz_id
      state = unique(state),
      n_obs = n(),
      
      # UNCONDITIONAL wage growth (raw - this is what instrument captures)
      wage_growth_uncond = last(log_weighted_annual_avg_wkly_wage) - 
        first(log_weighted_annual_avg_wkly_wage),

      #fitted_wage = unique(fitted_wage),
      
      # CONDITIONAL spending growth (after removing controls)
      spending_growth_cond = last(spending_resid) - first(spending_resid),
      
      # Also keep unconditional spending for comparison
      spending_growth_uncond = last(log_real_Elem_Educ_Total_Exp_pp) - 
        first(log_real_Elem_Educ_Total_Exp_pp),
      
      # Control variable growth (to understand what's happening)
      ig_revenue_growth = last(log_real_Total_IG_Revenue_pp) - 
        first(log_real_Total_IG_Revenue_pp),
      gdp_growth = last(log_real_gdp_priv_ind_pc) - 
        first(log_real_gdp_priv_ind_pc),
      enrollment_growth = last(log_Enrollment) - first(log_Enrollment),
      
      # Means for context
      mean_enrollment = mean(log_Enrollment, na.rm = TRUE),
      mean_ig_revenue = mean(log_real_Total_IG_Revenue_pp, na.rm = TRUE),
      mean_gdp = mean(log_real_gdp_priv_ind_pc, na.rm = TRUE),
      
      .groups = "drop"
    ) %>%
    mutate(
      state = state_code,
      
      # Driver score: UNCONDITIONAL wage × CONDITIONAL spending
      driver_score_mixed = wage_growth_uncond * spending_growth_cond,
      
      # Also keep fully unconditional for comparison
      driver_score_uncond = wage_growth_uncond * spending_growth_uncond,
      
      # Magnitudes for ranking
      driver_magnitude_mixed = abs(driver_score_mixed),
      driver_magnitude_uncond = abs(driver_score_uncond),
      sig_state = state %in% sig_states
    )
  
  cz_conditional$state_name <- sapply(cz_conditional$state, get_state)
  
  return(cz_conditional)
}

# Run for all significant states
cat("Calculating conditional leverage for", length(sig_states), "states...\n")
cz_conditional_all <- map_dfr(reg_states, get_conditional_cz_leverage)

# Quick diagnostic
diagnostic <- cz_conditional_all %>%
  summarize(
    mean_diff_spending = mean(abs(spending_growth_uncond - spending_growth_cond), na.rm = TRUE)
  )

cat("\n**Diagnostic Check:**\n")
cat("Mean absolute difference in spending growth:", round(diagnostic$mean_diff_spending, 3), "\n\n")

if (diagnostic$mean_diff_spending < 0.01) {
  cat("⚠️ WARNING: Conditional and unconditional spending values are nearly identical.\n")
  cat("This suggests the residualization did not work properly.\n\n")
}

stopifnot("Wrong state-level results data frame passed to heterogeneity_analysis.R" = results_df %>% filter(state %in% sig_states) %>% select(state, estimate, ftest, r2_within) %>% arrange(state) %>% identical(arrange(select(results_df_cleaned_iv, state, estimate, ftest, r2_within), state)))

# Calculate state-level correlations
state_correlations <- cz_conditional_all %>%
  group_by(state, state_name) %>%
  summarize(
    n_czs = n(),
    # Fully unconditional
    cor_uncond = cor(wage_growth_uncond, spending_growth_uncond, use = "complete.obs"),
    # Mixed: unconditional wage × conditional spending (THIS IS THE KEY ONE)
    cor_mixed_cond = cor(wage_growth_uncond, spending_growth_cond, use = "complete.obs"),
    # Change
    cor_change_mixed = cor_mixed_cond - cor_uncond,
    sig_state = unique(sig_state),
    .groups = "drop"
  ) %>%
  left_join(
    results_df %>% select(state, estimate, ftest, r2_within),
    by = "state"
  ) %>%
  arrange(desc(abs(estimate)))

# Identify top drivers using MIXED conditional scores
top_drivers_cond <- cz_conditional_all %>%
  group_by(state) %>%
  mutate(
    rank_within_state = rank(-driver_magnitude_mixed),
    top_5 = rank_within_state <= 5,
    state_name = unique(state_name)
  ) %>%
  ungroup()

# Define mechanism classification thresholds
AMPLIFICATION_THRESHOLD <- 0.2   # Positive correlation ≥ 0.2
INSULATION_THRESHOLD <- 0.2      # Absolute correlation < 0.2
# Equalization: correlation < -0.2

#==============================================================================
# TABLE 1: Mechanism Typology Summary
#==============================================================================

mechanism_typology <- state_correlations %>%
  # Exclude Oregon from IV estimates (but keep for typology)
  mutate(
    # Classify mechanism based on conditional correlation
    mechanism = case_when(
      cor_mixed_cond >= AMPLIFICATION_THRESHOLD ~ "Amplification",
      abs(cor_mixed_cond) < INSULATION_THRESHOLD ~ "Insulation",
      cor_mixed_cond <= -AMPLIFICATION_THRESHOLD ~ "Equalization",
      TRUE ~ "Mixed"
    ),
    mechanism = factor(mechanism, 
                       levels = c("Amplification", "Insulation", "Equalization")),
    
    # Equity implications
    equity_effect = case_when(
      mechanism == "Amplification" ~ "Reinforces inequality",
      mechanism == "Insulation" ~ "Neutral/Stabilizing",
      mechanism == "Equalization" ~ "Redistributive"
    ),
    
    # Flag Oregon for exclusion from IV
    note = ifelse(state == "OR", "Excluded from main IV results", "")
  ) %>%
  arrange(mechanism, desc(cor_mixed_cond)) %>%
  select(state, state_name, sig_state, n_czs, estimate, ftest, cor_mixed_cond, 
         mechanism, equity_effect, note)

mechanism_typ_for_kable <- select(filter(mechanism_typology, sig_state), -c(sig_state, state))

# Create the table
cat("\n## Table 1: Typology of State Fiscal Transmission Mechanisms\n\n")

table1 <- kable(
  mechanism_typ_for_kable,
  caption = "Classification of States by Wage-Spending Transmission Mechanism",
  digits = 3,
  col.names = c("State", "# CZs", "IV Estimate", "F-statistic", 
                "Conditional ρ", "Mechanism", "Equity Effect", "Notes"),
  align = c("l", "c", "c", "c", "c", "l", "l", "l")
) %>%
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE,
    font_size = 11
  ) %>%
  column_spec(5, bold = TRUE) %>%
  column_spec(6, bold = TRUE) %>%
  pack_rows(
    "Amplification: Wage Growth → Spending Growth (Reinforces Inequality)", 
    min(which(mechanism_typ_for_kable$mechanism == "Amplification")),
    max(which(mechanism_typ_for_kable$mechanism == "Amplification")),
    label_row_css = "background-color: #ffe6e6; font-weight: bold;"
  ) %>%
  pack_rows(
    "Insulation: Spending Buffered from Wage Fluctuations (Stabilizing)", 
    min(which(mechanism_typ_for_kable$mechanism == "Insulation")),
    max(which(mechanism_typ_for_kable$mechanism == "Insulation")),
    label_row_css = "background-color: #fff4e6; font-weight: bold;"
  ) %>%
  pack_rows(
    "Equalization: Wage Growth → Spending Decline (Redistributive)", 
    min(which(mechanism_typ_for_kable$mechanism == "Equalization")),
    max(which(mechanism_typ_for_kable$mechanism == "Equalization")),
    label_row_css = "background-color: #e6f4e6; font-weight: bold;"
  ) %>%
  footnote(
    general = "Conditional ρ measures correlation between wage growth and residual spending growth (after controls). Mechanisms classified as: Amplification (ρ ≥ 0.2), Insulation (|ρ| < 0.2), Equalization (ρ ≤ -0.2). Oregon excluded from IV results due to sign inconsistencies (see Appendix).",
    general_title = "Note:",
    footnote_as_chunk = TRUE
  )

print(table1)
table1

#==============================================================================
# FIGURE 1: Mechanism Classification Scatter
#==============================================================================

cat("\n## Figure 1: Three Fiscal Transmission Mechanisms\n\n")

# Add mechanism to state_correlations
state_mech <- state_correlations %>%
  mutate(
    mechanism = case_when(
      cor_mixed_cond >= AMPLIFICATION_THRESHOLD ~ "Amplification",
      abs(cor_mixed_cond) < INSULATION_THRESHOLD ~ "Insulation",
      cor_mixed_cond <= -AMPLIFICATION_THRESHOLD ~ "Equalization",
      TRUE ~ "Mixed"
    ),
    mechanism = factor(mechanism, 
                       levels = c("Amplification", "Insulation", "Equalization")),
    exclude_oregon = state == "OR"
  )

fig1 <- ggplot(state_mech, aes(x = cor_mixed_cond, y = estimate)) +
  # Shaded regions for mechanisms
  annotate("rect", xmin = AMPLIFICATION_THRESHOLD, xmax = Inf, 
           ymin = -Inf, ymax = Inf, fill = "#ffe6e6", alpha = 0.3) +
  annotate("rect", xmin = -INSULATION_THRESHOLD, xmax = INSULATION_THRESHOLD, 
           ymin = -Inf, ymax = Inf, fill = "#fff4e6", alpha = 0.3) +
  annotate("rect", xmin = -Inf, xmax = -AMPLIFICATION_THRESHOLD, 
           ymin = -Inf, ymax = Inf, fill = "#e6f4e6", alpha = 0.3) +
  
  # Reference lines
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.5) +
  geom_vline(xintercept = c(-INSULATION_THRESHOLD, INSULATION_THRESHOLD), 
             linetype = "dotted", alpha = 0.3) +
  
  # Points - Oregon marked differently
  geom_point(
    data = state_mech, #%>% filter(!exclude_oregon),
    aes(size = ftest, color = mechanism),
    alpha = 0.8
  ) +
  geom_point(
    data = state_mech %>% filter(!sig_state),
    aes(size = ftest),
    color = "black",
    shape = 4,  # X shape for excluded
    stroke = 2,
    alpha = 0.8
  ) +
  
  # Labels
  geom_text_repel(
    aes(label = state_name, color = mechanism),
    size = 3.5,
    fontface = "bold",
    box.padding = 0.5,
    max.overlaps = 20,
    segment.color = "gray50",
    segment.size = 0.3
  ) +
  
  # Colors
  scale_color_manual(
    values = c(
      "Amplification" = "#d7191c",
      "Insulation" = "#fdae61",
      "Equalization" = "#2c7bb6"
    ),
    name = "Mechanism Type",
    labels = c(
      "Amplification\n(Reinforces Inequality)",
      "Insulation\n(Stabilizing)",
      "Equalization\n(Redistributive)"
    )
  ) +
  scale_size_continuous(range = c(4, 10), guide = "none") +
  
  # Labels
  labs(
    title = "Three Mechanisms of Fiscal Transmission from Wages to Education Spending",
    subtitle = str_wrap("States classified by conditional correlation (ρ) between wage growth and residual spending growth. Shaded regions show mechanism types. Oregon (×) excluded from main IV analysis due to diagnostic failures.", 110),
    x = "Conditional Correlation (ρ): Wage Growth → Residual Spending Growth",
    y = "IV Estimate (Effect of 1% Wage Increase on Spending)",
    caption = "Note: Amplification (ρ ≥ 0.2): local prosperity increases local spending, reinforcing inequality.\nInsulation (|ρ| < 0.2): state systems buffer spending from wage shocks.\nEqualization (ρ ≤ -0.2): high-wage areas subsidize low-wage areas."
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 10, color = "gray30", margin = margin(b = 10)),
    legend.position = "right",
    legend.title = element_text(face = "bold", size = 10),
    legend.text = element_text(size = 9),
    panel.grid.minor = element_blank(),
    plot.caption = element_text(hjust = 0, size = 9, color = "gray50", margin = margin(t = 10))
  ) + 
  common_theme

print(fig1)
ggsave(here("output/fig_mechanism_typology.png"), fig1, width = 12, height = 8, dpi = 300)

#==============================================================================
# FIGURE 2: Within-State Patterns by Mechanism Type
#==============================================================================

cat("\n## Figure 2: CZ-Level Patterns by Mechanism Type\n\n")

# Prepare full data with top 5 flagged
driver_plot_data <- cz_conditional_all %>%
  # Add top 5 flag
  group_by(state) %>%
  mutate(
    rank_within_state = rank(-driver_magnitude_mixed),
    is_top_5 = rank_within_state <= 5,
    is_top_driver = rank_within_state == 1
  ) %>%
  ungroup() %>%
  # Add state-level info
  left_join(
    state_correlations %>% select(state, cor_mixed_cond),
    by = "state"
  ) %>%
  mutate(
    # Classify states by mechanism for coloring
    mechanism = case_when(
      cor_mixed_cond < 0 ~ "Anomalous",
      cor_mixed_cond >= 0.5 ~ "Strong",
      cor_mixed_cond >= 0.3 ~ "Moderate",
      TRUE ~ "Weak"
    ),
    mechanism = factor(mechanism, levels = c("Strong", "Moderate", "Weak", "Anomalous")),
    # Create state label with correlation
    state_label = paste0(state_name, " (Corr.Coef.=", round(cor_mixed_cond, 2), ")")
  ) %>%
  # Order states by conditional correlation for faceting
  arrange(desc(cor_mixed_cond)) %>%
  mutate(
    state_label = fct_reorder(state_label, cor_mixed_cond, .desc = TRUE)
  )

# Add mechanism classification to CZ data
fig2_data <- driver_plot_data %>%
  mutate(
    mechanism = case_when(
      cor_mixed_cond >= AMPLIFICATION_THRESHOLD ~ "Amplification",
      abs(cor_mixed_cond) < INSULATION_THRESHOLD ~ "Insulation",
      cor_mixed_cond <= -AMPLIFICATION_THRESHOLD ~ "Equalization",
      TRUE ~ "Mixed"
    ),
    mechanism = factor(mechanism, 
                       levels = c("Amplification", "Insulation", "Equalization")),
    state_label = paste0(state_name, " (ρ=", round(cor_mixed_cond, 2), ")")
  ) %>%
  arrange(desc(cor_mixed_cond)) %>%
  mutate(state_label = fct_reorder(state_label, cor_mixed_cond, .desc = TRUE))

# One row per significant state label — used to draw bold panel borders below
sig_border_data <- fig2_data %>%
  filter(sig_state) %>%
  select(state_label) %>%
  distinct()

fig2 <- ggplot(fig2_data, aes(x = wage_growth_uncond, y = spending_growth_cond)) +
  # Bold border for significant-state panels (drawn first, behind data)
  geom_rect(
    data        = sig_border_data,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill        = NA,
    color       = "black",
    linewidth   = 0.9,
    inherit.aes = FALSE
  ) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3, linewidth = 0.3) +
  #geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3, linewidth = 0.3) +
  
  # All CZs in grey
  geom_point(
    data = fig2_data %>% filter(!is_top_5),
    color = "grey70",
    size = 1.5,
    alpha = 0.4
  ) +
  
  # Top 5 drivers colored by mechanism
  geom_point(
    data = fig2_data %>% filter(is_top_5),
    aes(size = abs(driver_score_mixed), color = mechanism),
    alpha = 0.8
  ) +
  
  # Trend line colored by mechanism
  geom_smooth(
    method = "lm",
    se = FALSE,
    aes(color = mechanism),
    linewidth = 0.8,
    linetype = "solid"
  ) +
  
  # Label top driver only
  geom_text_repel(
    data = fig2_data %>% filter(is_top_driver),
    aes(label = cz_name, color = mechanism),
    size = 2.5,
    fontface = "bold",
    max.overlaps = 10,
    box.padding = 0.3,
    segment.size = 0.3
  ) +
  
  # Facet by state
  facet_wrap(~state_label, scales = "free", ncol = 4) +
  
  # Colors by mechanism
  scale_color_manual(
    values = c(
      "Amplification" = "#d7191c",
      "Insulation" = "#fdae61",
      "Equalization" = "#2c7bb6"
    ),
    name = "Mechanism"
  ) +
  scale_size_continuous(range = c(2, 6), guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  
  # Labels
  labs(
    title = "Within-State Wage-Spending Patterns by Mechanism Type",
    subtitle = str_wrap("Grey = all CZs; Colored = top 5 drivers. Amplification (red) shows positive slopes; Insulation (orange) shows flat slopes; Equalization (blue) shows negative slopes. States ordered by correlation strength.", 130),
    x = "Wage Growth (log points)",
    y = "Residual Spending Growth (log points, after controls)",
    caption = "Note: Top driver CZ labeled. Amplification reinforces inequality; Insulation stabilizes spending; Equalization redistributes from rich to poor areas."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    plot.subtitle = element_text(size = 9, color = "gray30", margin = margin(b = 10)),
    strip.text = element_text(face = "bold", size = 8),
    strip.background = element_rect(fill = "gray95", color = NA),
    legend.position = "bottom",
    legend.title = element_text(face = "bold", size = 9),
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.8, "lines"),
    plot.caption = element_text(hjust = 0, size = 8, color = "gray50", margin = margin(t = 10))
  )  + 
  common_theme

print(fig2)
ggsave(here("output/fig_mechanism_patterns.png"), fig2, width = 10, height = 16, dpi = 300)

#==============================================================================
# SUMMARY STATISTICS BY MECHANISM
#==============================================================================

cat("\n## Summary Statistics by Mechanism Type\n\n")

mechanism_summary <- mechanism_typology %>%
  group_by(mechanism) %>%
  summarize(
    n_states = n(),
    mean_iv_estimate = mean(estimate, na.rm = TRUE),
    sd_iv_estimate = sd(estimate, na.rm = TRUE),
    mean_correlation = mean(cor_mixed_cond, na.rm = TRUE),
    min_correlation = min(cor_mixed_cond, na.rm = TRUE),
    max_correlation = max(cor_mixed_cond, na.rm = TRUE),
    .groups = "drop"
  )

kable(
  mechanism_summary,
  caption = "Summary Statistics by Mechanism Type",
  digits = 3,
  col.names = c("Mechanism", "# States", "Mean IV", "SD IV", 
                "Mean ρ", "Min ρ", "Max ρ")
) %>%
  kable_styling(bootstrap_options = c("striped", "hover"))

# Save outputs
write_csv(mechanism_typology, here("output/mechanism_typology.csv"))
write_csv(mechanism_summary, here("output/mechanism_summary.csv"))

#```
