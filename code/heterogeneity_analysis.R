source(here('code/source_code/useful_functions.R'))
source(here('code/source_code/dicts.R'))
# Get the significant states from your state-by-state analysis
sig_states <- unique(results_df_cleaned_iv$state)

# Function to calculate conditional CZ leverage (accounting for controls)
get_conditional_cz_leverage <- function(state_code) {

  # Consistent sample across both regressions: all variables needed
  state_data_clean <- df_ivs %>%
    filter(state == state_code) %>%
    filter(
      !is.na(log_real_Elem_Educ_Total_Exp_pp),
      !is.na(l1_log_real_Elem_Educ_Total_Exp_pp),
      !is.na(log_weighted_annual_avg_wkly_wage),
      !is.na(l1_log_weighted_annual_avg_wkly_wage),
      !is.na(log_real_Total_IG_Revenue_pp),
      !is.na(log_real_gdp_priv_ind_pc),
      !is.na(log_Enrollment),
      !is.na(pct_black),
      !is.na(pct_hispanic),
      !is.na(l1_lev_gdp_ss_2d),
      !is.na(l2_lev_gdp_ss_2d)
    )

  # Step 1a: First stage — matches main IV spec exactly
  # log_wage ~ l1_log_wage + l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d + controls | unit + year
  wage_first_stage <- feols(
    log_weighted_annual_avg_wkly_wage ~
      l1_log_weighted_annual_avg_wkly_wage +
      l1_lev_gdp_ss_2d + l2_lev_gdp_ss_2d +
      l1_log_real_Elem_Educ_Total_Exp_pp +
      log_real_Total_IG_Revenue_pp +
      log_real_gdp_priv_ind_pc +
      log_Enrollment +
      pct_black +
      pct_hispanic |
      unit + year,
    data = state_data_clean
  )

  # Step 1b: Spending residualised on controls — matches main IV spec, unit + year FEs
  spending_on_controls <- feols(
    log_real_Elem_Educ_Total_Exp_pp ~
      l1_log_real_Elem_Educ_Total_Exp_pp +
      log_real_Total_IG_Revenue_pp +
      log_real_gdp_priv_ind_pc +
      log_Enrollment +
      pct_black +
      pct_hispanic |
      unit + year,
    data = state_data_clean
  )

  # Step 2: Attach fitted wages and spending net of controls
  # resid() strips FEs; fitted() includes them — add FEs back to spending residuals
  # so both axes reflect the same variation (spending after removing Xβ, keeping FEs)
  spending_fes <- fixef(spending_on_controls)
  unit_fe_vec  <- spending_fes$unit[as.character(state_data_clean$unit)]
  year_fe_vec  <- spending_fes$year[as.character(state_data_clean$year)]

  state_data_with_resid <- state_data_clean %>%
    mutate(
      wage_fitted    = as.numeric(fitted(wage_first_stage)),
      spending_resid = as.numeric(resid(spending_on_controls)) + unit_fe_vec + year_fe_vec
    )

  # Step 3: CZ-level summary (for state correlations, mechanism typology, existing figs)
  cz_conditional <- state_data_with_resid %>%
    arrange(unit, year) %>%
    group_by(unit) %>%
    summarize(
      cz_name = unique(cz_id),
      state   = unique(state),
      n_obs   = n(),

      # Fitted wage growth (exogenous component from first stage)
      wage_growth_fitted = last(wage_fitted) - first(wage_fitted),

      # Raw wage growth kept for comparison
      wage_growth_uncond = last(log_weighted_annual_avg_wkly_wage) -
        first(log_weighted_annual_avg_wkly_wage),

      # Conditional spending growth (residualised on controls)
      spending_growth_cond   = last(spending_resid) - first(spending_resid),
      spending_growth_uncond = last(log_real_Elem_Educ_Total_Exp_pp) -
        first(log_real_Elem_Educ_Total_Exp_pp),

      # Control variable growth
      ig_revenue_growth = last(log_real_Total_IG_Revenue_pp) - first(log_real_Total_IG_Revenue_pp),
      gdp_growth        = last(log_real_gdp_priv_ind_pc)     - first(log_real_gdp_priv_ind_pc),
      enrollment_growth = last(log_Enrollment)               - first(log_Enrollment),

      mean_enrollment = mean(log_Enrollment,              na.rm = TRUE),
      mean_ig_revenue = mean(log_real_Total_IG_Revenue_pp, na.rm = TRUE),
      mean_gdp        = mean(log_real_gdp_priv_ind_pc,     na.rm = TRUE),

      .groups = "drop"
    ) %>%
    mutate(
      state = state_code,

      # Driver score: fitted wage (exogenous) × conditional spending
      driver_score_mixed      = wage_growth_fitted * spending_growth_cond,
      driver_score_uncond     = wage_growth_uncond * spending_growth_uncond,
      driver_magnitude_mixed  = abs(driver_score_mixed),
      driver_magnitude_uncond = abs(driver_score_uncond),
      sig_state = state %in% sig_states
    )

  cz_conditional$state_name <- sapply(cz_conditional$state, get_state)

  return(list(
    cz_summary = cz_conditional,
    cz_panel   = state_data_with_resid %>% mutate(state_name = get_state(state_code))
  ))
}

# Run for all significant states
cat("Calculating conditional leverage for", length(sig_states), "states...\n")
results_list    <- purrr::map(reg_states, get_conditional_cz_leverage)
cz_conditional_all <- purrr::map_dfr(results_list, "cz_summary")
cz_panel_all       <- purrr::map_dfr(results_list, "cz_panel")

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
    # Key: fitted wage (exogenous) × conditional spending
    cor_mixed_cond = cor(wage_growth_fitted,  spending_growth_cond,   use = "complete.obs"),
    # Fully unconditional for comparison
    cor_uncond     = cor(wage_growth_uncond,  spending_growth_uncond, use = "complete.obs"),
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

#==============================================================================
# FIGURES A–C: Three Views of the Wage–Spending Relationship
#==============================================================================

# Shared state ordering: mechanism first, then by correlation within mechanism
state_order <- mechanism_typology %>%
  arrange(mechanism, desc(cor_mixed_cond)) %>%
  mutate(state_label = paste0(state_name, " (ρ=", round(cor_mixed_cond, 2), ")")) %>%
  select(state, state_name, state_label, mechanism, cor_mixed_cond, sig_state)

# Shared sig-state border data (used in all three figures)
sig_border_data_abc <- state_order %>%
  filter(sig_state) %>%
  select(state_label) %>%
  distinct()

# State-year means of first-stage fitted wages and residualised spending (used in all three figures)
state_panel <- cz_panel_all %>%
  group_by(state, year) %>%
  summarize(
    mean_wage_fitted    = mean(wage_fitted,    na.rm = TRUE),
    mean_spending_resid = mean(spending_resid, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(state_order, by = "state") %>%
  group_by(state) %>%
  arrange(year) %>%
  mutate(
    d_wage_fitted    = mean_wage_fitted    - lag(mean_wage_fitted),
    d_spending_resid = mean_spending_resid - lag(mean_spending_resid)
  ) %>%
  ungroup() %>%
  mutate(state_label = fct_reorder(state_label, cor_mixed_cond, .desc = TRUE))

#------------------------------------------------------------------------------
# FIGURE A: Indexed time series — wages and spending indexed to 0 at start
#------------------------------------------------------------------------------

fig_a_data <- state_panel %>%
  filter(!is.na(d_wage_fitted), !is.na(d_spending_resid)) %>%
  pivot_longer(
    cols      = c(d_wage_fitted, d_spending_resid),
    names_to  = "variable",
    values_to = "value"
  ) %>%
  mutate(variable = recode(variable,
    "d_wage_fitted"    = "Fitted wages (first stage)",
    "d_spending_resid" = "Residual spending"
  ))

fig_a <- ggplot(fig_a_data, aes(x = year, y = value, color = variable, linetype = variable)) +
  geom_rect(
    data        = sig_border_data_abc,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill        = NA, color = "black", linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3, linewidth = 0.3) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~state_label, ncol = 4) +
  scale_color_manual(
    values = c("Fitted wages (first stage)" = "#d7191c", "Residual spending" = "#2c7bb6"),
    name   = NULL
  ) +
  scale_linetype_manual(
    values = c("Fitted wages (first stage)" = "solid", "Residual spending" = "dashed"),
    name   = NULL
  ) +
  labs(
    title    = "Wage and Spending Growth Over Time by State",
    subtitle = "Year-on-year changes in first-stage fitted wages and residualised spending. States ordered by wage–spending correlation (ρ).",
    x        = NULL,
    y        = "Annual change in log points",
    caption  = "Solid red = fitted wages (first stage); dashed blue = residual spending. Bold border = state included in main IV results."
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    strip.text    = element_text(face = "bold", size = 8),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.8, "lines")
  ) +
  common_theme

print(fig_a)
ggsave(here("output/fig_wage_spending_indexed_ts.png"), fig_a, width = 12, height = 16, dpi = 300)

#------------------------------------------------------------------------------
# FIGURE B: Annual-change scatter — year-on-year changes, one dot per CZ-year
#------------------------------------------------------------------------------

cz_level <- cz_panel_all %>%
  filter(!is.na(wage_fitted), !is.na(spending_resid)) %>%
  left_join(state_order, by = "state") %>%
  mutate(state_label = fct_reorder(state_label, cor_mixed_cond, .desc = TRUE))

fig_b <- ggplot(cz_level, aes(x = wage_fitted, y = spending_resid)) +
  geom_rect(
    data        = sig_border_data_abc,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill        = NA, color = "black", linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  geom_point(color = "grey60", size = 0.8, alpha = 0.35) +
  geom_smooth(method = "lm", se = FALSE, color = "#2c3e50", linewidth = 0.9) +
  facet_wrap(~state_label, scales = "free", ncol = 4) +
  labs(
    title    = "First-Stage Fitted Wages and Residual Spending by State",
    subtitle = "Each point = one CZ-year. Line = OLS fit (slope = IV pass-through). Bold border = included in main IV results.",
    x        = "First-stage fitted wages (log)",
    y        = "Residual spending — net of controls (log)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    strip.text    = element_text(face = "bold", size = 8),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.8, "lines")
  ) +
  common_theme

print(fig_b)
ggsave(here("output/fig_wage_spending_annual_changes.png"), fig_b, width = 12, height = 16, dpi = 300)

#------------------------------------------------------------------------------
# FIGURE C: Connected scatter (phase plot) — trajectory through wage–spending space
#------------------------------------------------------------------------------

# Label only first and last year per state for readability
year_labels_c <- state_panel %>%
  filter(!is.na(mean_wage_fitted), !is.na(mean_spending_resid)) %>%
  group_by(state) %>%
  filter(year == min(year) | year == max(year)) %>%
  ungroup()

fig_c <- ggplot(state_panel %>% filter(!is.na(mean_wage_fitted), !is.na(mean_spending_resid)),
               aes(x = mean_wage_fitted, y = mean_spending_resid)) +
  geom_rect(
    data        = sig_border_data_abc,
    aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf),
    fill        = NA, color = "black", linewidth = 0.9,
    inherit.aes = FALSE
  ) +
  geom_path(aes(color = year), linewidth = 0.9) +
  geom_point(aes(color = year), size = 1.5) +
  geom_text(
    data    = year_labels_c,
    aes(label = year),
    size    = 2.5,
    vjust   = -0.8,
    color   = "grey30"
  ) +
  facet_wrap(~state_label, scales = "free", ncol = 4) +
  scale_color_viridis_c(
    name   = "Year",
    option = "plasma",
    breaks = c(min(state_panel$year, na.rm = TRUE), max(state_panel$year, na.rm = TRUE))
  ) +
  labs(
    title    = "Wage–Spending Trajectories Over Time by State",
    subtitle = "Each point = state-year mean. Path connects years chronologically. x = first-stage fitted wages; y = spending net of controls.",
    x        = "First-stage fitted wages (log)",
    y        = "Residual spending — net of controls (log)"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title    = element_text(face = "bold", size = 13),
    strip.text    = element_text(face = "bold", size = 8),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.spacing = unit(0.8, "lines")
  ) +
  common_theme

print(fig_c)
ggsave(here("output/fig_wage_spending_phase.png"), fig_c, width = 12, height = 16, dpi = 300)


#==============================================================================
# FIGURE D: Industry Decomposition of Shift-Share Variance
# Shows which industries drive cross-CZ dispersion of the instrument by state
# Two-category design: "Cyclical/commodity" vs "Other"
#==============================================================================

# Industries to highlight: cyclical, commodity, or locally volatile sectors
# (Agriculture, Mining/Natural Resources, Utilities, Construction,
#  Wholesale Trade, Accommodation/Food Services)
CYCLICAL_INDS <- c("11", "21", "22", "23", "42", "72")

# ── 1. Build cross-CZ shift-share variance decomposition ─────────────────────
# Shift-share IV = sum_k (share_{l,k} * Delta log_natl_wage_k)
# Cross-CZ variance decomposes as: sum_k g_k^2 * Var_l(z_{l,k})
# where g_k = national log wage change in industry k, z_{l,k} = base-year share

# Identify columns
share_cols_all <- grep("^share_annual_avg_emplvl_",       names(ss_temp_filled), value = TRUE)
lwage_cols_all <- grep("^log_natl_annual_avg_wkly_wage_", names(ss_temp_filled), value = TRUE)

strip_prefix <- function(x, pfx) stringr::str_remove(x, paste0("^", pfx))

common_ind_codes <- intersect(
  strip_prefix(share_cols_all, "share_annual_avg_emplvl_"),
  strip_prefix(lwage_cols_all, "log_natl_annual_avg_wkly_wage_")
)

# Base-year employment shares per CZ-state (earliest available year)
base_year_shares <- ss_temp_filled %>%
  group_by(cz_id) %>%
  filter(year == min(year)) %>%
  ungroup() %>%
  select(cz_id, state,
         all_of(paste0("share_annual_avg_emplvl_",       common_ind_codes))) %>%
  pivot_longer(
    cols      = all_of(paste0("share_annual_avg_emplvl_", common_ind_codes)),
    names_to  = "var",
    values_to = "base_share"
  ) %>%
  mutate(ind_code = strip_prefix(var, "share_annual_avg_emplvl_")) %>%
  select(-var)

# National log-wage growth (last minus first year) per industry
natl_wage_growth <- ss_temp_filled %>%
  select(year, all_of(paste0("log_natl_annual_avg_wkly_wage_", common_ind_codes))) %>%
  pivot_longer(
    cols      = all_of(paste0("log_natl_annual_avg_wkly_wage_", common_ind_codes)),
    names_to  = "var",
    values_to = "log_natl_wage"
  ) %>%
  mutate(ind_code = strip_prefix(var, "log_natl_annual_avg_wkly_wage_")) %>%
  select(-var) %>%
  group_by(ind_code) %>%
  summarize(
    g_k = last(log_natl_wage[order(year)]) - first(log_natl_wage[order(year)]),
    .groups = "drop"
  )

# Per-state: industry contribution = g_k^2 * Var_l(z_{l,k}) across CZs in state
share_var <- base_year_shares %>%
  left_join(natl_wage_growth, by = "ind_code") %>%
  filter(!is.na(base_share), !is.na(g_k)) %>%
  group_by(state, ind_code) %>%
  summarize(
    var_share       = var(base_share, na.rm = TRUE),
    g_k             = unique(g_k),
    ind_contribution = g_k^2 * coalesce(var_share, 0),
    .groups         = "drop"
  )

# State-level cross-CZ SD of the full shift-share
ss_variance <- base_year_shares %>%
  left_join(natl_wage_growth, by = "ind_code") %>%
  filter(!is.na(base_share), !is.na(g_k)) %>%
  group_by(state, cz_id) %>%
  summarize(
    ss_value = sum(g_k * base_share, na.rm = TRUE),
    .groups  = "drop"
  ) %>%
  group_by(state) %>%
  summarize(
    mean_sd_cross_cz = sd(ss_value, na.rm = TRUE),
    .groups          = "drop"
  ) %>%
  mutate(sig_state = state %in% sig_states)

# ── 2. Attach industry labels and two-category flag ──────────────────────────

# industry_mapping must be in scope (tribble with industry_code, category, broad_category)
variance_decomp <- share_var %>%
  group_by(state) %>%
  mutate(prop = ind_contribution / sum(ind_contribution, na.rm = TRUE)) %>%
  ungroup() %>%
  left_join(
    industry_mapping %>% select(industry_code, category, broad_category),
    by = c("ind_code" = "industry_code")
  ) %>%
  mutate(
    ind_label = coalesce(category, paste0("Ind. ", ind_code)),
    # Two-category grouping: cyclical/commodity vs everything else
    ind_group = if_else(
      ind_code %in% CYCLICAL_INDS,
      "Cyclical / commodity",
      "Other industries"
    )
  ) %>%
  left_join(ss_variance %>% select(state, mean_sd_cross_cz, sig_state), by = "state") %>%
  mutate(
    state_name = sapply(state, get_state),
    state_name = fct_reorder(state_name, mean_sd_cross_cz)
  )

# Within each bar, show the cyclical share labelled by its constituent industries
# Compute proportion of the BAR that falls in each group
group_props <- variance_decomp %>%
  group_by(state, state_name, mean_sd_cross_cz, sig_state, ind_group) %>%
  summarize(group_prop = sum(prop, na.rm = TRUE), .groups = "drop") %>%
  mutate(state_name = fct_reorder(state_name, mean_sd_cross_cz))

# Label just the cyclical share percentage inside each bar
bar_labels <- group_props %>%
  filter(ind_group == "Cyclical / commodity") %>%
  mutate(label = scales::percent(group_prop, accuracy = 1))

# ── 3. Plot ───────────────────────────────────────────────────────────────────

fig_d_cols <- c(
  "Cyclical / commodity" = "#e07b39",   # warm amber
  "Other industries"     = "grey80"
)

fig_d <- ggplot(group_props,
                aes(x = state_name, y = group_prop, fill = ind_group)) +
  geom_col(
    aes(color = sig_state),
    width    = 0.75,
    position = "stack"
  ) +
  geom_text(
    data    = bar_labels,
    aes(x = state_name, y = group_prop / 2, label = label),
    inherit.aes = FALSE,
    size    = 2.8,
    color   = "white",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = fig_d_cols,
    name   = "Industry group"
  ) +
  scale_color_manual(
    values = c(`TRUE` = "#1a1a2e", `FALSE` = NA),
    guide  = "none"
  ) +
  scale_y_continuous(labels = scales::percent_format(), expand = expansion(mult = c(0, 0.03))) +
  coord_flip() +
  labs(
    title    = "Share of Shift-Share Instrument Variance Driven by Cyclical / Commodity Industries",
    subtitle = str_wrap(
      "Each bar = one state's cross-CZ dispersion decomposed by industry.
       Cyclical/commodity = Agriculture, Mining, Utilities, Construction, Wholesale Trade, Hospitality.
       States ordered by total instrument dispersion (low → high). Bold border = significant IV state.",
      100
    ),
    x        = NULL,
    y        = "Share of cross-CZ variance explained",
    caption  = "Cyclical/commodity industries (NAICS 11, 21, 22, 23, 42, 72) in amber; all other industries in grey.\nBar percentages show cyclical share within each state."
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(face = "bold", size = 12),
    plot.subtitle   = element_text(size = 9, color = "grey30", margin = margin(b = 8)),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    plot.caption    = element_text(hjust = 0, size = 8, color = "grey50", margin = margin(t = 8))
  ) +
  common_theme

print(fig_d)
ggsave(here("output/fig_industry_variance_decomp.png"), fig_d, width = 10, height = 9, dpi = 300)

# ── 4. Companion table: cyclical share by state ───────────────────────────────
cyclical_share_tbl <- group_props %>%
  filter(ind_group == "Cyclical / commodity") %>%
  left_join(ss_variance %>% select(state, mean_sd_cross_cz), by = c("state_name" = "state_name"),
            relationship = "many-to-many") %>%
  select(state_name, sig_state, group_prop, mean_sd_cross_cz) %>%
  arrange(desc(group_prop)) %>%
  rename(
    State              = state_name,
    `Sig. IV state`    = sig_state,
    `Cyclical share`   = group_prop,
    `Instrument SD (cross-CZ)` = mean_sd_cross_cz
  )

print(kable(cyclical_share_tbl, digits = 3,
            caption = "Cyclical/commodity share of shift-share variance by state") %>%
        kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = FALSE))
