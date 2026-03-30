#==============================================================================
# INDUSTRY COMPOSITION ANALYSIS: INTEGRATION WITH MECHANISM TYPOLOGY
#==============================================================================

library(tidyverse)
library(fixest)
library(kableExtra)
library(ggrepel)
library(patchwork)
library(here)

#==============================================================================
# STEP 1: Load Industry Share Data and Calculate Composition Metrics
#==============================================================================

# # Load the shift-share data with industry employment shares
ss_temp_filled <- readRDS(here("code/ss_cache_manual/ss_temp_filled.RDS"))  # Adjust path as needed

# Industry codes and names
industry_mapping <- tribble(
  ~industry_code, ~industry_name, ~category, ~broad_category,
  "11", "Agriculture, Forestry, Fishing, and Hunting", "Agriculture", "Primary",
  "21", "Mining", "Natural Resources", "Primary",
  "22", "Utilities", "Utilities", "Utilities",
  "23", "Construction", "Construction", "Construction",
  "31_33", "Manufacturing", "Manufacturing", "Manufacturing",
  "42", "Wholesale Trade", "Wholesale Trade", "Trade",
  "44_45", "Retail Trade", "Retail Trade", "Trade",
  "48_49", "Transportation and Warehousing", "Transportation", "Transportation",
  "51", "Information", "Information", "Information",
  "52", "Finance and Insurance", "Finance", "Finance",
  "53", "Real Estate and Rental and Leasing", "Real Estate", "Real Estate",
  "54", "Professional, Scientific, and Technical Services", "Professional Services", "Services",
  "55", "Management of Companies and Enterprises", "Management", "Services",
  "56", "Administrative Support and Waste Management", "Administrative", "Services",
  "61", "Educational Services", "Education", "Public Services",
  "62", "Health Care and Social Assistance", "Healthcare", "Public Services",
  "71", "Arts, Entertainment, and Recreation", "Arts/Entertainment", "Services",
  "72", "Accommodation and Food Services", "Hospitality", "Services",
  "81", "Other Services (except Public Administration)", "Other Services", "Services",
  "92", "Public Administration", "Public Admin", "Public Services"
)

# Get share column names
share_cols <- grep("^share_annual_avg_emplvl_\\d+$", names(ss_temp_filled), value = TRUE)

# Calculate industry composition metrics for each CZ
# Use base year (first year in data) for each CZ
cz_industry_composition <- ss_temp_filled %>%
  # Get base year for each CZ
  group_by(cz_id) %>%
  filter(year == min(year)) %>%
  ungroup() %>%
  # Reshape to long format
  select(cz_id, all_of(share_cols)) %>%
  pivot_longer(
    cols = all_of(share_cols),
    names_to = "industry_var",
    values_to = "share"
  ) %>%
  mutate(
    # Extract industry code from variable name
    industry_code = str_extract(industry_var, "\\d+$")
  ) %>%
  # Add industry names
  left_join(industry_mapping, by = "industry_code") %>%
  # Calculate metrics by CZ
  group_by(cz_id) %>%
  summarize(
    # Dominant industry (highest share)
    dominant_industry = first(industry_code[which.max(share)]),
    dominant_industry_name = first(industry_name[which.max(share)]),
    dominant_category = first(category[which.max(share)]),
    dominant_broad_category = first(broad_category[which.max(share)]),
    dominant_share = max(share, na.rm = TRUE),
    
    # Industry concentration (Herfindahl-Hirschman Index)
    # HHI = sum of squared shares
    # HHI close to 1 = highly concentrated (one dominant industry)
    # HHI close to 0 = diversified across many industries
    hhi = sum(share^2, na.rm = TRUE),
    
    # Number of significant industries (>5% employment)
    n_industries = sum(share > 0.05, na.rm = TRUE),
    
    # Entropy measure (Shannon entropy - another diversity metric)
    # Higher entropy = more diverse
    # Replace zeros with small value to avoid log(0)
    entropy = -sum(share * log(share + 0.0001), na.rm = TRUE),
    
    # Gini coefficient for industry concentration (alternative to HHI)
    # Sort shares and calculate Gini
    gini = {
      sorted_shares <- sort(share)
      n <- length(sorted_shares)
      sum((2 * (1:n) - n - 1) * sorted_shares) / (n * sum(sorted_shares))
    },
    
    .groups = "drop"
  )

# Merge with your existing CZ data
# Note: Need to match cz_id to unit
fig2_data_industry <- fig2_data %>%
  left_join(cz_industry_composition, by = c("unit" = "cz_id"))

#==============================================================================
# OPTION 1: Point Size by Industry Concentration (HHI)
#==============================================================================

cat("\n## Option 1: Point Size Reflects Industry Specialization\n\n")

fig_option1 <- fig2_data_industry %>%
  ggplot(aes(x = wage_growth_uncond, y = spending_growth_cond)) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3, linewidth = 0.3) +
  #geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3, linewidth = 0.3) +
  
  # All CZs - size by HHI (concentration)
  geom_point(
    data = fig2_data_industry %>% filter(!is_top_5),
    aes(size = hhi),
    color = "grey70",
    alpha = 0.4
  ) +
  
  # Top 5 drivers - size by HHI, color by mechanism
  geom_point(
    data = fig2_data_industry %>% filter(is_top_5),
    aes(size = hhi, color = mechanism),
    alpha = 0.8
  ) +
  
  # Trend line
  geom_smooth(
    method = "lm", se = FALSE,
    aes(color = mechanism),
    linewidth = 0.8
  ) +
  
  # Label top driver with industry
  geom_text_repel(
    data = fig2_data_industry %>% filter(is_top_driver),
    aes(label = paste0(cz_name, "\n(", dominant_category, ")"), color = mechanism),
    size = 2.5,
    fontface = "bold",
    max.overlaps = 10,
    box.padding = 0.3
  ) +
  
  facet_wrap(~state_label, scales = "free", ncol = 4) +
  
  scale_color_manual(
    values = c("Amplification" = "#d7191c", "Insulation" = "#fdae61", "Equalization" = "#2c7bb6"),
    name = "Mechanism"
  ) +
  scale_size_continuous(
    range = c(1, 6),
    name = "Industry\nConcentration\n(HHI)",
    breaks = c(0.1, 0.2, 0.3, 0.4),
    labels = c("Very\nDiversified", "Diversified", "Specialized", "Highly\nSpecialized")
  ) +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  
  labs(
    title = "Wage-Spending Patterns by Industry Specialization",
    subtitle = "Point size reflects industry concentration (HHI). Larger = more specialized; smaller = more diversified.",
    x = "Wage Growth (log points)",
    y = "Residual Spending Growth (log points)",
    caption = "Note: Does industry specialization predict stronger driver effects? Are specialized CZs more sensitive to wage shocks?"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 8),
    strip.background = element_rect(fill = "gray95", color = NA),
    panel.spacing = unit(0.8, "lines")
  )

print(fig_option1)
ggsave(here("output/fig_industry_option1_size.png"), fig_option1, width = 14, height = 10, dpi = 300)

#==============================================================================
# OPTION 2: Point Color by Dominant Industry Category
#==============================================================================

cat("\n## Option 2: Points Colored by Dominant Industry\n\n")

fig_option2 <- fig2_data_industry %>%
  ggplot(aes(x = wage_growth_uncond, y = spending_growth_cond)) +
  # Reference lines
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.3, linewidth = 0.3) +
  #geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.3, linewidth = 0.3) +
  
  # All CZs colored by broad industry category
  geom_point(
    data = fig2_data_industry %>% filter(!is_top_5),
    aes(color = dominant_broad_category),
    size = 1.5,
    alpha = 0.5
  ) +
  
  # Top 5 drivers with black outline
  geom_point(
    data = fig2_data_industry %>% filter(is_top_5),
    aes(fill = dominant_broad_category),
    size = 3,
    shape = 21,
    color = "black",
    stroke = 1
  ) +
  
  # Trend line (grey, not colored)
  geom_smooth(
    method = "lm", se = FALSE,
    color = "black",
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  
  # Label top driver
  geom_text_repel(
    data = fig2_data_industry %>% filter(is_top_driver),
    aes(label = paste0(cz_name, "\n(", dominant_category, ")")),
    size = 2.5,
    fontface = "bold",
    max.overlaps = 10
  ) +
  
  facet_wrap(~state_label, scales = "free", ncol = 4) +
  
  scale_color_brewer(palette = "Set2", name = "Industry Type") +
  scale_fill_brewer(palette = "Set2", name = "Industry Type") +
  scale_x_continuous(expand = expansion(mult = 0.1)) +
  scale_y_continuous(expand = expansion(mult = 0.1)) +
  
  labs(
    title = "Industry Composition of Driver Commuting Zones",
    subtitle = "Points colored by dominant industry. Black outline = top 5 drivers. Do certain industries drive effects?",
    x = "Wage Growth (log points)",
    y = "Residual Spending Growth (log points)",
    caption = "Note: Are resource states driven by Primary industries? Service states by Services?"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(face = "bold", size = 8),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

print(fig_option2)
ggsave(here("output/fig_industry_option2_color.png"), fig_option2, width = 14, height = 10, dpi = 300)

#==============================================================================
# OPTION 3: Industry Composition Heatmap by State
#==============================================================================

cat("\n## Option 3: Which Industries Drive Effects in Each State?\n\n")

# Identify top industries by driver score in each state
top_industries_by_state <- fig2_data_industry %>%
  filter(is_top_5, !is.na(dominant_broad_category)) %>%
  group_by(state, state_name, dominant_broad_category) %>%
  summarize(
    avg_driver_score = mean(abs(driver_score_mixed), na.rm = TRUE),
    n_czs = n(),
    avg_hhi = mean(hhi, na.rm = TRUE),
    .groups = "drop"
  )

# Heatmap
fig_option3 <- ggplot(top_industries_by_state, 
                      aes(x = reorder(state_name, avg_driver_score), 
                          y = dominant_broad_category, 
                          fill = avg_driver_score)) +
  geom_tile(color = "white", linewidth = 0.5) +
  geom_text(aes(label = n_czs), color = "white", fontface = "bold", size = 3) +
  scale_fill_gradient2(
    low = "white", mid = "orange", high = "darkred",
    midpoint = median(top_industries_by_state$avg_driver_score),
    name = "Avg Driver\nScore"
  ) +
  labs(
    title = "Industry Composition of Top Driver CZs by State",
    subtitle = "Number shows count of top-5 driver CZs in each industry category. Color shows average driver score.",
    x = "State (ordered by driver strength)",
    y = "Dominant Industry Category",
    caption = "Note: Do Amplification states cluster in certain industries? Are resource states driven by Primary industries?"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    legend.position = "right",
    panel.grid = element_blank()
  )

print(fig_option3)
ggsave(here("output/fig_industry_option3_heatmap.png"), fig_option3, width = 12, height = 8, dpi = 300)

#==============================================================================
# OPTION 4: Industry Diversity Distributions by Mechanism (DENSITY OVERLAY)
#==============================================================================

cat("\n## Option 4: Industry Diversity Distributions by Mechanism Type\n\n")

# Prepare data: CZ diversity metrics by mechanism
diversity_by_mechanism <- fig2_data_industry %>%
  filter(!is.na(mechanism), !is.na(hhi)) %>%
  select(unit, state, state_name, mechanism, hhi, entropy, n_industries, 
         wage_growth_uncond, spending_growth_cond, driver_score_mixed) %>%
  distinct()

# Panel A: Density plot of HHI by mechanism
panel_a <- ggplot(diversity_by_mechanism, aes(x = hhi, fill = mechanism)) +
  geom_density(alpha = 0.6, linewidth = 0.8) +
  geom_vline(
    data = diversity_by_mechanism %>% 
      group_by(mechanism) %>% 
      summarize(median_hhi = median(hhi, na.rm = TRUE)),
    aes(xintercept = median_hhi, color = mechanism),
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_fill_manual(
    values = c("Amplification" = "#d7191c", "Insulation" = "#fdae61", "Equalization" = "#2c7bb6")
  ) +
  scale_color_manual(
    values = c("Amplification" = "#d7191c", "Insulation" = "#fdae61", "Equalization" = "#2c7bb6")
  ) +
  labs(
    title = "Industry Concentration (HHI) by Mechanism Type",
    subtitle = "Dashed lines show median. Higher HHI = more specialized.",
    x = "Herfindahl-Hirschman Index (Industry Concentration)",
    y = "Density",
    fill = "Mechanism"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Panel B: Density plot of Entropy by mechanism
panel_b <- ggplot(diversity_by_mechanism, aes(x = entropy, fill = mechanism)) +
  geom_density(alpha = 0.6, linewidth = 0.8) +
  geom_vline(
    data = diversity_by_mechanism %>% 
      group_by(mechanism) %>% 
      summarize(median_entropy = median(entropy, na.rm = TRUE)),
    aes(xintercept = median_entropy, color = mechanism),
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_fill_manual(
    values = c("Amplification" = "#d7191c", "Insulation" = "#fdae61", "Equalization" = "#2c7bb6")
  ) +
  scale_color_manual(
    values = c("Amplification" = "#d7191c", "Insulation" = "#fdae61", "Equalization" = "#2c7bb6")
  ) +
  labs(
    title = "Industry Diversity (Entropy) by Mechanism Type",
    subtitle = "Dashed lines show median. Higher entropy = more diversified.",
    x = "Industry Diversity (Entropy)",
    y = "Density",
    fill = "Mechanism"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Combine panels
fig_option4 <- panel_a + panel_b + 
  plot_layout(guides = "collect", ncol = 2) &
  theme(legend.position = "bottom")

fig_option4 <- fig_option4 +
  plot_annotation(
    title = "Economic Diversity and Fiscal Transmission Mechanisms",
    subtitle = "Do Amplification states have more specialized economies? Do Insulation states have more diversified economies?",
    caption = str_wrap("Note: If Amplification states show higher HHI (right-skewed), specialized economies may drive stronger wage-spending linkages. If Insulation states show higher Entropy (left-skewed HHI), diversification may buffer spending from shocks.", 130),
    theme = theme(
      plot.title = element_text(face = "bold", size = 14),
      plot.subtitle = element_text(size = 11, color = "gray30")
    )
  )

print(fig_option4)
ggsave(here("output/fig_industry_option4_density.png"), fig_option4, width = 14, height = 7, dpi = 300)

#==============================================================================
# OPTION 5: Combined - Scatter with Marginal Density Plots
#==============================================================================

cat("\n## Option 5: Scatter with Marginal Density by Mechanism\n\n")

# Create main scatter plot
scatter_main <- fig2_data_industry %>%
  filter(!is.na(mechanism)) %>%
  ggplot(aes(x = hhi, y = abs(driver_score_mixed))) +
  geom_point(aes(color = mechanism, size = dominant_share), alpha = 0.6) +
  geom_smooth(aes(color = mechanism), method = "lm", se = FALSE, linewidth = 1) +
  scale_color_manual(
    values = c("Amplification" = "#d7191c", "Insulation" = "#fdae61", "Equalization" = "#2c7bb6")
  ) +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  labs(
    x = "Industry Concentration (HHI)",
    y = "Driver Score Magnitude",
    color = "Mechanism"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

# Marginal density - HHI
density_hhi <- ggplot(diversity_by_mechanism, aes(x = hhi, fill = mechanism)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(
    values = c("Amplification" = "#d7191c", "Insulation" = "#fdae61", "Equalization" = "#2c7bb6")
  ) +
  theme_void() +
  theme(legend.position = "none")

# Marginal density - Driver score
density_driver <- fig2_data_industry %>%
  filter(!is.na(mechanism)) %>%
  ggplot(aes(x = abs(driver_score_mixed), fill = mechanism)) +
  geom_density(alpha = 0.6) +
  scale_fill_manual(
    values = c("Amplification" = "#d7191c", "Insulation" = "#fdae61", "Equalization" = "#2c7bb6")
  ) +
  coord_flip() +
  theme_void() +
  theme(legend.position = "none")

# Combine with patchwork
fig_option5 <- density_hhi + plot_spacer() + scatter_main + density_driver +
  plot_layout(
    ncol = 2, nrow = 2,
    widths = c(4, 1),
    heights = c(1, 4)
  ) +
  plot_annotation(
    title = "Industry Specialization and Driver Strength by Mechanism",
    subtitle = "Do specialized CZs (high HHI) show stronger driver effects? Does this vary by mechanism?",
    theme = theme(plot.title = element_text(face = "bold", size = 13))
  )

print(fig_option5)
ggsave(here("output/fig_industry_option5_marginal.png"), fig_option5, width = 12, height = 10, dpi = 300)

#==============================================================================
# STATISTICAL TESTS: Industry Diversity by Mechanism
#==============================================================================

cat("\n## Statistical Tests: Industry Diversity by Mechanism\n\n")

# Test 1: ANOVA - Does HHI differ by mechanism?
if (nrow(diversity_by_mechanism) > 0) {
  hhi_anova <- aov(hhi ~ mechanism, data = diversity_by_mechanism)
  cat("ANOVA: HHI by Mechanism\n")
  print(summary(hhi_anova))
  
  # Test 2: Pairwise comparisons
  cat("\nPairwise Comparisons (Tukey HSD):\n")
  print(TukeyHSD(hhi_anova))
}

# Test 3: Summary statistics
diversity_summary <- diversity_by_mechanism %>%
  group_by(mechanism) %>%
  summarize(
    n_czs = n(),
    mean_hhi = mean(hhi, na.rm = TRUE),
    median_hhi = median(hhi, na.rm = TRUE),
    sd_hhi = sd(hhi, na.rm = TRUE),
    mean_entropy = mean(entropy, na.rm = TRUE),
    median_entropy = median(entropy, na.rm = TRUE),
    mean_n_industries = mean(n_industries, na.rm = TRUE),
    .groups = "drop"
  )

cat("\n")
kable(
  diversity_summary,
  caption = "Industry Diversity Metrics by Mechanism Type",
  digits = 3
) %>%
  kable_styling(bootstrap_options = c("striped", "hover")) %>%
  print()

# Save summary
write_csv(diversity_summary, here("output/industry_diversity_by_mechanism.csv"))

#==============================================================================
# INTERPRETATION GUIDE
#==============================================================================

cat("\n## Interpretation Guide\n\n")
cat("If Amplification states have HIGHER HHI (more specialized):\n")
cat("  → Specialized economies amplify wage shocks into spending changes\n")
cat("  → Single-industry towns are more responsive to wage fluctuations\n\n")

cat("If Insulation states have LOWER HHI (more diversified):\n")
cat("  → Economic diversity buffers spending from wage shocks\n")
cat("  → Diversified economies stabilize education finance\n\n")

cat("If Equalization state (Oregon) has specific industry pattern:\n")
cat("  → Check if dominated by industries with strong state regulation\n")
cat("  → Check if tech/high-wage industries trigger transfer cuts\n\n")

cat("\n**Files saved:**\n")
cat("- output/fig_industry_option1_size.png\n")
cat("- output/fig_industry_option2_color.png\n")
cat("- output/fig_industry_option3_heatmap.png\n")
cat("- output/fig_industry_option4_density.png\n")
cat("- output/fig_industry_option5_marginal.png\n")
cat("- output/industry_diversity_by_mechanism.csv\n")