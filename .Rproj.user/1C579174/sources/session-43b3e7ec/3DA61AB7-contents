# =============================================================================
# 04_comparison.R
# India MET Reserving Project
# Purpose: Consolidated comparison of CL vs BF across all three lines.
#          Produce final reserve recommendation table and summary visuals
#          for the Quarto report and Shiny dashboard.
#
# This is the "so what" script — the actuarial judgement layer.
# Numbers without interpretation are useless. This script explains
# WHICH method to prefer for WHICH line and WHY.
#
# Author: [Your Name]
# =============================================================================

library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)
library(scales)
library(ChainLadder)

load("data/processed/synthetic_triangles.RData")
load("data/processed/cl_results.RData")
load("data/processed/bf_results.RData")

dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)


# -----------------------------------------------------------------------------
# SECTION 1: Method Selection Framework
# -----------------------------------------------------------------------------

# For each line and accident year, select the PREFERRED method based on:
#
# Rule 1 — IMMATURE AYs (latest dev period <= 2):
#   Prefer BF. CL has low credibility when little has emerged.
#   Threshold: if % emerged < 60%, use BF.
#
# Rule 2 — CAT-CONTAMINATED AYs:
#   Engineering AY2018, AY2019 — use BF regardless of maturity.
#   Reason: CL link ratios are distorted upward.
#
# Rule 3 — MATURE AYs (latest dev period >= 5):
#   Prefer CL. Sufficient emergence to trust the development pattern.
#
# Rule 4 — INTERMEDIATE AYs:
#   Use a credibility blend: IBNR = w × IBNR_BF + (1-w) × IBNR_CL
#   where w = (1 - % emerged). As more emerges, CL gets more weight.

# Development patterns (cumulative % emerged) from 05_synthetic_data.R
motor_dev_pattern  <- c(0.45, 0.68, 0.80, 0.88, 0.93, 0.96, 0.98, 1.00)
eng_dev_pattern    <- c(0.55, 0.78, 0.90, 0.95, 0.97, 0.98, 0.99, 1.00)
treaty_dev_pattern <- c(0.50, 0.72, 0.85, 0.92, 0.96, 0.98, 0.99, 1.00)

# CAT-contaminated AYs (Engineering only)
cat_ays <- c("AY2018", "AY2019")

select_method <- function(results_bf_tbl, dev_pattern, line_name,
                           cat_contaminated_ays = character(0)) {

  n_ay <- nrow(results_bf_tbl)

  # Which development period is latest for each AY?
  # AY1 (2016) has 8 periods. AY8 (2023) has 1 period.
  latest_dev_idx <- (8 - (0:(n_ay - 1)))
  pct_emerged    <- dev_pattern[latest_dev_idx]

  results_bf_tbl %>%
    mutate(
      PctEmerged   = round(pct_emerged * 100, 1),
      IsCAT        = AccidentYear %in% cat_contaminated_ays,
      # Credibility weight for BF (higher weight = more BF)
      BF_weight    = pmax(0, pmin(1, 1 - pct_emerged)),
      # Preferred method
      PreferredMethod = case_when(
        IsCAT                  ~ "BF (CAT contamination)",
        pct_emerged < 0.60     ~ "BF (immature)",
        pct_emerged >= 0.90    ~ "CL (mature)",
        TRUE                   ~ "Blend"
      ),
      # Best estimate IBNR
      IBNR_BestEst = case_when(
        PreferredMethod == "CL (mature)"          ~ IBNR_CL,
        PreferredMethod %in% c("BF (immature)",
                               "BF (CAT contamination)") ~ IBNR_BF,
        TRUE  ~ round(BF_weight * IBNR_BF + (1 - BF_weight) * IBNR_CL, 1)
      )
    )
}

final_motor  <- select_method(results_bf_motor,  motor_dev_pattern,  "Motor")
final_eng    <- select_method(results_bf_eng,     eng_dev_pattern,    "Engineering",
                               cat_contaminated_ays = cat_ays)
final_treaty <- select_method(results_bf_treaty, treaty_dev_pattern, "Treaty")

all_final <- bind_rows(final_motor, final_eng, final_treaty)

cat("=== FINAL RESERVE ESTIMATES WITH METHOD SELECTION ===\n")
print(all_final %>% select(Line, AccidentYear, PctEmerged, PreferredMethod,
                            IBNR_CL, IBNR_BF, IBNR_BestEst, IsCAT))


# -----------------------------------------------------------------------------
# SECTION 2: Portfolio Summary Table
# -----------------------------------------------------------------------------

# This is the TABLE you'd show in a board pack or actuarial report

summary_table <- all_final %>%
  group_by(Line) %>%
  summarise(
    TotalPremium_Cr    = sum(Premium_Cr),
    TotalLatestObs_Cr  = sum(LatestObs_Cr),
    TotalIBNR_CL       = round(sum(IBNR_CL, na.rm = TRUE), 1),
    TotalIBNR_BF       = round(sum(IBNR_BF, na.rm = TRUE), 1),
    TotalIBNR_BestEst  = round(sum(IBNR_BestEst, na.rm = TRUE), 1),
    CL_vs_BF_Diff_pct  = round((TotalIBNR_CL / TotalIBNR_BF - 1) * 100, 1),
    .groups = "drop"
  )

cat("\n=== PORTFOLIO SUMMARY (All AYs Combined) ===\n")
print(summary_table)

# Save as CSV for the report
write.csv(summary_table,
          file = "outputs/tables/portfolio_summary.csv",
          row.names = FALSE)

write.csv(all_final,
          file = "outputs/tables/full_reserve_estimates.csv",
          row.names = FALSE)

cat("\nSaved: outputs/tables/portfolio_summary.csv\n")
cat("Saved: outputs/tables/full_reserve_estimates.csv\n")


# -----------------------------------------------------------------------------
# SECTION 3: The Key Comparison Chart
# -----------------------------------------------------------------------------

# Grouped bar: CL vs BF vs Best Estimate, by Line
plot_data_summary <- summary_table %>%
  select(Line, TotalIBNR_CL, TotalIBNR_BF, TotalIBNR_BestEst) %>%
  pivot_longer(cols = -Line, names_to = "Method", values_to = "IBNR_Cr") %>%
  mutate(Method = recode(Method,
    "TotalIBNR_CL"      = "Chain Ladder",
    "TotalIBNR_BF"      = "Bornhuetter-Ferguson",
    "TotalIBNR_BestEst" = "Best Estimate"
  ))

p_summary <- ggplot(plot_data_summary,
                    aes(x = Line, y = IBNR_Cr, fill = Method)) +
  geom_col(position = "dodge", alpha = 0.88) +
  scale_fill_manual(values = c(
    "Chain Ladder"         = "#e67e22",
    "Bornhuetter-Ferguson" = "#1a5276",
    "Best Estimate"        = "#1e8449"
  )) +
  scale_y_continuous(labels = comma) +
  labs(
    title    = "India MET Reserving: CL vs BF vs Best Estimate IBNR",
    subtitle = "Best Estimate applies method selection rules (maturity + CAT flag)",
    x        = "Line of Business",
    y        = "Total IBNR (INR Crores)",
    fill     = "Method",
    caption  = "Synthetic data calibrated to IRDAI 2022-23 and GIC Re disclosures"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

ggsave("outputs/plots/final_comparison.png", p_summary,
       width = 11, height = 7, dpi = 150)
cat("Saved: outputs/plots/final_comparison.png\n")


# -----------------------------------------------------------------------------
# SECTION 4: Development Pattern Stability Chart
# -----------------------------------------------------------------------------

# Show the individual link ratios across AYs — the diagnostic that justifies
# why Engineering needs BF

plot_link_ratio_stability <- function(cl_triangle, line_name) {
  
  ata_matrix <- ata(cl_triangle)
  
  # Convert to data frame properly before reshaping
  ata_df <- as.data.frame(ata_matrix)
  ata_df$AccidentYear <- rownames(ata_matrix)
  
  ata_long <- ata_df %>%
    pivot_longer(cols = -AccidentYear,
                 names_to  = "DevPeriod",
                 values_to = "LinkRatio") %>%
    filter(!is.na(LinkRatio))
  
  p <- ggplot(ata_long, aes(x = DevPeriod, y = LinkRatio, colour = AccidentYear)) +
    geom_point(size = 2.5, alpha = 0.8) +
    geom_hline(
      data = ata_long %>%
        group_by(DevPeriod) %>%
        summarise(mean_lr = mean(LinkRatio, na.rm = TRUE), .groups = "drop"),
      aes(yintercept = mean_lr),
      colour = "black", linetype = "dashed", linewidth = 0.7,
      inherit.aes = FALSE
    ) +
    labs(
      title    = paste0(line_name, ": Individual Link Ratios by Accident Year"),
      subtitle = "Dashed line = volume-weighted average used by CL.\nOutliers indicate CAT contamination.",
      x        = "Development Period",
      y        = "Age-to-Age Factor",
      colour   = "Accident Year"
    ) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(
    paste0("outputs/plots/link_ratio_stability_", tolower(line_name), ".png"),
    p, width = 11, height = 6, dpi = 150
  )
  cat(paste0("Saved: link_ratio_stability_", tolower(line_name), ".png\n"))
  return(p)
}

plot_link_ratio_stability(motor_cl_tri,  "Motor")
plot_link_ratio_stability(eng_cl_tri,    "Engineering")
plot_link_ratio_stability(treaty_cl_tri, "Treaty")


# -----------------------------------------------------------------------------
# SECTION 5: Save Everything for Report and Dashboard
# -----------------------------------------------------------------------------

save(
  all_final,
  summary_table,
  final_motor, final_eng, final_treaty,
  file = "data/processed/final_results.RData"
)

cat("\n=== SAVED: data/processed/final_results.RData ===\n")
cat("Next step: Render report/india_met_reserving.qmd\n")
cat("Then: Build dashboard/app.R\n")
cat("\n=== PROJECT STATUS ===\n")
cat("Phase 1 (R Scripts): COMPLETE\n")
cat("Phase 2 (Quarto Report): Next\n")
cat("Phase 3 (Shiny Dashboard): After report\n")
