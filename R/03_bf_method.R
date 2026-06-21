# =============================================================================
# 03_bf_method.R
# India MET Reserving Project
# Purpose: Apply the Bornhuetter-Ferguson method to Motor, Engineering, and
#          Treaty triangles. Compare BF vs CL — particularly for immature AYs
#          and CAT-contaminated Engineering years.
#
# WHAT IS BORNHUETTER-FERGUSON? (Plain English)
#   BF is a credibility blend between two reserve estimates:
#     1. "Emergence" method: How much of ultimate has already emerged?
#        IBNR = Ultimate × (1 - % emerged). Use an a priori ultimate.
#     2. Chain Ladder: Use actual experience to project.
#
#   BF formula:
#     IBNR_BF = A Priori Ultimate × (1 - 1/CDF)
#     Ultimate_BF = Latest Observed + IBNR_BF
#
#   WHERE:
#     - A Priori Ultimate = Premium × Expected Loss Ratio (your best prior estimate)
#     - CDF = cumulative development factor (product of link ratios)
#     - (1 - 1/CDF) = % of ultimate NOT YET EMERGED = the "tail" weight
#
#   WHY BF IS BETTER FOR IMMATURE YEARS (AY2022-23, AY2023-24):
#     In immature AYs, very little has emerged. CL gives all credibility to a
#     small, noisy observed amount. BF gives more weight to the a priori — 
#     a more stable estimate. This is actuarial common sense.
#
#   WHY BF IS BETTER FOR CAT-CONTAMINATED YEARS (Eng AY2018, AY2019):
#     The CAT inflated link ratios. BF uses the a priori ELR which is based on
#     non-CAT expectations, so it doesn't extrapolate the CAT experience forward.
#
# Author: [Your Name]
# =============================================================================


# -----------------------------------------------------------------------------
# SECTION 0: Load Libraries and Prior Results
# -----------------------------------------------------------------------------

library(ChainLadder)
library(dplyr)
library(tibble)
library(ggplot2)
library(tidyr)

load("data/processed/synthetic_triangles.RData")
load("data/processed/cl_results.RData")

dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)


# -----------------------------------------------------------------------------
# SECTION 1: A Priori Expected Loss Ratios
# -----------------------------------------------------------------------------

# The most important input to BF is the a priori ELR.
# This is YOUR actuarial judgement of what the ultimate loss ratio should be
# BEFORE looking at actual experience. It must be justified.
#
# Sources for Indian market ELRs:
#
# MOTOR:
#   - IRDAI Annual Report 2022-23: industry net incurred loss ratio for Motor = 76%
#   - We use 77% as a conservative prior (slightly above industry given our book)
#   - For AY2018 (Kerala floods): we still use 77% for Motor TP (flood is NOT
#     a standard Motor peril in India — MACT claims are the tail, not property)
#
# ENGINEERING:
#   - IRDAI data shows Eng combined ratio ~98-105% in CAT years, ~85-90% in normal
#   - Net loss ratio (after RI recoveries) for Engineering: ~70% in normal years
#   - We set a priori at 70% for ALL years — this deliberately does NOT reflect
#     the CAT spikes, which is the whole point of using BF here
#   - The CAT years will show BF < CL, which is the correct actuarial signal
#
# TREATY:
#   - GIC Re Annual Report 2022-23: net combined ratio ~98%, expense ratio ~25%
#   - Implied net loss ratio: ~73%
#   - We use 73% as a priori for Treaty

# A priori ELRs (same for all AYs within a line — we'll sensitivity-test later)
apriori_elr <- list(
  Motor   = 0.77,
  Eng     = 0.70,
  Treaty  = 0.73
)

# A priori ultimates = Premium × ELR
apriori_ult <- list(
  Motor  = motor_premium  * apriori_elr$Motor,
  Eng    = eng_premium    * apriori_elr$Eng,
  Treaty = treaty_premium * apriori_elr$Treaty
)

cat("=== A PRIORI ULTIMATES (INR Crores) ===\n")
cat("\nMotor:\n");   print(round(apriori_ult$Motor,  1))
cat("\nEngineering:\n"); print(round(apriori_ult$Eng, 1))
cat("\nTreaty:\n");  print(round(apriori_ult$Treaty, 1))


# -----------------------------------------------------------------------------
# SECTION 2: Run BF via ChainLadder Package
# -----------------------------------------------------------------------------

# BF in ChainLadder: BFmethod() function
# Inputs:
#   Triangle  : the cl_triangle object
#   ult       : vector of a priori ultimates (one per AY)
#   estimated.weights : how much weight to give the a priori vs emergence
#                       Default = 1 means pure BF (standard)

run_bf <- function(cl_triangle, apriori_ult_vec, line_name) {
  
  cat(paste0("\n=== BORNHUETTER-FERGUSON: ", line_name, " ===\n"))
  
  n_ay  <- nrow(cl_triangle)
  n_dev <- ncol(cl_triangle)
  
  # Step 1: Get CDF (cumulative development factors) from Chain Ladder
  # CDF tells us: by Dev period j, what % of ultimate has emerged?
  mack_temp <- MackChainLadder(cl_triangle)
  
  # Link ratios (f) — one per development period transition
  f <- mack_temp$f  # length n_dev - 1
  
  # CDF from each period to ultimate (chain multiply from right to left)
  # CDF[j] = f[j] * f[j+1] * ... * f[n_dev-1]
  cdf <- numeric(n_dev)
  cdf[n_dev] <- 1.0  # at last dev period, CDF = 1 (fully developed)
  for (j in (n_dev - 1):1) {
    cdf[j] <- f[j] * cdf[j + 1]
  }
  
  # Step 2: For each AY, find which dev period is latest (last non-NA)
  latest_dev_idx <- apply(cl_triangle, 1, function(row) {
    max(which(!is.na(row)))
  })
  
  # Step 3: Latest observed diagonal
  latest_obs <- apply(cl_triangle, 1, function(row) {
    obs <- row[!is.na(row)]
    tail(obs, 1)
  })
  
  # Step 4: BF formula
  # % NOT YET EMERGED = (1 - 1/CDF) at the latest dev period for each AY
  pct_unreported <- 1 - (1 / cdf[latest_dev_idx])
  
  # IBNR_BF = A Priori Ultimate × % not yet emerged
  ibnr_bf    <- apriori_ult_vec * pct_unreported
  
  # Ultimate_BF = Latest Observed + IBNR_BF
  ultimate_bf <- latest_obs + ibnr_bf
  
  results <- tibble(
    AccidentYear   = rownames(cl_triangle),
    LatestDevIdx   = latest_dev_idx,
    PctUnreported  = round(pct_unreported * 100, 1),
    APrioriUlt     = round(apriori_ult_vec, 1),
    LatestObs      = round(latest_obs, 1),
    IBNR_BF        = round(ibnr_bf, 1),
    Ultimate_BF    = round(ultimate_bf, 1)
  )
  
  print(results)
  return(results)
}

# -----------------------------------------------------------------------------
# SECTION 3: Extract BF Results
# -----------------------------------------------------------------------------

extract_bf_results <- function(bf_tbl, mack_obj, premium, line_name) {
  
  # Latest observed from the original triangle (use mack object for this)
  latest_obs <- apply(mack_obj$Triangle, 1, function(row) {
    obs <- row[!is.na(row)]
    tail(obs, 1)
  })
  
  # CL IBNR from mack object
  ultimate_cl <- mack_obj$FullTriangle[, ncol(mack_obj$FullTriangle)]
  ibnr_cl     <- ultimate_cl - latest_obs
  
  results_tbl <- tibble(
    Line          = line_name,
    AccidentYear  = bf_tbl$AccidentYear,
    Premium_Cr    = round(premium, 1),
    LatestObs_Cr  = round(latest_obs, 1),
    Ultimate_BF   = round(bf_tbl$Ultimate_BF, 1),
    IBNR_BF       = round(bf_tbl$IBNR_BF, 1),
    Ultimate_CL   = round(ultimate_cl, 1),
    IBNR_CL       = round(ibnr_cl, 1),
    IBNR_Diff     = round(bf_tbl$IBNR_BF - ibnr_cl, 1),
    Diff_pct      = round((bf_tbl$IBNR_BF / ibnr_cl - 1) * 100, 1)
  )
  
  cat(paste0("\n--- BF vs CL Comparison: ", line_name, " ---\n"))
  print(results_tbl)
  return(results_tbl)
}
# -----------------------------------------------------------------------------
# SECTION 4: The Key Actuarial Insight — When BF and CL Diverge
# -----------------------------------------------------------------------------

cat("\n=== ACTUARIAL COMMENTARY: BF vs CL Divergence ===\n\n")

cat("MOTOR:\n")
cat("  BF and CL should be CLOSE for Motor because:\n")
cat("  - High volume line → link ratios are stable and credible\n")
cat("  - CL credibility is high → BF weight shifts toward CL anyway\n")
cat("  - Small divergence in immature AYs (AY2022-23, AY2023-24) is expected\n\n")

cat("ENGINEERING:\n")
cat("  BF and CL will DIVERGE significantly because:\n")
cat("  - CAT contamination in AY2018, AY2019 inflated CL link ratios\n")
cat("  - CL projects those inflated ratios into future → OVERESTIMATES reserve\n")
cat("  - BF uses 70% ELR prior → NOT contaminated by CAT → more appropriate\n")
cat("  - Actuarial BEST ESTIMATE for Engineering: BF is preferred\n\n")

cat("TREATY:\n")
cat("  Intermediate divergence:\n")
cat("  - Treaty absorbs CAT from cedants but is diversified across many risks\n")
cat("  - BF provides stability for immature years\n")
cat("  - Both methods should be presented; blend may be appropriate\n")


# -----------------------------------------------------------------------------
# SECTION 5: Sensitivity Analysis on A Priori ELR
# -----------------------------------------------------------------------------

# One of the most important things to show: how sensitive is BF IBNR
# to the choice of a priori ELR? This demonstrates actuarial rigour.

sensitivity_bf <- function(cl_triangle, premium, base_elr, line_name,
                           elr_range = seq(-0.10, 0.10, by = 0.025)) {

  cat(paste0("\n=== ELR SENSITIVITY: ", line_name, " ===\n"))

  sensitivity_results <- map_dfr(elr_range, function(delta) {

    test_elr  <- base_elr + delta
    test_ult  <- premium * test_elr

    bf_test <- bf_test <- run_bf(cl_triangle, test_ult, "sensitivity")
    tibble(
      ELR_tested = round(test_elr * 100, 1),
      Total_IBNR = round(sum(bf_test$IBNR_BF, na.rm = TRUE), 1)
    )

    tibble(
      ELR_tested  = round(test_elr * 100, 1),
      Total_IBNR  = round(sum(bf_test$IBNR, na.rm = TRUE), 1)
    )
  })

  print(sensitivity_results)
  return(sensitivity_results)
}

# Need purrr for map_dfr
library(purrr)

sens_motor  <- sensitivity_bf(motor_cl_tri,  motor_premium,  apriori_elr$Motor,  "Motor")
sens_eng    <- sensitivity_bf(eng_cl_tri,    eng_premium,    apriori_elr$Eng,    "Engineering")
sens_treaty <- sensitivity_bf(treaty_cl_tri, treaty_premium, apriori_elr$Treaty, "Treaty")


# -----------------------------------------------------------------------------
# SECTION 6: Visualisations
# -----------------------------------------------------------------------------

# Plot: BF vs CL IBNR by Accident Year — the most important comparison chart

plot_bf_vs_cl <- function(results_bf_tbl, line_name) {

  # Reshape to long format for ggplot
  plot_data <- results_bf_tbl %>%
    select(AccidentYear, IBNR_BF, IBNR_CL) %>%
    pivot_longer(cols = c(IBNR_BF, IBNR_CL),
                 names_to  = "Method",
                 values_to = "IBNR_Cr") %>%
    mutate(Method = recode(Method,
                           "IBNR_BF" = "Bornhuetter-Ferguson",
                           "IBNR_CL" = "Chain Ladder"))

  p <- ggplot(plot_data, aes(x = AccidentYear, y = IBNR_Cr, fill = Method)) +
    geom_col(position = "dodge", alpha = 0.85) +
    scale_fill_manual(values = c("Bornhuetter-Ferguson" = "#1a5276",
                                  "Chain Ladder"         = "#e67e22")) +
    labs(
      title    = paste0(line_name, ": BF vs Chain Ladder IBNR"),
      subtitle = "Divergence signals where CL assumptions break down",
      x        = "Accident Year",
      y        = "IBNR (INR Crores)",
      fill     = "Method",
      caption  = "Source: Synthetic triangles calibrated to IRDAI / GIC Re parameters"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          legend.position = "top")

  ggsave(
    filename = paste0("outputs/plots/bf_vs_cl_", tolower(line_name), ".png"),
    plot     = p,
    width    = 11, height = 6, dpi = 150
  )

  cat(paste0("Saved: outputs/plots/bf_vs_cl_", tolower(line_name), ".png\n"))
  return(p)
}

plot_bf_vs_cl(results_bf_motor,  "Motor")
plot_bf_vs_cl(results_bf_eng,    "Engineering")
plot_bf_vs_cl(results_bf_treaty, "Treaty")

# Plot: ELR Sensitivity for Engineering (most sensitive line)
ggplot(sens_eng, aes(x = ELR_tested, y = Total_IBNR)) +
  geom_line(colour = "#1a5276", linewidth = 1.2) +
  geom_point(colour = "#e74c3c", size = 2) +
  geom_vline(xintercept = apriori_elr$Eng * 100,
             linetype = "dashed", colour = "grey40") +
  annotate("text", x = apriori_elr$Eng * 100 + 0.5, y = max(sens_eng$Total_IBNR) * 0.95,
           label = "Base ELR", hjust = 0, size = 3.5) +
  labs(
    title    = "Engineering: BF IBNR Sensitivity to A Priori ELR",
    subtitle = "Slope indicates reliance on prior vs. actual emergence",
    x        = "A Priori ELR (%)",
    y        = "Total IBNR (INR Crores)"
  ) +
  theme_minimal(base_size = 12)

ggsave("outputs/plots/bf_elr_sensitivity_engineering.png", width = 10, height = 6, dpi = 150)


# -----------------------------------------------------------------------------
# SECTION 7: Save BF Results
# -----------------------------------------------------------------------------

save(
  bf_motor,   bf_eng,   bf_treaty,
  results_bf_motor, results_bf_eng, results_bf_treaty,
  all_results_bf,
  sens_motor, sens_eng, sens_treaty,
  apriori_elr, apriori_ult,
  file = "data/processed/bf_results.RData"
)

cat("\n=== SAVED: data/processed/bf_results.RData ===\n")
cat("Next step: Run 04_comparison.R\n")
