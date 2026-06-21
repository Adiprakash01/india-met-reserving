# =============================================================================
# 02_chain_ladder.R
# India MET Reserving Project
# Purpose: Apply the Chain Ladder method to Motor, Engineering, and Treaty
#          triangles. Produce development factors, ultimates, IBNR reserves,
#          and Mack standard errors. Export results for the Quarto report.
#
# WHAT IS CHAIN LADDER? (Plain English)
#   Chain Ladder asks: "On average, how much does each accident year's losses
#   grow from one development period to the next?"
#   These growth ratios (called Link Ratios or Age-to-Age factors) are averaged
#   across accident years, then chained together to project incomplete years
#   to their ultimate value.
#
#   IBNR = Ultimate Projected - Latest Observed (what we still need to reserve)
#
# MACK (1993) adds standard errors to CL — tells you how uncertain your reserve is.
#
# Author: [Your Name]
# =============================================================================


# -----------------------------------------------------------------------------
# SECTION 0: Load Libraries and Data
# -----------------------------------------------------------------------------

library(ChainLadder)
library(dplyr)
library(tibble)
library(ggplot2)
library(scales)    # for formatting numbers in plots

# Load synthetic triangles generated in 05_synthetic_data.R
load("data/processed/synthetic_triangles.RData")

# Create output directory for plots if it doesn't exist
dir.create("outputs/plots", recursive = TRUE, showWarnings = FALSE)


# -----------------------------------------------------------------------------
# SECTION 1: Age-to-Age Development Factors
# -----------------------------------------------------------------------------

# Before running MackChainLadder(), understand what it's doing under the hood.
# The link ratios (f_j) are the building blocks of everything.

compute_link_ratios <- function(triangle_matrix, line_name) {

  cat(paste0("\n=== LINK RATIOS: ", line_name, " ===\n"))

  n_ay  <- nrow(triangle_matrix)
  n_dev <- ncol(triangle_matrix)

  # For each development period j, compute f_j (volume-weighted average)
  # Volume-weighted = sum of numerators / sum of denominators across AYs
  # This is more stable than simple average when premium volumes differ across AYs

  link_ratios <- numeric(n_dev - 1)

  for (j in 1:(n_dev - 1)) {
    numerator   <- 0
    denominator <- 0

    for (i in 1:n_ay) {
      # Only include AY i if we observe BOTH period j AND period j+1
      if (!is.na(triangle_matrix[i, j]) && !is.na(triangle_matrix[i, j + 1])) {
        numerator   <- numerator   + triangle_matrix[i, j + 1]
        denominator <- denominator + triangle_matrix[i, j]
      }
    }

    if (denominator > 0) {
      link_ratios[j] <- numerator / denominator
    } else {
      link_ratios[j] <- NA
    }
  }

  names(link_ratios) <- paste0("Dev", 1:(n_dev-1), "->", 2:n_dev)
  print(round(link_ratios, 4))

  # Tail factor: we assume 1.000 here (all development complete by period 8)
  # In real Indian Motor TP books, you'd add a tail factor > 1.0
  cat("Tail factor assumed: 1.0000 (revisit for Motor TP in real data)\n")

  return(link_ratios)
}

motor_ldf   <- compute_link_ratios(motor_triangle,  "Motor")
eng_ldf     <- compute_link_ratios(eng_triangle,    "Engineering")
treaty_ldf  <- compute_link_ratios(treaty_triangle, "Treaty")


# -----------------------------------------------------------------------------
# SECTION 2: Mack Chain Ladder
# -----------------------------------------------------------------------------

# MackChainLadder() does everything:
#   - Computes volume-weighted link ratios
#   - Projects the triangle to ultimate
#   - Estimates Mack standard error (process + parameter variance)
#   - Does NOT assume a distribution — distribution-free reserve estimate

run_mack_cl <- function(cl_triangle, line_name, est_sigma = "Mack") {

  cat(paste0("\n=== MACK CHAIN LADDER: ", line_name, " ===\n"))

  mack_result <- MackChainLadder(
    Triangle  = cl_triangle,
    est.sigma = est_sigma,   # "Mack" uses Mack's formula for sigma estimation
    tail      = FALSE        # No tail factor — change to numeric value if needed
  )

  # Print the summary: ultimates, IBNR, Mack SE, CV
  print(summary(mack_result))

  return(mack_result)
}

mack_motor  <- run_mack_cl(motor_cl_tri,  "Motor")
mack_eng    <- run_mack_cl(eng_cl_tri,    "Engineering")
mack_treaty <- run_mack_cl(treaty_cl_tri, "Treaty")


# -----------------------------------------------------------------------------
# SECTION 3: Extract Results into Clean Tables
# -----------------------------------------------------------------------------

extract_mack_results <- function(mack_obj, premium, line_name) {
  
  n_ay     <- nrow(mack_obj$Triangle)
  ay_names <- rownames(mack_obj$Triangle)
  
  # Ultimate = last column of FullTriangle (fully developed projection)
  ultimate <- mack_obj$FullTriangle[, ncol(mack_obj$FullTriangle)]
  
  # Latest observed = last non-NA value in each row of original triangle
  latest_obs <- apply(mack_obj$Triangle, 1, function(row) {
    obs <- row[!is.na(row)]
    tail(obs, 1)
  })
  
  # IBNR = Ultimate - Latest Observed
  ibnr <- ultimate - latest_obs
  
  results_tbl <- tibble(
    Line         = line_name,
    AccidentYear = ay_names,
    Premium_Cr   = round(premium, 1),
    LatestObs_Cr = round(latest_obs, 1),
    Ultimate_Cr  = round(ultimate, 1),
    IBNR_Cr      = round(ibnr, 1),
    MackSE_Cr    = round(mack_obj$Mack.S.E[, ncol(mack_obj$Mack.S.E)], 1),
    CV_pct       = round(mack_obj$Mack.S.E[, ncol(mack_obj$Mack.S.E)] / ibnr * 100, 1),
    UltLR_pct    = round(ultimate / premium * 100, 1)
  )
  
  cat(paste0("\n--- Clean Results: ", line_name, " ---\n"))
  print(results_tbl)
  return(results_tbl)
}


# -----------------------------------------------------------------------------
# SECTION 4: Diagnostics — Is Chain Ladder Appropriate?
# -----------------------------------------------------------------------------

# CL has two key assumptions:
# 1. Development factors are stable across accident years (no trend)
# 2. Accident years are independent (no calendar year effects)
#
# For Indian Engineering lines, assumption 1 is VIOLATED in CAT years.
# For Indian Motor TP, assumption 2 can be violated if court awards trend up.
# This is why we also run BF in 03_bf_method.R.

cat("\n=== DIAGNOSTIC: Are Development Factors Stable? ===\n")
cat("Look at the per-AY link ratios below. High variance = CL may be unreliable.\n\n")

# ChainLadder's ata() function shows all individual AY link ratios
cat("--- Motor Individual Link Ratios ---\n")
print(round(ata(motor_cl_tri), 3))

cat("\n--- Engineering Individual Link Ratios ---\n")
print(round(ata(eng_cl_tri), 3))

# For Engineering: AY2018 and AY2019 should show unusually high ratios
# due to the CAT contamination we built in. This is the FLAG.
cat("\nNOTE: Engineering AY3 (2018-19) and AY4 (2019-20) link ratios will be\n")
cat("elevated vs other AYs due to CAT contamination. CL will OVERESTIMATE\n")
cat("future development for non-CAT years if these aren't excluded or weighted.\n")
cat("BF method (03_bf_method.R) handles this more robustly.\n")


# -----------------------------------------------------------------------------
# SECTION 5: Bootstrap CL (Distribution of Reserves)
# -----------------------------------------------------------------------------

# Mack gives point estimate + SE. Bootstrap CL gives a full distribution.
# This is closer to what a real reserving actuary would show to management.

run_bootstrap_cl <- function(cl_triangle, line_name, n_sims = 999) {

  cat(paste0("\n=== BOOTSTRAP CHAIN LADDER: ", line_name, " ===\n"))

  boot_result <- BootChainLadder(
    Triangle = cl_triangle,
    R        = n_sims,      # number of simulations
    process.distr = "od.pois"  # overdispersed Poisson — standard for CL bootstrap
  )

  print(summary(boot_result))

  # Extract total IBNR distribution
  total_ibnr_dist <- boot_result$IBNR.Totals

  cat(paste0("\nTotal IBNR Percentiles (", line_name, "):\n"))
  ptiles <- quantile(total_ibnr_dist, probs = c(0.05, 0.25, 0.50, 0.75, 0.95))
  print(round(ptiles, 1))

  return(boot_result)
}

boot_motor  <- run_bootstrap_cl(motor_cl_tri,  "Motor")
boot_eng    <- run_bootstrap_cl(eng_cl_tri,    "Engineering")
boot_treaty <- run_bootstrap_cl(treaty_cl_tri, "Treaty")


# -----------------------------------------------------------------------------
# SECTION 6: Visualisations
# -----------------------------------------------------------------------------

# Plot 1: IBNR by Accident Year for each line

plot_ibnr_by_ay <- function(results_tbl, line_name) {

  p <- ggplot(results_tbl, aes(x = AccidentYear, y = IBNR_Cr)) +
    geom_col(fill = "#1a5276", alpha = 0.85) +
    geom_errorbar(
      aes(ymin = IBNR_Cr - MackSE_Cr, ymax = IBNR_Cr + MackSE_Cr),
      width = 0.3, colour = "#e74c3c", linewidth = 0.8
    ) +
    labs(
      title    = paste0(line_name, ": Chain Ladder IBNR by Accident Year"),
      subtitle = "Error bars = ± 1 Mack Standard Error",
      x        = "Accident Year",
      y        = "IBNR (INR Crores)",
      caption  = "Source: Synthetic data calibrated to IRDAI / GIC Re parameters"
    ) +
    theme_minimal(base_size = 12) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggsave(
    filename = paste0("outputs/plots/cl_ibnr_", tolower(line_name), ".png"),
    plot     = p,
    width    = 10, height = 6, dpi = 150
  )

  cat(paste0("Saved: outputs/plots/cl_ibnr_", tolower(line_name), ".png\n"))
  return(p)
}

plot_ibnr_by_ay(results_motor,  "Motor")
plot_ibnr_by_ay(results_eng,    "Engineering")
plot_ibnr_by_ay(results_treaty, "Treaty")


# Plot 2: Bootstrap IBNR distribution (fan chart) for Motor
plot(boot_motor, main = "Motor: Bootstrap IBNR Distribution")


# -----------------------------------------------------------------------------
# SECTION 7: Save Results for BF Comparison and Reporting
# -----------------------------------------------------------------------------

save(
  mack_motor,   mack_eng,   mack_treaty,
  boot_motor,   boot_eng,   boot_treaty,
  results_motor, results_eng, results_treaty,
  all_results_cl,
  file = "data/processed/cl_results.RData"
)

cat("\n=== SAVED: data/processed/cl_results.RData ===\n")
cat("Next step: Run 03_bf_method.R\n")
cat("\nKey question before you proceed: For Engineering, did you notice the\n")
cat("elevated link ratios in AY2018-19? That is EXACTLY why BF exists.\n")
