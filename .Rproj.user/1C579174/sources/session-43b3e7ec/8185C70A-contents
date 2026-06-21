# =============================================================================
# 05_synthetic_data.R
# India MET Reserving Project
# Purpose: Generate synthetic but realistic loss development triangles for
#          Motor, Engineering, and Treaty lines calibrated to Indian market
#          parameters sourced from IRDAI reports and GIC Re disclosures.
#
# Why synthetic first?
#   Public IRDAI data is not in triangle format. We build synthetic triangles
#   with realistic Indian market assumptions, validate our Chain Ladder and
#   Bornhuetter-Ferguson implementations here, then overlay real extracted
#   data in later scripts.
#
# Author: [Your Name]
# Date: June 2025
# Packages: ChainLadder, actuar, dplyr, tibble
# =============================================================================


# -----------------------------------------------------------------------------
# SECTION 0: Setup
# -----------------------------------------------------------------------------

# Install packages if you don't have them yet.
# Run this block once, then comment it out.
# install.packages("ChainLadder")
# install.packages("actuar")
# install.packages("dplyr")
# install.packages("tibble")
# install.packages("ggplot2")

library(ChainLadder)   # Core reserving methods: MackCL, BootstrapCL, BF
library(actuar)        # Actuarial distributions (useful for severity simulation)
library(dplyr)         # Data manipulation
library(tibble)        # Clean data frames
library(ggplot2)       # Plotting


# -----------------------------------------------------------------------------
# SECTION 1: Why These Parameters? (READ THIS BEFORE THE CODE)
# -----------------------------------------------------------------------------

# The numbers below are NOT made up randomly. They are calibrated to:
#
# MOTOR (Third Party + Own Damage combined):
#   - IRDAI Annual Report 2022-23: Industry incurred loss ratio ~75-80% for Motor
#   - TP claims are long-tail in India (court delays, MACT tribunals) → slower
#     development in later periods compared to mature markets
#   - Development pattern: most claims reported by year 2, but TP bodily injury
#     can develop for 5+ years
#
# ENGINEERING (Industrial All Risk, CAR, EAR):
#   - Small volume, high severity, catastrophe-exposed
#   - Indian Eng lines hit by: Chennai floods 2015, Kerala floods 2018, Cyclone
#     Fani 2019 — so triangles have "contamination" in specific AYs
#   - IRDAI shows Eng combined ratio often >100% in CAT years
#   - Development is faster than Motor but lumpy
#
# TREATY (GIC Re proportional + XL treaty portfolio):
#   - GIC Re Annual Report 2022-23: Treaty premium ~INR 8,000-9,000 Cr
#   - Mix of short-tail (fire, property) and long-tail (liability, marine)
#   - We simulate a blended treaty portfolio with intermediate tail
#
# Accident Year (AY) range: 2016-17 to 2023-24 (8 years)
# Development periods: 1 to 8 (years of development)
# Currency: INR Crores (1 Cr = 10 million INR)


# -----------------------------------------------------------------------------
# SECTION 2: Helper Function — Build a Triangle from Parameters
# -----------------------------------------------------------------------------

# This function generates a cumulative loss development triangle.
# Inputs:
#   n_ay          : number of accident years
#   n_dev         : number of development periods
#   premium       : vector of earned premiums by AY (length = n_ay)
#   ult_lr        : ultimate loss ratio (scalar or vector by AY)
#   dev_pattern   : cumulative % of ultimate emerged by each dev period
#                   (must be length n_dev, last value = 1.0)
#   cv_process    : coefficient of variation for process noise (random fluctuation)
#   seed          : random seed for reproducibility

generate_triangle <- function(n_ay,
                              n_dev,
                              premium,
                              ult_lr,
                              dev_pattern,
                              cv_process = 0.05,
                              seed = 42) {

  set.seed(seed)

  # Validate inputs
  stopifnot(length(dev_pattern) == n_dev)
  stopifnot(abs(dev_pattern[n_dev] - 1.0) < 1e-6)  # last pattern must = 1
  stopifnot(length(premium) == n_ay)

  # If ult_lr is a single number, repeat it for all AYs
  if (length(ult_lr) == 1) {
    ult_lr <- rep(ult_lr, n_ay)
  }

  # Calculate ultimate expected losses by AY
  ultimate <- premium * ult_lr

  # Build the full (unobserved) incremental triangle first
  # Then convert to cumulative and apply the "upper-left" diagonal cut
  # (i.e., only keep data where AY + dev - 1 <= current calendar year)

  triangle_matrix <- matrix(NA, nrow = n_ay, ncol = n_dev)

  for (i in 1:n_ay) {
    for (j in 1:n_dev) {

      # Only populate cells that would be observable
      # AY index i corresponds to accident year (start_year + i - 1)
      # Observable if: i + j - 1 <= n_dev  (standard upper-left triangle)
      if (i + j - 1 <= n_dev) {

        # Expected cumulative losses at this development period
        expected_cum <- ultimate[i] * dev_pattern[j]

        # Add process noise using a lognormal distribution
        # Mean = expected_cum, CV = cv_process
        # Lognormal params: mu and sigma derived from mean and CV
        if (expected_cum > 0) {
          sigma_ln <- sqrt(log(1 + cv_process^2))
          mu_ln    <- log(expected_cum) - 0.5 * sigma_ln^2
          triangle_matrix[i, j] <- rlnorm(1, meanlog = mu_ln, sdlog = sigma_ln)
        } else {
          triangle_matrix[i, j] <- 0
        }
      }
    }
  }

  # Row names = accident years, col names = development periods
  ay_labels  <- paste0("AY", 2016:(2016 + n_ay - 1))
  dev_labels <- paste0("Dev", 1:n_dev)
  rownames(triangle_matrix) <- ay_labels
  colnames(triangle_matrix) <- dev_labels

  return(triangle_matrix)
}


# -----------------------------------------------------------------------------
# SECTION 3: Motor Triangle
# -----------------------------------------------------------------------------

# Parameters calibrated to IRDAI Motor data
# Premium growing at ~12% per year (Indian motor fleet growth + TP price hikes)

motor_premium <- c(8500, 9520, 10660, 11940, 13370, 14970, 16770, 18780)
# Units: INR Crores. Base 8,500 Cr in AY2016, growing ~12% YoY.
# (Rough order of magnitude for a large composite insurer's Motor book)

# Ultimate loss ratio: ~78% average, with some year-to-year variation
# AY2018-19 slightly worse due to Kerala floods affecting Motor TP (statewide)
motor_ult_lr <- c(0.77, 0.78, 0.81, 0.78, 0.77, 0.76, 0.77, 0.78)

# Development pattern: % of ultimate emerged by Dev period 1, 2, ... 8
# Motor TP is slow — India-specific due to MACT court delays
# Own Damage is faster. This blended pattern reflects the mix.
# Source: Approximated from GIC Re published development factors, adjusted
#         for Indian judicial lag (typically +1 development period vs UK/Europe)
motor_dev_pattern <- c(0.45, 0.68, 0.80, 0.88, 0.93, 0.96, 0.98, 1.00)
#                       ^                                            ^
#                   Only 45% emerged   Slower than western        Full
#                   by end of year 1   markets due to MACT        ultimate

motor_triangle <- generate_triangle(
  n_ay        = 8,
  n_dev       = 8,
  premium     = motor_premium,
  ult_lr      = motor_ult_lr,
  dev_pattern = motor_dev_pattern,
  cv_process  = 0.04,   # Motor is high-volume → lower process variance
  seed        = 101
)

cat("=== MOTOR TRIANGLE (Cumulative Incurred Losses, INR Crores) ===\n")
print(round(motor_triangle, 1))


# -----------------------------------------------------------------------------
# SECTION 4: Engineering Triangle
# -----------------------------------------------------------------------------

# Engineering is low-volume, high-severity, CAT-exposed
# Premium much smaller; severe losses in specific AYs

eng_premium <- c(420, 460, 510, 555, 600, 650, 710, 775)
# Growing at ~10% YoY. Engineering premiums in India grew with infrastructure spend.

# AY2018 (Kerala floods) and AY2019 (Cyclone Fani) had elevated loss ratios
# This is the "contamination" problem — CAT years distort CL development factors
eng_ult_lr <- c(0.68, 0.70, 0.95, 0.85, 0.72, 0.70, 0.71, 0.72)
#                              ^AY2018   ^AY2019
#                         Kerala floods  Cyclone Fani

# Engineering develops faster than Motor (most losses reported within 3 years)
# But lumpy — large single-risk losses can emerge late
eng_dev_pattern <- c(0.55, 0.78, 0.90, 0.95, 0.97, 0.98, 0.99, 1.00)

eng_triangle <- generate_triangle(
  n_ay        = 8,
  n_dev       = 8,
  premium     = eng_premium,
  ult_lr      = eng_ult_lr,
  dev_pattern = eng_dev_pattern,
  cv_process  = 0.12,   # Engineering is low-volume → higher process variance
  seed        = 202
)

cat("\n=== ENGINEERING TRIANGLE (Cumulative Incurred Losses, INR Crores) ===\n")
print(round(eng_triangle, 1))


# -----------------------------------------------------------------------------
# SECTION 5: Treaty Triangle
# -----------------------------------------------------------------------------

# GIC Re treaty portfolio: blended proportional + XL across multiple cedants
# This represents a hypothetical treaty underwriter's portfolio

treaty_premium <- c(1200, 1320, 1450, 1595, 1755, 1930, 2120, 2335)

# Treaty loss ratio is influenced by the mix of business ceded
# Relatively stable in non-CAT years
treaty_ult_lr <- c(0.72, 0.73, 0.88, 0.80, 0.74, 0.73, 0.74, 0.75)
#                              ^AY2018 elevated (receives CAT from cedants)

# Treaty tail is intermediate — faster than pure TP Motor, slower than Property
treaty_dev_pattern <- c(0.50, 0.72, 0.85, 0.92, 0.96, 0.98, 0.99, 1.00)

treaty_triangle <- generate_triangle(
  n_ay        = 8,
  n_dev       = 8,
  premium     = treaty_premium,
  ult_lr      = treaty_ult_lr,
  dev_pattern = treaty_dev_pattern,
  cv_process  = 0.07,
  seed        = 303
)

cat("\n=== TREATY TRIANGLE (Cumulative Incurred Losses, INR Crores) ===\n")
print(round(treaty_triangle, 1))


# -----------------------------------------------------------------------------
# SECTION 6: Convert to ChainLadder Triangle Objects
# -----------------------------------------------------------------------------

# The ChainLadder package has its own triangle class.
# We need to convert our matrices to this format for the CL and BF functions.
# as.triangle() from ChainLadder does this — it expects a matrix with
# accident years as rows and development periods as columns.

motor_cl_tri   <- as.triangle(motor_triangle)
eng_cl_tri     <- as.triangle(eng_triangle)
treaty_cl_tri  <- as.triangle(treaty_triangle)

# Quick sanity check — plot the development patterns
# This is the most important visual in reserving: do losses develop smoothly?
cat("\n=== PLOTTING DEVELOPMENT PATTERNS ===\n")
cat("Check plots/ folder for output\n")

# We'll save plots properly in later scripts. For now, quick visual check:
plot(motor_cl_tri,
     main = "Motor: Cumulative Loss Development by Accident Year",
     xlab = "Development Period",
     ylab = "Cumulative Incurred Losses (INR Cr)")


# -----------------------------------------------------------------------------
# SECTION 7: Save Triangles for Use in Later Scripts
# -----------------------------------------------------------------------------

# Save as .RData so 02_chain_ladder.R and 03_bf_method.R can load them
# without re-running the simulation

save(
  motor_triangle,   motor_cl_tri,   motor_premium,   motor_ult_lr,
  eng_triangle,     eng_cl_tri,     eng_premium,     eng_ult_lr,
  treaty_triangle,  treaty_cl_tri,  treaty_premium,  treaty_ult_lr,
  file = "data/processed/synthetic_triangles.RData"
)

cat("\n=== SAVED: data/processed/synthetic_triangles.RData ===\n")
cat("Next step: Run 02_chain_ladder.R\n")


# -----------------------------------------------------------------------------
# SECTION 8: Quick Validation — Does Our Triangle Look Right?
# -----------------------------------------------------------------------------

# Before moving to CL/BF, sanity-check the triangles:
# 1. Are losses monotonically increasing across development periods? (should be)
# 2. Are the implied ultimates (latest diagonal / dev pattern) close to true ultimates?

validate_triangle <- function(triangle_matrix, dev_pattern, premium, ult_lr, line_name) {

  cat(paste0("\n--- Validation: ", line_name, " ---\n"))
  n_ay  <- nrow(triangle_matrix)
  n_dev <- ncol(triangle_matrix)

  # Check monotonicity (each row should be non-decreasing)
  mono_check <- apply(triangle_matrix, 1, function(row) {
    obs <- row[!is.na(row)]
    all(diff(obs) >= 0)
  })

  cat("Monotonicity check (all TRUE = good):\n")
  print(mono_check)

  # Implied ultimate: latest observed / cumulative dev pattern at that period
  latest_diag     <- diag(triangle_matrix[n_ay:1, ])[n_ay:1]
  latest_dev_idx  <- n_dev - (0:(n_ay - 1))  # which dev period is latest for each AY
  latest_dev_pct  <- dev_pattern[latest_dev_idx]
  implied_ult     <- latest_diag / latest_dev_pct
  true_ult        <- premium * ult_lr

  comparison <- tibble(
    AccidentYear  = rownames(triangle_matrix),
    TrueUltimate  = round(true_ult, 1),
    ImpliedUlt    = round(implied_ult, 1),
    Diff_pct      = round((implied_ult / true_ult - 1) * 100, 1)
  )

  cat("Implied vs True Ultimate (differences due to process noise):\n")
  print(comparison)
}

validate_triangle(motor_triangle,  motor_dev_pattern,  motor_premium,  motor_ult_lr,  "Motor")
validate_triangle(eng_triangle,    eng_dev_pattern,    eng_premium,    eng_ult_lr,    "Engineering")
validate_triangle(treaty_triangle, treaty_dev_pattern, treaty_premium, treaty_ult_lr, "Treaty")

cat("\n=== 05_synthetic_data.R COMPLETE ===\n")
cat("Triangles look reasonable if Diff_pct is within +/- 10% for most AYs.\n")
cat("Larger deviations in immature AYs (AY2022+) are expected and normal.\n")
