# India MET Reserving: Chain Ladder & Bornhuetter-Ferguson Automation

**Loss development triangle automation for Indian Motor, Engineering, and Treaty lines**  
Built in R · IFoA-aligned methodology · Calibrated to IRDAI & GIC Re data

---

## What This Project Does

This tool automates actuarial loss reserve estimation for three non-life insurance lines common in the Indian reinsurance market:

| Line | Tail Characteristics | Key Challenge |
|------|---------------------|---------------|
| **Motor** (TP + OD) | Long-tail (MACT delays) | Slower development than western markets |
| **Engineering** (IAR, CAR, EAR) | Short-medium tail, lumpy | CAT contamination (Kerala 2018, Fani 2019) |
| **Treaty** (Proportional + XL) | Blended tail | Diversified cedant mix, immature years |

Two reserving methods are implemented:
- **Chain Ladder (Mack, 1993)** — development pattern extrapolation with standard errors
- **Bornhuetter-Ferguson** — credibility blend of a priori ELR and actual emergence

A method selection framework applies actuarial judgement rules to determine which method (or blend) is most appropriate for each accident year.

---

## Repository Structure

```
india-met-reserving/
│
├── README.md
├── data/
│   ├── raw/              ← IRDAI PDFs, GIC Re annual report extracts
│   ├── processed/        ← Constructed triangles (.RData)
│   └── synthetic/        ← Simulated triangles for methodology validation
│
├── R/
│   ├── 05_synthetic_data.R   ← Generate realistic Indian MET triangles
│   ├── 02_chain_ladder.R     ← Mack CL + Bootstrap CL implementation
│   ├── 03_bf_method.R        ← BF with a priori ELR selection + sensitivity
│   └── 04_comparison.R       ← Method selection + final reserve estimates
│
├── report/
│   └── india_met_reserving.qmd   ← Quarto actuarial report (renders to PDF)
│
├── dashboard/
│   └── app.R                 ← Shiny interactive dashboard
│
└── outputs/
    ├── plots/                ← All generated charts (PNG)
    └── tables/               ← CSV outputs for reporting
```

---

## Methodology

### Chain Ladder (Mack)

Volume-weighted age-to-age factors:

$$f_j = \frac{\sum_{i=1}^{n-j} C_{i,j+1}}{\sum_{i=1}^{n-j} C_{i,j}}$$

Mack (1993) standard error provides a distribution-free measure of reserve uncertainty without assuming a specific claims distribution.

Bootstrap CL (overdispersed Poisson) is used for full IBNR distribution.

### Bornhuetter-Ferguson

IBNR estimated as:

$$\text{IBNR}_{BF} = \text{ELR} \times \text{Premium} \times \left(1 - \frac{1}{CDF}\right)$$

Where:
- **ELR** (a priori expected loss ratio) is sourced from IRDAI industry data and GIC Re disclosures
- **CDF** is the cumulative development factor from Chain Ladder
- **(1 - 1/CDF)** is the proportion of ultimate *not yet emerged*

### A Priori ELR Sources

| Line | ELR Used | Source |
|------|----------|--------|
| Motor | 77% | IRDAI Annual Report 2022-23, industry net incurred loss ratio |
| Engineering | 70% | IRDAI Eng segment, normalised for CAT years |
| Treaty | 73% | GIC Re Annual Report 2022-23, implied net loss ratio |

### Method Selection Rules

| Condition | Preferred Method |
|-----------|-----------------|
| % emerged < 60% (immature AY) | BF |
| CAT-contaminated AY (Eng 2018, 2019) | BF |
| % emerged ≥ 90% (mature AY) | Chain Ladder |
| Intermediate (60–90% emerged) | Credibility blend |

---

## Key Findings

1. **Motor**: CL and BF are closely aligned. High volume → stable link ratios → CL is credible. Minor BF preference for AY2022-23 (immature).

2. **Engineering**: Significant CL–BF divergence in AY2018 and AY2019. CAT losses from Kerala floods and Cyclone Fani inflated CL link ratios. BF is the preferred method. CL overstates the reserve by applying CAT development factors to non-CAT future years.

3. **Treaty**: Intermediate divergence. BF preferred for the two most recent AYs. Blend appropriate for AY2020-21 to AY2021-22.

---

## Indian Market Context

**Why Motor TP development is slower than western benchmarks:**  
Indian Motor Third Party claims are adjudicated by Motor Accidents Claims Tribunals (MACT). Judicial delays mean that bodily injury claims can develop for 5–7 years, significantly longer than typical UK/European Motor TP patterns. This affects tail factor selection in production reserving work.

**Why Engineering triangles have CAT contamination:**  
Indian Engineering lines suffered three major catastrophe events in close succession — Chennai floods (2015), Kerala floods (2018), and Cyclone Fani (2019). Each contaminated accident year's development pattern. Insurers and reinsurers using pure CL without exclusion of these years will project inflated development into future non-CAT years.

**Treaty portfolio context:**  
GIC Re's treaty book (INR ~8,000-9,000 Cr treaty premium as of 2022-23) spans a wide cedant mix. Development patterns are blended across short-tail (property, fire) and long-tail (marine, liability) cessions. The methodology here models a hypothetical proportional + XL treaty portfolio.

---

## How to Run

### Prerequisites

```r
install.packages(c("ChainLadder", "actuar", "dplyr", "tibble",
                   "ggplot2", "tidyr", "purrr", "scales"))
```

### Execution Order

```r
source("R/05_synthetic_data.R")   # Generate triangles
source("R/02_chain_ladder.R")     # Run Chain Ladder
source("R/03_bf_method.R")        # Run BF
source("R/04_comparison.R")       # Final comparison + export
```

### Render Report

```r
quarto::quarto_render("report/india_met_reserving.qmd")
```

### Run Dashboard

```r
shiny::runApp("dashboard/app.R")
```

---

## Data Sources

- **IRDAI Annual Report 2022-23**: [irdai.gov.in](https://irdai.gov.in)
- **IRDAI Handbook of Indian Insurance Statistics 2022-23**: [irdai.gov.in](https://irdai.gov.in)
- **GIC Re Annual Report 2022-23**: [gicofindia.com](https://www.gicofindia.com)
- **Swiss Re Sigma**: Market context and global reinsurance benchmarks
- **Mack, T. (1993)**: *Distribution-Free Calculation of the Standard Error of Chain Ladder Reserve Estimates*, ASTIN Bulletin

Triangles in this project are **synthetic** — generated from calibrated parameters rather than extracted from proprietary data. The simulation methodology and parameter sources are documented in `R/05_synthetic_data.R`.

---

## About

Built as part of an IFoA actuarial qualification journey, focused on the Indian reinsurance market. Feedback welcome from practitioners in Indian non-life reserving.

**Background**: B.Com (Economics & Analytics), SVKM's Narsee Monjee College, Mumbai. CB2 cleared. Targeting reinsurance actuarial roles in India.

---

*For methodology questions or to discuss Indian MET reserving, connect on LinkedIn.*
