# =============================================================================
# 01_params.R
# All parameters for the NHIS 2026 Block 7c synthetic-data evaluation.
# Sourced by every other script. Do not run directly.
#
# Design reference: NHIS 2026 Schedule; PLFS 2024-25 Annual Report
# =============================================================================

# ── Reproducibility ──────────────────────────────────────────────────────────
MASTER_SEED  <- 20260419L
N_HOUSEHOLDS <- 75000L

# ── Paths ────────────────────────────────────────────────────────────────────
# Resolves paths relative to the project root. This script lives in 'R Files/'
# so the project root is one level up.
.find_scripts_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0)
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1]), mustWork = FALSE)))
  for (i in seq_len(sys.nframe())) {
    f <- sys.frame(i)$ofile
    if (!is.null(f) && nchar(f) > 0)
      return(dirname(normalizePath(f, mustWork = FALSE)))
  }
  warning("Cannot detect script directory; assuming working directory is 'R Files/'")
  normalizePath(".", mustWork = FALSE)
}
SCRIPTS_DIR <- .find_scripts_dir()
PROJECT_DIR <- dirname(SCRIPTS_DIR)
DATA_DIR    <- file.path(PROJECT_DIR, "data")
OUTPUT_DIR  <- file.path(PROJECT_DIR, "outputs")

# ── Panel structure ──────────────────────────────────────────────────────────
# Survey starts April 2026. Each panel surveys in one calendar month and
# refers to the preceding calendar month (NHIS Block 7c "last month").
# Ledger spans March 2026 -- February 2027 (12 months = one full cycle).
PANELS <- data.frame(
  panel           = 1:4,
  survey_month    = c("2026-04","2026-07","2026-10","2027-01"),
  reference_month = c("2026-03","2026-06","2026-09","2026-12"),
  stringsAsFactors = FALSE
)
LEDGER_MONTHS <- c(
  "2026-03","2026-04","2026-05","2026-06",
  "2026-07","2026-08","2026-09","2026-10",
  "2026-11","2026-12","2027-01","2027-02"
)
# Readable month labels aligned to LEDGER_MONTHS (for figures)
MONTH_LABELS <- c("Mar","Apr","May","Jun","Jul","Aug",
                  "Sep","Oct","Nov","Dec","Jan","Feb")

# ── Businesses per household ──────────────────────────────────────────────────
# Consistent with ASUSE 2024 (86.5% of enterprises are sole OAE, Dvara 2026 /
# MoSPI 2024). Produces ~9% multi-business HH.
NUM_BUS_PROBS <- c("1" = 0.91, "2" = 0.08, "3" = 0.01)

# ── Household typology ────────────────────────────────────────────────────────
# NSS 73rd Round: 98.3% of unincorporated non-ag enterprises are perennial
# (MoSPI 2016). We apply this directly: Cat 1+2 = 98%, Cat 3+4+5 = 2%.
#
#   Cat 1/2  Single or multi-business, continuous year-round
#            (flat vs seasonal split emerges from industry mix)
#   Cat 3    Single business, partial-year, contiguous operating window
#   Cat 4    Multiple businesses, concurrent year-round
#   Cat 5    Multiple businesses, sequential non-overlapping windows
#
# Probabilities below are P(cat | single-bus HH) and P(cat | multi-bus HH).
# Cat 3 is only possible for single-bus HH; Cat 4/5 only for multi-bus HH.
CAT3_PROB_GIVEN_SINGLE <- 0.011   # ~1.0% of all HH given 91% are single-bus
CAT5_PROB_GIVEN_MULTI  <- 0.056   # ~0.5% of all HH given 9% are multi-bus

# Partial-year window lengths (contiguous months of operation)
CAT3_WINDOW_MIN <- 5L; CAT3_WINDOW_MAX <- 8L
CAT5_WINDOW_MIN <- 3L; CAT5_WINDOW_MAX <- 5L
CAT5_MIN_GAP    <- 2L    # months of gap between the two Cat 5 windows

# ── Industries (11 categories, mapped to PLFS Table 27 NIC sections) ─────────
INDUSTRIES <- list(
  retail       = list(desc="Retail trade",          nic=c(45,46,47), section="G"),
  food_svc     = list(desc="Food services",          nic=c(55,56),    section="I"),
  personal_svc = list(desc="Personal services",      nic=c(95,96),    section="S"),
  mfg_light    = list(desc="Manufacturing (light)",  nic=c(10,13,14,15,16,17,18), section="C"),
  mfg_heavy    = list(desc="Manufacturing (heavy)",  nic=c(22,23,24,25,27,28,29,31,32), section="C"),
  construction = list(desc="Construction",           nic=c(41,42,43), section="F"),
  transport    = list(desc="Transport",              nic=c(49,50,52,53), section="H"),
  professional = list(desc="Professional services",  nic=c(62,69,70,71,73,74), section="prof"),
  health       = list(desc="Health services",         nic=c(86,87),    section="Q"),
  education    = list(desc="Education",               nic=c(85),       section="P"),
  other_svc    = list(desc="Other services",         nic=c(58,61,77,81,82,93), section="other")
)
IND_NAMES <- names(INDUSTRIES)

# Fallback all-India industry weights (used when state-level parse fails)
IND_WEIGHTS_RURAL_FB <- c(
  retail=0.22, food_svc=0.05, personal_svc=0.18,
  mfg_light=0.10, mfg_heavy=0.07, construction=0.13,
  transport=0.10, professional=0.02, health=0.04, education=0.02, other_svc=0.07
)

IND_WEIGHTS_URBAN_FB <- c(
  retail=0.27, food_svc=0.09, personal_svc=0.16,
  mfg_light=0.10, mfg_heavy=0.07, construction=0.07,
  transport=0.10, professional=0.06, health=0.04, education=0.04, other_svc=0.00
)

# ── Seasonality multipliers (12 months March..February) ─────────────────────
# Calibrated to documented patterns in Indian small-business sector.
# Retail/personal services peak Oct-Nov (Diwali); food services peak
# Oct-Feb (wedding season); construction troughs Jun-Aug (monsoon).
SEASONALITY <- list(
  retail       = c(0.78,0.85,0.92,0.95,0.98,1.05,1.12,1.30,1.35,1.15,0.95,0.80),
  food_svc     = c(0.95,0.85,0.80,0.70,0.75,0.85,0.95,1.15,1.30,1.25,1.15,1.10),
  personal_svc = c(0.80,0.80,0.85,0.70,0.75,0.90,1.00,1.15,1.40,1.40,1.20,0.85),
  mfg_light    = c(0.85,0.90,0.95,1.00,1.00,1.05,1.20,1.15,1.10,1.00,0.95,0.85),
  mfg_heavy    = c(0.98,1.00,1.02,1.02,1.00,0.98,1.00,1.02,1.05,1.00,0.96,0.97),
  construction = c(1.25,1.15,1.00,0.65,0.55,0.50,0.65,0.85,1.15,1.30,1.25,1.20),
  transport    = c(0.95,1.05,1.10,0.85,0.80,0.85,0.95,1.10,1.15,1.10,1.10,1.00),
  professional = c(1.00,1.02,1.05,1.00,0.95,0.98,1.00,1.00,1.00,1.00,1.00,1.00),
  health       = c(0.95,0.85,0.85,1.10,1.25,1.25,1.05,0.90,0.95,1.10,1.15,0.95),
  education    = c(1.10,1.25,1.20,0.70,0.65,0.85,1.10,1.20,0.90,0.80,0.75,0.80),
  other_svc    = c(0.95,0.95,1.00,0.95,0.95,1.00,1.05,1.10,1.10,1.05,0.95,0.95)
)
SEASONALITY_SCALE         <- 1.0   # fixed; no sweep
EXPENSE_SEASONALITY_DAMP  <- 0.60  # expenses follow revenue but more muted
EXPENSE_LAG_MONTHS        <- 1L    # expenses lag revenue by 1 month

# ── Closure parameters (Cat 1/2/4 only; Cat 3/5 use window assignment) ───────
# Expected months closed per year, by industry.
BASE_CLOSURE_MONTHS <- c(
  retail=0.20, food_svc=0.50, personal_svc=0.30,
  mfg_light=0.50, mfg_heavy=0.40, construction=2.00,
  transport=0.50, professional=0.40, health=0.60, education=1.20,
  other_svc=0.60
)
# Seasonal weights on closure probability (indexed March..February)
CLOSURE_SEASONAL_WEIGHTS <- list(
  construction = c(0.3,0.5,1.0,3.0,4.0,3.5,2.0,1.0,0.5,0.4,0.4,0.3),
  health       = c(0.8,0.7,0.7,1.3,1.6,1.6,1.2,0.8,0.9,1.2,1.3,0.9),
  education    = c(1.0,0.5,0.5,2.5,2.8,1.5,0.8,0.5,1.2,1.8,2.0,1.5)
)
CLOSURE_IDIO_BETA_A    <- 1.5
CLOSURE_IDIO_BETA_B    <- 20.0
CLOSED_MONTH_EXP_MIN   <- 200
CLOSED_MONTH_EXP_MAX   <- 1500

# ── Revenue distribution ──────────────────────────────────────────────────────
# Lognormal body calibrated to PLFS Table 40 state-level earnings.
# Pareto tail above Rs. 5 lakh/month (threshold; alpha = shape parameter).
LOGNORMAL_SIGMA_URBAN <- 1.05
LOGNORMAL_SIGMA_RURAL <- 0.95
CALIB_FACTOR_URBAN    <- 0.296  # empirically derived HH-profit/bus-revenue ratio
CALIB_FACTOR_RURAL    <- 0.269
PARETO_THRESHOLD      <- 500000
PARETO_ALPHA          <- 2.2
PARETO_PROB_URBAN     <- 0.008
PARETO_PROB_RURAL     <- 0.002

# ── Multi-business parameters ─────────────────────────────────────────────────
SECONDARY_SCALE_MIN <- 0.3; SECONDARY_SCALE_MAX <- 0.8
TERTIARY_SCALE_MIN  <- 0.2; TERTIARY_SCALE_MAX  <- 0.5
MONTHLY_NOISE_SD    <- 0.12
UNRELATED_CORR      <- 0.05

# Within-household industry correlation (for revenue co-movement)
RELATED_INDUSTRIES <- list(
  retail       = c(food_svc=0.70, personal_svc=0.30, other_svc=0.20),
  food_svc     = c(retail=0.70, other_svc=0.30),
  personal_svc = c(retail=0.60, other_svc=0.30),
  mfg_light    = c(retail=0.45, other_svc=0.25),
  mfg_heavy    = c(retail=0.35, other_svc=0.20),
  construction = c(transport=0.40, other_svc=0.30),
  transport    = c(other_svc=0.30, retail=0.20),
  professional = c(other_svc=0.30, health=0.15, education=0.10),
  health       = c(retail=0.20, other_svc=0.15),
  education    = c(retail=0.15, other_svc=0.15, professional=0.10),
  other_svc    = c(retail=0.25, personal_svc=0.20)
)
RELATED_PROB <- 0.60  # probability secondary business is drawn from related set

# ── Gross margin ranges by industry ──────────────────────────────────────────
GROSS_MARGIN_RANGE <- list(
  retail       = c(0.08, 0.20),
  food_svc     = c(0.15, 0.35),
  personal_svc = c(0.40, 0.65),
  mfg_light    = c(0.15, 0.30),
  mfg_heavy    = c(0.12, 0.25),
  construction = c(0.10, 0.25),
  transport    = c(0.20, 0.40),
  professional = c(0.50, 0.80),
  health       = c(0.35, 0.60),
  education    = c(0.55, 0.80),
  other_svc    = c(0.25, 0.50)
)

# ── Demographics ──────────────────────────────────────────────────────────────
HH_SIZE_URBAN <- c("2"=0.10,"3"=0.25,"4"=0.35,"5"=0.18,"6"=0.08,"7"=0.04)
HH_SIZE_RURAL <- c("2"=0.06,"3"=0.15,"4"=0.28,"5"=0.22,"6"=0.15,"7"=0.14)
SOCIAL_GROUP_URBAN <- c(ST=0.04, SC=0.13, OBC=0.45, Other=0.38)
SOCIAL_GROUP_RURAL <- c(ST=0.13, SC=0.21, OBC=0.46, Other=0.20)

# ── Simulator passes ──────────────────────────────────────────────────────────
# Pass A: design-only (no reporting error)
# Pass B: 5% revenue under-reporting, 5% expense over-reporting, multiplicative
REPORT_BIAS_REV <- 0.95
REPORT_BIAS_EXP <- 1.05

# ── Helper: weighted sample from named vector ─────────────────────────────────
wsample <- function(x_named, size = 1, replace = TRUE) {
  sample(names(x_named), size = size, prob = x_named, replace = replace)
}

# State-specific industry weights derived from PLFS Table 26 and 27
state_industry_weights <- function(state_row, sector) {
  raw <- c(
    retail       = state_row[["ind_G_trade"]],
    food_svc     = state_row[["ind_I_accomfood"]],
    personal_svc = state_row[["ind_S_other_svc"]] * 0.82,
    mfg_light    = state_row[["ind_C_mfg"]] * 0.58,
    mfg_heavy    = state_row[["ind_C_mfg"]] * 0.42,
    construction = state_row[["ind_F_construction"]],
    transport    = state_row[["ind_H_transport"]],
    professional = state_row[["ind_professional"]],
    health       = state_row[["ind_Q_health"]],
    education    = state_row[["ind_P_education"]],
    other_svc    = state_row[["ind_S_other_svc"]] * 0.18
  )
  raw[is.na(raw)] <- 0
  total <- sum(raw)
  if (total <= 0) {
    return(if (sector == "urban") IND_WEIGHTS_URBAN_FB else IND_WEIGHTS_RURAL_FB)
  }
  w <- raw / total
  if (sector == "urban") {
    shift <- 0.40
    w <- (1 - shift) * w + shift * IND_WEIGHTS_URBAN_FB[names(w)]
    w <- w / sum(w)
  }
  w
}

# lognormal mu for state-sector
state_mu_log <- function(state_row, sector) {
  if (sector == "urban") {
    target <- state_row[["urban_mean_earnings"]]
    sigma  <- LOGNORMAL_SIGMA_URBAN
    calib  <- CALIB_FACTOR_URBAN
  } else {
    target <- state_row[["rural_mean_earnings"]]
    sigma  <- LOGNORMAL_SIGMA_RURAL
    calib  <- CALIB_FACTOR_RURAL
  }
  r_primary <- target * 1.17 / calib
  mu <- log(r_primary) - sigma^2 / 2
  list(mu = mu, sigma = sigma)
}

# Seasonal expense pattern for an industry
expense_season <- function(ind) {
  s <- SEASONALITY[[ind]]
  damped <- 1.0 + EXPENSE_SEASONALITY_DAMP * (s - 1.0)
  # lag by EXPENSE_LAG_MONTHS positions (roll forward)
  n <- length(damped)
  lag <- EXPENSE_LAG_MONTHS %% n
  c(damped[(n - lag + 1):n], damped[1:(n - lag)])
}
