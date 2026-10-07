# =============================================================================
# 02_generate.R
# Generate the synthetic NHIS ground-truth dataset.
#
# Outputs (written to data/):
#   households.csv       — one row per household
#   businesses.csv       — one row per business
#   monthly_ledger.csv   — one row per business × month (12 months)
#
# Run time: approximately 4-6 minutes for N = 75,000 HH.
# =============================================================================
suppressMessages({
  library(data.table)
})

# Source params (resolves paths relative to this script). Loads parameters
# into the current environment if not already present.
if (!exists("MASTER_SEED")) {
  .this_file <- (function() {
    args <- commandArgs(trailingOnly = FALSE)
    file_arg <- grep("^--file=", args, value = TRUE)
    if (length(file_arg) > 0) return(sub("^--file=", "", file_arg[1]))
    for (i in seq_len(sys.nframe())) {
      f <- sys.frame(i)$ofile
      if (!is.null(f)) return(f)
    }
    "."
  })()
  source(file.path(dirname(normalizePath(.this_file, mustWork = FALSE)),
                   "01_params.R"))
}

set.seed(MASTER_SEED)

# ── Load state-level PLFS calibration data ───────────────────────────────────
states_df <- as.data.table(read.csv(file.path(DATA_DIR, "state_level_inputs.csv")))

# ── State allocation: proportional to non-ag self-emp HH population ──────────
state_allocate <- function(n_total) {
  states_df[, w_r := rural_est_hh_x100  * rural_se_nonag_pct  / 100]
  states_df[, w_u := urban_est_hh_x100  * urban_se_total_pct  / 100 * 0.862]
  states_df[, total_w := w_r + w_u]
  total_w <- sum(states_df$total_w)
  shares <- states_df$total_w / total_w
  n_each <- as.integer(round(n_total * shares))
  # rounding fix
  diff <- n_total - sum(n_each)
  if (diff != 0) n_each[which.max(n_each)] <- n_each[which.max(n_each)] + diff
  rural_share <- states_df$w_r / states_df$total_w
  rural_share[is.nan(rural_share)] <- 0.5
  n_r <- as.integer(round(n_each * rural_share))
  n_u <- n_each - n_r
  data.table(state = states_df$state, n_rural = n_r, n_urban = n_u)
}

alloc <- state_allocate(N_HOUSEHOLDS)
cat(sprintf("Generating %s households across %d states...\n",
            format(N_HOUSEHOLDS, big.mark=","), nrow(alloc)))

# ── HH type assignment ────────────────────────────────────────────────────────
assign_hh_type <- function(n_bus) {
  if (n_bus == 1) {
    if (runif(1) < CAT3_PROB_GIVEN_SINGLE) "single_partial" else "single_continuous"
  } else {
    if (runif(1) < CAT5_PROB_GIVEN_MULTI)  "multi_sequential" else "multi_concurrent"
  }
}

# ── Contiguous operating window for Cat 3 ────────────────────────────────────
cat3_window <- function() {
  length <- sample(CAT3_WINDOW_MIN:CAT3_WINDOW_MAX, 1)
  start  <- sample(1:(12 - length + 1), 1)
  op     <- rep(FALSE, 12)
  op[start:(start + length - 1)] <- TRUE
  op
}

# ── Sequential non-overlapping windows for Cat 5 ─────────────────────────────
cat5_windows <- function(n_bus) {
  for (attempt in 1:30) {
    lengths <- sample(CAT5_WINDOW_MIN:CAT5_WINDOW_MAX, n_bus, replace = TRUE)
    total_needed <- sum(lengths) + CAT5_MIN_GAP * (n_bus - 1)
    if (total_needed > 12) next
    slack <- 12 - total_needed
    extra_gaps <- as.integer(diff(c(0, sort(sample(0:slack, n_bus - 1,
                                                   replace = TRUE)), slack)))
    windows <- vector("list", n_bus)
    pos <- 1 + extra_gaps[1]
    for (i in seq_len(n_bus)) {
      op <- rep(FALSE, 12)
      op[pos:(pos + lengths[i] - 1)] <- TRUE
      windows[[i]] <- op
      if (i < n_bus) pos <- pos + lengths[i] + CAT5_MIN_GAP + extra_gaps[i + 1]
    }
    # Verify no overlap and all within bounds
    ok <- all(sapply(windows, length) == 12) &&
          sum(Reduce("+", lapply(windows, as.integer))) == sum(lengths) &&
          max(which(Reduce("|", windows))) <= 12
    if (ok) return(windows)
  }
  # Fallback: evenly spaced
  w_len <- (12 - CAT5_MIN_GAP * (n_bus - 1)) %/% n_bus
  windows <- vector("list", n_bus)
  pos <- 1
  for (i in seq_len(n_bus)) {
    op <- rep(FALSE, 12)
    op[pos:min(12, pos + w_len - 1)] <- TRUE
    windows[[i]] <- op
    pos <- pos + w_len + CAT5_MIN_GAP
  }
  windows
}

# ── Revenue draw: lognormal body + Pareto tail ────────────────────────────────
draw_revenue <- function(mu, sigma, sector) {
  pareto_prob <- if (sector == "urban") PARETO_PROB_URBAN else PARETO_PROB_RURAL
  if (runif(1) < pareto_prob) {
    monthly <- PARETO_THRESHOLD * (1 - runif(1))^(-1 / PARETO_ALPHA)
  } else {
    monthly <- rlnorm(1, meanlog = mu, sdlog = sigma)
    monthly <- min(monthly, PARETO_THRESHOLD * 0.95)
  }
  monthly * 12  # annual
}

# ── Sample industry (with related-industry correlation) ───────────────────────
sample_industry <- function(iw, primary = NULL) {
  if (is.null(primary)) {
    return(wsample(iw))
  }
  if (runif(1) < RELATED_PROB && !is.null(RELATED_INDUSTRIES[[primary]])) {
    rel <- RELATED_INDUSTRIES[[primary]]
    return(wsample(rel))
  }
  wsample(iw)
}

corr_for_pair <- function(primary, secondary) {
  rel <- RELATED_INDUSTRIES[[primary]]
  if (!is.null(rel) && secondary %in% names(rel)) rel[[secondary]] else UNRELATED_CORR
}

# ── Pre-compute scaled seasonality and expense seasonality ───────────────────
season_rev <- lapply(SEASONALITY, function(s) {
  1.0 + SEASONALITY_SCALE * (s - 1.0)
})
season_exp <- lapply(names(SEASONALITY), function(ind) expense_season(ind))
names(season_exp) <- names(SEASONALITY)

# ── Main generation loop ─────────────────────────────────────────────────────
hh_rows      <- vector("list", N_HOUSEHOLDS)
bus_rows     <- vector("list", N_HOUSEHOLDS * 1.1)
ledger_rows  <- vector("list", N_HOUSEHOLDS * 1.1 * 12)
hh_id <- 1L; bid <- 1L; lid <- 1L

for (si in seq_len(nrow(alloc))) {
  st    <- alloc$state[si]
  srow  <- states_df[state == st]

  for (sector in c("rural", "urban")) {
    n <- if (sector == "rural") alloc$n_rural[si] else alloc$n_urban[si]
    if (n == 0) next

    iw       <- state_industry_weights(srow, sector)
    mu_sigma <- state_mu_log(srow, sector)
    mu       <- mu_sigma$mu
    sigma    <- mu_sigma$sigma

    hh_sz_p  <- if (sector == "urban") HH_SIZE_URBAN else HH_SIZE_RURAL
    sg_p     <- if (sector == "urban") SOCIAL_GROUP_URBAN else SOCIAL_GROUP_RURAL

    for (i in seq_len(n)) {
      panel_i  <- sample(1:4, 1)
      pan_row  <- PANELS[PANELS$panel == panel_i, ]
      n_bus    <- as.integer(wsample(NUM_BUS_PROBS))
      hh_type  <- assign_hh_type(n_bus)
      hh_sz    <- as.integer(wsample(hh_sz_p))
      sg       <- wsample(sg_p)
      earn_mem <- max(1L, min(hh_sz, n_bus + sample(0:1, 1)))

      hh_rows[[hh_id]] <- list(
        hh_id           = hh_id,
        state           = st,
        sector          = sector,
        panel           = panel_i,
        survey_month    = pan_row$survey_month,
        reference_month = pan_row$reference_month,
        hh_size         = hh_sz,
        social_group    = sg,
        num_businesses  = n_bus,
        num_earning_members = earn_mem,
        hh_type         = hh_type,
        mu_log          = mu,
        sigma_log       = sigma
      )

      # ── Determine operating windows ────────────────────────────────────
      if (hh_type == "single_partial") {
        op_windows <- list(cat3_window())
      } else if (hh_type == "multi_sequential") {
        op_windows <- cat5_windows(n_bus)
      } else {
        op_windows <- replicate(n_bus, rep(TRUE, 12), simplify = FALSE)
      }

      # ── Industries, revenues, gross margins ───────────────────────────
      primary_ind <- sample_industry(iw)
      industries  <- character(n_bus)
      corrs       <- numeric(n_bus)
      industries[1] <- primary_ind; corrs[1] <- 1.0
      for (b in seq_len(n_bus)[-1]) {
        industries[b] <- sample_industry(iw, primary_ind)
        corrs[b]      <- corr_for_pair(primary_ind, industries[b])
      }

      prim_annual_rev <- draw_revenue(mu, sigma, sector)
      prim_gm <- runif(1, GROSS_MARGIN_RANGE[[primary_ind]][1],
                          GROSS_MARGIN_RANGE[[primary_ind]][2])
      hh_closure_boost <- rbeta(1, CLOSURE_IDIO_BETA_A, CLOSURE_IDIO_BETA_B)

      # Shared HH-level monthly noise factor (drives within-HH revenue correlation)
      shared_noise <- rlnorm(12, meanlog = -0.5 * MONTHLY_NOISE_SD^2,
                             sdlog = MONTHLY_NOISE_SD)

      for (b in seq_len(n_bus)) {
        ind <- industries[b]
        gm  <- if (b == 1) prim_gm else
                 runif(1, GROSS_MARGIN_RANGE[[ind]][1], GROSS_MARGIN_RANGE[[ind]][2])
        ann_rev <- if (b == 1) prim_annual_rev else {
          sc <- if (b == 2) runif(1, SECONDARY_SCALE_MIN, SECONDARY_SCALE_MAX) else
                            runif(1, TERTIARY_SCALE_MIN,  TERTIARY_SCALE_MAX)
          prim_annual_rev * sc
        }

        # Monthly noise: blend shared and idiosyncratic per correlation
        idio_noise  <- rlnorm(12, meanlog = -0.5 * MONTHLY_NOISE_SD^2,
                              sdlog = MONTHLY_NOISE_SD)
        corr_b      <- corrs[b]
        if (b == 1) {
          combined_noise <- shared_noise
        } else {
          combined_noise <- exp(sqrt(corr_b) * log(shared_noise) +
                               sqrt(1 - corr_b) * log(idio_noise))
        }

        base_monthly     <- ann_rev / 12
        base_monthly_exp <- base_monthly * (1 - gm)
        monthly_rev <- base_monthly * season_rev[[ind]] * combined_noise
        monthly_exp <- base_monthly_exp * season_exp[[ind]] * combined_noise

        # ── Operating flags ─────────────────────────────────────────────
        op_win <- op_windows[[b]]
        if (hh_type %in% c("single_partial", "multi_sequential")) {
          operated <- op_win
        } else {
          # Random closure with seasonal weighting
          base_cl <- BASE_CLOSURE_MONTHS[ind]
          seas_w  <- CLOSURE_SEASONAL_WEIGHTS[[ind]]
          if (is.null(seas_w)) seas_w <- rep(1.0, 12)
          seas_w  <- seas_w / mean(seas_w)
          p_close <- pmin(0.95, (base_cl / 12) * seas_w + hh_closure_boost / 12)
          operated <- runif(12) > p_close
        }

        # Zero out closed months (with small holding cost for expenses)
        monthly_rev[!operated] <- 0
        monthly_exp[!operated] <- runif(sum(!operated),
                                        CLOSED_MONTH_EXP_MIN,
                                        CLOSED_MONTH_EXP_MAX)

        nic <- sample(INDUSTRIES[[ind]]$nic, 1)

        bus_rows[[bid]] <- list(
          business_id          = bid,
          hh_id                = hh_id,
          business_seq         = b,
          industry             = ind,
          industry_desc        = INDUSTRIES[[ind]]$desc,
          nic_2digit           = nic,
          annual_revenue_true  = round(sum(monthly_rev)),
          gross_margin         = round(gm, 4),
          corr_with_primary    = round(corr_b, 4),
          hh_closure_propensity = round(hh_closure_boost, 4)
        )

        for (m in seq_len(12)) {
          ledger_rows[[lid]] <- list(
            business_id   = bid,
            hh_id         = hh_id,
            month         = LEDGER_MONTHS[m],
            month_idx     = m - 1L,
            month_label   = MONTH_LABELS[m],
            operated      = operated[m],
            revenue_true  = round(monthly_rev[m]),
            expenses_true = round(monthly_exp[m]),
            profit_true   = round(monthly_rev[m] - monthly_exp[m])
          )
          lid <- lid + 1L
        }
        bid <- bid + 1L
      }
      hh_id <- hh_id + 1L
    }
  }
  cat(sprintf("  state %d/%d: %s\n", si, nrow(alloc), st))
}

# ── Assemble and write ────────────────────────────────────────────────────────
cat("Assembling tables...\n")
hh_dt <- rbindlist(hh_rows[seq_len(hh_id - 1)])
hh_dt[, c("mu_log","sigma_log") := NULL]  # internal calibration cols

bus_dt    <- rbindlist(bus_rows[seq_len(bid - 1)])
ledger_dt <- rbindlist(ledger_rows[seq_len(lid - 1)])

fwrite(hh_dt,     file.path(DATA_DIR, "households.csv"))
fwrite(bus_dt,    file.path(DATA_DIR, "businesses.csv"))
fwrite(ledger_dt, file.path(DATA_DIR, "monthly_ledger.csv"))

cat(sprintf("\nWrote:\n  households.csv    (%s rows)\n  businesses.csv    (%s rows)\n  monthly_ledger.csv (%s rows)\n",
            format(nrow(hh_dt), big.mark=","),
            format(nrow(bus_dt), big.mark=","),
            format(nrow(ledger_dt), big.mark=",")))

# ── Sanity checks ─────────────────────────────────────────────────────────────
cat("\n=== Sanity checks ===\n")
cat("HH typology:\n")
print(hh_dt[, .N, by = hh_type][order(-N)][, pct := round(100*N/nrow(hh_dt),1)])

annual_profit <- ledger_dt[, .(annual_profit = sum(profit_true)), by = hh_id]
combined      <- merge(hh_dt[, .(hh_id, state, sector)], annual_profit, by = "hh_id")

cat("\nAll-India monthly HH profit:\n")
for (sec in c("rural","urban")) {
  sub <- combined[sector == sec, annual_profit / 12]
  cat(sprintf("  %s: mean Rs.%s  median Rs.%s  n=%s\n",
              sec, format(round(mean(sub)), big.mark=","),
              format(round(median(sub)), big.mark=","),
              format(length(sub), big.mark=",")))
}

# Per-state calibration check vs PLFS Table 40 targets
cat("\nPer-state calibration (target vs actual mean monthly profit):\n")
state_check <- merge(
  combined[, .(actual = mean(annual_profit/12)), by = .(state, sector)],
  rbind(
    states_df[, .(state, sector="rural",  target = rural_mean_earnings * 1.17)],
    states_df[, .(state, sector="urban",  target = urban_mean_earnings * 1.17)]
  ),
  by = c("state","sector")
)[, diff_pct := round((actual - target)/target*100, 1)]
print(state_check[abs(diff_pct) > 10])  # flag only states >10% off target
cat("(States not shown are within +/-10% of PLFS Table 40 target)\n")
