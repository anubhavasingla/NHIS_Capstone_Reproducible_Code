# =============================================================================
# 03_simulate.R
# Apply the NHIS Block 7c measurement rule to the ground-truth ledger.
#
# Two passes:
#   Pass A  Design-only: respondent reports true last-month revenue and
#           expenses, true months-of-operation. No reporting error.
#   Pass B  Design + reporting bias: revenue * 0.95, expenses * 1.05
#           (5% under-report, 5% over-report), multiplicative, no noise.
#
# Outputs (written to data/):
#   survey_business_responses.csv — business-level reference-month data
#   survey_hh_responses.csv       — HH-level annual income estimates, both passes
#   state_sector_bias.csv         — state x sector bias summary
#
# Outputs (written to outputs/, matching tables in the capstone document):
#   table3_all_india_bias.csv     — Table 3: All-India NHIS bias under both passes
#   table4_bias_by_hh_type.csv    — Table 4: NHIS bias by household type
#   table5_panel_level_bias.csv   — Table 5: Panel-level NHIS bias
# =============================================================================
suppressMessages({
  library(data.table)
})

# Source params if not already loaded (allows standalone use)
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

cat("Loading ground-truth data...\n")
hh     <- fread(file.path(DATA_DIR, "households.csv"))
bus    <- fread(file.path(DATA_DIR, "businesses.csv"))
ledger <- fread(file.path(DATA_DIR, "monthly_ledger.csv"))

# ── For each business, extract reference-month observation ───────────────────
cat("Extracting reference-month observations...\n")

# Bring reference_month onto ledger
hh_ref <- hh[, .(hh_id, panel, reference_month, hh_type, state, sector)]
ledger <- merge(ledger, hh_ref, by = "hh_id")

ref_obs <- ledger[month == reference_month]

# Count months operated per business in the ledger (Q4.5 analog)
months_op <- ledger[, .(months_operated = sum(operated)), by = business_id]

# Join
bus_ref <- merge(ref_obs, months_op, by = "business_id")
bus_ref <- merge(bus_ref, bus[, .(business_id, hh_id, business_seq,
                                   industry, industry_desc, nic_2digit,
                                   gross_margin, annual_revenue_true)],
                 by = c("business_id","hh_id"))

# ── Pass A: design-only ────────────────────────────────────────────────────
bus_ref[, rev_reported_A := revenue_true]
bus_ref[, exp_reported_A := expenses_true]
bus_ref[, profit_A       := rev_reported_A - exp_reported_A]
bus_ref[, annual_est_A   := profit_A * months_operated]

# ── Pass B: +5% expense, -5% revenue ─────────────────────────────────────
bus_ref[, rev_reported_B := revenue_true  * REPORT_BIAS_REV]
bus_ref[, exp_reported_B := expenses_true * REPORT_BIAS_EXP]
bus_ref[, profit_B       := rev_reported_B - exp_reported_B]
bus_ref[, annual_est_B   := profit_B * months_operated]

fwrite(bus_ref, file.path(DATA_DIR, "survey_business_responses.csv"))
cat(sprintf("  survey_business_responses.csv (%s rows)\n",
            format(nrow(bus_ref), big.mark=",")))

# ── Aggregate to HH level ──────────────────────────────────────────────────
hh_est <- bus_ref[, .(
  annual_income_A = sum(annual_est_A),
  annual_income_B = sum(annual_est_B),
  n_businesses    = .N
), by = hh_id]

# True annual profit from ledger
true_profit <- ledger[, .(annual_profit_true = sum(profit_true)), by = hh_id]

hh_out <- merge(hh, hh_est, by = "hh_id")
hh_out <- merge(hh_out, true_profit, by = "hh_id")
hh_out[, bias_abs_A := annual_income_A - annual_profit_true]
hh_out[, bias_abs_B := annual_income_B - annual_profit_true]
hh_out[, bias_pct_A := 100 * bias_abs_A / annual_profit_true]
hh_out[, bias_pct_B := 100 * bias_abs_B / annual_profit_true]

fwrite(hh_out, file.path(DATA_DIR, "survey_hh_responses.csv"))

# ── State x sector bias summary ──────────────────────────────────────────────
state_bias <- hh_out[, .(
  n             = .N,
  true_mean     = mean(annual_profit_true),
  nhis_mean_A    = mean(annual_income_A),
  bias_pct_A    = 100 * (mean(annual_income_A) - mean(annual_profit_true)) /
                        mean(annual_profit_true),
  nhis_mean_B    = mean(annual_income_B),
  bias_pct_B    = 100 * (mean(annual_income_B) - mean(annual_profit_true)) /
                        mean(annual_profit_true)
), by = .(state, sector)]
fwrite(state_bias, file.path(DATA_DIR, "state_sector_bias.csv"))


# =============================================================================
# Table outputs for the capstone document
# Each CSV reproduces a table that appears in Section 3.1.1 of the paper.
# =============================================================================
dir.create(OUTPUT_DIR, showWarnings = FALSE)

# ── Table 3: All-India NHIS bias under the two analysis passes ───────────────
mu_true_all <- mean(hh_out$annual_profit_true)
mu_A_all    <- mean(hh_out$annual_income_A)
mu_B_all    <- mean(hh_out$annual_income_B)

mu_true_r <- mean(hh_out[sector == "rural", annual_profit_true])
mu_A_r    <- mean(hh_out[sector == "rural", annual_income_A])
mu_B_r    <- mean(hh_out[sector == "rural", annual_income_B])
mu_true_u <- mean(hh_out[sector == "urban", annual_profit_true])
mu_A_u    <- mean(hh_out[sector == "urban", annual_income_A])
mu_B_u    <- mean(hh_out[sector == "urban", annual_income_B])

table3 <- data.table(
  Measure = c(
    "Mean true annual HH profit",
    "Mean NHIS-estimated profit",
    "All-India bias",
    "Rural bias",
    "Urban bias"
  ),
  `Pass A: Design only` = c(
    sprintf("Rs. %s", format(round(mu_true_all), big.mark=",")),
    sprintf("Rs. %s", format(round(mu_A_all),    big.mark=",")),
    sprintf("%.1f%%",  100 * (mu_A_all - mu_true_all) / mu_true_all),
    sprintf("%.1f%%",  100 * (mu_A_r   - mu_true_r)   / mu_true_r),
    sprintf("%.1f%%",  100 * (mu_A_u   - mu_true_u)   / mu_true_u)
  ),
  `Pass B: Design + reporting` = c(
    sprintf("Rs. %s", format(round(mu_true_all), big.mark=",")),
    sprintf("Rs. %s", format(round(mu_B_all),    big.mark=",")),
    sprintf("%.1f%%",  100 * (mu_B_all - mu_true_all) / mu_true_all),
    sprintf("%.1f%%",  100 * (mu_B_r   - mu_true_r)   / mu_true_r),
    sprintf("%.1f%%",  100 * (mu_B_u   - mu_true_u)   / mu_true_u)
  )
)
fwrite(table3, file.path(OUTPUT_DIR, "table3_all_india_bias.csv"))

# ── Table 4: NHIS bias by household type ─────────────────────────────────────
# Cat 1/2 (continuous) is reported as a combined row in the paper.
type_labels_paper <- c(
  single_continuous = "Cat 1/2: Continuous year-round",
  multi_concurrent  = "Cat 4: Multi-business, concurrent",
  single_partial    = "Cat 3: Single business, partial-year",
  multi_sequential  = "Cat 5: Multi-business, sequential"
)
type_order_paper <- c("single_continuous","multi_concurrent",
                      "single_partial","multi_sequential")

n_total <- nrow(hh_out)
ht <- hh_out[, .(
  share  = 100 * .N / n_total,
  bias_A = 100 * (mean(annual_income_A) - mean(annual_profit_true)) /
                  mean(annual_profit_true),
  bias_B = 100 * (mean(annual_income_B) - mean(annual_profit_true)) /
                  mean(annual_profit_true)
), by = hh_type]
ht[, hh_type := factor(hh_type, levels = type_order_paper)]
setorder(ht, hh_type)

table4 <- data.table(
  `Household type`              = type_labels_paper[as.character(ht$hh_type)],
  `Share`                        = sprintf("%.1f%%", ht$share),
  `Design only`                  = sprintf("%.1f%%", ht$bias_A),
  `Design + reporting`           = sprintf("%.1f%%", ht$bias_B)
)
fwrite(table4, file.path(OUTPUT_DIR, "table4_bias_by_hh_type.csv"))

# ── Table 5: Panel-level NHIS bias ───────────────────────────────────────────
panel_summary <- hh_out[, .(
  reference_month = unique(reference_month),
  bias_A = 100 * (mean(annual_income_A) - mean(annual_profit_true)) /
                  mean(annual_profit_true),
  bias_B = 100 * (mean(annual_income_B) - mean(annual_profit_true)) /
                  mean(annual_profit_true)
), by = panel][order(panel)]

# Map reference month YYYY-MM to "Mon YYYY" for readability
month_pretty <- function(yyyy_mm) {
  parts <- strsplit(yyyy_mm, "-")[[1]]
  m_idx <- as.integer(parts[2])
  m_name <- c("January","February","March","April","May","June",
              "July","August","September","October","November","December")[m_idx]
  sprintf("%s %s", m_name, parts[1])
}

table5 <- data.table(
  Panel             = sprintf("Panel %d", panel_summary$panel),
  `Reference month` = sapply(panel_summary$reference_month, month_pretty),
  `Design only`     = sprintf("%.1f%%", panel_summary$bias_A),
  `Design + reporting` = sprintf("%.1f%%", panel_summary$bias_B)
)
fwrite(table5, file.path(OUTPUT_DIR, "table5_panel_level_bias.csv"))


# ── Print headline results to console ────────────────────────────────────────
cat("\n=== NHIS Simulation Summary ===\n")
N <- nrow(hh_out)
mu_true <- mean(hh_out$annual_profit_true)
mu_A    <- mean(hh_out$annual_income_A)
mu_B    <- mean(hh_out$annual_income_B)
bias_A  <- 100 * (mu_A - mu_true) / mu_true
bias_B  <- 100 * (mu_B - mu_true) / mu_true

cat(sprintf("Households processed: %s\n", format(N, big.mark=",")))
cat(sprintf("\nAll-India annual HH profit:\n"))
cat(sprintf("  True  mean: Rs.%s\n", format(round(mu_true), big.mark=",")))
cat(sprintf("  Pass A (design-only):    Rs.%s  bias = %+.1f%%\n",
            format(round(mu_A), big.mark=","), bias_A))
cat(sprintf("  Pass B (+5%%/+5%% report): Rs.%s  bias = %+.1f%%\n",
            format(round(mu_B), big.mark=","), bias_B))

cat("\nBy sector:\n")
hh_out[, .(
  true_mean = mean(annual_profit_true),
  bias_A    = 100*(mean(annual_income_A)-mean(annual_profit_true))/mean(annual_profit_true),
  bias_B    = 100*(mean(annual_income_B)-mean(annual_profit_true))/mean(annual_profit_true)
), by = sector] |> print()

cat("\nBy HH type:\n")
hh_out[, .(
  n      = .N,
  share  = round(100 * .N / N, 1),
  bias_A = round(100*(mean(annual_income_A)-mean(annual_profit_true))/mean(annual_profit_true), 1),
  bias_B = round(100*(mean(annual_income_B)-mean(annual_profit_true))/mean(annual_profit_true), 1)
), by = hh_type][order(-n)] |> print()

cat("\nBy panel:\n")
hh_out[, .(
  n      = .N,
  ref    = reference_month[1],
  bias_A = round(100*(mean(annual_income_A)-mean(annual_profit_true))/mean(annual_profit_true), 1),
  bias_B = round(100*(mean(annual_income_B)-mean(annual_profit_true))/mean(annual_profit_true), 1)
), by = panel][order(panel)] |> print()

cat(sprintf("\nTable CSVs written to %s:\n", OUTPUT_DIR))
cat("  table3_all_india_bias.csv      (matches Table 3 in paper)\n")
cat("  table4_bias_by_hh_type.csv     (matches Table 4 in paper)\n")
cat("  table5_panel_level_bias.csv    (matches Table 5 in paper)\n")
