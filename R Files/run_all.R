# =============================================================================
# run_all.R
# Master script: runs the full pipeline in sequence.
#
# Usage (from inside the 'R Files/' directory):
#   Rscript run_all.R
#
# Or source from an R session whose working directory is 'R Files/':
#   source("run_all.R")
#
# Steps:
#   1. 01_params.R     -- loads all parameters
#   2. 02_generate.R   -- builds households.csv, businesses.csv, monthly_ledger.csv
#   3. 03_simulate.R   -- applies NHIS measurement rule, two passes; writes
#                          tables 3, 4, 5 to outputs/
#   4. 04_figures.R    -- produces the four figures used in the paper
#
# Prerequisites:
#   R packages: data.table, ggplot2, scales
#   data/state_level_inputs.csv must be present (compiled from PLFS 2024-25
#   Annual Report; see README for column-by-column source mapping).
#
# Runtime: approximately 5-8 minutes on a standard laptop.
# Reproducible: all stochastic choices governed by MASTER_SEED = 20260419.
# =============================================================================

# Resolve script directory regardless of working directory
scripts_dir <- dirname(normalizePath(
  sub("--file=","",commandArgs(trailingOnly=FALSE)[grep("^--file=",commandArgs(trailingOnly=FALSE))][1]),
  mustWork = FALSE))

# Load parameters once into the global environment so all steps share them
source(file.path(scripts_dir, "01_params.R"))

run_step <- function(script_name, label) {
  cat(sprintf("\n%s\n%s\n", strrep("=", 60), label))
  t0 <- proc.time()
  source(file.path(scripts_dir, script_name))
  elapsed <- (proc.time() - t0)[["elapsed"]]
  cat(sprintf("  Done in %.1f seconds.\n", elapsed))
}

run_step("02_generate.R", "STEP 1: Generate ground-truth dataset")
run_step("03_simulate.R", "STEP 2: Simulate NHIS measurement (both passes)")
run_step("04_figures.R",  "STEP 3: Produce figures")

cat(sprintf("\n%s\nAll steps complete. See data/ and outputs/ directories.\n",
            strrep("=", 60)))
