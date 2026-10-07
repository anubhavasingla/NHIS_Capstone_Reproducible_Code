# NHIS_Capstone_Reproducible_Code
This is the reproducibility package attached to my final year capstone, written at UC Berkeley in collaboration with the World Bank and Advised by Prof. Carlos Schmidt Padilla

For the assumptions behind the synthetic data and the limits of what it can show, see [LIMITATIONS.md](LIMITATIONS.md).

The full capstone paper is in [paper/](paper/Anubhava_ResearchSample_Capstone.pdf). The tables and figures in `outputs/` come from a different run than the one reported in the paper, so a few numbers differ slightly (for example, all-India design-only bias of −9.7% here against −9.8% in the paper). I plan to reconcile the two in a future update.

This folder contains the code and inputs needed to reproduce the figures and
tables in Section 3.1.1 of the capstone, which evaluates the NHIS 2026
non-agricultural self-employment module using a synthetic ground-truth
population calibrated to PLFS 2024-25.

## Folder structure

```
project_root/
├── R Files/                  # All R scripts
│   ├── 01_params.R           # Parameters; sourced by every script
│   ├── 02_generate.R         # Builds the synthetic ground-truth dataset
│   ├── 03_simulate.R         # Applies NHIS Block 7c rule, both passes
│   ├── 04_figures.R          # Produces the four figures used in the paper
│   └── run_all.R             # Master script
├── data/                     # Inputs and generated datasets
│   └── state_level_inputs.csv
└── outputs/                  # Tables and figures produced by the pipeline
```

## How to run

From inside the `R Files/` directory:

```bash
Rscript run_all.R
```

Or from an R session with working directory set to `R Files/`:

```r
source("run_all.R")
```

Runtime is approximately 5 to 8 minutes on a standard laptop. All stochastic
choices are governed by `MASTER_SEED = 20260419L` set in `01_params.R`, so the
output is bit-for-bit reproducible across runs on the same R version.

## Prerequisites

R 4.0 or later, with the following packages installed:

```r
install.packages(c("data.table", "ggplot2", "scales"))
```

## Inputs

The pipeline requires one external input: `data/state_level_inputs.csv`. This
file is hand-compiled from the PLFS 2024-25 Annual Report and is the only
source of state-level calibration data. The columns map directly to published
PLFS tables:

| Column | Source |
|---|---|
| `state` | State name |
| `ind_A_agri` to `ind_S_other_svc`, `ind_professional` | PLFS Table 27 (state-level distribution of workers by NIC section) |
| `rural_est_hh_x100`, `urban_est_hh_x100`, `total_est_hh_x100` | PLFS Table 2 (estimated households by state and sector) |
| `rural_mean_earnings`, `urban_mean_earnings`, `total_mean_earnings` | PLFS Table 40 (mean monthly earnings of self-employed workers) |
| `rural_se_ag_pct`, `rural_se_nonag_pct`, `rural_se_total_pct` | PLFS Table 3 (rural self-employment shares) |
| `urban_se_total_pct`, `urban_rw_pct`, `urban_casual_pct`, `urban_others_pct` | PLFS Table 4 (urban employment status distribution) |
| `rural_sample_hh`, `urban_sample_hh` | PLFS Statement 1 (sample distribution by state) |

If you want to verify the input numbers against the source, the PLFS 2024-25
Annual Report is available on the MOSPI website. The values in this file are
verbatim transcriptions; no transformation has been applied.

## Outputs

Running the pipeline produces:

**In `data/`** (intermediate datasets, not directly used in the paper):
- `households.csv` — one row per household
- `businesses.csv` — one row per business
- `monthly_ledger.csv` — one row per business-month
- `survey_business_responses.csv` — business-level reference-month observations
- `survey_hh_responses.csv` — household-level annual income estimates (both passes)
- `state_sector_bias.csv` — bias summary by state and sector

**In `outputs/`** (used directly in the paper):
- `table3_all_india_bias.csv` — Table 3 in the paper
- `table4_bias_by_hh_type.csv` — Table 4 in the paper
- `table5_panel_level_bias.csv` — Table 5 in the paper
- `fig1_seasonality.png` — Figure 1 in the paper
- `fig2_distribution.png` — Figure 2 in the paper
- `fig3_bias_by_hh_type.png` — Figure 3 in the paper
- `fig4_panel_bias.png` — Figure 4 in the paper
- `fig5_state_heatmap.png` — Figure 5 in the paper

## Notes

The four scripts in `R Files/` can be run individually as long as `01_params.R`
is sourced first. `run_all.R` does this automatically. Each individual script
also re-sources `01_params.R` if it has not already been loaded into the
environment, so any of them can be run standalone for debugging.

The headline bias estimates reported in the paper (around 10 per cent for Pass A
and around 40 per cent for Pass B) are stable to the choice of random seed.
Appendix 3 of the paper documents this robustness check across four alternative
seeds.
