# =============================================================================
# 04_figures.R
# Produce the five publication figures used in the capstone document.
#
# Outputs (written to outputs/):
#   fig1_seasonality.png     — Figure 1 in paper: Revenue seasonality across
#                               11 industries
#   fig2_distribution.png    — Figure 2 in paper: Distribution of annual
#                               household income (log scale)
#   fig3_bias_by_hh_type.png — Figure 3 in paper: NHIS bias by household type
#   fig4_panel_bias.png      — Figure 4 in paper: Panel-level NHIS bias
#   fig5_state_heatmap.png   — Figure 5 in paper: State-level heterogeneity
#                               in NHIS bias
# =============================================================================
suppressMessages({
  library(data.table)
  library(ggplot2)
  library(scales)
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

survey     <- fread(file.path(DATA_DIR, "survey_hh_responses.csv"))
state_bias <- fread(file.path(DATA_DIR, "state_sector_bias.csv"))
hh         <- fread(file.path(DATA_DIR, "households.csv"))

dir.create(OUTPUT_DIR, showWarnings = FALSE)

palette_main <- c("#2166ac","#d6604d","#4dac26","#d01c8b","#f1a340","#998ec3")
theme_nhis <- theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 12),
        plot.subtitle = element_text(size = 9, colour = "grey40"),
        legend.position = "bottom",
        panel.grid.minor = element_blank())

# ── Figure 1: Revenue seasonality across 11 industries ───────────────────────
seas_df <- rbindlist(lapply(names(SEASONALITY), function(ind) {
  data.table(
    industry    = INDUSTRIES[[ind]]$desc,
    month_label = factor(MONTH_LABELS, levels = MONTH_LABELS),
    month_idx   = 1:12,
    multiplier  = 1.0 + SEASONALITY_SCALE * (SEASONALITY[[ind]] - 1.0)
  )
}))

# 11 colours: 6 from palette_main + 5 additional
palette_11 <- c(palette_main,
                "#a6cee3","#b2df8a","#fb9a99","#e31a1c","#cab2d6")

p1 <- ggplot(seas_df, aes(x = month_label, y = multiplier * 100,
                          colour = industry, group = industry)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  geom_hline(yintercept = 100, linetype = "dashed", colour = "grey50") +
  scale_y_continuous(labels = function(x) paste0(x, "%")) +
  scale_colour_manual(values = palette_11) +
  labs(title    = "Revenue seasonality across industries",
       subtitle = "Deviation of monthly revenue from each industry's annual mean",
       x = "Month (NHIS reference period, March-February)",
       y = "Revenue relative to industry mean",
       colour = NULL) +
  theme_nhis +
  theme(legend.position = "right",
        legend.text = element_text(size = 8),
        axis.text.x = element_text(size = 8))
ggsave(file.path(OUTPUT_DIR, "fig1_seasonality.png"), p1,
       width = 11, height = 6, dpi = 150)

# ── Figure 2: Distribution of annual household income ────────────────────────
d2 <- rbind(
  survey[, .(value = annual_profit_true, measure = "Ground truth")],
  survey[, .(value = annual_income_A,    measure = "NHIS (design only)")],
  survey[, .(value = annual_income_B,    measure = "NHIS (with reporting bias)")]
)
d2 <- d2[value > 100]  # trim implausible zeros for density display

p2 <- ggplot(d2, aes(x = value, fill = measure, colour = measure)) +
  geom_density(alpha = 0.30, linewidth = 0.7) +
  scale_x_log10(labels = label_number(scale = 1e-3, suffix = "K"),
                breaks  = c(1e3, 1e4, 1e5, 1e6, 1e7)) +
  scale_fill_manual(values   = palette_main[1:3]) +
  scale_colour_manual(values = palette_main[1:3]) +
  labs(title    = "Distribution of annual household income (log scale)",
       subtitle = "Both NHIS estimates track the ground-truth distribution; reporting bias shifts the distribution leftward more than design bias alone",
       x = "Annual household income (Rs, log scale)",
       y = "Density", fill = NULL, colour = NULL) +
  theme_nhis
ggsave(file.path(OUTPUT_DIR, "fig2_distribution.png"), p2,
       width = 9, height = 5, dpi = 150)

# ── Figure 3: NHIS bias by household type, both passes ───────────────────────
type_labels <- c(
  single_continuous = "Continuous\n(Cat 1/2)",
  single_partial    = "Single partial-year\n(Cat 3)",
  multi_concurrent  = "Multi concurrent\n(Cat 4)",
  multi_sequential  = "Multi sequential\n(Cat 5)"
)
type_order <- c("single_continuous","multi_concurrent",
                "single_partial","multi_sequential")

ht_agg <- survey[, .(
  bias_A = 100*(mean(annual_income_A)-mean(annual_profit_true))/mean(annual_profit_true),
  bias_B = 100*(mean(annual_income_B)-mean(annual_profit_true))/mean(annual_profit_true),
  n      = .N
), by = hh_type]
ht_agg[, label := type_labels[hh_type]]
ht_agg[, hh_type := factor(hh_type, levels = type_order)]
setorder(ht_agg, hh_type)

d3 <- melt(ht_agg, id.vars = c("hh_type","label","n"),
           measure.vars = c("bias_A","bias_B"),
           variable.name = "pass", value.name = "bias")
d3[, pass := fifelse(pass == "bias_A", "Design only", "Design + reporting bias")]
d3[, pass := factor(pass, levels = c("Design only", "Design + reporting bias"))]

p3 <- ggplot(d3, aes(x = label, y = bias, fill = pass)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_text(aes(label = sprintf("%.0f%%", bias),
                y = bias - sign(bias) * 1.5),
            position = position_dodge(0.7), size = 3.2, vjust = 0.5) +
  scale_fill_manual(values = palette_main[1:2]) +
  labs(title    = "NHIS bias by household type",
       subtitle = "Sequential (Cat 5) and partial-year (Cat 3) HH face the deepest bias. Reporting bias adds substantially to all types.",
       x = NULL, y = "NHIS bias (% of true mean)", fill = NULL) +
  theme_nhis
ggsave(file.path(OUTPUT_DIR, "fig3_bias_by_hh_type.png"), p3,
       width = 9, height = 5, dpi = 150)

# ── Figure 4: Panel-level NHIS bias, both passes ─────────────────────────────
panel_stats <- hh[, .(
  panel_label  = sprintf("Panel %d\n(%s survey,\n%s reference)",
                         unique(panel), unique(survey_month),
                         unique(reference_month))
), by = .(panel, survey_month, reference_month)]

hh_panel <- merge(survey, panel_stats, by = c("panel","survey_month","reference_month"))
hh_panel_agg <- hh_panel[, .(
  bias_A = 100*(mean(annual_income_A)-mean(annual_profit_true))/mean(annual_profit_true),
  bias_B = 100*(mean(annual_income_B)-mean(annual_profit_true))/mean(annual_profit_true)
), by = panel_label]

d4 <- melt(hh_panel_agg, id.vars = "panel_label",
           variable.name = "pass", value.name = "bias")
d4[, pass := fifelse(pass == "bias_A", "Design only", "Design + reporting bias")]
d4[, pass := factor(pass, levels = c("Design only", "Design + reporting bias"))]

p4 <- ggplot(d4, aes(x = panel_label, y = bias, fill = pass)) +
  geom_col(position = position_dodge(0.7), width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
  geom_text(aes(label = sprintf("%.0f%%", bias),
                y = bias + ifelse(bias < 0, -1.5, 1.5)),
            position = position_dodge(0.7), size = 3.2) +
  scale_fill_manual(values = palette_main[1:2]) +
  labs(title    = "Panel-level NHIS bias",
       subtitle = "Each panel's reference month determines which seasonal pattern is captured. Panel averaging absorbs most but not all of the seasonal variation",
       x = NULL, y = "NHIS bias (% of true mean)", fill = NULL) +
  theme_nhis
ggsave(file.path(OUTPUT_DIR, "fig4_panel_bias.png"), p4,
       width = 9, height = 5, dpi = 150)

# ── Figure 5: State x sector heatmap (design-only) ───────────────────────────
# Drop Delhi rural (sample too small) but keep Delhi urban
state_bias_filtered <- state_bias[!(state == "Delhi" & sector == "rural")]

# Order y-axis by rural bias (most negative at bottom). Delhi has no rural,
# so place it at the top.
rural_states <- state_bias_filtered[sector == "rural"][order(bias_pct_A), state]
all_states_ordered <- c(rural_states, setdiff(unique(state_bias_filtered$state), rural_states))
state_bias_filtered[, state := factor(state, levels = all_states_ordered)]

p5 <- ggplot(state_bias_filtered, aes(x = sector, y = state, fill = bias_pct_A)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  geom_text(aes(label = sprintf("%.0f%%", bias_pct_A)),
            size = 2.7, colour = "black") +
  scale_fill_gradient(low = "#d7191c", high = "#ffffbf",
                      name = "NHIS bias (%)") +
  labs(title    = "NHIS design-only bias across states and sectors",
       subtitle  = sprintf("Bias ranges from %.0f%% to %.0f%% across all state-sector cells",
                           min(state_bias_filtered$bias_pct_A),
                           max(state_bias_filtered$bias_pct_A)),
       x = NULL, y = NULL) +
  theme_nhis + theme(legend.position = "bottom")
ggsave(file.path(OUTPUT_DIR, "fig5_state_heatmap.png"), p5,
       width = 7, height = 9, dpi = 150)


cat(sprintf("All figures written to %s\n", OUTPUT_DIR))
cat("  fig1_seasonality.png       -- Figure 1 in paper (Revenue seasonality)\n")
cat("  fig2_distribution.png      -- Figure 2 in paper (Income distribution)\n")
cat("  fig3_bias_by_hh_type.png   -- Figure 3 in paper (Bias by HH type)\n")
cat("  fig4_panel_bias.png        -- Figure 4 in paper (Panel-level bias)\n")
cat("  fig5_state_heatmap.png     -- Figure 5 in paper (State-level heatmap)\n")
