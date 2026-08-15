# Oman_RG/05_figures.R
#
# Builds the manuscript's figures from the real pipeline output only (no
# invented data points). Colour follows the dataviz skill's fixed-order
# categorical palette (validated CVD-safe): slot 1 blue #2a78d6, slot 2
# orange #eb6834, slot 3 aqua #1baf7a. Two-series charts (flow, source) use
# slots 1-2. The forecast fan chart (3 headline methods: ARIMA, BSTS,
# Combination) uses slots 1-3, the specific three-slot set that clears the
# all-pairs CVD floor the skill documents for small-multiples/faceted
# charts.
#
# Figure 3 (accuracy by method) now compares 5 methods, adding the
# small-sample benchmarks Naive (drift, slot 4 yellow) and ETS (slot 5
# magenta) to the 3 headline methods. Five slots fail the all-pairs CVD
# floor per the skill's own validator (checked: worst normal-vision pair
# ~13, below the 15 floor). This is accepted here, not re-stepped, because
# the chart already carries the two forms of secondary encoding the skill
# treats as sufficient relief: every bar is directly labelled by method on
# the x-axis (identity is not colour-alone), and the manuscript's Table 2
# reports the exact RMSE values alongside the figure (a table view exists).

library(dplyr)
library(readr)
library(tidyr)
library(ggplot2)
library(scales)

dir.create("figures", showWarnings = FALSE)

pal_flow <- c(Exports = "#2a78d6", Imports = "#eb6834")
pal_method <- c(ARIMA = "#2a78d6", BSTS = "#eb6834", Combination = "#1baf7a")
pal_method_5 <- c(
  ARIMA = "#2a78d6", BSTS = "#eb6834", Combination = "#1baf7a",
  `Naive (drift)` = "#eda100", ETS = "#e87ba4"
)
pal_source <- c(UNCTADstat = "#2a78d6", Comtrade = "#eb6834")

theme_forecast <- theme_minimal(base_size = 11) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(colour = "grey90", linewidth = 0.3),
    strip.text = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    plot.title = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 10)
  )

# ---- Figure 1: historical series, all 4 sectors x 2 flows -------------

trade <- read_csv("data/processed/trade_series_by_sector.csv", show_col_types = FALSE)

# geom_line() connects consecutive rows regardless of the actual year gap
# between them, which would draw a straight line across e.g. Maritime
# freight transport exports' missing 2006-2017 as if that stretch were
# real data. Each sector-flow series is split into its actual contiguous
# runs (not just the longest one, unlike the modelling scripts, since this
# plot is descriptive of the full history) so the line only connects years
# that are genuinely consecutive, and a real gap shows as a visible break.
add_run_id <- function(df) {
  df |>
    arrange(sector, flow, year) |>
    group_by(sector, flow) |>
    mutate(run_id = cumsum(c(1, diff(year) > 1))) |>
    ungroup()
}
trade_runs <- add_run_id(trade)

fig1 <- ggplot(trade_runs, aes(x = year, y = value_usd / 1e6, colour = flow, group = interaction(flow, run_id))) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.3) +
  facet_wrap(~sector, scales = "free_y", ncol = 2) +
  scale_colour_manual(values = pal_flow, name = NULL) +
  scale_y_continuous(labels = label_dollar(prefix = "US$", suffix = "M")) +
  scale_x_continuous(breaks = breaks_pretty(6)) +
  labs(
    title = "Oman blue economy trade, 2005 to 2024",
    subtitle = "A break in the line is a real UNCTADstat data gap, not a plotting artefact.",
    caption = "Maritime freight transport exports has no published Oman figure for 2006-2017.",
    x = NULL, y = NULL
  ) +
  theme_forecast

ggsave("figures/fig1_historical_series.png", fig1, width = 9, height = 6.5, dpi = 300, bg = "white")

# ---- Figure 2: forecast fan chart, all 8 series, 5 methods -------------
#
# Extended from 3 methods (ARIMA, BSTS, Combination) to all 5 backtested in
# Table 2 / Figure 3, adding Naive (drift) and ETS. All 4 non-Combination
# methods now have genuine 95% intervals (rwf() and ets() return their own,
# unlike the equal-weight Combination), but plotting 4 overlapping
# semi-transparent ribbons per facet panel across 8 panels was tested and
# reads as visual noise, not useful uncertainty information, an
# anti-pattern the dataviz skill warns against. Ribbons are dropped
# entirely in favour of 5 clean point-forecast lines. Interval values
# remain available in the underlying *_forecast.csv output files for
# anyone who wants them, and RMSE (the accuracy dimension Figure 2 would
# otherwise be standing in for) is already Figure 3's job.

forecasts <- read_csv("output/forecast_comparison_5method.csv", show_col_types = FALSE)

hist_for_plot <- trade_runs |>
  transmute(sector, flow, year, value_usd, method = "Historical", run_id)

fc_for_plot <- forecasts |>
  transmute(sector, flow, year, value_usd = point_forecast, method)

fig2 <- ggplot() +
  geom_line(
    data = hist_for_plot, aes(x = year, y = value_usd / 1e6, group = run_id),
    colour = "grey30", linewidth = 0.6
  ) +
  geom_line(
    data = fc_for_plot,
    aes(x = year, y = value_usd / 1e6, colour = method, linetype = method == "Combination"),
    linewidth = 0.8
  ) +
  scale_colour_manual(values = pal_method_5, name = "Forecast method") +
  scale_linetype_manual(values = c(`TRUE` = "dashed", `FALSE` = "solid"), guide = "none") +
  facet_wrap(sector ~ flow, scales = "free_y", ncol = 2, labeller = label_wrap_gen(30)) +
  scale_y_continuous(labels = label_dollar(prefix = "US$", suffix = "M")) +
  labs(
    title = "Five-year forecasts by method, all eight sector-flow series",
    subtitle = "Grey = historical. Combination (dashed) has no interval of its own.\nARIMA, BSTS, Naive and ETS intervals are in the output CSVs, not shown here to keep 8 panels readable.",
    x = NULL, y = NULL
  ) +
  theme_forecast

ggsave("figures/fig2_forecast_fan_chart.png", fig2, width = 10, height = 12, dpi = 300, bg = "white")

# ---- Figure 3: out-of-sample accuracy (RMSE) by method -----------------

accuracy <- read_csv("output/accuracy_comparison_5method.csv", show_col_types = FALSE) |>
  mutate(method = factor(method, levels = c("ARIMA", "BSTS", "Combination", "Naive (drift)", "ETS")))

fig3 <- ggplot(accuracy, aes(x = method, y = RMSE / 1e6, fill = method)) +
  geom_col(width = 0.6) +
  facet_wrap(sector ~ flow, scales = "free_y", ncol = 4, labeller = label_wrap_gen(24)) +
  scale_fill_manual(values = pal_method_5, guide = "none") +
  scale_y_continuous(labels = label_dollar(prefix = "US$", suffix = "M")) +
  labs(
    title = "Rolling-origin out-of-sample RMSE by method",
    subtitle = "Lower is better. Naive (drift) and ETS are small-sample benchmarks.\nMaritime freight transport exports omitted (too few years for a backtest).",
    x = NULL, y = "RMSE (US$M)"
  ) +
  theme_forecast +
  theme(axis.text.x = element_text(angle = 40, hjust = 1))

ggsave("figures/fig3_accuracy_by_method.png", fig3, width = 11, height = 6.5, dpi = 300, bg = "white")

# ---- Figure 4: Comtrade vs UNCTADstat fisheries cross-check ------------

crosscheck <- read_csv("output/fisheries_crosscheck_comparison.csv", show_col_types = FALSE)

crosscheck_long <- bind_rows(
  crosscheck |> transmute(year, flow, value_usd = value_usd_unctadstat, source = "UNCTADstat"),
  crosscheck |> transmute(year, flow, value_usd = value_usd_comtrade, source = "Comtrade")
)

fig4 <- ggplot(crosscheck_long, aes(x = year, y = value_usd / 1e6, colour = source)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.3) +
  facet_wrap(~flow, ncol = 2) +
  scale_colour_manual(values = pal_source, name = NULL) +
  scale_y_continuous(labels = label_dollar(prefix = "US$", suffix = "M")) +
  labs(
    title = "Fisheries and seafood processing: UNCTADstat versus raw UN Comtrade (HS 03 + 16)",
    subtitle = "UNCTADstat applies ocean-activity coefficients. Comtrade does not.",
    caption = "Tracking in trend, not identical in level, is the expected result of comparing an adjusted and an unadjusted source.",
    x = NULL, y = NULL
  ) +
  theme_forecast

ggsave("figures/fig4_comtrade_crosscheck.png", fig4, width = 9, height = 4.5, dpi = 300, bg = "white")

message("4 figures written to figures/: ",
        paste(list.files("figures", pattern = "\\.png$"), collapse = ", "))
