# Oman_RG/ms1_trade_prediction/06_naive_ets_baselines.R
#
# Adds two small-sample-appropriate baseline forecasting methods, random
# walk with drift and ETS (Holt's linear trend, chosen automatically by
# ets()'s own model-selection criterion), to the ARIMA vs BSTS vs
# Combination comparison already built by 02_arima.R, 03_bsts.R and
# 04_forecast_accuracy_comparison.R.
#
# Rationale: the manuscript's Discussion leans on "the small-sample
# forecasting literature's general caution against treating any one
# time-series method as universally superior" without including the two
# methods that literature actually centres on as baselines, a naive
# benchmark and exponential smoothing. Neither needs more data than ARIMA
# or BSTS already use, unlike machine-learning approaches, which would need
# more observations than these series have and are not attempted here.
#
# Same continuity restriction and same rolling-origin backtest design as
# 02_arima.R (min_train_years = 10, one-step-ahead, expanding window), run
# independently here but checked against the ARIMA backtest's own origins
# and actuals at the end of this script so the five methods are compared
# on exactly the same test points, not just the same design.

library(dplyr)
library(readr)
library(forecast)

trade <- read_csv("ms1_trade_prediction/data/processed/trade_series_by_sector.csv", show_col_types = FALSE)

horizon <- 5 # matches 02_arima.R's headline forecast horizon

restrict_to_longest_run <- function(df) {
  df <- df |> arrange(year)
  years <- df$year
  if (length(years) <= 1) return(list(df = df, dropped_years = integer(0), original_n = length(years)))
  breaks <- c(0, cumsum(diff(years) > 1))
  run_lengths <- table(breaks)
  best_run <- as.integer(names(run_lengths)[which.max(run_lengths)])
  keep <- breaks == best_run
  list(df = df[keep, ], dropped_years = years[!keep], original_n = length(years))
}

min_train_years <- 10

fit_naive_one_step <- function(y_train) {
  # Random walk with drift: appropriate for a short trending annual series,
  # and the standard "did the fitted model actually beat doing this"
  # baseline in the forecasting literature.
  fc <- tryCatch(
    rwf(y_train, h = 1, drift = TRUE),
    error = function(e) naive(y_train, h = 1)
  )
  as.numeric(fc$mean[1])
}

fit_ets_one_step <- function(y_train) {
  model <- tryCatch(
    ets(y_train),
    error = function(e) NULL
  )
  if (is.null(model)) return(fit_naive_one_step(y_train)) # fall back if ets() cannot fit this short a series
  as.numeric(forecast(model, h = 1)$mean[1])
}

rolling_origin_backtest <- function(df, fit_fn) {
  df <- df |> arrange(year)
  y_full <- df$value_usd
  n <- length(y_full)

  if (n <= min_train_years + 1) {
    return(tibble(origin_year = integer(), actual = numeric(), point_forecast = numeric()))
  }

  results <- lapply(min_train_years:(n - 1), function(origin) {
    y_train <- y_full[1:origin]
    pf <- fit_fn(y_train)
    tibble(origin_year = df$year[origin], actual = y_full[origin + 1], point_forecast = pf)
  })
  bind_rows(results)
}


# Full-sample fits, for the five-year-ahead headline forecast table/figure.
# Unlike the equal-weight Combination, both rwf(drift = TRUE) and ets() are
# genuine probability models and return their own 95% intervals natively,
# so these are reported with intervals in Figure 2, not point forecasts only.

fit_naive_full <- function(df) {
  y <- df$value_usd
  tryCatch(rwf(y, h = horizon, drift = TRUE), error = function(e) naive(y, h = horizon))
}

fit_ets_full <- function(df) {
  y <- df$value_usd
  model <- tryCatch(ets(y), error = function(e) NULL)
  if (is.null(model)) return(rwf(y, h = horizon, drift = TRUE))
  forecast(model, h = horizon)
}

series_keys <- trade |> distinct(sector, flow, data_source)

naive_backtests <- list()
ets_backtests <- list()
naive_full_fits <- list()
ets_full_fits <- list()

for (i in seq_len(nrow(series_keys))) {
  sec <- series_keys$sector[i]
  fl <- series_keys$flow[i]
  df_raw <- trade |> filter(sector == sec, flow == fl)
  df <- restrict_to_longest_run(df_raw)$df
  key <- paste(sec, fl, sep = " | ")

  naive_backtests[[key]] <- rolling_origin_backtest(df, fit_naive_one_step) |> mutate(sector = sec, flow = fl)
  ets_backtests[[key]] <- rolling_origin_backtest(df, fit_ets_one_step) |> mutate(sector = sec, flow = fl)

  naive_full_fits[[key]] <- list(forecast = fit_naive_full(df), sector = sec, flow = fl, series = df)
  ets_full_fits[[key]] <- list(forecast = fit_ets_full(df), sector = sec, flow = fl, series = df)
}

naive_backtest_tbl <- bind_rows(naive_backtests)
ets_backtest_tbl <- bind_rows(ets_backtests)

dir.create("ms1_trade_prediction/output", showWarnings = FALSE, recursive = TRUE)
write_csv(naive_backtest_tbl, "ms1_trade_prediction/output/naive_backtest_origins.csv")
write_csv(ets_backtest_tbl, "ms1_trade_prediction/output/ets_backtest_origins.csv")

build_forecast_tbl <- function(fits) {
  bind_rows(lapply(fits, function(r) {
    tibble(
      sector = r$sector,
      flow = r$flow,
      year = max(r$series$year) + seq_len(horizon),
      point_forecast = as.numeric(r$forecast$mean),
      lo_95 = as.numeric(r$forecast$lower[, "95%"]),
      hi_95 = as.numeric(r$forecast$upper[, "95%"])
    )
  }))
}

naive_forecast_tbl <- build_forecast_tbl(naive_full_fits)
ets_forecast_tbl <- build_forecast_tbl(ets_full_fits)

write_csv(naive_forecast_tbl, "ms1_trade_prediction/output/naive_forecast.csv")
write_csv(ets_forecast_tbl, "ms1_trade_prediction/output/ets_forecast.csv")

# ---- Check origins match the ARIMA/BSTS backtest exactly -------------

arima_backtest <- read_csv("ms1_trade_prediction/output/arima_backtest_origins.csv", show_col_types = FALSE)
origin_check <- arima_backtest |>
  select(sector, flow, origin_year, actual) |>
  anti_join(naive_backtest_tbl |> select(sector, flow, origin_year, actual), by = c("sector", "flow", "origin_year", "actual"))
if (nrow(origin_check) > 0) {
  warning("Naive/ETS backtest origins do not match the ARIMA backtest exactly, see origin_check.")
}

# ---- Combine into a five-method accuracy comparison --------------------

bsts_backtest <- read_csv("ms1_trade_prediction/output/bsts_backtest_origins.csv", show_col_types = FALSE) |>
  rename(bsts_forecast = point_forecast)
arima_backtest <- arima_backtest |> rename(arima_forecast = point_forecast)

joined <- arima_backtest |>
  inner_join(bsts_backtest, by = c("sector", "flow", "origin_year", "actual")) |>
  inner_join(naive_backtest_tbl |> rename(naive_forecast = point_forecast), by = c("sector", "flow", "origin_year", "actual")) |>
  inner_join(ets_backtest_tbl |> rename(ets_forecast = point_forecast), by = c("sector", "flow", "origin_year", "actual")) |>
  mutate(combination_forecast = (arima_forecast + bsts_forecast) / 2)

backtest_long <- bind_rows(
  joined |> transmute(sector, flow, origin_year, actual, method = "ARIMA", point_forecast = arima_forecast),
  joined |> transmute(sector, flow, origin_year, actual, method = "BSTS", point_forecast = bsts_forecast),
  joined |> transmute(sector, flow, origin_year, actual, method = "Combination", point_forecast = combination_forecast),
  joined |> transmute(sector, flow, origin_year, actual, method = "Naive (drift)", point_forecast = naive_forecast),
  joined |> transmute(sector, flow, origin_year, actual, method = "ETS", point_forecast = ets_forecast)
)

accuracy_comparison_5method <- backtest_long |>
  group_by(method, sector, flow) |>
  summarise(
    RMSE = sqrt(mean((actual - point_forecast)^2)),
    MAE = mean(abs(actual - point_forecast)),
    MAPE = mean(abs(actual - point_forecast) / actual) * 100,
    n_origins = n(),
    .groups = "drop"
  ) |>
  arrange(sector, flow, method)

preferred_method_5 <- accuracy_comparison_5method |>
  group_by(sector, flow) |>
  slice_min(RMSE, n = 1) |>
  ungroup() |>
  select(sector, flow, preferred_method = method, RMSE, n_origins)

write_csv(accuracy_comparison_5method, "ms1_trade_prediction/output/accuracy_comparison_5method.csv")
write_csv(preferred_method_5, "ms1_trade_prediction/output/preferred_method_5method.csv")

# ---- Five-method forecast table, for Figure 2 ---------------------------

arima_forecast <- read_csv("ms1_trade_prediction/output/arima_forecast.csv", show_col_types = FALSE) |>
  mutate(method = "ARIMA")
bsts_forecast <- read_csv("ms1_trade_prediction/output/bsts_forecast.csv", show_col_types = FALSE) |>
  mutate(method = "BSTS")
combination_forecast <- arima_forecast |>
  select(sector, flow, year, arima_pf = point_forecast) |>
  inner_join(
    bsts_forecast |> select(sector, flow, year, bsts_pf = point_forecast),
    by = c("sector", "flow", "year")
  ) |>
  mutate(method = "Combination", point_forecast = (arima_pf + bsts_pf) / 2, lo_95 = NA_real_, hi_95 = NA_real_) |>
  select(sector, flow, year, point_forecast, lo_95, hi_95, method)

forecast_comparison_5method <- bind_rows(
  arima_forecast |> select(sector, flow, year, point_forecast, lo_95, hi_95, method),
  bsts_forecast |> select(sector, flow, year, point_forecast, lo_95, hi_95, method),
  combination_forecast,
  naive_forecast_tbl |> mutate(method = "Naive (drift)"),
  ets_forecast_tbl |> mutate(method = "ETS")
) |>
  arrange(sector, flow, year, method)

write_csv(forecast_comparison_5method, "ms1_trade_prediction/output/forecast_comparison_5method.csv")

message(
  "5-method comparison written (ARIMA, BSTS, Combination, Naive with drift, ETS).\n",
  "Full-sample ", horizon, "-year forecasts written for Naive and ETS.\n",
  "Preferred method by sector x flow:\n",
  paste(capture.output(print(preferred_method_5, n = Inf)), collapse = "\n")
)
