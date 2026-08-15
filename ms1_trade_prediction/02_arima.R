# Oman_RG/ms1_trade_prediction/02_arima.R
#
# Fits ARIMA models (via forecast::auto.arima) to each sector x flow series
# produced by 01_import_unctadstat.R (sector-level, not the national
# aggregate), with structural-break dummies (covid_shock, red_sea_shock)
# entered as exogenous regressors where the series has variation on them.
#
# Two things are produced per series:
#  1. A rolling-origin (expanding-window) one-step-ahead backtest, which is
#     genuine out-of-sample accuracy, not the in-sample fit statistics an
#     earlier version of this script reported.
#  2. A full-sample model and its horizon-year-ahead forecast, for the
#     paper's headline forecast table. Future shock dummies are set to 0
#     (no shock assumed over the forecast horizon), the conservative default.
#
# Real UNCTADstat series can have missing years (e.g. Maritime freight
# transport exports for Oman: 2005, then a gap, then 2018-2024). Sorting by
# year and treating adjacent rows as adjacent years would silently splice
# non-consecutive years together as if they were one year apart. Every
# series is restricted to its longest run of genuinely consecutive calendar
# years before any model is fit; restrict_to_longest_run() records what was
# dropped so the manuscript can report it rather than hide it.

library(dplyr)
library(readr)
library(tidyr)
library(forecast)

trade <- read_csv("ms1_trade_prediction/data/processed/trade_series_by_sector.csv", show_col_types = FALSE)

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

horizon <- 5
min_train_years <- 10 # rolling-origin backtest starts once at least this many years are available

series_keys <- trade |> distinct(sector, flow, data_source)

# Build the xreg matrix for one series: only include a shock column if it has
# non-zero variance in that series (red_sea_shock is 0 everywhere except
# Transport and logistics, so it is dropped for the other two sectors rather
# than passed in as a constant column, which auto.arima cannot use anyway).
build_xreg <- function(df) {
  candidate_cols <- c("covid_shock", "red_sea_shock")
  usable <- candidate_cols[sapply(candidate_cols, function(cn) length(unique(df[[cn]])) > 1)]
  if (length(usable) == 0) return(NULL)
  as.matrix(df[, usable, drop = FALSE])
}

# Re-checks variance within the training window itself, not just the full
# series: a shock dummy can be constant (all zero) in an early training
# window even though it varies over the full series, e.g. covid_shock before
# 2020 is in scope. auto.arima silently drops a constant regressor, which
# then desyncs the column count expected by forecast()'s xreg argument. This
# keeps xreg_train and xreg_next in agreement with what auto.arima actually
# fits, window by window.
usable_in_window <- function(xreg_train) {
  if (is.null(xreg_train)) return(NULL)
  keep <- apply(xreg_train, 2, function(col) length(unique(col)) > 1)
  if (!any(keep)) return(NULL)
  colnames(xreg_train)[keep]
}

fit_arima_one_step <- function(y, xreg_train, xreg_next) {
  cols <- usable_in_window(xreg_train)
  xreg_train <- if (is.null(cols)) NULL else xreg_train[, cols, drop = FALSE]
  xreg_next <- if (is.null(cols)) NULL else xreg_next[cols]

  model <- tryCatch(
    if (is.null(xreg_train)) auto.arima(y) else auto.arima(y, xreg = xreg_train),
    error = function(e) auto.arima(y) # fall back to univariate if xreg fails to fit
  )
  fc <- if (is.null(xreg_train) || is.null(xreg_next)) {
    forecast(model, h = 1)
  } else {
    forecast(model, h = 1, xreg = matrix(xreg_next, nrow = 1, dimnames = list(NULL, cols)))
  }
  as.numeric(fc$mean[1])
}

rolling_origin_backtest <- function(df) {
  df <- df |> arrange(year)
  y_full <- df$value_usd
  n <- length(y_full)
  xreg_full <- build_xreg(df)

  if (n <= min_train_years + 1) {
    return(tibble(origin_year = integer(), actual = numeric(), point_forecast = numeric()))
  }

  results <- lapply(min_train_years:(n - 1), function(origin) {
    y_train <- y_full[1:origin]
    xreg_train <- if (is.null(xreg_full)) NULL else xreg_full[1:origin, , drop = FALSE]
    xreg_next <- if (is.null(xreg_full)) NULL else xreg_full[origin + 1, ]
    pf <- fit_arima_one_step(y_train, xreg_train, xreg_next)
    tibble(origin_year = df$year[origin], actual = y_full[origin + 1], point_forecast = pf)
  })
  bind_rows(results)
}

fit_full_sample <- function(df) {
  df <- df |> arrange(year)
  y <- df$value_usd
  xreg_train <- build_xreg(df)
  model <- tryCatch(
    if (is.null(xreg_train)) auto.arima(y) else auto.arima(y, xreg = xreg_train),
    error = function(e) auto.arima(y)
  )
  if (is.null(xreg_train)) {
    fc <- forecast(model, h = horizon)
  } else {
    # Conservative assumption: no shock over the forecast horizon.
    xreg_future <- matrix(0, nrow = horizon, ncol = ncol(xreg_train), dimnames = list(NULL, colnames(xreg_train)))
    fc <- forecast(model, h = horizon, xreg = xreg_future)
  }
  list(model = model, forecast = fc)
}

arima_backtests <- list()
arima_full_fits <- list()

continuity_log <- list()
dir.create("ms1_trade_prediction/output", showWarnings = FALSE, recursive = TRUE)

for (i in seq_len(nrow(series_keys))) {
  sec <- series_keys$sector[i]
  fl <- series_keys$flow[i]
  df_raw <- trade |> filter(sector == sec, flow == fl)
  restricted <- restrict_to_longest_run(df_raw)
  df <- restricted$df
  key <- paste(sec, fl, sep = " | ")

  continuity_log[[key]] <- tibble(
    sector = sec, flow = fl,
    original_n = restricted$original_n,
    used_n = nrow(df),
    used_years = paste(range(df$year), collapse = "-"),
    dropped_years = if (length(restricted$dropped_years) == 0) "" else paste(restricted$dropped_years, collapse = ", ")
  )

  arima_backtests[[key]] <- rolling_origin_backtest(df) |> mutate(sector = sec, flow = fl)
  arima_full_fits[[key]] <- c(fit_full_sample(df), list(sector = sec, flow = fl, series = df))
}

continuity_tbl <- bind_rows(continuity_log)
write_csv(continuity_tbl, "ms1_trade_prediction/output/series_continuity_log.csv")

arima_backtest_tbl <- bind_rows(arima_backtests)

arima_fit_stats <- arima_backtest_tbl |>
  group_by(sector, flow) |>
  summarise(
    RMSE = sqrt(mean((actual - point_forecast)^2)),
    MAE = mean(abs(actual - point_forecast)),
    MAPE = mean(abs(actual - point_forecast) / actual) * 100,
    n_origins = n(),
    .groups = "drop"
  )

arima_forecast_tbl <- bind_rows(lapply(arima_full_fits, function(r) {
  tibble(
    sector = r$sector,
    flow = r$flow,
    year = max(r$series$year) + seq_len(horizon),
    point_forecast = as.numeric(r$forecast$mean),
    lo_95 = as.numeric(r$forecast$lower[, "95%"]),
    hi_95 = as.numeric(r$forecast$upper[, "95%"]),
    model_order = forecast:::arima.string(r$model, padding = FALSE)
  )
}))

dir.create("ms1_trade_prediction/output", showWarnings = FALSE, recursive = TRUE)
write_csv(arima_forecast_tbl, "ms1_trade_prediction/output/arima_forecast.csv")
write_csv(arima_fit_stats, "ms1_trade_prediction/output/arima_fit_stats.csv")
write_csv(arima_backtest_tbl, "ms1_trade_prediction/output/arima_backtest_origins.csv")
saveRDS(arima_full_fits, "ms1_trade_prediction/data/processed/arima_results.rds")

message(
  "ARIMA: rolling-origin backtest across ", nrow(series_keys), " sector-flow series, ",
  "horizon = ", horizon, " years. data_source = ", paste(unique(series_keys$data_source), collapse = ", ")
)
