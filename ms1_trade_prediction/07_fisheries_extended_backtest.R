# Oman_RG/ms1_trade_prediction/07_fisheries_extended_backtest.R
#
# Robustness check: does the Fisheries and seafood processing sector's
# preferred method (BSTS, both flows, per Table 2) hold up on a longer,
# single-methodology series? UNCTADstat's ocean-adjusted "Ocean goods"
# table only covers Oman 2012-2024 (13 years) for this sector, but raw UN
# Comtrade HS 03 + HS 16 data covers 2000-2024 (25 years, confirmed
# complete, no gaps, in 00c_fisheries_extended_comtrade.R). This script
# reruns all five methods (ARIMA, BSTS, Combination, Naive with drift,
# ETS) on that longer series, same rolling-origin design as the headline
# pipeline (min_train_years = 10), so the 2000-2011 window now also
# supports real backtest origins the 13-year UNCTADstat series could not.
#
# This is a robustness check on top of the headline UNCTADstat-based
# results, not a replacement for them. The headline sector-level analysis
# stays on UNCTADstat throughout the paper for methodological consistency
# across all four sectors; this script's series is raw HS-code trade, a
# different basis (see 00b_comtradr_crosscheck.R's own finding: Comtrade
# runs ~15% below UNCTADstat's ocean-adjusted figure on average). Reusing
# it as the headline series for one sector only, while the other three
# stay on UNCTADstat, would itself be an inconsistency worth avoiding.
#
# A covid_shock dummy (2020-2021) is added for comparability with the
# headline pipeline's treatment of this sector. red_sea_shock is not
# added, matching the headline pipeline's own scoping of that dummy to
# Maritime freight transport and Port services and logistics only.

library(dplyr)
library(readr)
library(forecast)
library(bsts)

fisheries <- read_csv("ms1_trade_prediction/data/processed/fisheries_extended_comtrade.csv", show_col_types = FALSE) |>
  mutate(covid_shock = as.integer(year %in% c(2020, 2021))) |>
  arrange(flow, year)

horizon <- 5
min_train_years <- 10
bsts_full_niter <- 2000
bsts_full_burn <- 500
bsts_backtest_niter <- 500
bsts_backtest_burn <- 100
seed <- 2026

# ---- ARIMA ---------------------------------------------------------------

fit_arima_one_step <- function(y_train, xreg_train, xreg_next) {
  has_xreg <- length(unique(xreg_train)) > 1
  model <- tryCatch(
    if (has_xreg) auto.arima(y_train, xreg = xreg_train) else auto.arima(y_train),
    error = function(e) auto.arima(y_train)
  )
  fc <- if (has_xreg) forecast(model, h = 1, xreg = xreg_next) else forecast(model, h = 1)
  as.numeric(fc$mean[1])
}

fit_arima_full <- function(df) {
  y <- df$value_usd
  xreg <- df$covid_shock
  has_xreg <- length(unique(xreg)) > 1
  model <- tryCatch(
    if (has_xreg) auto.arima(y, xreg = xreg) else auto.arima(y),
    error = function(e) auto.arima(y)
  )
  fc <- if (has_xreg) forecast(model, h = horizon, xreg = rep(0, horizon)) else forecast(model, h = horizon)
  list(model = model, forecast = fc)
}

# ---- BSTS -----------------------------------------------------------------

fit_bsts_one_step <- function(train_df, niter, burn) {
  has_xreg <- length(unique(train_df$covid_shock)) > 1
  ss <- AddLocalLinearTrend(list(), train_df$value_usd)
  if (has_xreg) {
    model <- bsts(value_usd ~ covid_shock, state.specification = ss, data = train_df, niter = niter, ping = 0, seed = seed)
    pred <- predict(model, horizon = 1, newdata = data.frame(covid_shock = 0), burn = burn)
  } else {
    model <- bsts(train_df$value_usd, state.specification = ss, niter = niter, ping = 0, seed = seed)
    pred <- predict(model, horizon = 1, burn = burn)
  }
  as.numeric(pred$mean[1])
}

fit_bsts_full <- function(df, niter, burn) {
  has_xreg <- length(unique(df$covid_shock)) > 1
  ss <- AddLocalLinearTrend(list(), df$value_usd)
  if (has_xreg) {
    model <- bsts(value_usd ~ covid_shock, state.specification = ss, data = df, niter = niter, ping = 0, seed = seed)
    pred <- predict(model, horizon = horizon, newdata = data.frame(covid_shock = rep(0, horizon)), burn = burn)
  } else {
    model <- bsts(df$value_usd, state.specification = ss, niter = niter, ping = 0, seed = seed)
    pred <- predict(model, horizon = horizon, burn = burn)
  }
  list(model = model, prediction = pred)
}

# ---- Naive (drift) and ETS -------------------------------------------------

fit_naive_one_step <- function(y_train) {
  fc <- tryCatch(rwf(y_train, h = 1, drift = TRUE), error = function(e) naive(y_train, h = 1))
  as.numeric(fc$mean[1])
}
fit_ets_one_step <- function(y_train) {
  model <- tryCatch(ets(y_train), error = function(e) NULL)
  if (is.null(model)) return(fit_naive_one_step(y_train))
  as.numeric(forecast(model, h = 1)$mean[1])
}

# ---- Rolling-origin backtest, all five methods -----------------------------

run_backtest <- function(df) {
  df <- df |> arrange(year)
  n <- nrow(df)
  results <- lapply(min_train_years:(n - 1), function(origin) {
    train_df <- df[1:origin, ]
    y_train <- train_df$value_usd
    actual <- df$value_usd[origin + 1]
    origin_year <- df$year[origin]

    arima_pf <- fit_arima_one_step(y_train, train_df$covid_shock, df$covid_shock[origin + 1])
    bsts_pf <- fit_bsts_one_step(train_df, bsts_backtest_niter, bsts_backtest_burn)
    naive_pf <- fit_naive_one_step(y_train)
    ets_pf <- fit_ets_one_step(y_train)
    combo_pf <- (arima_pf + bsts_pf) / 2

    tibble(
      origin_year = origin_year, actual = actual,
      ARIMA = arima_pf, BSTS = bsts_pf, Combination = combo_pf,
      `Naive (drift)` = naive_pf, ETS = ets_pf
    )
  })
  bind_rows(results)
}

flows <- unique(fisheries$flow)
backtest_wide <- bind_rows(lapply(flows, function(fl) {
  run_backtest(fisheries |> filter(flow == fl)) |> mutate(flow = fl)
}))

backtest_long <- backtest_wide |>
  tidyr::pivot_longer(cols = c(ARIMA, BSTS, Combination, `Naive (drift)`, ETS), names_to = "method", values_to = "point_forecast")

accuracy_extended <- backtest_long |>
  group_by(method, flow) |>
  summarise(
    RMSE = sqrt(mean((actual - point_forecast)^2)),
    MAE = mean(abs(actual - point_forecast)),
    n_origins = n(),
    .groups = "drop"
  ) |>
  arrange(flow, RMSE)

preferred_extended <- accuracy_extended |>
  group_by(flow) |>
  slice_min(RMSE, n = 1) |>
  ungroup()

dir.create("ms1_trade_prediction/output", showWarnings = FALSE, recursive = TRUE)
write_csv(backtest_wide, "ms1_trade_prediction/output/fisheries_extended_backtest_origins.csv")
write_csv(accuracy_extended, "ms1_trade_prediction/output/fisheries_extended_accuracy.csv")

# ---- Full-sample 5-year forecast, ARIMA and BSTS only (headline methods) --

arima_full <- lapply(flows, function(fl) {
  df <- fisheries |> filter(flow == fl) |> arrange(year)
  c(fit_arima_full(df), list(flow = fl, series = df))
})
bsts_full <- lapply(flows, function(fl) {
  df <- fisheries |> filter(flow == fl) |> arrange(year)
  c(fit_bsts_full(df, bsts_full_niter, bsts_full_burn), list(flow = fl, series = df))
})

arima_forecast_tbl <- bind_rows(lapply(arima_full, function(r) {
  tibble(flow = r$flow, year = max(r$series$year) + seq_len(horizon), method = "ARIMA",
         point_forecast = as.numeric(r$forecast$mean),
         lo_95 = as.numeric(r$forecast$lower[, "95%"]), hi_95 = as.numeric(r$forecast$upper[, "95%"]))
}))
bsts_forecast_tbl <- bind_rows(lapply(bsts_full, function(r) {
  tibble(flow = r$flow, year = max(r$series$year) + seq_len(horizon), method = "BSTS",
         point_forecast = as.numeric(r$prediction$mean),
         lo_95 = as.numeric(apply(r$prediction$distribution, 2, quantile, probs = 0.025)),
         hi_95 = as.numeric(apply(r$prediction$distribution, 2, quantile, probs = 0.975)))
}))

write_csv(bind_rows(arima_forecast_tbl, bsts_forecast_tbl), "ms1_trade_prediction/output/fisheries_extended_forecast.csv")

message(
  "Fisheries extended-history robustness check (Comtrade, 2000-2024, ", nrow(fisheries |> distinct(year)), " years):\n",
  paste(capture.output(print(preferred_extended, n = Inf)), collapse = "\n")
)
