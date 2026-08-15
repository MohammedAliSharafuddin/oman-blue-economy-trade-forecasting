# Oman_RG/ms1_trade_prediction/04_forecast_accuracy_comparison.R
#
# Compares ARIMA (02_arima.R) and BSTS (03_bsts.R) on genuine out-of-sample,
# rolling-origin backtest accuracy (not in-sample fit, which an earlier
# version of this script used), per sector x flow series rather than one
# national aggregate. A simple equal-weight combination of the two methods'
# forecasts is added as a third candidate at each origin, since forecast
# combination routinely beats either individual method in the forecasting
# literature and costs nothing extra to compute here.

library(dplyr)
library(readr)

arima_backtest <- read_csv("ms1_trade_prediction/output/arima_backtest_origins.csv", show_col_types = FALSE) |>
  rename(arima_forecast = point_forecast)
bsts_backtest <- read_csv("ms1_trade_prediction/output/bsts_backtest_origins.csv", show_col_types = FALSE) |>
  rename(bsts_forecast = point_forecast)

backtest_joined <- arima_backtest |>
  inner_join(bsts_backtest, by = c("sector", "flow", "origin_year", "actual")) |>
  mutate(combination_forecast = (arima_forecast + bsts_forecast) / 2)

backtest_long <- bind_rows(
  backtest_joined |> transmute(sector, flow, origin_year, actual, method = "ARIMA", point_forecast = arima_forecast),
  backtest_joined |> transmute(sector, flow, origin_year, actual, method = "BSTS", point_forecast = bsts_forecast),
  backtest_joined |> transmute(sector, flow, origin_year, actual, method = "Combination", point_forecast = combination_forecast)
)

accuracy_comparison <- backtest_long |>
  group_by(method, sector, flow) |>
  summarise(
    RMSE = sqrt(mean((actual - point_forecast)^2)),
    MAE = mean(abs(actual - point_forecast)),
    MAPE = mean(abs(actual - point_forecast) / actual) * 100,
    n_origins = n(),
    .groups = "drop"
  ) |>
  arrange(sector, flow, method)

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

forecast_comparison <- bind_rows(
  arima_forecast |> select(sector, flow, year, point_forecast, lo_95, hi_95, method),
  bsts_forecast |> select(sector, flow, year, point_forecast, lo_95, hi_95, method),
  combination_forecast
) |>
  arrange(sector, flow, year, method)

preferred_method <- accuracy_comparison |>
  group_by(sector, flow) |>
  slice_min(RMSE, n = 1) |>
  ungroup() |>
  select(sector, flow, preferred_method = method, RMSE, n_origins)

dir.create("ms1_trade_prediction/output", showWarnings = FALSE, recursive = TRUE)
write_csv(accuracy_comparison, "ms1_trade_prediction/output/accuracy_comparison.csv")
write_csv(forecast_comparison, "ms1_trade_prediction/output/forecast_comparison.csv")
write_csv(preferred_method, "ms1_trade_prediction/output/preferred_method_by_flow.csv")

message(
  "Accuracy comparison written (rolling-origin backtest, ARIMA vs BSTS vs Combination).\n",
  "Preferred method by sector x flow:\n",
  paste(capture.output(print(preferred_method, n = Inf)), collapse = "\n")
)
