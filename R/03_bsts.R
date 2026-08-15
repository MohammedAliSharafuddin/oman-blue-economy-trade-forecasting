# Oman_RG/03_bsts.R
#
# Fits Bayesian Structural Time Series models (via bsts) to each sector x
# flow series produced by 01_import_unctadstat.R, as the alternative to
# 02_arima.R. The structural-break dummies (covid_shock, red_sea_shock) are
# entered as a static regression component on top of the local linear trend,
# using bsts's own strength: an explicit, interpretable state decomposition
# around shocks, rather than treating BSTS as just a second black box to
# race against ARIMA.
#
# As in 02_arima.R, two things are produced per series: a rolling-origin
# one-step-ahead backtest (genuine out-of-sample accuracy) and a full-sample
# model with its horizon-year-ahead forecast for the paper's forecast table.
#
# The backtest refits a fresh model at every origin, which is expensive at
# the full niter used for the headline models. backtest_niter is
# deliberately smaller; this is a speed/precision tradeoff scoped to the
# backtest loop only, not the final reported models.
#
# Same continuity restriction as 02_arima.R: real UNCTADstat series can have
# missing years, and sorting by year then treating adjacent rows as adjacent
# years would splice non-consecutive years together. Every series is
# restricted to its longest run of genuinely consecutive calendar years
# before fitting.

library(dplyr)
library(readr)
library(bsts)

trade <- read_csv("data/processed/trade_series_by_sector.csv", show_col_types = FALSE)

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
min_train_years <- 10
full_niter <- 2000
full_burn <- 500
backtest_niter <- 500
backtest_burn <- 100
seed <- 2026

series_keys <- trade |> distinct(sector, flow, data_source)

build_regressors <- function(df) {
  candidate_cols <- c("covid_shock", "red_sea_shock")
  usable <- candidate_cols[sapply(candidate_cols, function(cn) length(unique(df[[cn]])) > 1)]
  usable
}

fit_bsts_series <- function(df, niter, burn) {
  df <- df |> arrange(year)
  reg_cols <- build_regressors(df)
  ss <- AddLocalLinearTrend(list(), df$value_usd)
  if (length(reg_cols) == 0) {
    model <- bsts(df$value_usd, state.specification = ss, niter = niter, ping = 0, seed = seed)
  } else {
    fml <- as.formula(paste("value_usd ~", paste(reg_cols, collapse = " + ")))
    model <- bsts(fml, state.specification = ss, data = df, niter = niter, ping = 0, seed = seed)
  }
  list(model = model, reg_cols = reg_cols)
}

rolling_origin_backtest <- function(df) {
  df <- df |> arrange(year)
  n <- nrow(df)
  if (n <= min_train_years + 1) {
    return(tibble(origin_year = integer(), actual = numeric(), point_forecast = numeric()))
  }
  results <- lapply(min_train_years:(n - 1), function(origin) {
    train_df <- df[1:origin, ]
    fit <- fit_bsts_series(train_df, backtest_niter, backtest_burn)
    pf <- if (length(fit$reg_cols) == 0) {
      pred <- predict(fit$model, horizon = 1, burn = backtest_burn)
      as.numeric(pred$mean[1])
    } else {
      newdata <- df[origin + 1, fit$reg_cols, drop = FALSE]
      pred <- predict(fit$model, horizon = 1, newdata = newdata, burn = backtest_burn)
      as.numeric(pred$mean[1])
    }
    tibble(origin_year = df$year[origin], actual = df$value_usd[origin + 1], point_forecast = pf)
  })
  bind_rows(results)
}

fit_full_sample <- function(df) {
  df <- df |> arrange(year)
  fit <- fit_bsts_series(df, full_niter, full_burn)
  if (length(fit$reg_cols) == 0) {
    pred <- predict(fit$model, horizon = horizon, burn = full_burn)
  } else {
    # Conservative assumption: no shock over the forecast horizon, matching 02_arima.R.
    future_reg <- as.data.frame(matrix(0, nrow = horizon, ncol = length(fit$reg_cols)))
    names(future_reg) <- fit$reg_cols
    pred <- predict(fit$model, horizon = horizon, newdata = future_reg, burn = full_burn)
  }
  list(model = fit$model, prediction = pred, series = df)
}

bsts_backtests <- list()
bsts_full_fits <- list()

for (i in seq_len(nrow(series_keys))) {
  sec <- series_keys$sector[i]
  fl <- series_keys$flow[i]
  df_raw <- trade |> filter(sector == sec, flow == fl)
  df <- restrict_to_longest_run(df_raw)$df
  key <- paste(sec, fl, sep = " | ")
  bsts_backtests[[key]] <- rolling_origin_backtest(df) |> mutate(sector = sec, flow = fl)
  bsts_full_fits[[key]] <- c(fit_full_sample(df), list(sector = sec, flow = fl))
}

bsts_backtest_tbl <- bind_rows(bsts_backtests)

bsts_fit_stats <- bsts_backtest_tbl |>
  group_by(sector, flow) |>
  summarise(
    RMSE = sqrt(mean((actual - point_forecast)^2)),
    MAE = mean(abs(actual - point_forecast)),
    MAPE = mean(abs(actual - point_forecast) / actual) * 100,
    n_origins = n(),
    .groups = "drop"
  )

bsts_forecast_tbl <- bind_rows(lapply(bsts_full_fits, function(r) {
  tibble(
    sector = r$sector,
    flow = r$flow,
    year = max(r$series$year) + seq_len(horizon),
    point_forecast = as.numeric(r$prediction$mean),
    lo_95 = as.numeric(apply(r$prediction$distribution, 2, quantile, probs = 0.025)),
    hi_95 = as.numeric(apply(r$prediction$distribution, 2, quantile, probs = 0.975))
  )
}))

dir.create("output", showWarnings = FALSE, recursive = TRUE)
write_csv(bsts_forecast_tbl, "output/bsts_forecast.csv")
write_csv(bsts_fit_stats, "output/bsts_fit_stats.csv")
write_csv(bsts_backtest_tbl, "output/bsts_backtest_origins.csv")
saveRDS(bsts_full_fits, "data/processed/bsts_results.rds")

message(
  "BSTS: rolling-origin backtest across ", nrow(series_keys), " sector-flow series, ",
  "horizon = ", horizon, " years. data_source = ", paste(unique(series_keys$data_source), collapse = ", ")
)
