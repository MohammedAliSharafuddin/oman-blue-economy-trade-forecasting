# Oman_RG/08_significance_tests.R
#
# Reviewer response: Table 2/3's "preferred method" was previously decided
# by lowest point-estimate RMSE alone, with no test of whether the gap
# between the winner and the runner-up is distinguishable from noise on
# backtests as short as 3 to 15 origins. This script runs a
# Diebold-Mariano test (Diebold & Mariano, 1995) for every sector-flow
# series, winner versus runner-up, on the squared-error loss differential
# from the same backtest origins already used for Table 2/3's RMSE.
#
# DM test caveats stated openly rather than glossed over: dm.test()'s
# asymptotic theory is derived for longer horizons than these series
# provide (3 to 15 origins), so p-values here are indicative, not a
# substitute for a larger sample. This is exactly the paper's own
# small-sample argument applied to itself: the test is run and reported
# honestly, including where it cannot reject "no difference" given so few
# origins, instead of hiding that limitation behind an unqualified
# "preferred method" label.

library(dplyr)
library(readr)
library(forecast)

read_bt <- function(path, col_name) {
  read_csv(path, show_col_types = FALSE) |> rename(!!col_name := point_forecast)
}

arima_bt <- read_bt("output/arima_backtest_origins.csv", "ARIMA")
bsts_bt <- read_bt("output/bsts_backtest_origins.csv", "BSTS")
naive_bt <- read_bt("output/naive_backtest_origins.csv", "Naive (drift)")
ets_bt <- read_bt("output/ets_backtest_origins.csv", "ETS")

joined <- arima_bt |>
  inner_join(bsts_bt, by = c("sector", "flow", "origin_year", "actual")) |>
  inner_join(naive_bt, by = c("sector", "flow", "origin_year", "actual")) |>
  inner_join(ets_bt, by = c("sector", "flow", "origin_year", "actual")) |>
  mutate(Combination = (ARIMA + BSTS) / 2) |>
  arrange(sector, flow, origin_year)

accuracy <- read_csv("output/accuracy_comparison_5method.csv", show_col_types = FALSE)

series_keys <- joined |> distinct(sector, flow)

dm_results <- lapply(seq_len(nrow(series_keys)), function(i) {
  sec <- series_keys$sector[i]
  fl <- series_keys$flow[i]
  df <- joined |> filter(sector == sec, flow == fl) |> arrange(origin_year)

  ranked <- accuracy |> filter(sector == sec, flow == fl) |> arrange(RMSE)
  winner <- ranked$method[1]
  runner_up <- ranked$method[2]

  e_winner <- df$actual - df[[winner]]
  e_runner <- df$actual - df[[runner_up]]

  n_origins <- nrow(df)
  dm <- tryCatch(
    dm.test(e_winner, e_runner, alternative = "less", h = 1, power = 2),
    error = function(e) NULL
  )

  tibble(
    sector = sec, flow = fl, n_origins = n_origins,
    winner = winner, runner_up = runner_up,
    winner_RMSE = ranked$RMSE[1], runner_up_RMSE = ranked$RMSE[2],
    dm_statistic = if (is.null(dm)) NA_real_ else as.numeric(dm$statistic),
    p_value = if (is.null(dm)) NA_real_ else dm$p.value,
    significant_at_10pct = if (is.null(dm)) NA else dm$p.value < 0.10
  )
})

dm_tbl <- bind_rows(dm_results) |> arrange(sector, flow)

write_csv(dm_tbl, "output/diebold_mariano_tests.csv")

message("Diebold-Mariano tests, winner vs runner-up, all 7 backtestable series:")
print(dm_tbl, n = Inf, width = Inf)
message(
  "\n", sum(dm_tbl$significant_at_10pct, na.rm = TRUE), " of ", nrow(dm_tbl),
  " series show a winner distinguishable from the runner-up at p < 0.10."
)
