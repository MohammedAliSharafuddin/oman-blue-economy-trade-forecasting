# Forecasting Oman's Blue Economy Trade

Data and R code accompanying the manuscript *Forecasting Oman's Blue Economy
Trade: A Sector-Disaggregated Comparison of Five Small-Sample Forecasting
Methods with Structural-Break Adjustment* (submitted to the *Journal of
Maritime Research*).

Forecasts export and import values across four sectors of Oman's blue
economy (fisheries and seafood processing, coastal and maritime tourism,
maritime freight transport, and port services and logistics), comparing
five forecasting methods (ARIMA, Bayesian structural time series, a random
walk with drift, exponential smoothing, and an equal-weight ARIMA/BSTS
combination) by genuine out-of-sample rolling-origin backtesting, with
Diebold-Mariano significance testing (Holm-corrected across all rankings)
on the result.

## Data

- `data/raw/unctadstat_export.csv` — the cleaned, long-format trade series
  (year, flow, sector, value in USD), assembled from UNCTADstat's *Ocean
  goods* and *Ocean services* analytical tables for Oman, 2005-2024.
- `data/raw/unctadstat_downloads/` — the individual per-flow, per-category
  CSV exports as downloaded from UNCTADstat, before cleaning.
- `data/processed/` — cleaned series and cross-check totals produced by the
  pipeline's first two stages.

UNCTADstat data are available at <https://unctadstat.unctad.org>. UN
Comtrade data (used for the fisheries cross-check) are available at
<https://comtradeplus.un.org> via the free API.

**`00b_comtradr_crosscheck.R` and `00c_fisheries_extended_comtrade.R`
need a free UN Comtrade API key.** Register at
<https://comtradeplus.un.org>, then create a `.Renviron` file in the
repository root containing `COMTRADE_PRIMARY=your_key_here`. Both
scripts call `readRenviron(".Renviron")` and stop with an explicit error
if the key is missing or the API call fails, rather than substituting
placeholder data. `05_figures.R` depends on `00b`'s output
(`output/fisheries_crosscheck_comparison.csv`) for Figure 4, so `00b`
must run before it.

## Code

All scripts live in `R/` and are numbered in run order. Run every script
with the repository root as the working directory:

| Script | Purpose |
|---|---|
| `00_clean_unctadstat_downloads.R` | Combine the raw per-file UNCTADstat downloads into `data/raw/unctadstat_export.csv` |
| `00b_comtradr_crosscheck.R` | Pull the fisheries sector from UN Comtrade via `comtradr` as an independent cross-check (needs `COMTRADE_PRIMARY`, see above) |
| `00c_fisheries_extended_comtrade.R` | Extend the fisheries Comtrade series back to 2000 for a longer-horizon robustness check (needs `COMTRADE_PRIMARY`, see above) |
| `01_import_unctadstat.R` | Reshape the cleaned export into per-sector time series |
| `02_arima.R` | Fit ARIMA models with structural-break regressors |
| `03_bsts.R` | Fit Bayesian structural time series models with structural-break regressors |
| `04_forecast_accuracy_comparison.R` | Rolling-origin backtest, ARIMA vs BSTS vs their combination |
| `05_figures.R` | Build the manuscript's 4 figures from pipeline output (needs `00b`'s output, see above) |
| `06_naive_ets_baselines.R` | Add the random-walk-with-drift and ETS small-sample benchmark methods |
| `07_fisheries_extended_backtest.R` | Backtest against the extended (2000-) fisheries series |
| `08_significance_tests.R` | Diebold-Mariano test, winner vs runner-up, all backtestable series, Holm-corrected |

```r
# from the repository root
source("R/00_clean_unctadstat_downloads.R")
source("R/00b_comtradr_crosscheck.R")
source("R/01_import_unctadstat.R")
source("R/02_arima.R")
source("R/03_bsts.R")
source("R/06_naive_ets_baselines.R")
source("R/04_forecast_accuracy_comparison.R")
source("R/05_figures.R")
source("R/00c_fisheries_extended_comtrade.R")
source("R/07_fisheries_extended_backtest.R")
source("R/08_significance_tests.R")
```

R packages used: `dplyr`, `readr`, `tidyr`, `forecast`, `bsts`, `comtradr`,
`ggplot2`, `scales`.

**A note on exact reproducibility.** `03_bsts.R` passes a fixed seed
(2026) to every `bsts()` call, but this does not make BSTS's own MCMC
sampler fully deterministic end to end: rerunning this pipeline was
found to move BSTS-derived RMSE figures (and, downstream, the
`Combination` method and the Diebold-Mariano statistics that involve
BSTS) by roughly 0-3% between runs on identical input data. Every
qualitative conclusion the manuscript draws (which method wins each
series, and that no Diebold-Mariano ranking survives Holm correction)
was checked and held across repeated runs; the specific decimal figures
printed in the manuscript's tables may not match a fresh run bit for
bit. ARIMA, the naive-drift and ETS baselines, and the Comtrade
cross-check percentages are fully deterministic and reproduce exactly.

## Output

`output/` holds every intermediate and final result table the manuscript
reports numbers from, including the per-origin backtest errors and the
Diebold-Mariano test results (`diebold_mariano_tests.csv`). `figures/`
holds the 4 manuscript figures as generated by `05_figures.R`.

## License

Code is released under the MIT License (see `LICENSE`). The underlying
UNCTADstat and UN Comtrade data remain subject to those providers' own
terms of use.
