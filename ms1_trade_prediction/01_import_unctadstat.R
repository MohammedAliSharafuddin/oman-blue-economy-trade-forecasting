# Oman_RG/ms1_trade_prediction/01_import_unctadstat.R
#
# Imports Oman's blue economy sector trade series (exports and imports) from
# an UNCTADstat bulk CSV export. UNCTADstat (unctadstat.unctad.org) has no
# CRAN-supported R client, so the expected workflow is: query UNCTADstat's
# "General Profile" or "Trade Matrix" tables for Oman, filter to the blue
# economy sectors (fisheries and seafood processing, coastal and maritime
# tourism, transport and logistics), and use its own "Download > CSV" export,
# saved to data/raw/unctadstat_export.csv with columns:
# year, flow (Export/Import), sector, value_usd.
#
# Sector-level series are the primary analysis unit from here on (see
# 02_arima.R and 03_bsts.R): each sector is forecast separately rather than
# collapsed into one national series, so that method preference can be
# compared across sectors instead of decided once on an aggregate.
#
# Every output row carries a `data_source` column, "synthetic_placeholder"
# or "unctadstat_real", so no downstream table or figure can be mistaken for
# real findings while the placeholder is still in use. Check this column
# before citing any number from this pipeline.

library(dplyr)
library(readr)
library(tidyr)

raw_path <- "data/raw/unctadstat_export.csv"
is_synthetic <- !file.exists(raw_path)

if (is_synthetic) {
  message(
    "No file found at ", raw_path, ". Generating a synthetic placeholder ",
    "annual trade series (2005-2024) so the pipeline can be exercised end ",
    "to end. This is NOT real UNCTADstat data. Every output row is tagged ",
    "data_source = 'synthetic_placeholder'. Replace data/raw/",
    "unctadstat_export.csv with the actual UNCTADstat export before any ",
    "number from this pipeline is used in the manuscript."
  )
  set.seed(2026)
  years <- 2005:2024
  sectors <- c("Fisheries and seafood processing", "Coastal and maritime tourism", "Transport and logistics")
  # Synthetic shocks are injected (COVID dip 2020-2021, a smaller Red Sea-era
  # dip in transport/logistics 2023-2024) purely so the structural-break
  # code paths below have something to exercise before real data arrives.
  # These dips are not evidence of anything; they exist only to test code.
  synthetic <- expand.grid(year = years, flow = c("Export", "Import"), sector = sectors) |>
    as_tibble() |>
    arrange(sector, flow, year) |>
    group_by(sector, flow) |>
    mutate(
      trend = seq(100, 100 + 8 * (n() - 1), length.out = n()),
      covid_dip = ifelse(year %in% 2020:2021, -25, 0),
      red_sea_dip = ifelse(sector == "Transport and logistics" & year %in% 2023:2024, -15, 0),
      value_usd = pmax(0, trend + covid_dip + red_sea_dip + rnorm(n(), sd = 12)) * 1e6
    ) |>
    ungroup() |>
    select(year, flow, sector, value_usd)
  dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
  write_csv(synthetic, raw_path)
}

trade_raw <- read_csv(raw_path, show_col_types = FALSE) |>
  mutate(data_source = if (is_synthetic) "synthetic_placeholder" else "unctadstat_real")

# Structural-break dummies. COVID-19 disruption (2020-2021) is applied across
# every sector, since it was an economy-wide demand and logistics shock. The
# Red Sea shipping crisis (2023-2024, Suez rerouting around the Cape of Good
# Hope) is applied only to Transport and logistics, since that is the sector
# it plausibly hits directly; applying it economy-wide would overstate the
# claim. Adjust these date ranges and sector assignments once real data and
# a literature check confirm the actual shock windows for Oman specifically.
trade_by_sector <- trade_raw |>
  mutate(
    covid_shock = as.integer(year %in% 2020:2021),
    red_sea_shock = as.integer(sector == "Transport and logistics" & year %in% 2023:2024)
  ) |>
  arrange(sector, flow, year)

# National aggregate, kept for descriptive reporting only (the paper's
# opening figures, say) -- it is not the unit any model in 02/03 is fit on.
trade_national <- trade_by_sector |>
  group_by(year, flow) |>
  summarise(
    value_usd = sum(value_usd),
    covid_shock = max(covid_shock),
    data_source = first(data_source),
    .groups = "drop"
  ) |>
  arrange(flow, year)

dir.create("ms1_trade_prediction/data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(trade_national, "ms1_trade_prediction/data/processed/trade_series.csv")
write_csv(trade_by_sector, "ms1_trade_prediction/data/processed/trade_series_by_sector.csv")

message(
  "Trade series written: ", nrow(trade_by_sector), " sector-flow-year rows across ",
  length(unique(trade_by_sector$sector)), " sectors. data_source = ",
  unique(trade_by_sector$data_source)
)
