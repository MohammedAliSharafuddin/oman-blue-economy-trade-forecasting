# Oman_RG/ms1_trade_prediction/00c_fisheries_extended_comtrade.R
#
# Builds an extended-history version of the Fisheries and seafood
# processing series, 2000-2024 (25 years) instead of the UNCTADstat
# "Ocean goods" table's 2012-2024 (13 years), using raw UN Comtrade HS 03
# + HS 16 trade throughout, Oman, export and import.
#
# This does NOT splice raw Comtrade onto UNCTADstat's ocean-adjusted
# figures. 00b_comtradr_crosscheck.R already established Comtrade runs
# about 15% below UNCTADstat's ocean-adjusted figure on average (a
# methodology difference, not an error). Joining the two at 2012 would
# create a level discontinuity that reads as trade change in the series
# without being one, the exact problem this pipeline's own
# 01_import_unctadstat.R already rejected when it kept Maritime freight
# transport and Port services and logistics as two series instead of
# summing them. So this series is built from Comtrade alone, start to
# finish, one consistent methodology throughout, and used only as a
# robustness check on top of the headline UNCTADstat-based sector, not as
# a replacement for it.
#
# Requires the same COMTRADE_PRIMARY API key as 00b_comtradr_crosscheck.R
# (Oman_RG/.Renviron, gitignored). The API caps a single query at 12
# consecutive years, so 2000-2024 is split into three calls per HS
# code/flow and combined.

library(comtradr)
library(dplyr)
library(readr)

readRenviron(".Renviron")

hs_codes <- c("03", "16")
flows <- c("export", "import")
year_chunks <- list(2000:2011, 2012:2023, 2024:2024)

fetch_one <- function(hs, flow, years) {
  tryCatch(
    ct_get_data(
      reporter = "OMN", partner = "World", commodity_code = hs,
      start_date = min(years), end_date = max(years), flow_direction = flow
    ),
    error = function(e) {
      warning("comtradr fetch failed for HS", hs, " ", flow, " ", min(years), "-", max(years), ": ", conditionMessage(e))
      NULL
    }
  )
}

raw <- bind_rows(lapply(hs_codes, function(hs) {
  bind_rows(lapply(flows, function(fl) {
    bind_rows(lapply(year_chunks, function(yrs) fetch_one(hs, fl, yrs)))
  }))
}))

if (nrow(raw) == 0) {
  stop("No data returned from Comtrade. Check COMTRADE_PRIMARY in .Renviron and API status.")
}

fisheries_extended <- raw |>
  transmute(
    year = ref_year,
    flow = paste0(tools::toTitleCase(tolower(flow_desc)), "s"), # "Export" -> "Exports", matches trade_series_by_sector.csv's plural convention
    hs_code = cmd_code,
    value_usd = primary_value
  ) |>
  group_by(year, flow) |>
  summarise(value_usd = sum(value_usd, na.rm = TRUE), n_hs_codes = n(), .groups = "drop") |>
  arrange(flow, year)

n_years <- fisheries_extended |> distinct(year) |> nrow()
n_expected <- length(2000:2024)
if (n_years < n_expected) {
  warning(
    "Extended fisheries series has ", n_years, " distinct years, expected ", n_expected,
    ". Check for a real gap before treating this as a complete 2000-2024 series."
  )
}

dir.create("ms1_trade_prediction/data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(fisheries_extended, "ms1_trade_prediction/data/processed/fisheries_extended_comtrade.csv")

message(
  "Extended fisheries series (Comtrade HS 03+16, Oman, ", min(fisheries_extended$year),
  "-", max(fisheries_extended$year), "): ", nrow(fisheries_extended), " year-flow rows, ",
  n_years, " distinct years."
)
