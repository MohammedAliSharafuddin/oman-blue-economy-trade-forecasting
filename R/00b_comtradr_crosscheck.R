# Oman_RG/00b_comtradr_crosscheck.R
#
# Independent cross-check of the Fisheries and seafood processing sector
# against raw UN Comtrade goods trade (not UNCTADstat's ocean-adjusted
# figures), since this is the one sector that maps cleanly onto real HS
# codes: HS 03 (fish and crustaceans, molluscs and other aquatic
# invertebrates) and HS 16 (prepared/preserved fish, i.e. seafood
# processing). Oman, 2012-2024, Export and Import, matching the
# UNCTADstat series' own year range for this sector.
#
# Requires a free UN Comtrade API key (COMTRADE_PRIMARY in .Renviron,
# gitignored, never commit it). The API caps a single query at 12
# consecutive years, so 2012-2024 is split into two calls per HS
# code/flow and combined.
#
# This is a cross-check, not a replacement: UNCTADstat's "Marine
# fisheries" figure is ocean-adjusted (coefficients applied to isolate
# ocean-based activity from raw HS trade), so it will not match Comtrade's
# raw HS 03/16 totals exactly. Expect the same order of magnitude, not
# identical numbers; report the difference honestly rather than picking
# whichever source looks more convenient.

library(comtradr)
library(dplyr)
library(readr)

readRenviron(".Renviron")

hs_codes <- c("03", "16") # fish/crustaceans; prepared fish (seafood processing)
flows <- c("export", "import")
year_chunks <- list(2012:2023, 2024:2024)

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
  stop("No data returned from Comtrade. Check COMTRADE_PRIMARY in .Renviron and API status before assuming this sector has no cross-check.")
}

comtrade_fisheries <- raw |>
  transmute(
    year = ref_year,
    flow = tools::toTitleCase(tolower(flow_desc)), # "Export"/"Import" -> match casing used elsewhere
    hs_code = cmd_code,
    value_usd = primary_value
  ) |>
  group_by(year, flow) |>
  summarise(value_usd_comtrade = sum(value_usd, na.rm = TRUE), n_hs_codes = n(), .groups = "drop") |>
  arrange(flow, year)

dir.create("output", showWarnings = FALSE, recursive = TRUE)
write_csv(comtrade_fisheries, "output/comtrade_fisheries_crosscheck.csv")

# Compare against the UNCTADstat-derived fisheries series already in the
# pipeline, where the flow labels are "Exports"/"Imports" (plural, per the
# UNCTADstat source) rather than comtradr's "Export"/"Import" singular.
unctad_fisheries <- read_csv("data/processed/trade_series_by_sector.csv", show_col_types = FALSE) |>
  filter(sector == "Fisheries and seafood processing") |>
  mutate(flow = sub("s$", "", flow)) |> # "Exports" -> "Export" to match comtradr's labels
  select(year, flow, value_usd_unctadstat = value_usd)

comparison <- comtrade_fisheries |>
  inner_join(unctad_fisheries, by = c("year", "flow")) |>
  mutate(
    ratio = value_usd_comtrade / value_usd_unctadstat,
    pct_diff = (value_usd_comtrade - value_usd_unctadstat) / value_usd_unctadstat * 100
  ) |>
  arrange(flow, year)

write_csv(comparison, "output/fisheries_crosscheck_comparison.csv")

message("Comtrade cross-check: ", nrow(comtrade_fisheries), " year-flow rows fetched.")
message("Comparison against UNCTADstat fisheries series, ", nrow(comparison), " overlapping year-flow rows:")
print(comparison |> summarise(
  mean_pct_diff = mean(pct_diff, na.rm = TRUE),
  median_pct_diff = median(pct_diff, na.rm = TRUE),
  min_pct_diff = min(pct_diff, na.rm = TRUE),
  max_pct_diff = max(pct_diff, na.rm = TRUE)
))
