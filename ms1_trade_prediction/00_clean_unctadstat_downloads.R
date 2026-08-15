# Oman_RG/ms1_trade_prediction/00_clean_unctadstat_downloads.R
#
# Reshapes the raw UNCTADstat pivot-table exports in
# data/raw/unctadstat_downloads/ into the single long-format file
# 01_import_unctadstat.R expects at data/raw/unctadstat_export.csv
# (columns: year, flow, sector, value_usd).
#
# Each source file is one Flow x Product/Category combination for Oman,
# downloaded from two UNCTADstat datasets:
#   - "Ocean goods: Bilateral trade by product group - Annual (analytical)"
#     (US.OceanTrade), values in US$ thousands, years 2012-2024 (2025 not
#     yet publishable).
#   - "Ocean services: Trade - Annual (analytical)" (US.OceanServices),
#     values in US$ millions, years 2005-2024 (2025 not yet publishable).
# Units differ between the two datasets and are converted to plain US$
# here so the pipeline's value_usd column means the same thing everywhere.
#
# Sector mapping (owner-agreed 2026-08-13):
#   Fisheries and seafood processing = Marine fisheries, aquaculture and
#     hatcheries + Seafood processing (both from Ocean goods)
#   Coastal and maritime tourism = Marine and coastal tourism (Ocean services)
#   Transport and logistics = Maritime freight transport + Port services,
#     related infrastructure and logistical services (both Ocean services)
#
# "Not publishable" and blank cells become NA, not zero and not an
# interpolated guess. Where a sector's series has an internal gap (not just
# missing at the start/end), that is a real UNCTADstat data-availability
# limitation, not a cleaning artefact, and 02_arima.R / 03_bsts.R restrict
# modelling to the longest contiguous run rather than bridging the gap.

library(dplyr)
library(readr)
library(tidyr)
library(stringr)

raw_dir <- "data/raw/unctadstat_downloads"
files <- list.files(raw_dir, pattern = "\\.csv$", full.names = TRUE)

read_one <- function(path) {
  meta_path <- sub("\\.csv$", "_metadata.txt", path)
  meta <- read_lines(meta_path)
  flow <- str_trim(str_extract(meta[str_detect(meta, "Flow:")], "(?<=Flow:).*"))
  product_line <- meta[str_detect(meta, "Product:|Category:")]
  product <- str_trim(str_extract(product_line, "(?<=Product:|Category:).*"))
  is_goods <- str_detect(path, "OceanTrade")

  df <- read_csv(path, show_col_types = FALSE) |> filter(Economy_Label == "Oman")
  if (nrow(df) == 0) {
    warning("No Oman row in ", path, ", skipping.")
    return(NULL)
  }

  value_cols <- names(df)[str_detect(names(df), "^\\d{4}_.*_Value$")]
  out <- tibble(col = value_cols, raw_value = as.numeric(unlist(df[1, value_cols]))) |>
    mutate(
      year = as.integer(str_extract(col, "^\\d{4}")),
      # thousands (Ocean goods) or millions (Ocean services) -> plain US$
      value_usd = if (is_goods) raw_value * 1e3 else raw_value * 1e6,
      flow = flow,
      product = product,
      source_file = basename(path)
    ) |>
    filter(!is.na(value_usd)) |>
    select(year, flow, product, value_usd, source_file)
  out
}

parsed <- bind_rows(lapply(files, read_one))

# Freight and port services are kept as SEPARATE sectors rather than summed
# into one "Transport and logistics" series. Checked before deciding this:
# freight has no data for Oman 2006-2017 (only 2005 and 2018-2024), while
# port services is complete 2005-2024. Summing them would mean the combined
# series reflects port-services-only for 2006-2017 and freight+port from
# 2018 onward, a silent composition break that would look like real growth
# in the series but is actually a change in what is being counted. The
# fisheries + seafood processing pair was checked the same way and both
# components are present in every year 2012-2024, so that sum is safe.
product_to_sector <- c(
  "Marine fisheries, aquaculture and hatcheries" = "Fisheries and seafood processing",
  "Seafood processing" = "Fisheries and seafood processing",
  "Marine and coastal tourism" = "Coastal and maritime tourism",
  "Maritime transport and related services: freight" = "Maritime freight transport",
  "Port services, related infrastructure services and logistical services" = "Port services and logistics"
)

sector_rows <- parsed |>
  filter(product %in% names(product_to_sector)) |>
  mutate(sector = product_to_sector[product]) |>
  group_by(year, flow, sector) |>
  summarise(value_usd = sum(value_usd), n_products_summed = n(), .groups = "drop") |>
  arrange(sector, flow, year)

# Cross-check rows (Ocean goods Total, Ocean services Total) are kept
# separately for a sanity check, not fed into the sector table the
# forecasting pipeline consumes.
crosscheck_rows <- parsed |>
  filter(product %in% c("Total", "Ocean-related services")) |>
  transmute(year, flow, sector = paste0("[cross-check] ", product), value_usd)

trade_out <- sector_rows |>
  transmute(year, flow, sector, value_usd) |>
  mutate(data_source = "unctadstat_real")

dir.create("data/raw", showWarnings = FALSE, recursive = TRUE)
dir.create("ms1_trade_prediction/data/processed", showWarnings = FALSE, recursive = TRUE)
write_csv(trade_out, "data/raw/unctadstat_export.csv")
write_csv(crosscheck_rows, "ms1_trade_prediction/data/processed/unctadstat_crosscheck_totals.csv")

message("Wrote ", nrow(trade_out), " real rows to data/raw/unctadstat_export.csv, sectors: ",
        paste(unique(trade_out$sector), collapse = "; "))
message("Year ranges by sector-flow:")
print(trade_out |> group_by(sector, flow) |> summarise(min_year = min(year), max_year = max(year), n = n(), .groups = "drop"))
