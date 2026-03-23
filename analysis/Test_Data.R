library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(ggplot2)

# ------------------------------------------------------------------------------
# Getting started
# ------------------------------------------------------------------------------
# To reproduce this script, create a folder named "data_raw" and download the
# following files from:
# https://zenodo.org/records/14808331
#
# Required files:
# - fdk_site_fullyearsequence.csv
# - fdk_site_info.csv
# - FLUXDATAKIT_FLUXNET.tar.gz

# ------------------------------------------------------------------------------
# Read in site info
# ------------------------------------------------------------------------------
df_site_info <- read_csv("data_raw/fdk_site_info.csv")

# ------------------------------------------------------------------------------
# Read in site full year sequence
# ------------------------------------------------------------------------------
df_site_full_year_sequence <- read_csv("data_raw/fdk_site_fullyearsequence.csv")

# ------------------------------------------------------------------------------
# Combine the two data frames
# ------------------------------------------------------------------------------
df_merged <- left_join(df_site_info, df_site_full_year_sequence, by = "sitename")

# ------------------------------------------------------------------------------
# Potentially suitable sites for the analysis
# ------------------------------------------------------------------------------
# Filter stations that have at least 15 years of GPP observations.
df_site_gpp_15years <- dplyr::filter(
  df_merged,
  year_end_gpp - year_start_gpp >= 15
)

# Filter stations with vegetation types DBF, MF, or ENF, as these are typical
# forest types in Europe and North America.
df_potential_stations <- dplyr::filter(
  df_site_gpp_15years,
  igbp_land_use %in% c("DBF", "MF", "ENF")
)

# After filtering, 13 possible sites remain for the analysis.
# For the following steps, one station of each vegetation type is selected.
# CH-Dav, IT-Lav, and US-NR1 are excluded because these sites are located at
# higher elevation, which according to the literature may cause unusual
# responses to frost events after SOS.
#
# Therefore, we download the data of the following sites:
# - BE-Vie
# - DE-Hai
# - DE-Tha

# ------------------------------------------------------------------------------
# Selection of sites for the analysis
# ------------------------------------------------------------------------------
stations <- c(
  "BE-Vie",
  "DE-Hai",
  "DE-Tha"
)

# Create data frame containing only the selected stations.
df_selected_stations <- dplyr::filter(
  df_potential_stations,
  sitename %in% stations
)

# ------------------------------------------------------------------------------
# Read in the CSV files for the selected stations
# ------------------------------------------------------------------------------
tar_file <- "data_raw/FLUXDATAKIT_FLUXNET.tar.gz"

if (!file.exists(tar_file)) {
  stop(paste("File not found:", tar_file))
}

files_in_tar <- tryCatch(
  untar(tar_file, list = TRUE),
  error = function(e) {
    stop(paste("Could not list contents of tar archive:", e$message))
  }
)

if (length(files_in_tar) == 0) {
  stop("Tar archive is empty or could not be read.")
}

files_selected <- files_in_tar[
  grepl(
    paste0("^FLX_(", paste(stations, collapse = "|"), ")_FLUXDATAKIT_FULLSET_DD_"),
    files_in_tar
  )
]

print(files_selected)
print(length(files_selected))

# Create output directory for extracted files.
dir.create("data/FLUXDATAKIT_FLUXNET_test", recursive = TRUE, showWarnings = FALSE)

# Extract selected station files from the archive.
untar(
  "data_raw/FLUXDATAKIT_FLUXNET.tar.gz",
  files = files_selected,
  exdir = "data/FLUXDATAKIT_FLUXNET_test"
)
