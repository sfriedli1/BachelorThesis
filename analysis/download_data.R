library(readr)
library(dplyr)
library(stringr)
library(lubridate)

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
# Downloading data for the following stations: BE-Vie, DE-Hai, DE-Tha, US-Ha1, CH-Lae
# ------------------------------------------------------------------------------
stations <- c(
  "BE-Vie",
  "DE-Hai",
  "DE-Tha",
  "US-Ha1",
  "CH-Lae"
)

tar_file <- "data_raw/FLUXDATAKIT_FLUXNET.tar.gz"

files_in_tar <- tryCatch(
  untar(tar_file, list = TRUE),
  error = function(e) {
    stop(paste("Could not list contents of tar archive:", e$message))
  }
)

# Select all the files from the stations
files_selected <- files_in_tar[
  grepl(
    paste0("^FLX_(", paste(stations, collapse = "|"), ")_FLUXDATAKIT_FULLSET_DD_"),
    files_in_tar
  )
]

# Create output directory for extracted files.
dir.create("data/test_data", recursive = TRUE, showWarnings = FALSE)

# Extract selected station files from the archive.
untar(
  "data_raw/FLUXDATAKIT_FLUXNET.tar.gz",
  files = files_selected,
  exdir = "data/test_data"
)

# ------------------------------------------------------------------------------
# Downloading the complete FLUXDATAKIT dataset
# ------------------------------------------------------------------------------
tar_file <- "data_raw/FLUXDATAKIT_FLUXNET.tar.gz"

files_in_tar <- tryCatch(
  untar(tar_file, list = TRUE),
  error = function(e) {
    stop(paste("Could not list contents of tar archive:", e$message))
  }
)

# create output directory for extracted files
dir.create("data_raw/all_fdk_sites", recursive = TRUE, showWarnings = FALSE)

# extract all files in tar_file
untar(tar_file, 
      exdir = "data_raw/all_fdk_sites")