library(dplyr)

# function to calculate threshold 
calc_site_thresholds <- function(data, gpp_col, frac) {
  data |>
    group_by(site_id) |>
    summarise(
      gpp_max_site = max(.data[[gpp_col]], na.rm = TRUE),
      threshold = frac * gpp_max_site,
      .groups = "drop"
    ) |>
    mutate(threshold_frac = frac)
}

# function to calculate SOS
identify_sos <- function(data, thresholds, gpp_col, doy_col = "doy") {
  data |>
    left_join(
      thresholds |> select(site_id, threshold, threshold_frac),
      by = "site_id"
    ) |>
    group_by(site_id, year) |>
    summarise(
      SOS = if (any(.data[[gpp_col]] >= first(threshold), na.rm = TRUE)) {
        min(.data[[doy_col]][.data[[gpp_col]] >= first(threshold)], na.rm = TRUE)
      } else {
        NA_real_
      },
      threshold = first(threshold),
      threshold_frac = first(threshold_frac),
      .groups = "drop"
    )
}