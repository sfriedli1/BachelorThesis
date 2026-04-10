library(dplyr)

# function to calculate GPP after LSF in event year
calc_event_window_gpp <- function(data, start_day, end_day, window_label) {
  data |>
    filter(
      doy > (last_frost_after_sos_doy + start_day - 1),
      doy <= (last_frost_after_sos_doy + end_day)
    ) |>
    group_by(site_id, year) |>
    summarise(
      post_lsf_window = window_label,
      mean_gpp_dt = mean(GPP_DT_VUT_REF, na.rm = TRUE),
      mean_gpp_smoothed = mean(GPP_DT_smoothed, na.rm = TRUE),
      .groups = "drop"
    )
}

# function to calculate GPP in the same time window as after LSF over all site years
calc_reference_window_gpp <- function(event_data, ref_data, start_day, end_day, window_label) {
  event_data |>
    filter(
      doy > (last_frost_after_sos_doy + start_day - 1),
      doy <= (last_frost_after_sos_doy + end_day)
    ) |>
    select(site_id, event_year = year, doy) |>
    left_join(
      ref_data |>
        select(site_id, ref_year = year, doy, GPP_DT_VUT_REF, GPP_DT_smoothed),
      by = c("site_id", "doy")
    ) |>
    filter(ref_year != event_year) |>
    group_by(site_id, event_year) |>
    summarise(
      post_lsf_window = window_label,
      ref_gpp_dt = mean(GPP_DT_VUT_REF, na.rm = TRUE),
      ref_gpp_smoothed = mean(GPP_DT_smoothed, na.rm = TRUE),
      .groups = "drop"
    )
}

# function to calculate GPP in the same time window as after LSF in the 5 years before the event year
calc_reference_window_gpp_prev5 <- function(event_data, ref_data, start_day, end_day, window_label) {
  event_data |>
    filter(
      doy > (last_frost_after_sos_doy + start_day - 1),
      doy <= (last_frost_after_sos_doy + end_day)
    ) |>
    select(site_id, event_year = year, doy) |>
    left_join(
      ref_data |>
        select(site_id, ref_year = year, doy, GPP_DT_VUT_REF, GPP_DT_smoothed),
      by = c("site_id", "doy")
    ) |>
    filter(
      ref_year < event_year,
      ref_year >= event_year - 5
    ) |>
    group_by(site_id, event_year) |>
    summarise(
      post_lsf_window = window_label,
      ref5_gpp_dt = mean(GPP_DT_VUT_REF, na.rm = TRUE),
      ref5_gpp_smoothed = mean(GPP_DT_smoothed, na.rm = TRUE),
      n_ref_years = n_distinct(ref_year),
      .groups = "drop"
    )
}
