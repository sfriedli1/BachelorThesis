library(dplyr)
library(lubridate)

library(dplyr)
library(lubridate)

library(dplyr)
library(lubridate)

calculate_lsf_after_sos <- function(df, df_sos,
                                    frost_threshold = -2,
                                    spring_doy_max = 151,
                                    temp_col = "TMIN_F_MDS") {
  
  required_cols_df <- c(
    "site_id", "TIMESTAMP", temp_col,
    "GPP_DT_VUT_REF", "GPP_DT_smoothed"
  )
  missing_cols_df <- setdiff(required_cols_df, names(df))
  
  if (length(missing_cols_df) > 0) {
    stop(
      paste(
        "Missing columns in df:",
        paste(missing_cols_df, collapse = ", ")
      )
    )
  }
  
  required_cols_sos <- c("site_id", "year", "SOS")
  missing_cols_sos <- setdiff(required_cols_sos, names(df_sos))
  
  if (length(missing_cols_sos) > 0) {
    stop(
      paste(
        "Missing columns in df_sos:",
        paste(missing_cols_sos, collapse = ", ")
      )
    )
  }
  
  # --------------------------------------------------------------------------
  # 1. Prepare daily data and join SOS
  # --------------------------------------------------------------------------
  df_prepared <- df |>
    mutate(
      TIMESTAMP = ymd(as.character(TIMESTAMP)),
      year = year(TIMESTAMP)
    ) |>
    arrange(site_id, year, TIMESTAMP) |>
    group_by(site_id, year) |>
    mutate(
      DOY = row_number()
    ) |>
    ungroup() |>
    left_join(
      df_sos |>
        select(site_id, year, SOS),
      by = c("site_id", "year")
    ) |>
    mutate(
      is_frost_day = !is.na(.data[[temp_col]]) & .data[[temp_col]] < frost_threshold,
      is_in_spring_window = DOY <= spring_doy_max,
      is_after_sos = !is.na(SOS) & DOY >= SOS,
      is_frost_in_spring = is_frost_day & is_in_spring_window,
      is_frost_after_sos = is_frost_day & is_in_spring_window & is_after_sos
    )
  
  # --------------------------------------------------------------------------
  # 2. Last spring frost in spring window (independent of SOS)
  # --------------------------------------------------------------------------
  df_last_frost_spring <- df_prepared |>
    filter(is_frost_in_spring) |>
    group_by(site_id, year) |>
    slice_max(order_by = DOY, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(
      site_id,
      year,
      last_frost_date = TIMESTAMP,
      last_frost_doy = DOY
    )
  
  # --------------------------------------------------------------------------
  # 3. Last spring frost after SOS
  # --------------------------------------------------------------------------
  df_last_frost_after_sos <- df_prepared |>
    filter(is_frost_after_sos) |>
    group_by(site_id, year) |>
    slice_max(order_by = DOY, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(
      site_id,
      year,
      last_frost_after_sos_date = TIMESTAMP,
      last_frost_after_sos_doy = DOY
    )
  
  # --------------------------------------------------------------------------
  # 4. Daily output with selected columns
  # --------------------------------------------------------------------------
  df_daily <- df_prepared |>
    transmute(
      site_id,
      TMIN_F_MDS = .data[[temp_col]],
      GPP_DT_VUT_REF,
      date = TIMESTAMP,
      year,
      doy = DOY,
      GPP_DT_smoothed
    ) |>
    filter(doy <= spring_doy_max)
  
  # --------------------------------------------------------------------------
  # 5. Yearly summary
  # --------------------------------------------------------------------------
  df_yearly_summary <- df_prepared |>
    group_by(site_id, year) |>
    summarise(
      SOS = first(SOS),
      n_frost_days_spring = sum(is_frost_in_spring, na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(df_last_frost_spring, by = c("site_id", "year")) |>
    left_join(df_last_frost_after_sos, by = c("site_id", "year")) |>
    left_join(
      df_prepared |>
        group_by(site_id, year) |>
        summarise(
          n_frost_days_until_LSF = if (!all(is.na(is_frost_in_spring)) && any(is_frost_in_spring, na.rm = TRUE)) {
            lsf_doy <- max(DOY[is_frost_in_spring], na.rm = TRUE)
            sum(is_frost_day & DOY <= lsf_doy, na.rm = TRUE)
          } else {
            NA_real_
          },
          n_frost_days_after_SOS_until_LSF = if (any(is_frost_after_sos, na.rm = TRUE)) {
            lsf_after_sos_doy <- max(DOY[is_frost_after_sos], na.rm = TRUE)
            sum(is_frost_day & DOY >= first(SOS) & DOY <= lsf_after_sos_doy, na.rm = TRUE)
          } else {
            NA_real_
          },
          .groups = "drop"
        ),
      by = c("site_id", "year")
    )
  
  # --------------------------------------------------------------------------
  # 6. Return outputs
  # --------------------------------------------------------------------------
  return(list(
    daily = df_daily,
    yearly_summary = df_yearly_summary
  ))
}