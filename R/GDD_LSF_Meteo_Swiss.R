library(dplyr)
library(lubridate)

calculate_gdd_lsf_meteoswiss <- function(df, t0 = 5, frost_threshold = -2, spring_doy_max = 151) {
  
  required_cols <- c(
    "station_abbr",
    "reference_timestamp",
    "tre200d0",
    "tre200dn",
    "tre200dx"
  )
  
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing columns in the data frame:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  # --------------------------------------------------------------------------
  # 1. Prepare data
  # --------------------------------------------------------------------------
  df_prepared <- df %>%
    mutate(
      reference_timestamp = as.Date(reference_timestamp),
      year = year(reference_timestamp),
      month = month(reference_timestamp),
      day = day(reference_timestamp)
    ) %>%
    filter(!(month == 2 & day == 29)) %>%
    arrange(station_abbr, year, reference_timestamp) %>%
    group_by(station_abbr, year) %>%
    mutate(
      DOY = row_number(),
      T_mean = tre200d0,
      T_min = tre200dn,
      T_max = tre200dx,
      gdd_increment = if_else(!is.na(T_mean) & T_mean >= t0, T_mean - t0, 0),
      GDD = cumsum(gdd_increment)
    ) %>%
    ungroup()
  
  # --------------------------------------------------------------------------
  # 2. Limit to spring time and mark frost days
  # --------------------------------------------------------------------------
  df_spring <- df_prepared %>%
    filter(DOY <= spring_doy_max) %>%
    mutate(
      is_frost_day = !is.na(T_min) & T_min < frost_threshold
    )
  
  # --------------------------------------------------------------------------
  # 3. Identify last spring frost event per station and year
  # --------------------------------------------------------------------------
  df_last_frost <- df_spring %>%
    filter(is_frost_day) %>%
    group_by(station_abbr, year) %>%
    slice_max(order_by = DOY, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(
      station_abbr,
      year,
      last_frost_date = reference_timestamp,
      last_frost_doy = DOY,
      GDD_at_LSF = GDD
    )
  
  # --------------------------------------------------------------------------
  # 4. Daily spring series marking last spring frost
  # --------------------------------------------------------------------------
  df_spring_daily <- df_spring %>%
    left_join(
      df_last_frost %>% select(station_abbr, year, last_frost_date),
      by = c("station_abbr", "year")
    ) %>%
    mutate(
      is_last_spring_frost = !is.na(last_frost_date) & reference_timestamp == last_frost_date
    ) %>%
    select(
      station_abbr,
      year,
      reference_timestamp,
      DOY,
      T_mean,
      T_min,
      T_max,
      GDD,
      is_frost_day,
      is_last_spring_frost
    )
  
  # --------------------------------------------------------------------------
  # 5. Yearly summary table
  # --------------------------------------------------------------------------
  df_yearly_summary <- df_spring_daily %>%
    group_by(station_abbr, year) %>%
    summarise(
      last_frost_date = if (any(is_last_spring_frost, na.rm = TRUE)) {
        reference_timestamp[is_last_spring_frost][1]
      } else {
        as.Date(NA)
      },
      last_frost_doy = if (any(is_last_spring_frost, na.rm = TRUE)) {
        DOY[is_last_spring_frost][1]
      } else {
        NA_real_
      },
      GDD_at_LSF = if (any(is_last_spring_frost, na.rm = TRUE)) {
        GDD[is_last_spring_frost][1]
      } else {
        NA_real_
      },
      n_frost_days = sum(is_frost_day, na.rm = TRUE),
      .groups = "drop"
    )
  
  # --------------------------------------------------------------------------
  # 6. Return outputs
  # --------------------------------------------------------------------------
  return(list(
    spring_daily = df_spring_daily,
    yearly_summary = df_yearly_summary
  ))
}