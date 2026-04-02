library(dplyr)
library(lubridate)

calculate_gdd_lsf <- function(df, t0 = 5, frost_threshold = -2, spring_doy_max = 151) {
  
  required_cols <- c("site_id", "TIMESTAMP", "TA_F_MDS", "TMIN_F_MDS")
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
      TIMESTAMP = ymd(as.character(TIMESTAMP)),
      year = year(TIMESTAMP),
      month = month(TIMESTAMP),
      day = day(TIMESTAMP)
    ) %>%
    # remove 29th of february
    filter(!(month == 2 & day == 29)) %>%
    arrange(site_id, year, TIMESTAMP) %>%
    group_by(site_id, year) %>%
    # renumber the DOY after removing 29th of february
    mutate(
      DOY = row_number(),
      T_mean = TA_F_MDS,
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
      is_frost_day = !is.na(TMIN_F_MDS) & TMIN_F_MDS < frost_threshold
    )
  
  # --------------------------------------------------------------------------
  # 3. Identify last spring frost event per site
  # --------------------------------------------------------------------------
  df_last_frost <- df_spring %>%
    filter(is_frost_day) %>%
    group_by(site_id, year) %>%
    slice_max(order_by = DOY, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(
      site_id,
      year,
      last_frost_date = TIMESTAMP,
      last_frost_doy = DOY,
      GDD_at_LSF = GDD
    )
  
  # --------------------------------------------------------------------------
  # 4. Daily spring series marking last spring frost
  # --------------------------------------------------------------------------
  df_spring_daily <- df_spring %>%
    left_join(
      df_last_frost %>% select(site_id, year, last_frost_date),
      by = c("site_id", "year")
    ) %>%
    mutate(
      is_last_spring_frost = !is.na(last_frost_date) & TIMESTAMP == last_frost_date
    ) %>%
    select(
      site_id,
      year,
      TIMESTAMP,
      DOY,
      GDD,
      TMIN_F_MDS,
      is_frost_day,
      is_last_spring_frost
    )
  
  # --------------------------------------------------------------------------
  # 5. Yearly summary table
  # --------------------------------------------------------------------------
  df_yearly_summary <- df_spring_daily %>%
    group_by(site_id, year) %>%
    summarise(
      last_frost_date = if (any(is_last_spring_frost, na.rm = TRUE)) {
        TIMESTAMP[is_last_spring_frost][1]
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