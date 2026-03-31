library(dplyr)
library(lubridate)

calculate_gdd <- function(df, t0 = 5) {
  
  required_cols <- c("site_id", "TIMESTAMP", "TA_F_MDS")
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing columns in the data frame: ",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  df_gdd <- df %>%
    mutate(
      TIMESTAMP = ymd(as.character(TIMESTAMP)),
      year = year(TIMESTAMP),
      DOY = yday(TIMESTAMP),
      T_mean = TA_F_MDS,
      gdd_increment = if_else(!is.na(T_mean) & T_mean >= t0, T_mean - t0, 0)
    ) %>%
    arrange(site_id, year, TIMESTAMP) %>%
    group_by(site_id, year) %>%
    mutate(GDD = cumsum(gdd_increment)) %>%
    ungroup() %>%
    select(site_id, TIMESTAMP, DOY, GDD)
  
  return(df_gdd)
}