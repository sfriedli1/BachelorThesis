library(dplyr)
library(lubridate)
library(rgeco)
library(purrr)
library(tidyr)

source("../R/GDD.R")

find_frost_gdd <- function(
    df,
    frost_threshold = -2,
    gdd_base = 5,
    gdd_threshold = 50,
    merge_threshold = 0,
    leng_threshold = 1
) {
  
  required_cols <- c("site_id", "TIMESTAMP", "TMIN_F_MDS", "TA_F_MDS")
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(
      paste(
        "Missing columns in the data frame:",
        paste(missing_cols, collapse = ", ")
      )
    )
  }
  
  # Add GDD calculated by calculate_gdd()
  df_out <- df %>%
    mutate(
      TIMESTAMP = ymd(as.character(TIMESTAMP))
    ) %>%
    left_join(
      calculate_gdd(df, t0 = gdd_base),
      by = c("site_id", "TIMESTAMP")
    ) %>%
    mutate(
      year = lubridate::year(TIMESTAMP),
      is_frost = TMIN_F_MDS < frost_threshold
    ) %>%
    arrange(site_id, year, TIMESTAMP)
  
  # Detect frost events separately for each site and year
  events_all <- df_out %>%
    filter(DOY <= 151)%>%
    group_by(site_id, year) %>%
    group_modify(~{
      
      dat <- .x %>%
        mutate(
          is_frost = if_else(is.na(is_frost), FALSE, is_frost))
      key <- .y
      
      # Skip site-year combinations without frost days
      if (!any(dat$is_frost, na.rm = TRUE)) {
        return(tibble())
      }
      
      events <- rgeco::get_consecutive(
        dat$is_frost,
        merge_threshold = merge_threshold,
        leng_threshold = leng_threshold,
        do_merge = merge_threshold > 0
      )
      
      if (nrow(events) == 0) {
        return(tibble())
      }
      
      events %>%
        mutate(
          idx_end = idx_start + len - 1,
          start_date = dat$TIMESTAMP[idx_start],
          end_date = dat$TIMESTAMP[idx_end],
          gdd_start = dat$GDD[idx_start],
          gdd_end = dat$GDD[idx_end],
          
          Tmin_abs = map2_dbl(
            idx_start, idx_end,
            ~ min(dat$TMIN_F_MDS[.x:.y], na.rm = TRUE)
          ),
          
          summed_negative_temp = map2_dbl(
            idx_start, idx_end,
            ~ sum(pmin(dat$TMIN_F_MDS[.x:.y], 0), na.rm = TRUE)
          )
        ) %>%
        filter(gdd_start >= gdd_threshold) %>%
        mutate(
          event_number = row_number(),
          event_id = paste(key$site_id, key$year, event_number, sep = "_")
        )
    }) %>%
    ungroup()
  
  # Add event_id and day into event back to full daily data
  if (nrow(events_all) > 0) {
    
    event_days <- events_all %>%
      rowwise() %>%
      mutate(
        TIMESTAMP = list(seq.Date(start_date, end_date, by = "day")),
        dday = list(seq_along(TIMESTAMP) - 1)
      ) %>%
      unnest(c(TIMESTAMP, dday)) %>%
      ungroup() %>%
      select(site_id, year, TIMESTAMP, event_id, dday)
    
    df_out <- df_out %>%
      left_join(
        event_days,
        by = c("site_id", "year", "TIMESTAMP")
      )
    
  } else {
    
    df_out <- df_out %>%
      mutate(
        event_id = NA_character_,
        dday = NA_integer_
      )
  }
  
  return(
    list(
      df = df_out,
      events = events_all
    )
  )
}