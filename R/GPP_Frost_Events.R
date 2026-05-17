library(dplyr)
library(lubridate)
library(rgeco)
library(purrr)
library(tidyr)

find_frost_gpp <- function(
    df,
    partition = "DT",
    frost_threshold = -2,
    merge_threshold = 0,
    leng_threshold = 1
) {
  
  partition <- match.arg(partition, c("DT", "NT"))
  
  gpp_col <- paste0("GPP_", partition, "_trailing_4d_pos")
  threshold_col <- paste0("GPP_", partition, "_SOS_threshold")
  
  required_cols <- c("site_id", "TIMESTAMP", "TMIN_F_MDS", gpp_col, threshold_col)
  missing_cols <- setdiff(required_cols, names(df))
  
  if (length(missing_cols) > 0) {
    stop(paste("Missing columns:", paste(missing_cols, collapse = ", ")))
  }
  
  df_out <- df %>%
    mutate(
      TIMESTAMP = lubridate::ymd(as.character(TIMESTAMP)),
      year = lubridate::year(TIMESTAMP),
      DOY = lubridate::yday(TIMESTAMP),
      is_frost = TMIN_F_MDS < frost_threshold,
      GPP_above_threshold = .data[[gpp_col]] > .data[[threshold_col]]
    ) %>%
    arrange(site_id, year, TIMESTAMP)
  
  events_all <- df_out %>%
    filter(DOY <= 151) %>%
    group_by(site_id, year) %>%
    group_modify(~{
      
      dat <- .x %>%
        mutate(is_frost = if_else(is.na(is_frost), FALSE, is_frost))
      
      key <- .y
      
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
          
          GPP_start = dat[[gpp_col]][idx_start],
          GPP_threshold = dat[[threshold_col]][idx_start],
          GPP_above_start = GPP_start > GPP_threshold,
          
          GPP_above_event = purrr::map2_lgl(
            idx_start, idx_end,
            ~ any(dat$GPP_above_threshold[.x:.y], na.rm = TRUE)
          ),
          
          Tmin_abs = purrr::map2_dbl(
            idx_start, idx_end,
            ~ min(dat$TMIN_F_MDS[.x:.y], na.rm = TRUE)
          ),
          
          summed_negative_temp = purrr::map2_dbl(
            idx_start, idx_end,
            ~ sum(pmin(dat$TMIN_F_MDS[.x:.y], 0), na.rm = TRUE)
          )
        ) %>%
        filter(GPP_above_start) %>%
        mutate(
          partition = partition,
          event_number = row_number(),
          event_id = paste(key$site_id, key$year, partition, event_number, sep = "_")
        )
    }) %>%
    ungroup()
  
  if (nrow(events_all) > 0) {
    
    event_days <- events_all %>%
      rowwise() %>%
      mutate(
        TIMESTAMP = list(seq.Date(start_date, end_date, by = "day")),
        dday = list(seq_along(TIMESTAMP) - 1)
      ) %>%
      unnest(c(TIMESTAMP, dday)) %>%
      ungroup() %>%
      select(site_id, year, TIMESTAMP, event_id, dday, partition)
    
    df_out <- df_out %>%
      left_join(event_days, by = c("site_id", "year", "TIMESTAMP"))
    
  } else {
    
    df_out <- df_out %>%
      mutate(
        event_id = NA_character_,
        dday = NA_integer_,
        partition = partition
      )
  }
  
  return(
    list(
      df = df_out,
      events = events_all
    )
  )
}
