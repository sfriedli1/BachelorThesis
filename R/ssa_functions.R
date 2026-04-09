library(dplyr)
library(Rssa)
library(tidyr)
library(purrr)

# smooth GPP for series
ssa_smooth_series <- function(x, L, comps) {
  s <- ssa(x, L = L)
  r <- reconstruct(s, groups = list(signal = comps))
  as.numeric(r$signal)
}

# smooth DT GPP for all years and each site
run_ssa_all <- function(data, L, comps, doy_col = "doy") {
  data |>
    arrange(site_id, year, .data[[doy_col]]) |>
    group_by(site_id) |>
    group_modify(~{
      .x |>
        mutate(
          GPP_DT_smoothed = ssa_smooth_series(
            x = GPP_DT_VUT_REF,
            L = L,
            comps = comps
          )
        )
    }) |>
    ungroup()
}

# function for comparison of selection of parameters across multiple years
compare_ssa_settings <- function(data, sites, yr, var, doy_col = "doy") {
  data |>
    filter(site_id %in% sites) |>
    arrange(site_id, year, .data[[doy_col]]) |>
    group_by(site_id) |>
    group_modify(~{
      x <- .x |> pull({{ var }})
      
      .x |>
        mutate(
          gpp_raw = {{ var }},
          L60_C2  = ssa_smooth_series(x, L = 60,  comps = 1:2),
          L90_C2  = ssa_smooth_series(x, L = 90,  comps = 1:2),
          L90_C3  = ssa_smooth_series(x, L = 90,  comps = 1:3),
          L120_C3 = ssa_smooth_series(x, L = 120, comps = 1:3)
        ) |>
        filter(year == yr) |>
        transmute(
          doy = .data[[doy_col]],
          gpp_raw,
          L60_C2,
          L90_C2,
          L90_C3,
          L120_C3
        )
    }) |>
    ungroup() |>
    pivot_longer(
      cols = c(L60_C2, L90_C2, L90_C3, L120_C3),
      names_to = "setting",
      values_to = "gpp_smooth"
    )
}

# function for initial comparison of multiple settings of parameters
compare_ssa_grid <- function(data, sites, yr, var, doy_col,
                             L_values = c(30, 45, 60, 90, 120),
                             comp_max = 5) {
  
  # Grid for all combinations
  settings_grid <- expand.grid(
    L = L_values,
    comp_end = 1:comp_max
  ) |>
    as_tibble() |>
    mutate(
      setting = paste0("L", L, "_C", comp_end)
    )
  
  data |>
    filter(site_id %in% sites, year == yr) |>
    arrange(site_id, .data[[doy_col]]) |>
    group_by(site_id, year) |>
    group_modify(~{
      
      df_site <- .x
      
      out <- df_site |>
        transmute(
          doy = .data[[doy_col]],
          gpp_raw = {{ var }}
        )
      
      # calculate smoothed series for all combinations
      for (i in seq_len(nrow(settings_grid))) {
        L_val <- settings_grid$L[i]
        comp_end <- settings_grid$comp_end[i]
        setting_name <- settings_grid$setting[i]
        
        out[[setting_name]] <- ssa_smooth_series(
          x = df_site |> pull({{ var }}),
          L = L_val,
          comps = 1:comp_end
        )
      }
      
      out
    }) |>
    ungroup() |>
    pivot_longer(
      cols = starts_with("L"),
      names_to = "setting",
      values_to = "gpp_smooth"
    ) |>
    mutate(
      L = as.integer(sub("L([0-9]+)_C([0-9]+)", "\\1", setting)),
      comp_end = as.integer(sub("L([0-9]+)_C([0-9]+)", "\\2", setting))
    )
}
