library(dplyr)
library(Rssa)

# smooth GPP for one year
ssa_smooth_year <- function(x, L = 60, comps = 1:2) {
  s <- ssa(x, L = L)
  r <- reconstruct(s, groups = list(signal = comps))
  as.numeric(r$signal)
}

# smooth DT and NT GPP for all years and each site
run_ssa_all <- function(data, L = 60, comps = 1:2, doy_col = "doy") {
  data |>
    arrange(site_id, year, .data[[doy_col]]) |>
    group_by(site_id, year) |>
    group_modify(~{
      .x |>
        mutate(
          GPP_DT_SSA = ssa_smooth_year(
            x = GPP_DT_VUT_REF,
            L = L,
            comps = comps
          ),
          GPP_NT_SSA = ssa_smooth_year(
            x = GPP_NT_VUT_REF,
            L = L,
            comps = comps
          )
        )
    }) |>
    ungroup()
}

# compare multiple settings of parameters
compare_ssa_settings <- function(data, sites, yr, var, doy_col) {
  data |>
    filter(site_id %in% sites, year == yr) |>
    arrange(site_id, .data[[doy_col]]) |>
    group_by(site_id, year) |>
    group_modify(~{
      .x |>
        transmute(
          doy = .data[[doy_col]],
          gpp_raw = {{ var }},
          L45_C2 = ssa_smooth_year({{ var }}, L = 45, comps = 1:2),
          L60_C2 = ssa_smooth_year({{ var }}, L = 60, comps = 1:2),
          L90_C2 = ssa_smooth_year({{ var }}, L = 90, comps = 1:2),
          L60_C4 = ssa_smooth_year({{ var }}, L = 60, comps = 1:4)
        )
    }) |>
    ungroup() |>
    pivot_longer(
      cols = c(L45_C2, L60_C2, L90_C2, L60_C4),
      names_to = "setting",
      values_to = "gpp_smooth"
    )
}