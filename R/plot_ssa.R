library(dplyr)
library(ggplot2)

# plot all combinations of parameters
plot_ssa_initial_parameters <- function(df_ssa_compare, site, spring_only = FALSE) {
  
  df_plot <- df_ssa_compare |>
    filter(site_id == site)
  
  if (spring_only) {
    df_plot <- df_plot |> filter(doy <= 151)
  }
  
  ggplot(df_plot, aes(x = doy)) +
    geom_line(aes(y = gpp_smooth), color = "#0072B2", linewidth = 0.8) +
    facet_grid(comp_end ~ L, labeller = label_both) +
    labs(
      x = "Day of year",
      y = "GPP",
      title = paste("SSA smoothing comparison -", site)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 9),
      plot.title = element_text(face = "bold")
    )
}

plot_ssa_select_parameters <- function(df_ssa_compare, site, spring_only = FALSE) {
  
  df_plot <- df_ssa_compare |>
    filter(site_id == site)
  
  if (spring_only) {
    df_plot <- df_plot |> filter(doy <= 151)
  }
  
  yr_label <- unique(df_plot$year)
  
  ggplot(df_plot, aes(x = doy)) +
    geom_line(aes(y = gpp_smooth), color = "#0072B2", linewidth = 0.9) +
    facet_wrap(~setting, ncol = 2) +
    labs(
      x = "Day of year",
      y = "GPP",
      title = paste("SSA smoothing comparison -", site, "-", yr_label)
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      strip.text = element_text(size = 10),
      plot.title = element_text(face = "bold")
    )
}