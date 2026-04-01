library(purrr)

stations <- c("luz", "ber", "bas")
out_dir <- "data/test_data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

for (st in stations) {
  download.file(
    sprintf("https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/%s/ogd-smn_%s_d_historical.csv", st, st),
    file.path(out_dir, paste0(st, "_daily_historical.csv")),
    mode = "wb"
  )
  
  download.file(
    sprintf("https://data.geo.admin.ch/ch.meteoschweiz.ogd-smn/%s/ogd-smn_%s_d_recent.csv", st, st),
    file.path(out_dir, paste0(st, "_daily_recent.csv")),
    mode = "wb"
  )
}