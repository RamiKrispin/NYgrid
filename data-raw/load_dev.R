x <- readr::read_csv("csv/ny_grid.csv",
                     col_names = TRUE,
                     readr::cols(
                       time = readr::col_character(),
                       subba = readr::col_character(),
                       value = readr::col_double()
                     )) |>
  dplyr::mutate(time = as.POSIXlt(time, tz = "UTC"))

readr::spec(x)
head(ny_grid)
head(x)
lubridate::tz(x$time)
lubridate::tz(ny_grid$time)
