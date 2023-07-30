# Metadata ----
# Extracting the subba unique values

subba_nyis <- c("ZONA",
                "ZONB",
                "ZONC",
                "ZOND",
                "ZONE",
                "ZONF",
                "ZONG",
                "ZONH",
                "ZONI",
                "ZONJ",
                "ZONK")


parent <- "NYIS"
start_time <- "2018-06-19T00"
start <- as.POSIXlt("2018-06-19T00", tz = "UTC")
end <- lubridate::floor_date(Sys.time()- lubridate::days(2), unit = "day")
attr(end, "tzone") <- "UTC"
offset <- 24 * 30 * 3
api_key <- Sys.getenv("eia_key")
api_path <- "electricity/rto/region-sub-ba-data/data/"


df <- lapply(subba_nyis, function(i){

  print(i)
  d <- EIAapi::eia_backfill(start = start,
                       end = end,
                       offset = offset,
                       api_key = api_key,
                       api_path = api_path,
                       facets = list(parent = "NYIS",
                                     subba = i))

  return(d)
}) |>
  dplyr::bind_rows()


plotly::plot_ly(data = df,
                x = ~ time,
                y = ~ value,
                color = ~ subba,
                type = "scatter",
                mode = "line")

table(df$subba)

# Fixing missing values

for(i in which(df$value == 0)){
  area <- time <- value1 <- value2 <- NULL
  area <- df$subba[i]
  time <- df$time[i]
  value1 <- df$value[which(df$subba == area & df$time == time - lubridate::hours(1))]
  value2 <- df$value[which(df$subba == area & df$time == time + lubridate::hours(1))]
  df$value[i] <- (value1 + value2) / 2
}

df |> dplyr::filter(subba == "ZONA") |>
  plotly::plot_ly(x = ~ time,
                  y = ~ value,
                  type = "scatter",
                  mode = "line")
ny_grid_meta <- df |>
  dplyr::select(subba, subba_name, parent, parent_name, value_units) |>
  dplyr::distinct()

usethis::use_data(ny_grid_meta)

ny_grid <- df |> dplyr::select(time, subba, value)

write.csv(ny_grid, "./csv/ny_grid.csv", row.names = FALSE)

ny_grid |> dplyr::group_by(subba) |>
  dplyr::summarise(time_max = max(time))
