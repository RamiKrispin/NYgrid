# Parameters ----
subba <- NYgrid:::nyis_subba_codes()
parent <- "NYIS"
offset <- 24 * 30 * 3
api_key <- Sys.getenv("EIA_API_KEY")
api_path <- "electricity/rto/region-sub-ba-data/data/"
# Create metadata ----

ny_grid_meta <- lapply(subba, function(i){
  d <- EIAapi::eia_backfill(start = lubridate::floor_date(Sys.time()- lubridate::days(4), unit = "day"),
                            end = lubridate::floor_date(Sys.time()- lubridate::days(3), unit = "day"),
                            offset = offset,
                            api_key = api_key,
                            api_path = api_path,
                            facets = list(parent = parent,
                                          subba = i))
  return(d)
}) |>
  dplyr::bind_rows() |>
  dplyr::select(subba, subba_name, parent, parent_name, value_units) |>
  dplyr::distinct()

ny_grid_meta
usethis::use_data(ny_grid_meta, overwrite = TRUE)
