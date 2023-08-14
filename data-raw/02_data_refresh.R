# Parameters ----
subba <- NYgrid:::nyis_subba_codes()
parent <- "NYIS"
offset <- 24 * 30 * 3
api_key <- Sys.getenv("eia_key")
api_path <- "electricity/rto/region-sub-ba-data/data/"
# Load the data and metadata ----
main_df <- readr::read_csv("csv/ny_grid.csv",
                           col_names = TRUE,
                           readr::cols(
                             time = readr::col_character(),
                             subba = readr::col_character(),
                             value = readr::col_double()
                           )) |>
  dplyr::mutate(time = as.POSIXlt(time, tz = "UTC"))


# Loading metadata
nygrid_log <- readRDS(file = "./metadata/nygrid_log.RDS")
r1 <- which.max(nygrid_log$time)
r2 <- which.max(nygrid_log$end)


if(r1 != r2){
  stop("Cannot identify the most recent refresh")
} else {

  start <- nygrid_log$end[r2] + lubridate::hours(1)
  end <- lubridate::floor_date(Sys.time() - lubridate::hours(26), unit = "hour")

  attr(end, "tzone") <- "UTC"
  # Validite end > start
  if(start < end){

    df <- NYgrid:::nygrid_dr(start = start,
                             end = end,
                             offset = offset,
                             parent = parent,
                             subba = subba,
                             api_key = api_key,
                             api_path = api_path)

    nygrid_log_new <- NYgrid:::nygrid_dqc(input = df)


    if(nygrid_log_new$test9){
      if(max(main_df$time) + lubridate::hours(1) == start &&
         start == min(df$time)){

        appended_df <- rbind(main_df, df)

        nygrid_log_new$append[1] <- TRUE

        # Save data as csv file
        write.csv(appended_df, "./csv/ny_grid.csv", row.names = FALSE)

      }
    }

    log_new <- rbind(nygrid_log, nygrid_log_new)

    #Save log
    saveRDS(log_new, file = "./metadata/nygrid_log.RDS")
  }
}


