# Data Refresh ----
# Settings

parent <- "NYIS"
subba <- NYgrid:::nyis_subba_codes()
offset <- 24 * 30 * 3
api_key <- Sys.getenv("EIA_API_KEY")
api_path <- "electricity/rto/region-sub-ba-data/data"
api_meta_path <- "electricity/rto/region-sub-ba-data/"

# Read Metadata ----
meta <- EIAapi::eia_metadata(
    api_path = api_meta_path,
    api_key = api_key
)

start_period <- lubridate::ymd_h(meta$startPeriod, tz = "UTC")
end_period <- lubridate::ymd_h(meta$endPeriod, tz = "UTC")


log <- readRDS(file = "./metadata/nygrid_log.RDS")

log_last <- log |> dplyr::filter(append)
log_last <- log_last[which.max(log_last$index), ]

total_obs <- sum((log |>
    dplyr::filter(append))$num_obs)


if (log_last$end < end_period && log_last$end_act < end_period) {
    start <- log_last$end_act + lubridate::hours(1)
    end <- end_period

    if (start < end) {
        data_refresh <- lapply(subba, function(i) {
            d <- NULL
            print(i)
            d <- EIAapi::eia_backfill(
                api_path = api_path,
                api_key = api_key,
                offset = 2000,
                start = start,
                end = end,
                facets = list(subba = i)
            ) |>
                dplyr::select(time, subba, value)

            return(d)
        }) |>
            dplyr::bind_rows()

        attr(data_refresh, "class") <- c("data.frame", "nygrid")
        attr(data_refresh, "parent") <- parent
        attr(data_refresh, "subba") <- subba
        attr(data_refresh, "start") <- start
        attr(data_refresh, "end") <- end


        log_data_refresh <- NYgrid:::nygrid_dqc(input = data_refresh)
        # Check the data quaility tests
        if ((log_data_refresh$test1 & log_data_refresh$test2 &
            !log_data_refresh$test3 & !log_data_refresh$test4 &
            log_data_refresh$test5 & log_data_refresh$test6 &
            log_data_refresh$test7) &&
            (log_data_refresh$end_act > log_data_refresh$start_act)) {
            index <- max(log$index) + 1
            log_data_refresh$index[1] <- index
            # Loading the data
            main_df <- readr::read_csv("csv/ny_grid.csv",
                col_names = TRUE,
                readr::cols(
                    time = readr::col_character(),
                    subba = readr::col_character(),
                    value = readr::col_double()
                )
            ) |>
                dplyr::mutate(
                    time_temp = ifelse(nchar(time) == 10, paste(time, "00:00:00", sep = " "), time),
                    time = as.POSIXct(time_temp, format = "%Y-%m-%d %H:%M:%S", usetz = TRUE, tz = "UTC")
                ) |>
                dplyr::select(-time_temp)
            # Check if the number of observations is matching
            if (nrow(main_df) != total_obs) {
                append <- FALSE
                print("Append failure, the rows number of the loaded CSV files does not match the one on the log file")
                # Check the timestamp
            } else if (log_data_refresh$start_act[1] != max(main_df$time) + lubridate::hours(1)) {
                append <- FALSE
                print("Cannot append the data, there is missmatch between new data starting time and the end time of the main dataset")
            } else {
                append <- TRUE
            }

            if (append) {
                df <- rbind(main_df, data_refresh)
                log_data_refresh$append[1] <- append

                new_log <- rbind(log, log_data_refresh)
                print("Saving the data")
                write.csv(df, "./csv/ny_grid.csv", row.names = FALSE)
                print("Saving the log")
                saveRDS(new_log, file = "./metadata/nygrid_log.RDS")
            } else if (!append) {
                new_log <- rbind(log, log_data_refresh)
                log_data_refresh$append[1] <- append
                new_log <- rbind(log, log_data_refresh)
                saveRDS(new_log, file = "./metadata/nygrid_log.RDS")
            }
        }
    }
} else {
    print("No new data is currently available.")
}