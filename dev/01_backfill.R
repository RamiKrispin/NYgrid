# Initial Data Pull ----
# Settings

parent <- "NYIS"
subba <- NYgrid:::nyis_subba_codes()
offset <- 24 * 30 * 3
api_key <- Sys.getenv("EIA_API_KEY")
api_path <- "electricity/rto/region-sub-ba-data/data"
api_meta_path <- "electricity/rto/region-sub-ba-data/"
api_meta_parent <- "electricity/rto/region-sub-ba-data/facet/parent"

meta <- EIAapi::eia_metadata(
    api_path = api_meta_path,
    api_key = api_key
)

start_period <- lubridate::ymd_h(meta$startPeriod, tz = "UTC")
end_period <- lubridate::ymd_h(meta$endPeriod, tz = "UTC") - lubridate::days(1)




ny_init <- lapply(subba, function(i) {
    d <- NULL
    print(i)
    d <- EIAapi::eia_backfill(
        api_path = api_path,
        api_key = api_key,
        offset = 2000,
        start = start_period,
        end = end_period,
        facets = list(subba = i)
    ) |>
        dplyr::select(time, subba, value)

    return(d)
}) |>
    dplyr::bind_rows()



head(ny_init)
subba
unique(ny_init$subba)
# Fixing missing values
missing_values <- length(which(ny_init$value == 0))

for (i in which(ny_init$value == 0)) {
    area <- time <- value1 <- value2 <- NULL
    area <- ny_init$subba[i]
    time <- ny_init$time[i]
    value1 <- ny_init$value[which(ny_init$subba == area & ny_init$time == time - lubridate::hours(1))]
    value2 <- ny_init$value[which(ny_init$subba == area & ny_init$time == time + lubridate::hours(1))]
    ny_init$value[i] <- (value1 + value2) / 2
}


plotly::plot_ly(ny_init,
    x = ~time,
    y = ~value,
    color = ~subba,
    type = "scatter",
    mode = "line"
)


head(ny_init)
min(ny_init$time)
max(ny_init$time)
end_period

attr(ny_init, "class") <- c("data.frame", "nygrid")
attr(ny_init, "parent") <- parent
attr(ny_init, "subba") <- subba
attr(ny_init, "start") <- start_period
attr(ny_init, "end") <- end_period

nygrid_log <- NYgrid:::nygrid_dqc(input = ny_init)

if (nygrid_log$test9[1]) {
    nygrid_log$append <- TRUE

    # Save data as csv file
    write.csv(ny_init, "./csv/ny_grid.csv", row.names = FALSE)
    # Save log
    saveRDS(nygrid_log, file = "./metadata/nygrid_log.RDS")
}