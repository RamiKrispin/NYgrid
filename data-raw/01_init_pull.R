# Parameters ----
subba <- NYgrid:::nyis_subba_codes()
parent <- "NYIS"
start <- as.POSIXct("2018-06-19 05:00:00", tz = "UTC")
end <- lubridate::floor_date(Sys.time()- lubridate::days(3), unit = "day")
attr(end, "tzone") <- "UTC"
offset <- 24 * 30 * 3
api_key <- Sys.getenv("eia_key")
api_path <- "electricity/rto/region-sub-ba-data/data/"
# Backfill ----

df <- NYgrid:::nygrid_dr(start = start,
                         end = end,
                         offset = offset,
                         parent = parent,
                         subba = NYgrid:::nyis_subba_codes(),
                         api_key = api_key,
                         api_path = api_path)

# Fixing missing values
missing_values <- length(which(df$value == 0))

for(i in which(df$value == 0)){
  area <- time <- value1 <- value2 <- NULL
  area <- df$subba[i]
  time <- df$time[i]
  value1 <- df$value[which(df$subba == area & df$time == time - lubridate::hours(1))]
  value2 <- df$value[which(df$subba == area & df$time == time + lubridate::hours(1))]
  df$value[i] <- (value1 + value2) / 2
}

NYgrid:::nygrid_dqc(input = df)

plotly::plot_ly(data = df,
                x = ~ time,
                y = ~ value,
                color = ~ subba,
                type = "scatter",
                mode = "line")

table(df$subba)



# Create a log file
nygrid_log <- NYgrid:::nygrid_dqc(input = df)

if(nygrid_log$test9[1]){
  nygrid_log$append <- TRUE

  # Save data as csv file
  write.csv(df, "./csv/ny_grid.csv", row.names = FALSE)
  #Save log
  saveRDS(nygrid_log, file = "./metadata/nygrid_log.RDS")


}



