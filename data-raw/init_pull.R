# TODO
# Pull programaticly the subba values

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
length = 5000
iterations <- 20
c <- 1
df <- NULL
end_time <- lubridate::floor_date(Sys.time()- lubridate::days(2), unit = "day")




while(c < iterations){

  temp <- NULL

tryCatch({
  temp <- EIAapi::eia_get(api_key = Sys.getenv("eia_key"),
                          api_path = "electricity/rto/region-sub-ba-data/data/",
                          facets = list(parent = parent,
                                        subba = subba_nyis[2]),
                          format = "data.frame",
                          start = start_time,
                          length = length,
                          offset = length * (c -1))},

  error = function(c) message(c),
  warning = function(c) message(c),
  message = function(c) message(c))

  if(is.null(temp)){
    stop("Could pull the data, please check the error message...")
  }


  # gsub(pattern = "-", replacement = "_",x = names(temp))
  temp <- temp |>
    dplyr::mutate(time = lubridate::ymd_h(period, tz = "UTC")) |>
    dplyr::select(-period) |>
    dplyr::select(time, dplyr::everything()) |>
    dplyr::arrange(time)



  df <- dplyr::bind_rows(df, temp)

  if (max(temp$time) < end_time){
    print(max(temp$time))
    c <- c + 1
  } else {
    c <- iterations
  }
  print(c)


}


head(temp)
unique(temp$subba)
unique(temp$parent)
unique(temp$period)
table(temp$subba)


temp |> dplyr::group_by(`subba-name`) |>
  dplyr::summarise(total = dplyr::n(),
                   start = min(time),
                   end = max(time))
