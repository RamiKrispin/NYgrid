#' Backfile function
#' @description The function enables to pull historical data from the EIA API
#' by time range

start <- as.POSIXlt("2018-06-19T00", tz = "UTC")
end <- lubridate::floor_date(Sys.time()- lubridate::days(2), unit = "day")
attr(end, "tzone") <- "UTC"
offset <- 24 * 30 * 3
api_key <- Sys.getenv("eia_key")
api_path <- "electricity/rto/region-sub-ba-data/data/"

facets = list(parent = parent,
              subba = subba_nyis[3])

backfill <- function(start,
                     end,
                     offset,
                     api_key,
                     api_path,
                     facets){

  # Error handling

  # Validating the start and end arguments
  if(!any(lubridate::is.POSIXt(start),
          lubridate::is.Date(start))){
    stop("The start argument is not valid, the function suports POSIXlt, POSIXct, Date objects")
  } else if(!any(lubridate::is.POSIXt(end),
                  lubridate::is.Date(end))){
    stop("The end argument is not valid, the function suports POSIXlt, POSIXct, Date objects")
  } else if(!any(class(start) %in% class(end))){
    stop("The class of the start argument is different from the end argument class")
    # Validate the offset argument
  } else if(!is.numeric(offset) || offset < 0 || offset %% 1 != 0){
    stop("The offset argument is not valid, must be numeric")
    # Validate the api_key and api_path arguments
  } else if(!is.character(api_key)){
    stop("The api_key argument is not valid")
  } else if(!is.character(api_path)){
    stop("The api_path argument is not valid")
    # Validate the facets argument
  } else if(!is.list(facets)){
    stop("The facets argument must be a list object")
  }


# Classify the time stamp to either POSIXt or Date object
  if(lubridate::is.POSIXt(start)){
    time_class <- "POSIXt"
  } else if(lubridate::is.Date(start)){
    time_class <- "Date"
  } else {
    stop("Mismatch with start/end arguments class")
  }

  # Create a time/date vector

  if(time_class == "POSIXt"){
    time_vec <- seq.POSIXt(from = start, to = end, by = paste(offset, "hour"))
    if(max(time_vec) < end){
      time_vec <- c(time_vec, end)
    }
  } else if(time_class == "Date"){
  time_vec <- seq.Date(from = start, to = end, by = "day")
  }

  df <- lapply(seq_along(time_vec)[-length(time_vec)], function(i){
    print(i)
    temp <- start_h <- end_h <- start_time <- end_time <- NULL
    s <- time_vec[i]
    if(time_class == "POSIXt"){
      e <- time_vec[i + 1] - lubridate::hours(1)
      start_h <- lubridate::hour(s)
      end_h <- lubridate::hour(e)
      if(start_h < 10){
        start_time <- paste(substr(as.character(s), 1, 10), "T0", start_h, sep = "")
      } else {
        start_time <- paste(substr(as.character(start), 1, 10), "T", start_h, sep = "")
      }


      if(end_h < 10){
        end_time <- paste(substr(as.character(e), 1, 10), "T0", end_h, sep = "")
      } else {
        end_time <- paste(substr(as.character(e), 1, 10), "T", end_h, sep = "")
      }

      temp <- EIAapi::eia_get(api_key = api_key,
                              api_path = api_path,
                              facets = facets,
                              format = "data.frame",
                              start = start_time,
                              end = end_time,
                              length = NULL,
                              offset = NULL) |>
        dplyr::mutate(time = lubridate::ymd_h(period, tz = "UTC")) |>
        dplyr::select(-period) |>
        dplyr::select(time, dplyr::everything()) |>
        dplyr::arrange(time)



    } else if(time_class == "Date"){
      e <- time_vec[i + 1] - lubridate::days(1)
      # start_h <- lubridate::hour(s)
      # end_h <- lubridate::hour(e)
      # if(start_h < 10){
      #   start_time <- paste(substr(as.character(s), 1, 10), "T0", start_h, sep = "")
      # } else {
      #   start_time <- paste(substr(as.character(start), 1, 10), "T", start_h, sep = "")
      # }


      # if(end_h < 10){
      #   end_time <- paste(substr(as.character(e), 1, 10), "T0", end_h, sep = "")
      # } else {
      #   end_time <- paste(substr(as.character(e), 1, 10), "T", end_h, sep = "")
      # }

      temp <- EIAapi::eia_get(api_key = api_key,
                              api_path = api_path,
                              facets = facets,
                              format = "data.frame",
                              start = s,
                              end = e,
                              length = NULL,
                              offset = NULL) |>
        dplyr::mutate(time = lubridate::ymd_h(period, tz = "UTC")) |>
        dplyr::select(-period) |>
        dplyr::select(time, dplyr::everything()) |>
        dplyr::arrange(time)

    }

    names(temp) <- gsub(pattern = "-", replacement = "_", x = names(temp))

    return(temp)
  }) |>
    dplyr::bind_rows()


}






time_vec <- c(seq.POSIXt(from = start_time, to = end_time, by = paste(offset, "hour")),
               end_time)


df <- lapply(seq_along(time_vec)[-length(time_vec)], function(i){
  print(i)
  temp <- start_h <- end_h <- start_time <- end_time <- NULL
  start <- time_vec[i]
  end <- time_vec[i + 1] - lubridate::hours(1)

  start_h <- lubridate::hour(start)
  end_h <- lubridate::hour(end)

  if(start_h < 10){
    start_time <- paste(substr(as.character(start), 1, 10), "T0", start_h, sep = "")
  } else {
    start_time <- paste(substr(as.character(start), 1, 10), "T", start_h, sep = "")
  }


  if(end_h < 10){
    end_time <- paste(substr(as.character(end), 1, 10), "T0", end_h, sep = "")
  } else {
    end_time <- paste(substr(as.character(end), 1, 10), "T", end_h, sep = "")
  }



  temp <- EIAapi::eia_get(api_key = Sys.getenv("eia_key"),
                          api_path = "electricity/rto/region-sub-ba-data/data/",
                          facets = list(parent = parent,
                                        subba = subba_nyis[3]),
                          format = "data.frame",
                          start = start_time,
                          end = end_time,
                          length = NULL,
                          offset = NULL) |>
    dplyr::mutate(time = lubridate::ymd_h(period, tz = "UTC")) |>
    dplyr::select(-period) |>
    dplyr::select(time, dplyr::everything()) |>
    dplyr::arrange(time)

  names(temp) <- gsub(pattern = "-", replacement = "_", x = names(temp))

  return(temp)
}) |>
  dplyr::bind_rows()

temp <- EIAapi::eia_get(api_key = Sys.getenv("eia_key"),
                        api_path = "electricity/rto/region-sub-ba-data/data/",
                        facets = list(parent = parent,
                                      subba = subba_nyis[3]),
                        format = "data.frame",
                        start = start_time,
                        end = end_time,
                        length = NULL,
                        offset = NULL) |>
  dplyr::mutate(time = lubridate::ymd_h(period, tz = "UTC")) |>
  dplyr::select(-period) |>
  dplyr::select(time, dplyr::everything()) |>
  dplyr::arrange(time)


plot(df$time, df$value, type = "l")
