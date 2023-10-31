eia_backfill <- function (start, end, offset, api_key, api_path, facets) {
    
    if (!any(lubridate::is.POSIXt(start), lubridate::is.Date(start))) {
        stop("The start argument is not valid, the function suports POSIXlt, POSIXct, Date objects")
    }else if (!any(lubridate::is.POSIXt(end), lubridate::is.Date(end))) {
        stop("The end argument is not valid, the function suports POSIXlt, POSIXct, Date objects")
    }else if (!any(class(start) %in% class(end))) {
        stop("The class of the start argument is different from the end argument class")
    }else if (!is.numeric(offset) || offset < 0 || offset%%1 != 
        0) {
        stop("The offset argument is not valid, must be numeric")
    } else if (!is.character(api_key)) {
        stop("The api_key argument is not valid")
    } else if (!is.character(api_path)) {
        stop("The api_path argument is not valid")
    }else if (!is.null(facets) && !is.list(facets)) {
        stop("The facets argument must be a list object")
    }else if (offset > 5000) {
        message("The offset argument surpasses the API number of observations per call limit, setting it to 5000")
        offset <- 5000
    }

    if (lubridate::is.POSIXt(start)) {
        time_class <- "POSIXt"
    }else if (lubridate::is.Date(start)) {
        time_class <- "Date"
    }else {
        stop("Mismatch with start/end arguments class")
    }
    
    if (time_class == "POSIXt") {
        time_vec <- seq.POSIXt(from = start, to = end, by = paste(offset, 
            "hour"))
        if (max(time_vec) < end) {
            time_vec <- c(time_vec, end)
        }
    }else if (time_class == "Date") {
        time_vec <- seq.Date(from = start, to = end, by = "day")
    }
    time_vec_seq <- seq_along(time_vec)[-length(time_vec)]
    df <- dplyr::arrange(dplyr::bind_rows(lapply(time_vec_seq, function(i) {
            temp <- start_h <- end_h <- start_time <- end_time <- NULL
            s <- time_vec[i]
            if (time_class == "POSIXt") {
                if (i < max(time_vec_seq)) {
                  e <- time_vec[i + 1] - lubridate::hours(1)
                }else {
                  e <- time_vec[i + 1]
                }
                start_h <- lubridate::hour(s)
                end_h <- lubridate::hour(e)
                if (start_h < 10) {
                  start_time <- paste(substr(as.character(s), 
                    1, 10), "T0", start_h, sep = "")
                } else {
                  start_time <- paste(substr(as.character(s), 
                    1, 10), "T", start_h, sep = "")
                }
                if (end_h < 10) {
                  end_time <- paste(substr(as.character(e), 1, 
                    10), "T0", end_h, sep = "")
                }else {
                  end_time <- paste(substr(as.character(e), 1, 
                    10), "T", end_h, sep = "")
                }
                temp <- dplyr::arrange(dplyr::select(dplyr::select(dplyr::mutate(EIAapi::eia_get(api_key = api_key, 
                  api_path = api_path, facets = facets, format = "data.frame", 
                  start = start_time, end = end_time, length = NULL, 
                  offset = NULL), time = lubridate::ymd_h(period, 
                  tz = "UTC")), -period), time, dplyr::everything()), 
                  time)
            } else if (time_class == "Date") {
                e <- time_vec[i + 1] - lubridate::days(1)
                temp <- dplyr::arrange(dplyr::select(dplyr::select(dplyr::mutate(EIAapi::eia_get(api_key = api_key, 
                  api_path = api_path, facets = facets, format = "data.frame", 
                  start = s, end = e, length = NULL, offset = NULL), 
                  time = lubridate::ymd_h(period, tz = "UTC")), 
                  -period), time, dplyr::everything()), time)
            }
            names(temp) <- gsub(pattern = "-", replacement = "_", 
                x = names(temp))
            return(temp)
        })), time)
    return(df)
}
