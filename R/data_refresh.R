#' Refresh the NYgrid Dataset
#' @description The function pull data the NYgrid dataset from the EIA API based
#' on time range using the EIAapi eia_backfill function
#' @param start The query start time in POSIXt format
#' @param end The query end time in POSIXt format
#' @param parent The series parent name
#' @param subba The sub-region codes
#' @param api_key A string, EIA API key, see https://www.eia.gov/opendata/ for registration to the API service
#' @param api_path A string, the API path to follow the API endpoint https://api.eia.gov/v2/.
#' The path can be found on the EIA API dashboard, for more details see https://www.eia.gov/opendata/browser/
#' @return A data.frame object

nygrid_dr <- function(start,
                      end,
                      offset,
                      parent,
                      subba,
                      api_key,
                      api_path,
                      verbose = TRUE){
  # Error handler
  if(!lubridate::is.POSIXt(start)){
    stop("The start object is not valid, must be POSIXct/lt object")
  } else  if(!lubridate::is.POSIXt(end)){
    stop("The end object is not valid, must be POSIXct/lt object")
  } else if(!is.numeric(offset) || offset < 1 || offset %% 1 != 0){
    stop("The offset argument is not a valid, must be an integer value")
  } else if(!is.character(parent)){
    stop("The parent argument is not valid, must be a character object")
  } else if(!is.character(subba)){
    stop("The subba argument is not valid, must be a character object")
  } else if(!is.logical(verbose)){
    stop("The verbose argument is not valid, must be a logical object")
  }

  # Pull the data

  df <- NULL


  df <- lapply(subba, function(i){

    if(verbose){
      cat("Subba: ", i, "\n")
    }
    tryCatch(
      d <- EIAapi::eia_backfill(start = start,
                                end = end,
                                offset = offset,
                                api_key = api_key,
                                api_path = api_path,
                                facets = list(parent = parent,
                                              subba = i)),
      error = function(c) print(c),
      warning = function(c) print(c),
      message = function(c) print(c)

    )

    return(d)
  }) |>
    dplyr::bind_rows() |>
    dplyr::select(time, subba, value)

  if(is.null(df)){
    stop("Could not pull the data, check the error log")
  }


  attr(df, "class") <- c("data.frame", "nygrid")
  attr(df, "parent") <- parent
  attr(df, "subba") <- subba
  attr(df, "start") <- start
  attr(df, "end") <- end


  return(df)
}



#' Data quality checks
#' @description The function conducts a set of data quality tests for the NYgrid dataset
#' @param input A nygrid object (output of the nygrid_dr function)
#' @return A data.frame object with the tests results

nygrid_dqc <- function(input){

  if(!"nygrid" %in% class(input)){
    stop("The input object is not valid 'nygrid' object")
  }
  obj_attr <- attributes(input)


  # Test 1 - All subba available?
  test1 <- all(obj_attr$subba %in% unique(input$subba))
  # Test 2 - All subba distribute equally?
  test2 <- all(diff(table(input$subba)) == 0)
  # Test 3 - Any missing values?
  test3 <- any(is.na(input$value))
  # Test 4 - Any negative or zero values?
  test4 <- any(input$value <= 0)
  # Test 5 - Correct dimensions?
  test5 <- ncol(input) == 3
  # Test 6 - Timestamp class correct?
  test6 <- lubridate::is.POSIXt(input$time)
  # Test the timestamp characteristics
  timestamp_df <- input |>
    dplyr::group_by(subba) |>
    dplyr::summarise(start = min(time),
                     end = max(time))

  # Test 7 - Does the timestamp start hour correct?
  test7 <- !any(is.na(timestamp_df$start)) & all(timestamp_df$start == obj_attr$start)
  # Test 8 - Does the timestamp end hour correct?
  test8 <- !any(is.na(timestamp_df$end)) & all(timestamp_df$end == obj_attr$end)

  # Test 9 - Summary
  test9 <- test1 & test2 & !test3 & !test4 & test5 & test6 & test7 & test8


  meta_df <- data.frame(index = 0,
                        time = Sys.time(),
                        test1 = test1,
                        test2 = test2,
                        test3 = test3,
                        test4 = test4,
                        test5 = test5,
                        test6 = test6,
                        test7 = test7,
                        test8 = test8,
                        test9 = test9,
                        start = obj_attr$start,
                        end = obj_attr$end,
                        start_act = min(input$time),
                        end_act = max(input$time), 
                        num_obs = nrow(input),
                        append = FALSE)

  return(meta_df)

}

