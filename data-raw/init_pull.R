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

while(c < iterations){

tryCatch({
  temp <- EIAapi::eia_get(api_key = Sys.getenv("eia_key"),
                          api_url = "https://api.eia.gov/v2/electricity/rto/region-sub-ba-data/data/",
                          facets = list(parent = parent),
                          format = "data.frame",
                          start = start_time,
                          length = length,
                          offset = offset * (c -1))},

  error = function(c) message(c),
  warning = function(c) message(c),
  message = function(c) message(c))


head(temp)
unique(temp$subba)
unique(temp$parent)
unique(temp$period)
table(temp$subba)
