# Create mock data for testing `ess_get_data()`

devtools::load_all()

load("path/to/myProfile.rda") # path to Essence credentials here

id <- Sys.getenv("ESSENCE_USER_ID") # user ID here

replace_data <- function(df) {
  df <- df[1, ]

  for (i in 1:ncol(df)) {
    if (is.character(df[[i]])) {
      df[[i]] <- "char"
    } else if (is.numeric(df[[i]])) {
      df[[i]] <- 0
    } else if ("POSIXct" %in% class(df[[i]])) {
      df[[i]] <- as.POSIXct("2025-01-01")
    } else if ("hms" %in% class(df[[i]])) {
      df[[i]] <- hms::hms(hours = 12)
    }
  }

  df
}

# Test data details
url <- ess_build_url(
  data_source = "patient",
  time_resolution = "daily",
  start = "2025-01-01",
  end = "2025-01-01",
  zipcodes = "64108",
  output = "dd",
  user_id = id
)

dd_mock <- ess_query_api(url)

dd_mock$dataDetails <- replace_data(dd_mock$dataDetails)

dd1 <- ess_get_data(url) |>
  replace_data()

dd2 <- ess_get_data(url, fix_colnames = FALSE) |>
  replace_data()

dd <- list(
  mock = dd_mock,
  mod_colnames = dd1,
  orig_colnames = dd2
)

# Test time series
url <- ess_build_url(
  data_source = "patient",
  time_resolution = "daily",
  start = "2025-01-01",
  end = "2025-01-01",
  zipcodes = "64108",
  output = "ts",
  user_id = id
)

ts_mock <- ess_query_api(url)

ts_mock$timeSeriesData <- replace_data(ts_mock$timeSeriesData)

ts1 <- ess_get_data(url) |>
  replace_data()

ts2 <- ess_get_data(url, fix_colnames = FALSE) |>
  replace_data()

ts <- list(
  mock = ts_mock,
  mod_colnames = ts1,
  orig_colnames = ts2
)

# Save
ls <- list(
  dd = dd,
  ts = ts
)

saveRDS(ls, "tests/testthat/fixtures/data_ess_get_data.rds")
