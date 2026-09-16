test_that("data details", {
  ls <- readRDS(test_path("fixtures", "data_ess_get_data.rds"))
  url <- ess_build_url(
    data_source = "patient",
    time_resolution = "daily",
    start = "2025-01-01",
    end = "2025-01-01",
    zipcodes = "64108",
    output = "dd",
    user_id = "****"
  )
  local_mocked_bindings(ess_query_api = function(...) ls$dd$mock)
  act <- ess_get_data(url)
  exp <- ls$dd$mod_colnames
  expect_equal(act, exp)
})

test_that("data details, orig colnames", {
  ls <- readRDS(test_path("fixtures", "data_ess_get_data.rds"))
  url <- ess_build_url(
    data_source = "patient",
    time_resolution = "daily",
    start = "2025-01-01",
    end = "2025-01-01",
    zipcodes = "64108",
    output = "dd",
    user_id = "****"
  )
  local_mocked_bindings(ess_query_api = function(...) ls$dd$mock)
  act <- ess_get_data(url, fix_colnames = FALSE)
  exp <- ls$dd$orig_colnames
  expect_equal(act, exp)
})

test_that("time series", {
  ls <- readRDS(test_path("fixtures", "data_ess_get_data.rds"))
  url <- ess_build_url(
    data_source = "patient",
    time_resolution = "daily",
    start = "2025-01-01",
    end = "2025-01-01",
    zipcodes = "64108",
    output = "ts",
    user_id = "****"
  )
  local_mocked_bindings(ess_query_api = function(...) ls$ts$mock)
  act <- ess_get_data(url)
  exp <- ls$ts$mod_colnames
  expect_equal(act, exp)
})

test_that("data details, orig colnames", {
  ls <- readRDS(test_path("fixtures", "data_ess_get_data.rds"))
  url <- ess_build_url(
    data_source = "patient",
    time_resolution = "daily",
    start = "2025-01-01",
    end = "2025-01-01",
    zipcodes = "64108",
    output = "ts",
    user_id = "****"
  )
  local_mocked_bindings(ess_query_api = function(...) ls$ts$mock)
  act <- ess_get_data(url, fix_colnames = FALSE)
  exp <- ls$ts$orig_colnames
  expect_equal(act, exp)
})
