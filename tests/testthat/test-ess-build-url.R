test_that("ts, hospital", {
  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = "syndromefield=somesyndrome",
    start = date - 30,
    end = date,
    data_source = "hospital",
    output = "ts",
    regions = c("Cass", "Clay", "Jackson", "Platte")
  )

  exp <- paste0(
    "https://moessence.inductivehealth.com/ih_essence/api/timeSeries?",
    paste(
      "aqtTarget=TimeSeries",
      "datasource=va_hosp",
      "geographySystem=hospitalregion",
      "geography=mo_cass",
      "geography=mo_clay",
      "geography=mo_jackson",
      "geography=mo_platte",
      "userId=1234",
      "timeResolution=daily",
      "startDate=01Jun2026",
      "endDate=01Jul2026",
      "percentParam=noPercent",
      "detector=probrepswitch",
      "hasBeenE=1",
      "syndromefield=somesyndrome",
      sep = "&"
    )
  )

  expect_equal(act, exp)
})

test_that("ts, patient", {
  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = "syndromefield=somesyndrome",
    start = date - 30,
    end = date,
    data_source = "patient",
    output = "ts",
    regions = c("Cass", "Clay")
  )

  exp <- paste0(
    "https://moessence.inductivehealth.com/ih_essence/api/timeSeries?",
    paste(
      "aqtTarget=TimeSeries",
      "datasource=va_er",
      "geographySystem=region",
      "geography=mo_cass",
      "geography=mo_clay",
      "userId=1234",
      "timeResolution=daily",
      "startDate=01Jun2026",
      "endDate=01Jul2026",
      "percentParam=noPercent",
      "detector=probrepswitch",
      "hasBeenE=1",
      "syndromefield=somesyndrome",
      sep = "&"
    )
  )

  expect_equal(act, exp)
})

test_that("ts, patient, zipcodes provided", {
  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = "syndromefield=somesyndrome",
    start = date - 30,
    end = date,
    data_source = "patient",
    output = "ts",
    zipcodes = c("64053", "64101", "64102", "64105", "64106")
  )

  exp <- paste0(
    "https://moessence.inductivehealth.com/ih_essence/api/timeSeries?",
    paste(
      "aqtTarget=TimeSeries",
      "datasource=va_er",
      "geographySystem=zipcode",
      "geography=64053,64101,64102,64105,64106",
      "userId=1234",
      "timeResolution=daily",
      "startDate=01Jun2026",
      "endDate=01Jul2026",
      "percentParam=noPercent",
      "detector=probrepswitch",
      "hasBeenE=1",
      "syndromefield=somesyndrome",
      sep = "&"
    )
  )

  expect_equal(act, exp)
})

test_that("dd, hospital, `dd_fields` is NULL", {
  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = "syndromefield=somesyndrome",
    start = date - 30,
    end = date,
    data_source = "hospital",
    output = "dd",
    regions = c("Jackson", "Platte")
  )

  exp <- paste0(
    "https://moessence.inductivehealth.com/ih_essence/api/dataDetails/csv?",
    paste(
      "aqtTarget=DataDetails",
      "datasource=va_hosp",
      "geographySystem=hospitalregion",
      "geography=mo_jackson",
      "geography=mo_platte",
      "userId=1234",
      "timeResolution=daily",
      "startDate=01Jun2026",
      "endDate=01Jul2026",
      "percentParam=noPercent",
      "detector=probrepswitch",
      "hasBeenE=1",
      "syndromefield=somesyndrome",
      sep = "&"
    )
  )

  expect_equal(act, exp)
})

test_that("dd, hospital, `dd_fields` is populated", {
  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = "syndromefield=somesyndrome",
    start = date - 30,
    end = date,
    data_source = "hospital",
    output = "dd",
    regions = c("Cass", "Clay", "Jackson", "Platte"),
    dd_fields = c("Date", "HospitalName", "HasBeenE")
  )

  exp <- paste0(
    "https://moessence.inductivehealth.com/ih_essence/api/dataDetails/csv?",
    paste(
      "aqtTarget=DataDetails",
      "field=Date",
      "field=HospitalName",
      "field=HasBeenE",
      "field=EssenceID",
      "datasource=va_hosp",
      "geographySystem=hospitalregion",
      "geography=mo_cass",
      "geography=mo_clay",
      "geography=mo_jackson",
      "geography=mo_platte",
      "userId=1234",
      "timeResolution=daily",
      "startDate=01Jun2026",
      "endDate=01Jul2026",
      "percentParam=noPercent",
      "detector=probrepswitch",
      "hasBeenE=1",
      "syndromefield=somesyndrome",
      sep = "&"
    )
  )

  expect_equal(act, exp)
})

test_that("`free_vars` used", {
  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = "syndromefield=somesyndrome",
    start = date - 30,
    end = date,
    data_source = "hospital",
    output = "ts",
    regions = c("Cass", "Clay", "Jackson", "Platte"),
    free_vars = list(one = "apple", two = "banana")
  )

  exp <- paste0(
    "https://moessence.inductivehealth.com/ih_essence/api/timeSeries?",
    paste(
      "aqtTarget=TimeSeries",
      "datasource=va_hosp",
      "geographySystem=hospitalregion",
      "geography=mo_cass",
      "geography=mo_clay",
      "geography=mo_jackson",
      "geography=mo_platte",
      "userId=1234",
      "timeResolution=daily",
      "startDate=01Jun2026",
      "endDate=01Jul2026",
      "percentParam=noPercent",
      "detector=probrepswitch",
      "hasBeenE=1",
      "one=apple&two=banana",
      "syndromefield=somesyndrome",
      sep = "&"
    )
  )

  expect_equal(act, exp)
})

test_that("error: hospital + zipcodes", {
  expect_error(
    ess_build_url(
      user_id = 1234,
      syndrome = "syndrome",
      start = Sys.Date() - 30,
      data_source = "hospital",
      output = "dd",
      zipcodes = "64108"
    ),
    paste(
      "See the function documentation for how to correctly pair geography and",
      "data source"
    )
  )
})

test_that("error: regions + zipcodes", {
  expect_error(
    ess_build_url(
      user_id = 1234,
      syndrome = "syndrome",
      start = Sys.Date() - 30,
      data_source = "patient",
      output = "dd",
      regions = "Jackson",
      zipcodes = "64105"
    ),
    paste(
      "See the function documentation for how to correctly pair geography and",
      "data source"
    )
  )
})

test_that("error: no regions or zipcodes", {
  expect_error(
    ess_build_url(
      user_id = 1234,
      syndrome = "syndrome",
      start = Sys.Date() - 30,
      data_source = "patient",
      output = "dd"
    ),
    paste(
      "See the function documentation for how to correctly pair geography and",
      "data source"
    )
  )
})

test_that("`free_vars` not a list", {
  expect_error(
    ess_build_url(
      user_id = 1234,
      syndrome = "syndrome",
      start = Sys.Date() - 30,
      data_source = "patient",
      output = "dd",
      free_vars = "one=apple"
    ),
    "`free_vars` must be a list"
  )
})
