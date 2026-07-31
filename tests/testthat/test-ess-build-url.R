test_that("ts, hospital", {
  syn <- paste(
    "medicalGroupingSystem=essencesyndromes",
    "ccddCategory=heat%20related%20illness%20v2",
    sep = "&"
  )

  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = syn,
    start = date - 30,
    end = date,
    data_source = "hospital",
    output = "ts",
    regions = c("Cass", "Clay", "Jackson", "Platte")
  )

  exp <- paste(
    "https://moessence.inductivehealth.com/ih_essence/api/timeSeries",
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
      "medicalGroupingSystem=essencesyndromes",
      "ccddCategory=heat%20related%20illness%20v2",
      sep = "&"
    ),
    sep = "?"
  )

  expect_equal(act, exp)
})

test_that("ts, patient", {
  syn <- paste(
    "medicalGroupingSystem=essencesyndromes",
    "ccddCategory=heat%20related%20illness%20v2",
    sep = "&"
  )

  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = syn,
    start = date - 30,
    end = date,
    data_source = "patient",
    output = "ts",
    regions = c("Cass", "Clay")
  )

  exp <- paste(
    "https://moessence.inductivehealth.com/ih_essence/api/timeSeries",
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
      "medicalGroupingSystem=essencesyndromes",
      "ccddCategory=heat%20related%20illness%20v2",
      sep = "&"
    ),
    sep = "?"
  )

  expect_equal(act, exp)
})

test_that("ts, patient, zipcodes provided", {
  syn <- paste(
    "medicalGroupingSystem=essencesyndromes",
    "ccddCategory=heat%20related%20illness%20v2",
    sep = "&"
  )

  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = syn,
    start = date - 30,
    end = date,
    data_source = "patient",
    output = "ts",
    zipcodes = c("64053", "64101", "64102", "64105", "64106")
  )

  exp <- paste(
    "https://moessence.inductivehealth.com/ih_essence/api/timeSeries",
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
      "medicalGroupingSystem=essencesyndromes",
      "ccddCategory=heat%20related%20illness%20v2",
      sep = "&"
    ),
    sep = "?"
  )

  expect_equal(act, exp)
})

test_that("dd, hospital, `dd_fields` is NULL", {
  syn <- paste(
    "medicalGroupingSystem=essencesyndromes",
    "ccddCategory=heat%20related%20illness%20v2",
    sep = "&"
  )

  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = syn,
    start = date - 30,
    end = date,
    data_source = "hospital",
    output = "dd",
    regions = c("Jackson", "Platte")
  )

  exp <- paste(
    "https://moessence.inductivehealth.com/ih_essence/api/dataDetails/csv",
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
      "medicalGroupingSystem=essencesyndromes",
      "ccddCategory=heat%20related%20illness%20v2",
      sep = "&"
    ),
    sep = "?"
  )

  expect_equal(act, exp)
})

test_that("dd, hospital, `dd_fields` is populated", {
  syn <- paste(
    "medicalGroupingSystem=essencesyndromes",
    "ccddCategory=heat%20related%20illness%20v2",
    sep = "&"
  )

  date <- as.Date("2026-07-01")

  act <- ess_build_url(
    user_id = 1234,
    syndrome = syn,
    start = date - 30,
    end = date,
    data_source = "hospital",
    output = "dd",
    regions = c("Cass", "Clay", "Jackson", "Platte"),
    dd_fields = c("Date", "HospitalName", "HasBeenE")
  )

  exp <- paste(
    "https://moessence.inductivehealth.com/ih_essence/api/dataDetails/csv",
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
      "medicalGroupingSystem=essencesyndromes",
      "ccddCategory=heat%20related%20illness%20v2",
      sep = "&"
    ),
    sep = "?"
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
    "`zipcodes` can only be used when `data_source = \"patient\"`"
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
    "Either `regions` or `zipcodes` must be populated, but not both"
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
    "Either `regions` or `zipcodes` must be populated, but not both"
  )
})
