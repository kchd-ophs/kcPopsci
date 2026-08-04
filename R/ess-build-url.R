#' Build a URL for the ESSENCE API
#'
#' @description
#' This function creates a URL (or a vector of URLs) to query the API for
#' Missouri's InductiveHealth ESSENCE system.
#'
#' @details
#'
#' ## Data source
#'
#' From the InductiveHealth ESSENCE user guide:
#'
#' "'By Patient Location' datasources are geographically binned based on
#' patient zip code. Users that do not have full access to all data in the
#' system should use this if they want to see records for patients that are
#' residents of their jurisdiction, regardless of where they sought care.
#'
#' "'By Hospital Location' datasources are geographically binned based on
#' facility zip code. Users that do not have full access to all data in the
#' system should use this if they want to see records from facilities in their
#' jurisdiction, regardless of patient residence."
#'
#' ## Geography
#'
#' When `data_source = "hospital"`, hospitals can be selected by county using
#' the `regions` argument or by hospital using the `hospitals` argument. When
#' `data_source = "patient"`, the area of residence can be selected by county
#' using the `regions` argument or by ZIP code using the `zipcodes` argument.
#'
#' The following counties in northwest Missouri are available to KCHD ESSENCE
#' users: Andrew, Atchison, Bates, Benton, Buchanan, Caldwell, Carroll, Cass,
#' Clay, Clinton, Daviess, DeKalb, Gentry, Grundy, Harrison, Henry, Holt,
#' Jackson, Johnson, Lafayette, Livingston, Mercer, Nodaway, Pettis, Platte,
#' Ray, Saline, and Worth.
#'
#' ## Data details
#'
#' There are `r length(ess_dd_vars)` fields available to specify in
#' `dd_fields`. Call `ess_dd_vars` for the full list.
#'
#' @param data_source Either `"hospital"` or `"patient"`.
#' @param time_resolution Can be `"daily"` (the default), `"weekly"`,
#' `"monthly"`, `"quarterly"`, or `"yearly"`.
#' @param start,end A date formatted YYYY-MM-DD (character or date). `end`
#' defaults to `Sys.Date()`.
#' @param syndrome A vector of strings in the format
#' `"medicalGroupingSystem=<grouping name>&<query category>=<query name>"`.
#' @param regions A vector of county names (case insensitive; omit the word
#' "county").
#' @param hospitals A vector of hospitals. Only used if
#' `datasource = "hospital"`.
#' @param zipcodes A vector of ZIP codes (numeric or character). Only used if
#' `data_source = "patient"`.
#' @param output Either `"dd"` (for data details) or `"ts"` (for time series).
#' @param dd_fields A vector of data details fields to pull. `NULL` returns all
#' available fields. If not `NULL`, "EssenceID" is added to prevent aggregation
#' of data.
#' @param user_id An ESSENCE user ID (numeric or character). This can be found
#' by creating a query in the ESSENCE software online. The ID follows the
#' "userId" field.
#'
#' @section Rnssp package:
#'
#' Functions prefixed with `"ess_"` work with data from the ESSENCE platform.
#' To access ESSENCE data by API, the Rnssp package is required.
#'
#' - Download Rnssp: `remotes::install_github("cdcgov/Rnssp")`
#' - [Package website](https://cdcgov.github.io/Rnssp/)
#'
#' @family essence helpers
#'
#' @returns A dataframe.
#' @export
#'
#' @examples
#' # Heat related illness v2 query
#' syn <- paste(
#'   "medicalGroupingSystem=essencesyndromes",
#'   "ccddCategory=heat%20related%20illness%20v2",
#'   sep = "&"
#' )
#'
#' ess_build_url(
#'   user_id = 1234,
#'   syndrome = syn,
#'   start = Sys.Date() - 30,
#'   data_source = "hospital",
#'   output = "ts",
#'   regions = c("Cass", "Clay", "Jackson", "Platte")
#' )
#'
ess_build_url <- function(
    data_source = c("hospital", "patient"),
    time_resolution = c("daily", "weekly", "monthly", "quarterly", "yearly"),
    start,
    end = Sys.Date(),
    syndrome = NULL,
    regions = NULL,
    hospitals = NULL,
    zipcodes = NULL,
    output = c("dd", "ts"),
    dd_fields = NULL,
    user_id
) {
  data_source <- match.arg(data_source)

  time_resolution <- match.arg(time_resolution)

  output <- match.arg(output)

  start <- as.Date(start); end <- as.Date(end)

  if (is.na(start) | is.na(end)) {
    stop("`start` and `end` must be valid dates formatted YYYY-MM-DD")
  }

  # Output parameters
  if (output == "dd") {
    if (is.null(dd_fields)) {
      params_out <- "aqtTarget=DataDetails"
    } else {
      dd_fields <- utils::URLencode(dd_fields)

      params_out <- paste(
        c(
          "aqtTarget=DataDetails",
          paste0("field=", c(dd_fields, "EssenceID"))
        ),
        collapse = "&"
      )
    }

    op <- "dataDetails/csv?"
  } else if (output == "ts") {
    op <- "timeSeries?"

    params_out <- "aqtTarget=TimeSeries"
  }

  # Data source
  if (data_source == "hospital") {
    params_ds <- "datasource=va_hosp"
  } else if (data_source == "patient") {
    params_ds <- "datasource=va_er"
  }

  # Geography
  if (!is.null(regions) & is.null(hospitals) & is.null(zipcodes)) {
    if (data_source == "hospital") {
      rgn <- "hospitalregion"
    } else if (data_source == "patient") {
      rgn <- "region"
    }

    params_geo <- paste(
      paste0("geographySystem=", rgn),
      paste(
        paste0("geography=", paste0("mo_", tolower(regions))),
        collapse = "&"
      ),
      sep = "&"
    )
  } else if (data_source == "hospital" & is.null(regions) & !is.null(hospitals)) {
    params_geo <- paste(
      "geographySystem=hospital",
      paste(
        paste0("geography=", hospitals),
        collapse = "&"
      ),
      sep = "&"
    )
  } else if (data_source == "patient" & is.null(regions) & !is.null(zipcodes)) {
    params_geo <- paste(
      "geographySystem=zipcode",
      paste0("geography=", paste(zipcodes, collapse = ",")),
      sep = "&"
    )
  } else {
    stop(paste(
      "See the function documentation for how to correctly pair geography and",
      "data source"
    ))
  }

  # Syndrome
  if (is.null(syndrome)) {
    syn <- "medicalGroupingSystem=essencesyndromes"
  } else {
    syn <- syndrome
  }

  # Detector
  if (time_resolution %in% c("daily", "weekly")) {
    detector <- "detector=probrepswitch"
  } else {
    detector <- "detector=nodetectordetector"
  }

  # Additional parameters
  params_add <- paste(
    paste0("userId=", user_id),
    paste0("timeResolution=", time_resolution),
    paste0("startDate=", format(start, "%d%b%Y")),
    paste0("endDate=", format(end, "%d%b%Y")),
    "percentParam=noPercent",
    detector,
    "hasBeenE=1",
    sep = "&"
  )

  params <- paste(params_out, params_ds, params_geo, params_add, sep = "&")

  endpoint <- "https://moessence.inductivehealth.com/ih_essence/api/"

  paste0(endpoint, op, paste0(params, "&", syn))
}
