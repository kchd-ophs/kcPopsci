#' Build a URL for the ESSENCE API
#'
#' @description
#' This function creates a URL (or a vector of URLs) to query the API for
#' Missouri's ESSENCE system provided by InductiveHealth.
#'
#' @details
#' Geography for the query is determined as follows.
#'
#' - If `data_source = "hospital"`, records from all hospitals in Cass, Clay,
#' Jackson, and Platte counties will be downloaded.
#' - If `data_source = "patient"` and `zipcodes = NULL`, records for all
#' patients with ZIP codes in Cass, Clay, Jackson, and Platte counties will be
#' downloaded.
#' - If `data_source = "patient"` and `zipcodes` is not `NULL`, records for
#' only those patients with ZIP codes provided will be downloaded.
#'
#' @param user_id An ESSENCE user ID (numeric or character). This can be found
#' by creating a query in the ESSENCE software online. The ID follows the
#' "userId" field.
#' @param syndrome A vector of strings in the format
#' `"medicalGroupingSystem=<grouping name>&<query category>=<query name>"`.
#' @param start,end A date formatted YYYY-MM-DD (character or date). `end`
#' defaults to `Sys.Date()`.
#' @param data_source Either `"hospital"` or `"patient"`.
#' @param output Either `"dd"` (for data details) or `"ts"` (for time series).
#' @param dd_fields A vector of data details fields to pull. `NULL` returns all
#' available fields. If not `NULL`, "EssenceID" is added to prevent aggregation
#' of data.
#' @param zipcodes A vector of ZIP codes (numeric or character). Only used if
#' `data_source = "patient"`.
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
#'   output = "ts"
#' )
#'
ess_build_url <- function(
    user_id,
    syndrome,
    start,
    end = Sys.Date(),
    data_source = c("hospital", "patient"),
    output = c("dd", "ts"),
    dd_fields = NULL,
    zipcodes = NULL
) {
  data_source <- match.arg(data_source)

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

      dd_fields <- paste0("field=", c(dd_fields, "EssenceID"))

      params_out <- paste(
        c("aqtTarget=DataDetails", dd_fields),
        collapse = "&"
      )
    }

    op <- "dataDetails/csv?"
  } else if (output == "ts") {
    op <- "timeSeries?"

    params_out <- "aqtTarget=TimeSeries"
  }

  # Data source & geography parameters
  if (data_source == "hospital") {
    params_ds <- "datasource=va_hosp"

    params_geo <- paste(
      "geographySystem=hospitalregion",
      "geography=mo_cass",
      "geography=mo_clay",
      "geography=mo_jackson",
      "geography=mo_platte",
      sep = "&"
    )
  } else if (data_source == "patient") {
    params_ds <- "datasource=va_er"

    if (is.null(zipcodes)) {
      params_geo <- paste(
        "geographySystem=region",
        "geography=mo_cass",
        "geography=mo_clay",
        "geography=mo_jackson",
        "geography=mo_platte",
        sep = "&"
      )
    } else {
      params_geo <- paste(
        "geographySystem=zipcode",
        paste0("geography=", paste(zipcodes, collapse = ",")),
        sep = "&"
      )
    }
  }

  # Additional parameters
  params_add <- paste(
    paste0("userId=", user_id),
    paste0("startDate=", format(start, "%d%b%Y")),
    paste0("endDate=", format(end, "%d%b%Y")),
    "percentParam=noPercent",
    "detector=probrepswitch",
    "timeResolution=daily",
    "hasBeenE=1",
    sep = "&"
  )

  params <- paste(params_out, params_ds, params_geo, params_add, sep = "&")

  endpoint <- "https://moessence.inductivehealth.com/ih_essence/api/"

  paste0(endpoint, op, paste0(params, "&", syndrome))
}
