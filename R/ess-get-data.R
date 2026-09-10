#' Get data from the ESSENCE API
#'
#' @description
#' This function is a wrapper for [Rnssp::get_api_data()][api] that always
#' returns a dataframe with variable names in snake case.
#'
#' [api]:https://cdcgov.github.io/Rnssp/reference/get_api_data.html
#'
#' @details
#' An Rnssp profile object must be loaded into the global environment to use
#' this function. See [Rnssp: Creating an NSSP user profile][cred] for details.
#'
#' [cred]:https://cdcgov.github.io/Rnssp/articles/Rnssp_intro.html#creating-an-nssp-user-profile
#'
#' @param url A URL formatted to query the ESSENCE API for data details or time
#' series.
#' @param fix_colnames Logical: Use [fix_colnames()] to standardize column names
#' in the output?
#'
#' @inheritSection ess_build_url Rnssp package
#'
#' @family essence helpers
#'
#' @returns A dataframe.
#' @export
#'
#' @examples
#' \dontrun{
#' syn <- paste(
#'   "medicalGroupingSystem=essencesyndromes",
#'   "ccddCategory=heat%20related%20illness%20v2",
#'   sep = "&"
#' )
#'
#' url <- ess_build_url(
#'   user_id = 1234,
#'   syndrome = syn,
#'   start = Sys.Date() - 30,
#'   data_source = "hospital",
#'   output = "ts",
#'   regions = c("Cass", "Clay", "Jackson", "Platte")
#' )
#'
#' load("path/to/myProfile.rda")
#'
#' # Will throw an error if profile isn't loaded or `user_id` is invalid
#' df <- ess_get_data(url)
#' }
#'
ess_get_data <- function(url, fix_colnames = TRUE) {
  pkg <- requireNamespace("Rnssp", quietly = TRUE)

  if (!pkg) {
    stop("The Rnssp package must be installed to use this function")
  }

  if (grepl("aqtTarget=DataDetails", url)) {
    df <- ess_query_api(url, csv = TRUE)
  } else if (grepl("aqtTarget=TimeSeries", url)) {
    ls <- ess_query_api(url, csv = FALSE)

    df <- ls$timeSeriesData
  } else {
    stop(paste(
      "`url` should be formatted to query the ESSENCE API for either",
      "data details or time series"
    ))
  }

  if (fix_colnames) {
    colnames(df) <- fix_colnames(colnames(df))
  }

  df
}

ess_query_api <- function(url, csv) {
  Rnssp::get_api_data(url, fromCSV = csv)
}
