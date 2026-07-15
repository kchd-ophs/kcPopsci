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
ess_get_data <- function(url) {
  pkg1 <- requireNamespace("Rnssp", quietly = TRUE)

  pkg2 <- requireNamespace("readr", quietly = TRUE)

  if (!pkg1 | !pkg2) {
    stop("The Rnssp and readr packages must be installed to use this function")
  }

  if (grepl("aqtTarget=DataDetails", url)) {
    Rnssp::get_api_data(
      url,
      fromCSV = TRUE,
      col_types = readr::cols(.default = "c"),
      name_repair = fix_colnames
    )
  } else if (grepl("aqtTarget=TimeSeries", url)) {
    ls <- Rnssp::get_api_data(url)

    df <- ls$timeSeriesData

    colnames(df) <- fix_colnames(colnames(df))

    df$date <- as.Date(df$date)

    df
  } else {
    stop(paste(
      "`url` should be formatted to query the ESSENCE API for either",
      "data details or time series"
    ))
  }
}
