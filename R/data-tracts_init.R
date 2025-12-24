#' Sample population and death data by Census Tract
#'
#' Simulated population and death data using 10-year age bins by Census Tract.
#' Note that this dataset only includes 6 tracts. In practice there are ~150 in
#' KCMO.
#'
#' @format ## `tracts_init`
#' A data frame with 66 rows and 4 columns:
#' \describe{
#'   \item{GEOID}{Tract identifier}
#'   \item{age_bin}{Age group}
#'   \item{deaths}{Number of deaths within age group}
#'   \item{population}{Population size within age group}
#'   ...
#' }
#' @source Simulated data generated using ChatGPT
"tracts_init"
