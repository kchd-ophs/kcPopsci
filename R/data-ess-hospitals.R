#' Kansas City metro area hospitals
#'
#' Hospitals in the Kansas City metro area contributing to ESSENCE.
#'
#' @format A dataframe with `r nrow(ess_hospitals)` rows and
#' `r ncol(ess_hospitals)` columns.
#' \describe{
#'    \item{essence_name}{Hospital name as it appears in ESSENCE}
#'    \item{essence_id}{ESSENCE API ID value, for use in `hospitals` argument
#'    of [ess_build_url()]}
#'    \item{name}{Hospital name}
#'    \item{kc}{Location is in Kansas City, MO (logical)}
#'    \item{lat}{}
#'    \item{long}{}
#'    \item{street}{}
#'    \item{city}{}
#'    \item{state}{}
#'    \item{zipcode}{}
#'    \item{county}{}
#' }
#'
#' @family essence helpers
#'
"ess_hospitals"
