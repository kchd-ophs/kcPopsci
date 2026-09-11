#' Kansas City hospitals
#'
#' All hospitals located in Kansas City.
#'
#' @format A dataframe with `r nrow(hospitals)` rows and
#' `r ncol(hospitals)` columns.
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
"hospitals"
