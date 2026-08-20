#' Kansas City hospitals
#'
#' All hospitals located in Kansas City.
#'
#' @format A dataframe with `r nrow(kc_hospitals)` rows and
#' `r ncol(kc_hospitals)` columns.
#' \describe{
#'    \item{name}{Hospital name}
#'    \item{address}{Street address}
#'    \item{county}{County}
#'    \item{essence_id}{ESSENCE API ID value, for use in `hospitals` argument
#'    of [ess_build_url()]}
#' }
#'
#' @family essence helpers
#'
"kc_hospitals"
