#' Make age bins
#'
#' @description
#' Input ages and return age bins.
#'
#' @param x A numeric vector.
#' @param breaks An integer vector to be used as lower bounds for age bins.
#' This is passed to the `breaks` argument in [cut()].
#'
#' @returns A factor vector the same length as `x`.
#' @export
#'
#' @examples
#' ages <- seq(.5, 10, .5)
#'
#' # 5-year bins
#' data.frame(
#'   age = ages,
#'   age_group = bin_ages(ages, breaks = seq(0, 10, 5))
#' )
#'
#' # Variable size bins
#' data.frame(
#'   age = ages,
#'   age_group = bin_ages(ages, breaks = c(0, 3, 4, 5, 9))
#' )
#'
bin_ages <- function(x, breaks) {
  breaks <- c(breaks, Inf)

  start <- breaks[1:(length(breaks) - 1)]

  end <- breaks[2:length(breaks)] - 1

  lbl <- mapply(
    \(x, y) ifelse(identical(x, y), x, paste(x, y, sep = "-")),
    start, end
  )

  lbl <- sub("-Inf", "+", lbl)

  cut(x, breaks = breaks, labels = lbl, include.lowest = TRUE, right = FALSE)
}
