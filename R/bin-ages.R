#' Make age bins
#'
#' @description
#' Input ages and return age bins.
#'
#' @param x A numeric vector of ages.
#' @param breaks A numeric vector determining the lower bounds for age bins.
#' `"lifex5"` or `"lifex10"` returns age bins specific to life expectancy
#' calculations consisting mainly of either 5- or 10-year intervals.
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
#'   bin = bin_ages(ages, breaks = seq(0, 10, 5))
#' )
#'
#' # Variable size bins
#' data.frame(
#'   age = ages,
#'   bin = bin_ages(ages, breaks = c(0, 3, 4, 5, 9))
#' )
#'
#' # 10-year bins for life expectancy calculations
#' data.frame(
#'   age = 0:100,
#'   bin = bin_ages(0:100, breaks = "lifex10")
#' )
#'
bin_ages <- function(x, breaks) {
  if (length(breaks) == 1 && breaks == "lifex5") {
    breaks <- c(0, 1, seq(5, 85, 5))
  } else if (length(breaks) == 1 && breaks == "lifex10") {
    breaks <- c(0, 1, seq(5, 85, 10))
  } else if (!is.numeric(breaks)) {
    stop("Invalid `breaks` value. See function documentation.")
  }

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
