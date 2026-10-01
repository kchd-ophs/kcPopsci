#' Bin numeric values
#'
#' @param x A numeric vector.
#' @param breaks A numeric vector of unique values that determine the lower
#' bound for each bin. For life expectancy calculations, `"lifex5"` or
#' `"lifex10"` returns a specific set of age bins consisting mainly of either
#' 5- or 10-year intervals.
#'
#' @returns A factor vector the same length as `x`.
#' @export
#'
#' @examples
#' n <- seq(.5, 10, .5)
#'
#' # Bins of width 5
#' data.frame(
#'   n = n,
#'   bin = bin_values(n, breaks = seq(0, 10, 5))
#' )
#'
#' # Variable size bins
#' data.frame(
#'   n = n,
#'   bin = bin_values(n, breaks = c(0, 3, 4, 5, 9))
#' )
#'
#' # 10-year bins for life expectancy calculations
#' data.frame(
#'   age = 0:100,
#'   bin = bin_values(0:100, breaks = "lifex10")
#' )
#'
bin_values <- function(x, breaks) {
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

  cut(
    x,
    breaks = breaks,
    labels = lbl,
    include.lowest = TRUE,
    right = FALSE
  )
}
