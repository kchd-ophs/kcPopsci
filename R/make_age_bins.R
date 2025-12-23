#' Create age bins for life tables
#'
#' @description
#' NOTE: Not sure exactly how I want to implement this.
#' Create a factor with 1, 5 or 10 year age bins to use as rows in a life table.
#' Note that 10 year age bins are the standard for most OPHS calculations
#'
#' @param ages A numeric vector to cut into ages
#' @param width A single number (5 or 10) to use for age bin widths.
#'
#' @returns A factor with age bins
#' @export
#'
#' @examples
#' make_age_bins(5)
#'

make_age_bins <- function(ages,width=10) {

  if(!width %in% c(5,10)){
    error("Width must be equal to either 5 or 10")
  }

  breaks <- c(0,1,seq(5,85,width),Inf)
  labels <- paste(breaks[1:(length(breaks-1))],breaks[2:length(breaks)]-1,sep="-")
  labels <- labels[1:(length(labels)-1)]
  labels[1] <- "0-1"
  labels[length(labels)] <- "85+"

  cut(ages, breaks=breaks, labels=labels,
      include.lowest = TRUE,ordered_result = TRUE)
}


