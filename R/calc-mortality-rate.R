#' Calculate mortality rate
#'
#' @description
#' What the function does...
#'
#' @details
#' More detailed explanation...
#'
#' @param deaths Number of deaths.
#' @param pop Population size.
#'
#' @returns A numeric vector.
#' @export
#'
#' @examples
#' calc_mortality_rate(10, 1000)
#'
calc_mortality_rate <- function(deaths, pop) {
  deaths / pop
}

