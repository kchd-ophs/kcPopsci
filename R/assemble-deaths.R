#' Prepare Death File
#'
#' @description
#' Takes Vital Records death file and aggregates it by age bins for life
#' expectancy calculation
#'
#'
#' @param deaths A data frame/ data frame extension (e.g. a tibble),
#' with at minimum a column indicating age bins ("age_bin") and a column
#' indicating death counts. The first row should be the age bin from 0-1.
#' @param age_col Name of the column with ages to be binned.
#' @param ... Additional subgroups to be used for life expectancy calculation
#' (e.g. race, geography, year).
#' @param breaks either "lifex5" or "lifex10", indicating the age bin width to
#' pass to bin_ages(). If not provided, will use "lifex10"
#'
#' @export
#' @returns A data frame/data frame extension (e.g. a tibble) with counts of
#' deaths by standard age bins used for life expectancy calculation
#'
#' @examples
#'
#'set.seed(0)
#'n <- 100
#'df <- data.frame(
#'  age=sample(2:100, size=n,replace=TRUE),
#'  sex=sample(c("M","F"), size=n, replace=TRUE)
#')
#'assemble_deaths(deaths=df, age_col=age, breaks="lifex10")
#'assemble_deaths(deaths=df, age_col=age, sex, breaks="lifex5")
#'


assemble_deaths <- function(deaths, age_col, ...,
                            breaks = c("lifex5","lifex10")) {

  if (missing(breaks)){
    breaks <- "lifex10"
  } else {
    breaks <- match.arg(breaks)
  }
  print(breaks)

  deaths |>
    mutate(age_bin=bin_ages({{age_col}}, breaks = breaks)) |>
    count(.data$age_bin, ...) |>
    complete(.data$age_bin, ...) |>
    mutate(n=replace_na(n,0)) |>
    arrange(...,.data$age_bin)

}
