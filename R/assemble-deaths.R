#' Prepare Death File
#'
#' @description
#' Takes Vital Records death file and aggregates it by age bins for life
#' expectancy calculation
#'
#'
#' @param deaths A 2 column data frame/ data frame extension (e.g. a tibble),
#' with a column indicating age bins ("age_bin") and a column indicating death
#' counts. The first row should be the age bin from 0-1.
#' @param ... Additional subgroups to be used for life expectancy calculation
#' (e.g. race, geography, year)
#' @param width age bin width to pass to make_age_bins()
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
#'  AGE=sample(2:100,size=n,replace=TRUE),
#'  SEX=sample(c("M","F"),size=n,replace=TRUE)
#')
#'assemble_deaths(df, width=10)
#'assemble_deaths(df, SEX,width=5)
#'


assemble_deaths <- function(deaths, ..., width = 10) {

  deaths$age_bin <- make_age_bins(deaths$AGE, width = width)
  deaths |>
    count(age_bin, ...) |>
    complete(age_bin, ...) |>
    mutate(across(everything(), \(x) replace_na(x,0)))
}

