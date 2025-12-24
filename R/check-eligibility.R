#' Checks groups for eligibility
#'
#' @description
#' Checks if the population and death counts in a dataframe are sufficient to
#' calculate life expectancy
#'
#' @param df A data frame/data frame extension (e.g. a tibble), with columns
#' indicating age bins, population sizes, and death counts. The
#' first row should be the age bin from 0-1.
#' @param group_vars A vector of strings with the names of subpopulation groups
#' @param deaths Character indicating the column name with population sizes
#' @param population Character indicating the column name with population sizes
#' @param death_threshhold Minimum number of deaths in (sub)population to
#' be used for LE estimation
#' @param pop_threshhold Minimum populationXyears size in (sub)population to
#' be used for LE estimation
#' @returns A data frame/data frame extension (e.g. a tibble) with flags for
#' ineligible subgroups.
#'
#' @import dplyr
#' @export
#'
#' @examples
#' check_eligibility(tot_init)
#' check_eligibility(demo_init, group_vars = c("sex","race_ethnicity"))
#'

check_eligibility <- function(df,
                              group_vars = NULL,
                              population="population",
                              deaths="deaths",
                              death_threshhold = 20,
                              pop_threshhold = 4000) {

  # (sub)populations have sufficient # of deaths across age bins
  df <- df |>
    group_by(across(any_of(group_vars))) |>
    mutate(insuf_deaths = sum(.data[[deaths]]) < death_threshhold)

  # (sub)populations have sufficient population size across age bins
  df <- df |>
    group_by(across(any_of(group_vars))) |>
    mutate(insuf_pops = sum(.data[[population]]) < pop_threshhold)

  # tracts with population size <=0
  # negatives can occur after subtracting births from 0-5 population estimates
  df <- df |>
    mutate(zero_pop = .data[[population]] <= 0) |>
    group_by(across(any_of(group_vars))) |>
    mutate(zero_pop = as.logical(max(zero_pop)))

  df |> ungroup()
}

