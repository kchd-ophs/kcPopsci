#' Initialize populations and deaths to calculate a life table
#'
#' @description
#' Merges dataframes of death and population counts, plus checks for any
#' ineligible subgroups
#'
#'
#' @param deaths A 2 column data frame/ data frame extension (e.g. a tibble),
#' with a column indicating age bins ("age_bin") and a column indicating death counts. The
#' first row should be the age bin from 0-1.
#' @param pops A 2 column data frame/ data frame extension (e.g. a tibble),
#' with a column indicating age bins ("age_bin") and a column indicating population counts.
#' The first row should be the age bin from 0-1. Age bins must be identical
#' to those in deaths.
#' @param population_col Character indicating the name of the column with population counts
#' @param deaths_col Character indicating the name of the column with death counts
#' @param age_bin_col Character indicating the name of the column with age bins
#' @param group_vars A vector with the names of the grouping columns.
#' Argument does not need to include age_bin. If empty, no grouping variables will
#' be assumed.
#' @param drop_inelig logical. If true (the default), groups that are ineligible for life
#' expectancy calculations will be dropped, as will the columns indicating eligibility
#' @param death_threshhold Minimum number of deaths in (sub)population to
#' be used for LE estimation. Default is 20
#' @param pop_threshhold Minimum populationXyears size in (sub)population to
#' be used for LE estimation. Default is 4,000
#'
#' @export
#' @returns A data frame/data frame extension (e.g. a tibble) that can be used
#' to calculate a life table
#'
#' @examples
#'  set.seed(1)
#'ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
#'        "45-54","55-64","65-74","75-84","85+")
#'age_bins <- factor(ab,levels=ab)
#'p <- sample(2000:5000,length(age_bins))
#'pv <- rnorm(length(age_bins))
#'d <- sample(2:30,length(age_bins))

#'pops <- data.frame(ages=age_bins, p=p, pv=pv)
#'deaths <- data.frame(ages=age_bins, d=d)

#'lt <- life_table_prep(pops,
#'                      deaths,
#'                      age_bin_col="ages",
#'                      population_col="p",
#'                      deaths_col="d",
#'                      drop_inelig = FALSE)


life_table_prep <- function(pops,
                            deaths,
                            age_bin_col="age_bin",
                            population_col="population",
                            deaths_col="deaths",
                            group_vars=NULL,
                            drop_inelig=TRUE,
                            pop_threshhold=4000,
                            death_threshhold=20
                            ) {

  if (is.null(group_vars)){
    by <- age_bin_col
  }
  df_orig <- merge_pop_deaths(pops=pops,
                         deaths=deaths,
                         age_bin_col=age_bin_col,
                         by=group_vars)

  df_elig <- check_eligibility(df=df_orig,group_vars=group_vars,
                      population=population_col,
                      deaths=deaths_col,
                      pop_threshhold=pop_threshhold,
                      death_threshhold=death_threshhold)

  if (drop_inelig){
    df_elig |>
      dplyr::filter(!insuf_deaths & !insuf_pops & !zero_pop) |>
      dplyr::select(!c(insuf_deaths:zero_pop))
  } else{
    df_elig
  }

}
