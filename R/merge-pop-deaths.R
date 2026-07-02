#' Merge population and deaths by age bin
#'
#' @description
#' Ensures that the age bins in deaths and population are equal,
#' then merges them so that a life table can be calculated
#'
#' @details
#' that they can be used for life table calculation.
#'
#' @param deaths A 2 column data frame/ data frame extension (e.g. a tibble),
#' with a column indicating age bins ("age_bin") and a column indicating death counts. The
#' first row should be the age bin from 0-1.
#' @param pops A 2 column data frame/ data frame extension (e.g. a tibble),
#' with a column indicating age bins ("age_bin") and a column indicating population counts.
#' The first row should be the age bin from 0-1. Age bins must be identical
#' to those in deaths.
#' @param age_bin_col Character indicating the name of the column with age bins
#' @param by A vector with the names of the grouping columns to merge by.
#' Argument does not need to include age_bin. If empty, no grouping variables will
#' be assumed.
#' @param strict A logical value. If TRUE (the default), returns an error if
#' certain assumptions about deaths and pops are not met. Otherwise, returns
#' a warning.
#'
#' @export
#' @returns A data frame/data frame extension (e.g. a tibble)
#'
#' @examples
#'  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
#'  "45-54","55-64","65-74","75-84","85+")
#'  age_bins <- factor(ab,levels=ab)
#'  p <- sample(20:300,length(age_bins))
#'  pv <- rnorm(length(age_bins))
#'  d <- sample(2:30,length(age_bins))
#'
#'  pops <- dplyr::tibble(ages=age_bins, p=p, pv=pv)
#'  deaths <- dplyr::tibble(ages=age_bins, d=d)
#'  merge_pop_deaths(pops,deaths,age_bin_col="ages")
#'


merge_pop_deaths <- function(pops,deaths,age_bin_col="age_bin",
                             by=NULL,strict=TRUE) {

  if (!age_bin_col %in% names(pops) | !age_bin_col %in% names(pops)) {
    if(strict){
      stop("The age_bin_col must be present in both pops and deaths.")
    } else{
      warning("The age_bin_col is not present in both pops and deaths.")
    }
  }
  if (age_bin_col != "age_bin") {
    names(pops)[names(pops) == age_bin_col] <- "age_bin"
    names(deaths)[names(deaths) == age_bin_col] <- "age_bin"
  }
  if (is.null(by)) {
    by <- "age_bin"
  }
  else {
    by <- c("age_bin", by)
  }
  if (nrow(deaths) != nrow(pops)) {
    if(strict){
      stop("Both deaths and pops should have the same number of rows")
    } else{
      warning("Deaths and pops do not have the same number of rows")
    }
  }
  if (!identical(deaths$age_bin, pops$age_bin)) {
    if(strict){
      stop("Age bin names in deaths and pops must be identical")
    } else{
      warning("Age bin names in deaths and pops are not identical")
    }
  }
  dplyr::inner_join(pops, deaths, by = by) |>
    mutate(across(where(is.numeric), ~replace_na(., 0)))
}

