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
#' with a column indicating age bins and a column indicating death counts. The
#' first row should be the age bin from 0-1.
#' @param pops A 2 column data frame/ data frame extension (e.g. a tibble),
#' with a column indicating age bins and a column indicating population counts.
#' The first row should be the age bin from 0-1. Age bins must be identical
#' to those in deaths.
#' @returns A data frame/data frame extension (e.g. a tibble)
#' @export
#'
#' @examples
#'  require(dplyr)
#'  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
#'  "45-54","55-64","65-74","75-84","85+")
#'  age_bins <- factor(ab,levels=ab)
#'  p <- sample(20:300,length(age_bins))
#'  pv <- rnorm(length(age_bins))
#'  d <- sample(2:30,length(age_bins))
#'
#'  pops <- tibble(ages=age_bins, p=p, pv=pv)
#'  deaths <- tibble(ages=age_bins, d=d)
#'  merge_pop_deaths(pops,deaths)
#'


merge_pop_deaths <- function(pops,deaths) {
  requireNamespace("dplyr", quietly = TRUE)

  if(nrow(deaths)!=nrow(pops)){
    stop("Both deaths and pops should have the same number of rows")
  }

  if(ncol(deaths)!=2 | !ncol(pops) %in% c(2,3)){
    stop("Deaths must have exactly two columns. Pops must have two or three
         columns.")
  }

  colnames(pops)[1] <- "age_bin"
  colnames(deaths)[1] <- "age_bin"
  colnames(pops)[2] <- "population"
  colnames(deaths)[2] <- "deaths"
  if(ncol(pops)==3){
    colnames(pops)[3] <- "population_var"

  }

  if (!identical(deaths$age_bin,pops$age_bin)){
    stop("Age bin names in deaths and pops must be identical")
  }

  dplyr::inner_join(pops,deaths)

}

