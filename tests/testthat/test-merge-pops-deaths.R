test_that("uneven row counts caught", {
  set.seed(1)
  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
          "45-54","55-64","65-74","75-84","85+")
  age_bins <- factor(ab,levels=ab)

  pops <- dplyr::tibble(ages=age_bins[0:-1],
                 p=sample(20:300,length(age_bins)-1))
  deaths <- dplyr::tibble(ages=age_bins,
                   d=sample(2:30,length(age_bins)))

  expect_error(merge_pop_deaths(pops,deaths))
})

test_that("unexpected col counts caught", {

  set.seed(1)
  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
          "45-54","55-64","65-74","75-84","85+")
  age_bins <- factor(ab,levels=ab)

  pops <- dplyr::tibble(ages=age_bins,
                 p=sample(20:300,length(age_bins)))

  deaths <- dplyr::tibble(ages=age_bins,
                   d=sample(2:30,length(age_bins)),
                   e=LETTERS[1:length(age_bins)])

  expect_error(merge_pop_deaths(pops,deaths))
})

test_that("merge provides expected output", {

  set.seed(1)
  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
          "45-54","55-64","65-74","75-84","85+")
  age_bins <- factor(ab,levels=ab)


  p <- sample(20:300,length(age_bins))
  pv <- rnorm(length(age_bins))
  d <- sample(2:30,length(age_bins))

  pops <- dplyr::tibble(ages=age_bins,
                 p=p,
                 pv=pv)
  deaths <- dplyr::tibble(ages=age_bins,
                   d=d)

  act <- merge_pop_deaths(pops,deaths)
  exp <- dplyr::tibble(age_bin=age_bins,
                population=p,
                population_var=pv,
                deaths=d)

  expect_equal(act,exp)
})



