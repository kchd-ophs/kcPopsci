test_that("age_bin_col missing from dataset caught", {
  set.seed(1)
  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
          "45-54","55-64","65-74","75-84","85+")
  age_bins <- factor(ab,levels=ab)

  pops <- dplyr::tibble(ppl_age=age_bins,
                        p=sample(20:300,length(age_bins)))
  deaths <- dplyr::tibble(ages=age_bins,
                          d=sample(2:30,length(age_bins)))

  expect_error(merge_pop_deaths(pops,deaths,age_bin_col="ages"))
})


test_that("uneven row counts caught", {
  set.seed(1)
  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
          "45-54","55-64","65-74","75-84","85+")
  age_bins <- factor(ab,levels=ab)

  pops <- dplyr::tibble(ages=age_bins[0:-1],
                 p=sample(20:300,length(age_bins)-1))
  deaths <- dplyr::tibble(ages=age_bins,
                   d=sample(2:30,length(age_bins)))

  expect_error(merge_pop_deaths(pops,deaths,age_bin_col="ages"))
})


test_that("merge provides expected output WITHOUT grouping", {

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

  act <- merge_pop_deaths(pops,deaths,age_bin_col="ages")
  exp <- dplyr::tibble(age_bin=age_bins,
                p=p,
                pv=pv,
                d=d)

  expect_equal(act,exp)
})


test_that("merge provides expected output WITH grouping", {

  set.seed(1)
  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
          "45-54","55-64","65-74","75-84","85+")
  age_bins <- factor(ab,levels=ab)
  sexes <- c("Male","Female")
  n_ages <- length(ab)
  n_groups <- length(sexes)



  p <- sample(20:300, n_ages*n_groups)
  pv <- rnorm(n_ages*n_groups)
  d <- sample(2:30,n_ages*n_groups)

  pops <- dplyr::tibble(ages=rep(age_bins,n_groups),
                        sex=rep(sexes,each=n_ages),
                        p=p,
                        pv=pv)
  deaths <- dplyr::tibble(ages=rep(age_bins,n_groups),
                          sex=rep(sexes,each=n_ages),
                          d=d
  )

  act <- merge_pop_deaths(pops,deaths,age_bin_col="ages",by="sex")
  exp <- dplyr::tibble(age_bin=rep(age_bins,n_groups),
                       sex=rep(sexes,each=n_ages),
                       p=p,
                       pv=pv,
                       d=d)

  expect_equal(act,exp)
})



