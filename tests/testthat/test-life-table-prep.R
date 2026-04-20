#TODO: tests could be made more robust by checking actual results rather than
# just row counts
test_that("function runs", {
  set.seed(1)
  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
  "45-54","55-64","65-74","75-84","85+")
  age_bins <- factor(ab,levels=ab)
  p <- sample(2000:5000,length(age_bins))
  pv <- rnorm(length(age_bins))
  d <- sample(2:30,length(age_bins))

  pops <- tibble(ages=age_bins, p=p, pv=pv)
  deaths <- tibble(ages=age_bins, d=d)

  lt <- life_table_prep(pops,
                        deaths,
                        age_bin_col="ages",
                        population_col="p",
                        deaths_col="d",
                        drop_inelig = FALSE)
  testthat::expect_equal(nrow(lt),length(ab))

})

test_that("group properly kept/removed", {
  set.seed(1)
  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
          "45-54","55-64","65-74","75-84","85+")
  age_bins <- factor(ab,levels=ab)
  groups <- c("A","B")
  nbins <- length(age_bins)
  ngroups <- length(groups)

  p <- c(sample(200:500,nbins*ngroups-1),0)
  d <- sample(2:30,nbins*ngroups)

  pops <- tibble(ages=rep(age_bins,ngroups), g=rep(groups,each=nbins), p=p)
  deaths <- tibble(ages=rep(age_bins,ngroups), g=rep(groups,each=nbins), d=d)

  lt <- life_table_prep(pops,
                        deaths,
                        age_bin_col="ages",
                        population_col="p",
                        deaths_col="d",
                        group_vars = "g",
                        drop_inelig = TRUE)
  testthat::expect_equal(nrow(lt),nbins)
  lt2 <- life_table_prep(pops,
                        deaths,
                        age_bin_col="ages",
                        population_col="p",
                        deaths_col="d",
                        group_vars = "g",
                        drop_inelig = FALSE)
  testthat::expect_equal(nrow(lt2),nbins*ngroups)


})

test_that("extra argument passed as expected", {
  set.seed(1)
  ab <- c("0-1","1-4","5-14","15-24","25-34","35-44",
          "45-54","55-64","65-74","75-84","85+")
  age_bins <- factor(ab,levels=ab)
  groups <- c("A","B")
  nbins <- length(age_bins)
  ngroups <- length(groups)

  p <- sample(200:500,nbins*ngroups)
  d <- sample(2:30,nbins*ngroups)


  pops <- tibble(ages=rep(age_bins,ngroups), g=rep(groups,each=nbins), p=p)
  deaths <- tibble(ages=rep(age_bins,ngroups), g=rep(groups,each=nbins), d=d)

  lt <- life_table_prep(pops,
                        deaths,
                        age_bin_col="ages",
                        population_col="p",
                        deaths_col="d",
                        group_vars = "g",
                        drop_inelig = TRUE,
                        death_threshhold=6000000)

  testthat::expect_equal(nrow(lt),0)
})
