test_that("error for incorrect width", {
  ages <- sample(0:100,10,replace=TRUE)
  expect_error(make_age_bins(ages,7))
})
