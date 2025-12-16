test_that("multiplication works", {
  act <- calc_mortality_rate(10, 1000)
  exp <- .01
  expect_equal(act, exp)
})
