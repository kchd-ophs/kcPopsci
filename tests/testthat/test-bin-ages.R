test_that("2-year bins", {
  act <- bin_ages(seq(0, 5, .5), breaks = seq(0, 5, 2))

  exp <- as.factor(c(rep("0-1", 4), rep("2-3", 4), rep("4+", 3)))

  expect_equal(act, exp)
})

test_that("variable size bins", {
  act <- bin_ages(seq(0, 5, .5), breaks = c(0, 2, 3))

  exp <- as.factor(c(rep("0-1", 4), rep("2", 2), rep("3+", 5)))

  expect_equal(act, exp)
})
