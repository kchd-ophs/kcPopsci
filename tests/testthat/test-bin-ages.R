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

test_that("lifex5", {
  act <- bin_ages(0:100, breaks = "lifex5")

  exp <- c(
    "0", rep("1-4", 4), rep("5-9", 5), rep("10-14", 5), rep("15-19", 5),
    rep("20-24", 5), rep("25-29", 5), rep("30-34", 5), rep("35-39", 5),
    rep("40-44", 5), rep("45-49", 5), rep("50-54", 5), rep("55-59", 5),
    rep("60-64", 5), rep("65-69", 5), rep("70-74", 5), rep("75-79", 5),
    rep("80-84", 5), rep("85+", 16)
  )

  exp <- factor(exp, levels = unique(exp))

  expect_equal(act, exp)
})

test_that("lifex10", {
  act <- bin_ages(0:100, breaks = "lifex10")

  exp <- c(
    "0", rep("1-4", 4), rep("5-14", 10), rep("15-24", 10), rep("25-34", 10),
    rep("35-44", 10), rep("45-54", 10), rep("55-64", 10), rep("65-74", 10),
    rep("75-84", 10), rep("85+", 16)
  )

  exp <- factor(exp, levels = unique(exp))

  expect_equal(act, exp)
})

test_that("`breaks` error", {
  expect_error(
    bin_ages(0:100, breaks = "?"),
    "Invalid `breaks` value. See function documentation."
  )
})
