test_that("assembly works without disagreggate", {

  test_df <- tidyr::tibble(
    AGE=c(69,40,2,35,0,88,4,15,83,60,52),
    SEX=c("M","M","F","F","M","F","M","M","F","M","F")
  )

  expected <- dplyr::tibble(
    age_bin=factor(
      c("0","1-4","5-14","15-24","25-34","35-44","45-54","55-64",
        "65-74","75-84","85+"),
      levels = c("0","1-4","5-14","15-24","25-34","35-44","45-54","55-64",
                 "65-74","75-84","85+")
    ),
    n=c(1,2,0,1,0,2,1,1,1,1,1)
  )
  object <- assemble_deaths(deaths=test_df,
                            age_col=AGE,
                            breaks="lifex10")

  expect_equal(object,expected)
})

test_that("assembly works with disagreggate", {

  test_df <- tidyr::tibble(
    AGE=c(69,40,2,35,0,88,4,15,83,60,52),
    SEX=c("M","M","F","F","M","F","M","M","F","M","F")
  )
  #TODO: FIX EXPECTED
  expected <- dplyr::tibble(
    age_bin=rep(
      factor(
        c("0","1-4","5-14","15-24","25-34","35-44","45-54","55-64",
          "65-74","75-84","85+"),
      levels = c("0","1-4","5-14","15-24","25-34","35-44","45-54","55-64",
                 "65-74","75-84","85+")
      ),
    2),
    SEX=rep(c("F","M"),each=11),
    n=c(0,1,0,0,0,1,1,0,0,1,1,
        1,1,0,1,0,1,0,1,1,0,0)
  )
  object <- assemble_deaths(test_df,
                            age_col=AGE,
                            SEX,
                            breaks="lifex10")

  expect_equal(object,expected)
})
