test_that("cols_added", {
  names_act <- check_eligibility(tot_init) |> names()
  names_exp <- c("age_bin","deaths","population",
                 "insuf_deaths","insuf_pops","zero_pop")

  expect_equal(names_act, names_exp)
})

test_that("insufficient deaths caught", {

  # calc
  tracts_elig <- check_eligibility(tracts_init,group_vars = "GEOID")
  all_tracts <- unique(tracts_init$GEOID)

  # actual values
  inelig_df <- tracts_elig |>
    dplyr::filter(insuf_deaths)

  inelig_act <- unique(inelig_df$GEOID)
  elig_act <- all_tracts[!inelig_act %in% all_tracts]

  # expected values
  inelig_exp <- "29095000100"
  elig_exp <- all_tracts[!inelig_exp %in% all_tracts]

  expect_equal(inelig_act, inelig_exp)
  expect_equal(elig_act, elig_exp)
})

test_that("insufficient pops caught", {

  # calc
  tracts_elig <- check_eligibility(tracts_init,group_vars = "GEOID")
  all_tracts <- unique(tracts_init$GEOID)

  # actual values
  inelig_df <- tracts_elig |>
    dplyr::filter(insuf_pops)

  inelig_act <- unique(inelig_df$GEOID)
  elig_act <- all_tracts[!inelig_act %in% all_tracts]

  # expected values
  inelig_exp <- "29095000100"
  elig_exp <- all_tracts[!inelig_exp %in% all_tracts]

  expect_equal(inelig_act, inelig_exp)
  expect_equal(elig_act, elig_exp)

})

test_that("zero/neg pops caught", {

  # calc
  elig_init <- check_eligibility(demo_init,
                                 group_vars = c("sex","race_ethnicity")) |>
    mutate(comb = paste(sex,race_ethnicity,sep="-"))
  all_combs <- unique(elig_init$comb)


  inelig_df <- elig_init |>
    dplyr::filter(zero_pop)

  inelig_act <- unique(inelig_df$comb)
  elig_act <- all_combs[!inelig_act %in% all_combs]

  # expected values
  inelig_exp <- "Female-Asian"
  elig_exp <- all_combs[!inelig_exp %in% all_combs]

  expect_equal(inelig_act, inelig_exp)
  expect_equal(elig_act, elig_exp)

})

