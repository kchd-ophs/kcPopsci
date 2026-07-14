test_that("replacements", {
  s <- paste(
    "Some words and stuff plus punctuation",
    ". , : ; ? ! / * @ # - _ \" ' [ ] { } ( )",
    "And symbols",
    "| ` = + ^ ~ < > $ %",
    "checkThisCamelCase",
    "Letters674next0to827654digits4",
    "this&THAT",
    "THIS/that",
    "ACRONYMThenWord"
  )
  act <- fix_colnames(s)
  exp <- paste0(
    "some_words_and_stuff_plus_punctuation",
    "_or_",
    "and_symbols_",
    "check_this_camel_case_",
    "letters_674_next_0_to_827654_digits_4_",
    "this_and_that_",
    "this_or_that_",
    "acronym_then_word"
  )
  expect_equal(act, exp)
})

test_that("ordinals", {
  s <- c(
    "1st 2nd 3rd 4th 5th 11th 12th 13th 14th 20th 21st 32nd 43rd 54th 900th",
    "1and2",
    " 303rd ",
    "10001st 404 20002nd"
  )
  act <- fix_colnames(s)
  exp <- c(
    "1st_2nd_3rd_4th_5th_11th_12th_13th_14th_20th_21st_32nd_43rd_54th_900th",
    "1_and_2",
    "303rd",
    "10001st_404_20002nd"
  )
  expect_equal(act, exp)
})

test_that("punctuation", {
  s <- c(
    "Messy/Unpleasant Column Names",
    " Hello, World!!!",
    "hyphenated-words",
    "words in 'quotes' or even \"quotes\"",
    "_!_"
  )
  act <- fix_colnames(s)
  exp <- c(
    "messy_or_unpleasant_column_names",
    "hello_world",
    "hyphenated_words",
    "words_in_quotes_or_even_quotes",
    ""
  )
  expect_equal(act, exp)
})

test_that("`n_pfx` char", {
  s <- c("1", "2two", "three")
  act <- fix_colnames(s, n_pfx = "n_")
  exp <- c("n_1", "n_2_two", "three")
  expect_equal(act, exp)
})

test_that("`n_pfx` num", {
  s <- 1:2
  act <- fix_colnames(s, n_pfx = "n_")
  exp <- c("n_1", "n_2")
  expect_equal(act, exp)
})

test_that("duplicates", {
  s <- c("cat", "dog", "horse", "iguana", "horse", "iguana", "horse")
  act <- fix_colnames(s)
  exp <- c("cat", "dog", "horse1", "iguana1", "horse2", "iguana2", "horse3")
  expect_equal(act, exp)
})
