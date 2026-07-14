#' Fix column names
#'
#' @description
#' Change column names to snake case, remove or replace non-word characters,
#' and fix duplicates.
#'
#' @param x A vector of column names.
#' @param n_pfx A prefix added to column names starting with a digit.
#'
#' @returns A character vector the same length as `x`.
#' @export
#'
#' @examples
#' cols <- c(
#'   "Messy/Unpleasant Column Names",
#'   "checkThisCamelCase",
#'   "Letters674next0to827654digits4",
#'   "ordinal numbers: 1st 32nd 43rd 54th 900th",
#'   "this&THAT",
#'   "THIS/that",
#'   " Hello, World!!! ",
#'   "hyphenated-words",
#'   "words in 'quotes' or even \"quotes\"",
#'   "ACRONYMThenWord",
#'   "1"
#' )
#'
#' fix_colnames(cols, n_pfx = "n_")
#'
fix_colnames <- function(x, n_pfx = NULL) {
  chars1 <- paste(
    "\\.", ",", ":", ";", "\\?", "\\!", "\\*", "@", "#", "-",
    "\\[", "\\]", "\\{", "\\}", "\\(", "\\)",
    "\\|", "`", "=", "\\+", "\\^", "~", "<", ">", "\\$", "%",
    sep = "|"
  )

  chars2 <- paste("\\\"", "'", sep = "|")

  ordinals <- c(
    "(1[1-3])_(th_|th\\b)", # 11th, 12th, 13th
    "([2-9]?1)_(st_|st\\b)", # 1st, 21st...91st
    "([2-9]?2)_(nd_|nd\\b)", # 2nd, 22nd...92nd
    "([2-9]?3)_(rd_|rd\\b)", # 3rd, 23rd...93rd
    "([1-9]?[4-9])_(th_|th\\b)", # 4th-9th, 14th-19th...94th-99th
    "([1-9]0{0,10}0)_(th_|th\\b)" # 10th...900000000000th by 10s
  )

  x <- trimws(x, which = "both")

  x <- gsub("\\s", "_", x)

  # Separate camel case
  x <- gsub("([[:lower:]])([[:upper:]])", "\\1_\\2", x)

  # Separate uppercase sequence from a capitalized word
  x <- gsub("([[:upper:]])([[:upper:]][[:lower:]])", "\\1_\\2", x)

  # Separate digit + letter
  x <- gsub("(\\d)([[:alpha:]])", "\\1_\\2", x)

  x <- gsub("([[:alpha:]])(\\d)", "\\1_\\2", x)

  # "&" --> "and"
  x <- gsub("&", "_and_", x)

  # "/" --> "or"
  x <- gsub("/", "_or_", x)

  x <- gsub(chars1, "_", x)

  x <- gsub(chars2, "", x)

  # Remove multiples of "_"
  x <- gsub("_{2,}", "_", x)

  # Remove "_" at start or end
  x <-  gsub("^_|_$", "", x)

  x <- tolower(x)

  # Remove "_" when introduced in an ordinal number
  for (i in 1:length(ordinals)) {
    x <- gsub(ordinals[i], "\\1\\2", x, perl = TRUE)
  }

  # Add `n_pfx`
  if (!is.null(n_pfx)) {
    x <- gsub("(^\\d)", paste0(n_pfx, "\\1"), x, perl = TRUE)
  }

  # Number duplicate names
  if (any(duplicated(x))) {
    while (any(duplicated(x))) {
      dupes <- which(duplicated(x))

      i <- which(x == x[dupes[1]])

      x[i] <- paste0(x[i], seq_along(x[i]))
    }
  }

  x
}
