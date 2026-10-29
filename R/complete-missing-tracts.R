#' Clean tract information and complete missing tracts in vital statistic files
#' based on coordinates
#'
#' @description
#' Get clean, full 11 digit tract FIPS codes from vital statistics files based
#' on the original 3 digit FIPS county code, and 6 digit FIPS tract code.
#' Then completes missing tract data based on lat/lon locations.
#' If both listed tract information and lat/lon locations are missing, defaults
#' to removing record. Set drop_missing_tracts=FALSE to keep all tracts
#'
#'
#' @param df A dataframe with vital statistics data
#' @param tract_col The (unquoted) name of the column with 6 digit tract fips
#' @param county_col The (unquoted) name of the column with 3 digit county fips
#' @param coord_vec Character vector with the names of coordinates in the form c("lon", "lat")
#' @param geo An sf object with tract boundaries. by default this is an object called tracts_kc_geo,
#' which can be called from e.g. kcData::get_kc_sf(geo_type="tracts",year=2020)
#' @param geoid_col The (unquoted) desired name of the output column of the full 11 digit tract code
#' @param drop_missing_tracts If TRUE (the default), drops records where no tract can be identified.
#'
#' @returns df with a column specified by geoid_col that contains 11 digit tract fips codes
#' @importFrom rlang :=
#' @export
#'

complete_missing_tracts <- function(
  df,
  tract_col,
  county_col,
  coord_vec,
  geo = tracts_kc_geo,
  geoid_col = GEOID,
  drop_missing_tracts = TRUE
) {
  pkg <- requireNamespace("sf", quietly = TRUE)

  if (!pkg) {
    stop("The sf package must be installed to use this function")
  }

  # ensure that geoid$geoid_col is a character
  # (can cause issues if it is a factor)
  geo <- geo |>
    dplyr::mutate("{{geoid_col}}" := as.character({{ geoid_col }}))

  # assemble tract label based on county and tract ids
  df <- df |>
    dplyr::mutate(
      listed_tract = dplyr::case_when(
        !is.na({{ tract_col }}) & {{ tract_col }} != "000000" ~
          paste0("29", {{ county_col }}, {{ tract_col }})
      )
    ) |>
    dplyr::mutate(row_number = row_number())

  # identify tracts available from locations
  df_sf <- df |>
    dplyr::filter(!is.na(!!sym(coord_vec[1]))) |>
    sf::st_as_sf(coords = coord_vec, crs = sf::st_crs(geo))

  # final count of deaths by tracts
  df_tracts <- df_sf |>
    sf::st_join(geo) |>
    dplyr::rename(tract_by_coords = {{ geoid_col }}) #|>
  # dplyr::select(row_number, tract_by_coords) |>
  # sf::st_drop_geometry()

  df_tracts <- sf::st_drop_geometry(df_tracts[, c(
    "row_number",
    "tract_by_coords"
  )])

  # add tract by location
  df <- df |>
    dplyr::left_join(df_tracts, by = "row_number")

  # update missing tracts based on locations
  df <- df |>
    dplyr::mutate(
      "{{geoid_col}}" := ifelse(
        is.na(.data$listed_tract),
        .data$tract_by_coords,
        .data$listed_tract
      )
    )

  # optionally drop missing tracts
  if (drop_missing_tracts) {
    df <- df |> dplyr::filter(!is.na({{ geoid_col }}))
  }
  df
}
