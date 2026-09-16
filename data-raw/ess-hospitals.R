# Create `ess_hospitals`

library(tidygeocoder)

hosp <- read.csv("data-raw/kc-metro-hospitals.csv")

geo <- geocode(
  hosp,
  street = street,
  city = city,
  state = state,
  postalcode = zip,
  method = "arcgis",
  full_results = TRUE
)

ess_hospitals <- geo |>
  dplyr::select(
    essence_name, essence_id, name, kc, lat, long,
    street = attributes.StAddr,
    city = attributes.City,
    state = attributes.RegionAbbr,
    zipcode = attributes.Postal,
    county = attributes.Subregion
  ) |>
  dplyr::mutate(county = sub(" County", "", county))

usethis::use_data(ess_hospitals, overwrite = TRUE)
