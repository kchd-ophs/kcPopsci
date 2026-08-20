# Create `kc_hospitals`

kc_hospitals <- read.csv("data-raw/kc-hospitals.csv")

usethis::use_data(kc_hospitals, overwrite = TRUE)
