library(tidyverse)
library(sf)
library(janitor)

incidence <- read_csv(
  "data/processed/ohio_ovarian_cancer_incidence_clean.csv",
  show_col_types = FALSE
) |>
  clean_names()

ohio_counties <- st_read(
  "data/processed/ohio_counties.gpkg",
  quiet = TRUE
)

ohio_ovarian_sf <- ohio_counties |>
  left_join(
    incidence,
    by = "county"
  )

join_check <- ohio_ovarian_sf |>
  st_drop_geometry() |>
  summarise(
    total_counties = n(),
    counties_with_rate = sum(!is.na(incidence_rate)),
    counties_missing_rate = sum(is.na(incidence_rate))
  )

print(join_check)

unmatched_incidence <- incidence |>
  anti_join(
    st_drop_geometry(ohio_counties),
    by = "county"
  )

if (nrow(unmatched_incidence) > 0) {
  warning("Some cancer rows did not match county geometry:")
  print(unmatched_incidence$county)
}

st_write(
  ohio_ovarian_sf,
  "data/processed/ohio_ovarian_cancer_incidence.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

write_csv(
  st_drop_geometry(ohio_ovarian_sf),
  "data/processed/ohio_ovarian_cancer_analysis.csv"
)

message("Cancer data successfully joined to Ohio counties.")