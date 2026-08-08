library(sf)
library(tigris)
library(tidyverse)

options(
  tigris_use_cache = TRUE
)

ohio_counties <- counties(
  state = "OH",
  cb = TRUE,
  year = 2024,
  class = "sf"
) |>
  transmute(
    geoid = GEOID,
    county = NAME,
    geometry
  ) |>
  st_transform(4326)

if (nrow(ohio_counties) != 88) {
  warning(
    "Expected 88 Ohio counties but found ",
    nrow(ohio_counties)
  )
}

st_write(
  ohio_counties,
  "data/processed/ohio_counties.gpkg",
  delete_dsn = TRUE,
  quiet = TRUE
)

message(
  "Ohio county boundaries saved successfully."
)