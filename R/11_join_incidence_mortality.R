# ============================================================
# JOIN INCIDENCE AND MORTALITY TO OHIO COUNTY GEOMETRY
# ============================================================

library(tidyverse)
library(sf)
library(janitor)

incidence_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_incidence_clean.csv"
)

mortality_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_mortality_clean.csv"
)

county_file <- file.path(
  "data",
  "processed",
  "ohio_counties.gpkg"
)

output_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_combined.gpkg"
)

required_files <- c(
  incidence_file,
  mortality_file,
  county_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    "Missing required files:\n",
    paste(
      missing_files,
      collapse = "\n"
    )
  )
}

incidence <- read_csv(
  incidence_file,
  show_col_types = FALSE
) |>
  clean_names()

mortality <- read_csv(
  mortality_file,
  show_col_types = FALSE
) |>
  clean_names()

ohio_counties <- st_read(
  county_file,
  quiet = TRUE
) |>
  st_transform(4326)

combined_data <- incidence |>
  full_join(
    mortality,
    by = "county"
  )

ohio_combined <- ohio_counties |>
  left_join(
    combined_data,
    by = "county"
  )

join_summary <- ohio_combined |>
  st_drop_geometry() |>
  summarise(
    total_counties = n(),
    incidence_available = sum(
      !is.na(incidence_rate)
    ),
    mortality_available = sum(
      !is.na(mortality_rate)
    ),
    both_available = sum(
      !is.na(incidence_rate) &
        !is.na(mortality_rate)
    )
  )

print(join_summary)

cat("\nPortage County combined profile:\n")

print(
  ohio_combined |>
    filter(county == "Portage") |>
    st_drop_geometry() |>
    select(
      county,
      incidence_rate,
      average_annual_cases,
      mortality_rate,
      average_annual_deaths
    )
)

st_write(
  ohio_combined,
  output_file,
  delete_dsn = TRUE,
  quiet = TRUE
)

write_csv(
  st_drop_geometry(ohio_combined),
  file.path(
    "data",
    "processed",
    "ohio_ovarian_cancer_combined.csv"
  )
)

message(
  "\nCombined spatial dataset saved to:\n",
  output_file
)