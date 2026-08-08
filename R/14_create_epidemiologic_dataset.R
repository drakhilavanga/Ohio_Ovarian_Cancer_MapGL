# ============================================================
# 14_create_epidemiologic_dataset.R
#
# Join:
#   1. Ohio ovarian cancer incidence and mortality
#   2. USDA 2023 Rural–Urban Continuum Codes
#   3. CDC PLACES county contextual indicators
#
# Outputs:
#   - CSV analytical dataset
#   - GeoPackage spatial dataset
# ============================================================

library(tidyverse)
library(sf)
library(janitor)

# ============================================================
# 1. FILE PATHS
# ============================================================

cancer_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_combined.gpkg"
)

rucc_file <- file.path(
  "data",
  "processed",
  "ohio_rucc_2023_clean.csv"
)

places_file <- file.path(
  "data",
  "processed",
  "ohio_places_selected_wide.csv"
)

output_csv <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_epidemiologic_dataset.csv"
)

output_gpkg <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_epidemiologic_dataset.gpkg"
)

# ============================================================
# 2. CHECK REQUIRED FILES
# ============================================================

required_files <- c(
  cancer_file,
  rucc_file,
  places_file
)

missing_files <- required_files[
  !file.exists(required_files)
]

if (length(missing_files) > 0) {
  stop(
    paste0(
      "The following required files were not found:\n\n",
      paste(
        missing_files,
        collapse = "\n"
      )
    )
  )
}

dir.create(
  dirname(output_csv),
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# 3. READ DATA
# ============================================================

cancer <- st_read(
  cancer_file,
  quiet = TRUE
) |>
  clean_names()

rucc <- read_csv(
  rucc_file,
  show_col_types = FALSE
) |>
  clean_names()

places <- read_csv(
  places_file,
  show_col_types = FALSE
) |>
  clean_names()

cat("\nCancer columns:\n")
print(names(cancer))

cat("\nRUCC columns:\n")
print(names(rucc))

cat("\nPLACES columns:\n")
print(names(places))

# ============================================================
# 4. DETECT FIPS COLUMNS
# ============================================================

detect_fips_column <- function(
    data,
    dataset_name
) {
  available_columns <- names(data)
  
  candidate_columns <- c(
    "fips",
    "geoid",
    "geoid20",
    "geoid_20",
    "county_fips",
    "countyfips",
    "countyfp",
    "countyfp20",
    "location_id",
    "locationid"
  )
  
  detected <- candidate_columns[
    candidate_columns %in% available_columns
  ]
  
  if (length(detected) == 0) {
    stop(
      paste0(
        "\nNo county FIPS column was detected in ",
        dataset_name,
        ".\n\nAvailable columns:\n",
        paste(
          available_columns,
          collapse = ", "
        )
      )
    )
  }
  
  detected[[1]]
}

cancer_fips_column <- detect_fips_column(
  cancer,
  "the cancer dataset"
)

rucc_fips_column <- detect_fips_column(
  rucc,
  "the RUCC dataset"
)

places_fips_column <- detect_fips_column(
  places,
  "the PLACES dataset"
)

cat(
  "\nDetected cancer FIPS column: ",
  cancer_fips_column,
  "\n",
  sep = ""
)

cat(
  "Detected RUCC FIPS column: ",
  rucc_fips_column,
  "\n",
  sep = ""
)

cat(
  "Detected PLACES FIPS column: ",
  places_fips_column,
  "\n",
  sep = ""
)

# ============================================================
# 5. STANDARDIZE FIPS
# ============================================================

standardize_fips <- function(x) {
  x |>
    as.character() |>
    str_remove("\\.0+$") |>
    str_extract("\\d+") |>
    str_pad(
      width = 5,
      side = "left",
      pad = "0"
    )
}

cancer <- cancer |>
  mutate(
    fips = standardize_fips(
      .data[[cancer_fips_column]]
    )
  )

rucc <- rucc |>
  mutate(
    fips = standardize_fips(
      .data[[rucc_fips_column]]
    )
  )

places <- places |>
  mutate(
    fips = standardize_fips(
      .data[[places_fips_column]]
    )
  )

# ============================================================
# 6. STANDARDIZE COUNTY NAMES
# ============================================================

clean_county_name <- function(x) {
  x |>
    as.character() |>
    str_remove_all("\\(\\d+\\)") |>
    str_remove(
      regex(
        "\\s+county$",
        ignore_case = TRUE
      )
    ) |>
    str_squish() |>
    str_to_title()
}

if ("county" %in% names(cancer)) {
  cancer <- cancer |>
    mutate(
      county = clean_county_name(
        county
      )
    )
}

if ("county" %in% names(rucc)) {
  rucc <- rucc |>
    mutate(
      county = clean_county_name(
        county
      )
    )
}

if ("county" %in% names(places)) {
  places <- places |>
    mutate(
      county = clean_county_name(
        county
      )
    )
}

# ============================================================
# 7. CHECK FIPS QUALITY
# ============================================================

check_fips <- function(
    data,
    dataset_name
) {
  data_no_geometry <- if (inherits(data, "sf")) {
    st_drop_geometry(data)
  } else {
    data
  }
  
  duplicate_fips <- data_no_geometry |>
    count(
      fips,
      name = "rows"
    ) |>
    filter(
      rows > 1
    )
  
  missing_fips <- sum(
    is.na(data_no_geometry$fips) |
      data_no_geometry$fips == ""
  )
  
  cat(
    "\n",
    dataset_name,
    " summary:\n",
    sep = ""
  )
  
  print(
    data_no_geometry |>
      summarise(
        rows = n(),
        unique_fips = n_distinct(
          fips,
          na.rm = TRUE
        ),
        missing_fips = missing_fips
      )
  )
  
  if (nrow(duplicate_fips) > 0) {
    cat(
      "\nDuplicate FIPS values found in ",
      dataset_name,
      ":\n",
      sep = ""
    )
    
    print(duplicate_fips)
    
    stop(
      paste0(
        "Duplicate FIPS values detected in ",
        dataset_name,
        "."
      )
    )
  }
  
  if (missing_fips > 0) {
    stop(
      paste0(
        "Missing FIPS values detected in ",
        dataset_name,
        "."
      )
    )
  }
}

check_fips(
  cancer,
  "Cancer dataset"
)

check_fips(
  rucc,
  "RUCC dataset"
)

check_fips(
  places,
  "PLACES dataset"
)

# ============================================================
# 8. CHECK REQUIRED CANCER COLUMNS
# ============================================================

required_cancer_columns <- c(
  "county",
  "incidence_rate",
  "average_annual_cases",
  "mortality_rate",
  "average_annual_deaths"
)

missing_cancer_columns <- setdiff(
  required_cancer_columns,
  names(cancer)
)

if (length(missing_cancer_columns) > 0) {
  stop(
    paste0(
      "The cancer dataset is missing these columns:\n",
      paste(
        missing_cancer_columns,
        collapse = ", "
      )
    )
  )
}

# ============================================================
# 9. SELECT RUCC VARIABLES
# ============================================================

required_rucc_columns <- c(
  "fips",
  "population_2020",
  "rucc_2023",
  "rucc_description",
  "metro_status",
  "rurality_3_level",
  "rurality_ordinal"
)

missing_rucc_columns <- setdiff(
  required_rucc_columns,
  names(rucc)
)

if (length(missing_rucc_columns) > 0) {
  stop(
    paste0(
      "The RUCC dataset is missing these columns:\n",
      paste(
        missing_rucc_columns,
        collapse = ", "
      )
    )
  )
}

rucc_selected <- rucc |>
  select(
    fips,
    population_2020,
    rucc_2023,
    rucc_description,
    metro_status,
    rurality_3_level,
    rurality_ordinal
  )

# ============================================================
# 10. CHECK REQUIRED PLACES COLUMNS
# ============================================================

required_places_columns <- c(
  "fips",
  "prevalence_access2",
  "lower_ci_access2",
  "upper_ci_access2",
  "prevalence_bphigh",
  "lower_ci_bphigh",
  "upper_ci_bphigh",
  "prevalence_checkup",
  "lower_ci_checkup",
  "upper_ci_checkup",
  "prevalence_csmoking",
  "lower_ci_csmoking",
  "upper_ci_csmoking",
  "prevalence_depression",
  "lower_ci_depression",
  "upper_ci_depression",
  "prevalence_diabetes",
  "lower_ci_diabetes",
  "upper_ci_diabetes",
  "prevalence_foodinsecu",
  "lower_ci_foodinsecu",
  "upper_ci_foodinsecu",
  "prevalence_lacktrpt",
  "lower_ci_lacktrpt",
  "upper_ci_lacktrpt",
  "prevalence_lpa",
  "lower_ci_lpa",
  "upper_ci_lpa",
  "prevalence_mammouse",
  "lower_ci_mammouse",
  "upper_ci_mammouse",
  "prevalence_mhlth",
  "lower_ci_mhlth",
  "upper_ci_mhlth",
  "prevalence_obesity",
  "lower_ci_obesity",
  "upper_ci_obesity"
)

missing_places_columns <- setdiff(
  required_places_columns,
  names(places)
)

if (length(missing_places_columns) > 0) {
  stop(
    paste0(
      "The PLACES dataset is missing these columns:\n",
      paste(
        missing_places_columns,
        collapse = ", "
      )
    )
  )
}

# ============================================================
# 11. SELECT AND RENAME PLACES VARIABLES
# ============================================================

places_selected <- places |>
  select(
    all_of(
      required_places_columns
    )
  ) |>
  rename(
    uninsured_percent = prevalence_access2,
    uninsured_lower_ci = lower_ci_access2,
    uninsured_upper_ci = upper_ci_access2,
    
    high_blood_pressure_percent = prevalence_bphigh,
    high_blood_pressure_lower_ci = lower_ci_bphigh,
    high_blood_pressure_upper_ci = upper_ci_bphigh,
    
    annual_checkup_percent = prevalence_checkup,
    annual_checkup_lower_ci = lower_ci_checkup,
    annual_checkup_upper_ci = upper_ci_checkup,
    
    smoking_percent = prevalence_csmoking,
    smoking_lower_ci = lower_ci_csmoking,
    smoking_upper_ci = upper_ci_csmoking,
    
    depression_percent = prevalence_depression,
    depression_lower_ci = lower_ci_depression,
    depression_upper_ci = upper_ci_depression,
    
    diabetes_percent = prevalence_diabetes,
    diabetes_lower_ci = lower_ci_diabetes,
    diabetes_upper_ci = upper_ci_diabetes,
    
    food_insecurity_percent = prevalence_foodinsecu,
    food_insecurity_lower_ci = lower_ci_foodinsecu,
    food_insecurity_upper_ci = upper_ci_foodinsecu,
    
    transportation_barriers_percent = prevalence_lacktrpt,
    transportation_barriers_lower_ci = lower_ci_lacktrpt,
    transportation_barriers_upper_ci = upper_ci_lacktrpt,
    
    physical_inactivity_percent = prevalence_lpa,
    physical_inactivity_lower_ci = lower_ci_lpa,
    physical_inactivity_upper_ci = upper_ci_lpa,
    
    mammography_percent = prevalence_mammouse,
    mammography_lower_ci = lower_ci_mammouse,
    mammography_upper_ci = upper_ci_mammouse,
    
    frequent_mental_distress_percent = prevalence_mhlth,
    frequent_mental_distress_lower_ci = lower_ci_mhlth,
    frequent_mental_distress_upper_ci = upper_ci_mhlth,
    
    obesity_percent = prevalence_obesity,
    obesity_lower_ci = lower_ci_obesity,
    obesity_upper_ci = upper_ci_obesity
  )

# ============================================================
# 12. JOIN DATASETS
# ============================================================

epidemiologic_sf <- cancer |>
  left_join(
    rucc_selected,
    by = "fips"
  ) |>
  left_join(
    places_selected,
    by = "fips"
  )

# ============================================================
# 13. CREATE DERIVED VARIABLES
# ============================================================

epidemiologic_sf <- epidemiologic_sf |>
  mutate(
    incidence_rate = suppressWarnings(
      as.numeric(incidence_rate)
    ),
    
    mortality_rate = suppressWarnings(
      as.numeric(mortality_rate)
    ),
    
    average_annual_cases = suppressWarnings(
      as.numeric(average_annual_cases)
    ),
    
    average_annual_deaths = suppressWarnings(
      as.numeric(average_annual_deaths)
    ),
    
    population_2020 = suppressWarnings(
      as.numeric(population_2020)
    ),
    
    incidence_reportable = !is.na(incidence_rate),
    mortality_reportable = !is.na(mortality_rate),
    
    incidence_suppressed = is.na(incidence_rate),
    mortality_suppressed = is.na(mortality_rate),
    
    both_rates_reportable =
      incidence_reportable &
      mortality_reportable,
    
    neither_rate_reportable =
      !incidence_reportable &
      !mortality_reportable,
    
    surveillance_availability = case_when(
      incidence_reportable &
        mortality_reportable ~
        "Both incidence and mortality reportable",
      
      incidence_reportable &
        !mortality_reportable ~
        "Incidence only reportable",
      
      !incidence_reportable &
        mortality_reportable ~
        "Mortality only reportable",
      
      TRUE ~
        "Neither reportable"
    ),
    
    incidence_difference_from_ohio =
      incidence_rate - 9.8,
    
    mortality_difference_from_ohio =
      mortality_rate - 5.8,
    
    incidence_vs_ohio = case_when(
      is.na(incidence_rate) ~
        "Suppressed or unavailable",
      
      incidence_rate > 9.8 ~
        "Above Ohio point estimate",
      
      incidence_rate < 9.8 ~
        "Below Ohio point estimate",
      
      TRUE ~
        "Equal to Ohio point estimate"
    ),
    
    mortality_vs_ohio = case_when(
      is.na(mortality_rate) ~
        "Suppressed or unavailable",
      
      mortality_rate > 5.8 ~
        "Above Ohio point estimate",
      
      mortality_rate < 5.8 ~
        "Below Ohio point estimate",
      
      TRUE ~
        "Equal to Ohio point estimate"
    ),
    
    population_group = case_when(
      is.na(population_2020) ~
        NA_character_,
      
      population_2020 < 50000 ~
        "Less than 50,000",
      
      population_2020 < 100000 ~
        "50,000–99,999",
      
      population_2020 < 250000 ~
        "100,000–249,999",
      
      TRUE ~
        "250,000 or more"
    ),
    
    population_group = factor(
      population_group,
      levels = c(
        "Less than 50,000",
        "50,000–99,999",
        "100,000–249,999",
        "250,000 or more"
      ),
      ordered = TRUE
    ),
    
    metro_status = factor(
      metro_status,
      levels = c(
        "Metropolitan",
        "Nonmetropolitan"
      )
    ),
    
    rurality_3_level = factor(
      rurality_3_level,
      levels = c(
        "Metropolitan",
        "Nonmetropolitan adjacent to metro",
        "Nonmetropolitan nonadjacent"
      ),
      ordered = TRUE
    )
  )

# ============================================================
# 14. JOIN VALIDATION
# ============================================================

join_summary <- epidemiologic_sf |>
  st_drop_geometry() |>
  summarise(
    total_counties = n(),
    
    unique_fips = n_distinct(
      fips
    ),
    
    rucc_available = sum(
      !is.na(rucc_2023)
    ),
    
    places_available = sum(
      !is.na(smoking_percent)
    ),
    
    incidence_available = sum(
      incidence_reportable
    ),
    
    mortality_available = sum(
      mortality_reportable
    ),
    
    both_available = sum(
      both_rates_reportable
    ),
    
    neither_available = sum(
      neither_rate_reportable
    )
  )

cat(
  "\nCombined epidemiologic dataset summary:\n"
)

print(
  join_summary
)

if (nrow(epidemiologic_sf) != 88) {
  warning(
    paste0(
      "Expected 88 Ohio counties but found ",
      nrow(epidemiologic_sf),
      "."
    )
  )
}

if (join_summary$unique_fips != 88) {
  warning(
    paste0(
      "Expected 88 unique county FIPS values but found ",
      join_summary$unique_fips,
      "."
    )
  )
}

if (join_summary$rucc_available != 88) {
  warning(
    paste0(
      "RUCC matched ",
      join_summary$rucc_available,
      " of 88 counties."
    )
  )
}

if (join_summary$places_available != 88) {
  warning(
    paste0(
      "PLACES matched ",
      join_summary$places_available,
      " of 88 counties."
    )
  )
}

# ============================================================
# 15. IDENTIFY UNMATCHED COUNTIES
# ============================================================

unmatched_rucc <- epidemiologic_sf |>
  filter(
    is.na(rucc_2023)
  ) |>
  st_drop_geometry() |>
  select(
    county,
    fips
  )

unmatched_places <- epidemiologic_sf |>
  filter(
    is.na(smoking_percent)
  ) |>
  st_drop_geometry() |>
  select(
    county,
    fips
  )

if (nrow(unmatched_rucc) > 0) {
  cat(
    "\nCounties not matched to RUCC:\n"
  )
  
  print(
    unmatched_rucc
  )
}

if (nrow(unmatched_places) > 0) {
  cat(
    "\nCounties not matched to PLACES:\n"
  )
  
  print(
    unmatched_places
  )
}

# ============================================================
# 16. PORTAGE COUNTY PROFILE
# ============================================================

cat(
  "\nPortage County epidemiologic profile:\n"
)

portage_profile <- epidemiologic_sf |>
  filter(
    county == "Portage"
  ) |>
  st_drop_geometry() |>
  select(
    county,
    fips,
    population_2020,
    rucc_2023,
    rucc_description,
    metro_status,
    rurality_3_level,
    
    incidence_rate,
    average_annual_cases,
    incidence_reportable,
    incidence_difference_from_ohio,
    
    mortality_rate,
    average_annual_deaths,
    mortality_reportable,
    mortality_difference_from_ohio,
    
    smoking_percent,
    obesity_percent,
    physical_inactivity_percent,
    diabetes_percent,
    uninsured_percent,
    annual_checkup_percent,
    mammography_percent,
    high_blood_pressure_percent,
    depression_percent,
    frequent_mental_distress_percent,
    food_insecurity_percent,
    transportation_barriers_percent
  )

if (nrow(portage_profile) == 0) {
  warning("Portage County was not found in the joined dataset.")
} else {
  cat("\nPortage County profile by variable:\n")
  
  for (variable_name in names(portage_profile)) {
    value <- portage_profile[[variable_name]][1]
    
    if (length(value) == 0 || is.na(value)) {
      value_text <- "NA"
    } else {
      value_text <- as.character(value)
    }
    
    cat(
      sprintf(
        "%-42s %s\n",
        paste0(variable_name, ":"),
        value_text
      )
    )
  }
}

# ============================================================
# 17. SAVE CSV
# ============================================================

write_csv(
  epidemiologic_sf |>
    st_drop_geometry(),
  output_csv,
  na = ""
)

# ============================================================
# 18. SAVE GEOPACKAGE
# ============================================================

if (file.exists(output_gpkg)) {
  file.remove(
    output_gpkg
  )
}

st_write(
  epidemiologic_sf,
  output_gpkg,
  quiet = TRUE
)

# ============================================================
# 19. FINAL OUTPUT MESSAGE
# ============================================================

cat(
  "\nSaved epidemiologic CSV:\n",
  output_csv,
  "\n",
  sep = ""
)

cat(
  "\nSaved epidemiologic GeoPackage:\n",
  output_gpkg,
  "\n",
  sep = ""
)

cat(
  "\nScript completed successfully.\n"
)