# ============================================================
# 22_county_profile_builder.R
#
# Build app-ready county profiles that combine:
#   - Cancer incidence and mortality
#   - Confidence intervals and reportability
#   - Rankings and percentiles
#   - Comparison with Ohio
#   - Rurality and population
#   - CDC PLACES contextual measures
#   - Spatial-cluster results, if available
#
# Inputs:
#   data/processed/
#   ohio_ovarian_cancer_epidemiologic_dataset.csv
#
# Optional inputs:
#   outputs/tables/18_county_rankings.csv
#   outputs/tables/19_county_comparison_with_ohio.csv
#   outputs/tables/20_local_moran_incidence.csv
#   outputs/tables/20_local_moran_mortality.csv
#
# Outputs:
#   outputs/tables/
#   22_county_profiles.csv
#   22_portage_complete_profile.csv
#   22_county_profile_dictionary.csv
#   22_county_profile_summary.txt
# ============================================================

# ============================================================
# 1. PACKAGE CHECK
# ============================================================

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "tidyr",
  "janitor",
  "tibble"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install the following packages before running this script:\n",
      paste(
        missing_packages,
        collapse = ", "
      )
    )
  )
}

library(dplyr)
library(readr)
library(stringr)
library(tidyr)
library(janitor)
library(tibble)

# ============================================================
# 2. FILE PATHS
# ============================================================

epi_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_epidemiologic_dataset.csv"
)

rankings_file <- file.path(
  "outputs",
  "tables",
  "18_county_rankings.csv"
)

comparison_file <- file.path(
  "outputs",
  "tables",
  "19_county_comparison_with_ohio.csv"
)

local_incidence_file <- file.path(
  "outputs",
  "tables",
  "20_local_moran_incidence.csv"
)

local_mortality_file <- file.path(
  "outputs",
  "tables",
  "20_local_moran_mortality.csv"
)

output_dir <- file.path(
  "outputs",
  "tables"
)

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(epi_file)) {
  stop(
    paste0(
      "Required input file not found:\n",
      epi_file
    )
  )
}

# ============================================================
# 3. READ CORE DATA
# ============================================================

epi <- read_csv(
  epi_file,
  show_col_types = FALSE
) |>
  clean_names()

required_columns <- c(
  "county",
  "fips",
  "population_2020",
  "rucc_2023",
  "metro_status",
  "rurality_3_level",
  "incidence_rate",
  "incidence_lower_ci",
  "incidence_upper_ci",
  "average_annual_cases",
  "mortality_rate",
  "mortality_lower_ci",
  "mortality_upper_ci",
  "average_annual_deaths",
  "smoking_percent",
  "obesity_percent",
  "physical_inactivity_percent",
  "diabetes_percent",
  "uninsured_percent",
  "annual_checkup_percent",
  "mammography_percent",
  "food_insecurity_percent",
  "transportation_barriers_percent"
)

missing_columns <- setdiff(
  required_columns,
  names(epi)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The epidemiologic dataset is missing:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

epi <- epi |>
  mutate(
    fips = str_pad(
      as.character(fips),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

# ============================================================
# 4. OPTIONAL FILE READER
# ============================================================

read_optional_csv <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }
  
  read_csv(
    path,
    show_col_types = FALSE
  ) |>
    clean_names() |>
    mutate(
      fips = str_pad(
        as.character(fips),
        width = 5,
        side = "left",
        pad = "0"
      )
    )
}

rankings <- read_optional_csv(
  rankings_file
)

comparison <- read_optional_csv(
  comparison_file
)

local_incidence <- read_optional_csv(
  local_incidence_file
)

local_mortality <- read_optional_csv(
  local_mortality_file
)

# ============================================================
# 5. REDUCE OPTIONAL DATASETS TO APP-READY FIELDS
# ============================================================

if (!is.null(rankings)) {
  rankings_selected <- rankings |>
    select(
      fips,
      any_of(
        c(
          "incidence_rank_high_to_low",
          "incidence_percentile",
          "incidence_rank_display",
          "mortality_rank_high_to_low",
          "mortality_percentile",
          "mortality_rank_display"
        )
      )
    )
} else {
  rankings_selected <- tibble(
    fips = character()
  )
}

if (!is.null(comparison)) {
  comparison_selected <- comparison |>
    select(
      fips,
      any_of(
        c(
          "incidence_comparison_with_ohio",
          "mortality_comparison_with_ohio",
          "incidence_difference_from_ohio",
          "mortality_difference_from_ohio"
        )
      )
    )
} else {
  comparison_selected <- tibble(
    fips = character()
  )
}

if (!is.null(local_incidence)) {
  local_incidence_selected <- local_incidence |>
    select(
      fips,
      any_of(
        c(
          "local_moran_i",
          "local_p_value",
          "local_p_fdr",
          "local_cluster",
          "local_cluster_fdr"
        )
      )
    ) |>
    rename(
      incidence_local_moran_i =
        local_moran_i,
      incidence_local_p =
        local_p_value,
      incidence_local_p_fdr =
        local_p_fdr,
      incidence_local_cluster =
        local_cluster,
      incidence_local_cluster_fdr =
        local_cluster_fdr
    )
} else {
  local_incidence_selected <- tibble(
    fips = character()
  )
}

if (!is.null(local_mortality)) {
  local_mortality_selected <- local_mortality |>
    select(
      fips,
      any_of(
        c(
          "local_moran_i",
          "local_p_value",
          "local_p_fdr",
          "local_cluster",
          "local_cluster_fdr"
        )
      )
    ) |>
    rename(
      mortality_local_moran_i =
        local_moran_i,
      mortality_local_p =
        local_p_value,
      mortality_local_p_fdr =
        local_p_fdr,
      mortality_local_cluster =
        local_cluster,
      mortality_local_cluster_fdr =
        local_cluster_fdr
    )
} else {
  local_mortality_selected <- tibble(
    fips = character()
  )
}

# ============================================================
# 6. JOIN PROFILE COMPONENTS
# ============================================================

profiles <- epi |>
  left_join(
    rankings_selected,
    by = "fips"
  ) |>
  left_join(
    comparison_selected,
    by = "fips"
  ) |>
  left_join(
    local_incidence_selected,
    by = "fips"
  ) |>
  left_join(
    local_mortality_selected,
    by = "fips"
  )

# ============================================================
# 7. DERIVED DISPLAY FIELDS
# ============================================================

format_rate_ci <- function(
    rate,
    lower,
    upper
) {
  ifelse(
    is.na(rate),
    "Suppressed / unavailable",
    paste0(
      format(
        round(
          rate,
          1
        ),
        nsmall = 1
      ),
      " per 100,000 (95% CI ",
      format(
        round(
          lower,
          1
        ),
        nsmall = 1
      ),
      "–",
      format(
        round(
          upper,
          1
        ),
        nsmall = 1
      ),
      ")"
    )
  )
}

profiles <- profiles |>
  mutate(
    incidence_display = format_rate_ci(
      incidence_rate,
      incidence_lower_ci,
      incidence_upper_ci
    ),
    
    mortality_display = format_rate_ci(
      mortality_rate,
      mortality_lower_ci,
      mortality_upper_ci
    ),
    
    incidence_count_display = if_else(
      is.na(
        average_annual_cases
      ),
      "Unavailable",
      paste0(
        round(
          average_annual_cases,
          0
        ),
        " average annual cases"
      )
    ),
    
    mortality_count_display = if_else(
      is.na(
        average_annual_deaths
      ),
      "Unavailable",
      paste0(
        round(
          average_annual_deaths,
          0
        ),
        " average annual deaths"
      )
    ),
    
    population_display = format(
      population_2020,
      big.mark = ",",
      scientific = FALSE
    ),
    
    rurality_display = paste0(
      metro_status,
      " | RUCC ",
      rucc_2023,
      " | ",
      rurality_3_level
    ),
    
    contextual_burden_score = rowMeans(
      cbind(
        smoking_percent,
        obesity_percent,
        physical_inactivity_percent,
        diabetes_percent,
        uninsured_percent,
        food_insecurity_percent,
        transportation_barriers_percent
      ),
      na.rm = TRUE
    ),
    
    preventive_care_score = rowMeans(
      cbind(
        annual_checkup_percent,
        mammography_percent
      ),
      na.rm = TRUE
    )
  )

# ============================================================
# 8. CREATE LONG-FORM PROFILE TABLE
# ============================================================

profile_long <- profiles |>
  select(
    county,
    fips,
    population_2020,
    metro_status,
    rurality_3_level,
    incidence_rate,
    mortality_rate,
    smoking_percent,
    obesity_percent,
    physical_inactivity_percent,
    diabetes_percent,
    uninsured_percent,
    annual_checkup_percent,
    mammography_percent,
    food_insecurity_percent,
    transportation_barriers_percent
  ) |>
  pivot_longer(
    cols = -c(
      county,
      fips,
      population_2020,
      metro_status,
      rurality_3_level
    ),
    names_to = "indicator",
    values_to = "value"
  )

indicator_dictionary <- tribble(
  ~indicator, ~label, ~section, ~unit,
  "incidence_rate", "Ovarian cancer incidence", "Cancer burden", "per 100,000",
  "mortality_rate", "Ovarian cancer mortality", "Cancer burden", "per 100,000",
  "smoking_percent", "Current cigarette smoking", "Health context", "percent",
  "obesity_percent", "Obesity", "Health context", "percent",
  "physical_inactivity_percent", "Physical inactivity", "Health context", "percent",
  "diabetes_percent", "Diabetes", "Health context", "percent",
  "uninsured_percent", "Uninsured", "Access context", "percent",
  "annual_checkup_percent", "Annual checkup", "Preventive care", "percent",
  "mammography_percent", "Mammography", "Preventive care", "percent",
  "food_insecurity_percent", "Food insecurity", "Social context", "percent",
  "transportation_barriers_percent", "Transportation barriers", "Access context", "percent"
)

profile_long <- profile_long |>
  left_join(
    indicator_dictionary,
    by = "indicator"
  )

# ============================================================
# 9. PORTAGE COMPLETE PROFILE
# ============================================================

portage_profile <- profiles |>
  filter(
    str_to_lower(
      str_squish(
        county
      )
    ) == "portage"
  )

cat(
  "\nPortage County complete profile:\n"
)

if (nrow(portage_profile) == 0) {
  cat(
    "Portage County was not found.\n"
  )
} else {
  for (
    variable_name in names(
      portage_profile
    )
  ) {
    value <- portage_profile[[variable_name]][1]
    
    value_text <- if (
      length(value) == 0 ||
      is.na(value)
    ) {
      "NA"
    } else {
      as.character(value)
    }
    
    cat(
      sprintf(
        "%-48s %s\n",
        paste0(
          variable_name,
          ":"
        ),
        value_text
      )
    )
  }
}

# ============================================================
# 10. EXPORT FILES
# ============================================================

write_csv(
  profiles,
  file.path(
    output_dir,
    "22_county_profiles.csv"
  ),
  na = ""
)

write_csv(
  portage_profile,
  file.path(
    output_dir,
    "22_portage_complete_profile.csv"
  ),
  na = ""
)

write_csv(
  indicator_dictionary,
  file.path(
    output_dir,
    "22_county_profile_dictionary.csv"
  ),
  na = ""
)

write_csv(
  profile_long,
  file.path(
    output_dir,
    "22_county_profiles_long.csv"
  ),
  na = ""
)

# ============================================================
# 11. SUMMARY
# ============================================================

summary_lines <- c(
  "OHIO OVARIAN CANCER COUNTY PROFILE DATASET",
  "",
  paste0(
    "County profiles created: ",
    nrow(
      profiles
    ),
    "."
  ),
  paste0(
    "Ranking data included: ",
    ifelse(
      is.null(
        rankings
      ),
      "No",
      "Yes"
    ),
    "."
  ),
  paste0(
    "Ohio comparison data included: ",
    ifelse(
      is.null(
        comparison
      ),
      "No",
      "Yes"
    ),
    "."
  ),
  paste0(
    "Spatial incidence results included: ",
    ifelse(
      is.null(
        local_incidence
      ),
      "No",
      "Yes"
    ),
    "."
  ),
  paste0(
    "Spatial mortality results included: ",
    ifelse(
      is.null(
        local_mortality
      ),
      "No",
      "Yes"
    ),
    "."
  ),
  "",
  "Interpretation note:",
  paste0(
    "Cancer estimates and contextual indicators describe counties, not ",
    "individual residents. Contextual indicators should not be interpreted ",
    "as causes of ovarian cancer."
  )
)

write_lines(
  summary_lines,
  file.path(
    output_dir,
    "22_county_profile_summary.txt"
  )
)

cat(
  "\n",
  paste(
    summary_lines,
    collapse = "\n"
  ),
  "\n",
  sep = ""
)

cat(
  "\nCounty profile builder completed successfully.\n"
)