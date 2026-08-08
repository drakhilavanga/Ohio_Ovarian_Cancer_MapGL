# ============================================================
# 13_clean_places.R
# Clean CDC PLACES county-level contextual indicators
# ============================================================

library(tidyverse)
library(janitor)

# ------------------------------------------------------------
# File paths
# ------------------------------------------------------------

input_file <- file.path(
  "data",
  "raw",
  "CDC_PLACES_County_Data.csv"
)

output_long_file <- file.path(
  "data",
  "processed",
  "ohio_places_selected_long.csv"
)

output_wide_file <- file.path(
  "data",
  "processed",
  "ohio_places_selected_wide.csv"
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "CDC PLACES file not found:\n",
      input_file,
      "\n\nSave the downloaded file as ",
      "data/raw/CDC_PLACES_County_Data.csv"
    )
  )
}

dir.create(
  dirname(output_wide_file),
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Import
# ------------------------------------------------------------

places_raw <- read_csv(
  input_file,
  col_types = cols(
    .default = col_character()
  ),
  show_col_types = FALSE,
  progress = TRUE
) |>
  clean_names()

cat("\nImported PLACES rows:", nrow(places_raw), "\n")
cat("Imported PLACES columns:", ncol(places_raw), "\n\n")

print(names(places_raw))

# ------------------------------------------------------------
# Selected contextual indicators
#
# These are contextual county characteristics.
# They should not be presented as causes of ovarian cancer.
# ------------------------------------------------------------

selected_measure_ids <- c(
  "CSMOKING",   # Current cigarette smoking
  "OBESITY",    # Obesity
  "LPA",        # Physical inactivity
  "DIABETES",   # Diabetes
  "BPHIGH",     # High blood pressure
  "ACCESS2",    # Lack of health insurance, adults 18–64
  "CHECKUP",    # Routine annual checkup
  "MAMMOUSE",   # Mammography use, women 50–74
  "DEPRESSION", # Depression
  "MHLTH",      # Frequent mental distress
  "LACKTRPT",   # Transportation barriers
  "FOODINSECU"  # Food insecurity
)

# ------------------------------------------------------------
# Keep Ohio and age-adjusted prevalence
# ------------------------------------------------------------

ohio_places_long <- places_raw |>
  filter(
    state_abbr == "OH",
    measure_id %in% selected_measure_ids,
    data_value_type_id == "AgeAdjPrv"
  ) |>
  transmute(
    year = parse_integer(year),
    
    state_abbr,
    
    state = state_desc,
    
    county = location_name |>
      str_remove(
        regex(
          "\\s+county$",
          ignore_case = TRUE
        )
      ) |>
      str_squish(),
    
    fips = str_pad(
      location_id,
      width = 5,
      side = "left",
      pad = "0"
    ),
    
    category,
    
    measure_id,
    
    measure,
    
    short_question_text,
    
    prevalence = parse_number(
      data_value
    ),
    
    lower_ci = parse_number(
      low_confidence_limit
    ),
    
    upper_ci = parse_number(
      high_confidence_limit
    ),
    
    total_population = parse_number(
      total_population
    ),
    
    adult_population = parse_number(
      total_pop18plus
    )
  ) |>
  mutate(
    relative_ci_width = if_else(
      !is.na(prevalence) &
        prevalence > 0 &
        !is.na(lower_ci) &
        !is.na(upper_ci),
      
      (upper_ci - lower_ci) /
        prevalence,
      
      NA_real_
    )
  ) |>
  arrange(
    fips,
    measure_id
  )

# ------------------------------------------------------------
# Check whether each county has one record per selected measure
# ------------------------------------------------------------

duplicate_check <- ohio_places_long |>
  count(
    fips,
    measure_id,
    name = "records"
  ) |>
  filter(
    records > 1
  )

if (nrow(duplicate_check) > 0) {
  cat(
    "\nMultiple records detected for some county-measure combinations.\n",
    "Keeping the most recent year for each county and measure.\n"
  )
  
  print(
    duplicate_check |>
      slice_head(
        n = 20
      )
  )
}

# Keep most recent available record
ohio_places_long <- ohio_places_long |>
  group_by(
    fips,
    measure_id
  ) |>
  arrange(
    desc(year),
    .by_group = TRUE
  ) |>
  slice(1) |>
  ungroup()

# ------------------------------------------------------------
# Validate Ohio county coverage
# ------------------------------------------------------------

cat("\nPLACES cleaning summary:\n")

print(
  ohio_places_long |>
    summarise(
      rows = n(),
      counties = n_distinct(fips),
      measures = n_distinct(measure_id),
      minimum_year = min(
        year,
        na.rm = TRUE
      ),
      maximum_year = max(
        year,
        na.rm = TRUE
      )
    )
)

cat("\nSelected PLACES measures:\n")

print(
  ohio_places_long |>
    distinct(
      measure_id,
      short_question_text,
      measure
    ) |>
    arrange(
      measure_id
    )
)

cat("\nPortage County PLACES profile:\n")

print(
  ohio_places_long |>
    filter(
      county == "Portage"
    ) |>
    select(
      year,
      measure_id,
      short_question_text,
      prevalence,
      lower_ci,
      upper_ci
    )
)

# ------------------------------------------------------------
# Save long-format analytical file
# ------------------------------------------------------------

write_csv(
  ohio_places_long,
  output_long_file,
  na = ""
)

# ------------------------------------------------------------
# Create one-row-per-county wide file
# ------------------------------------------------------------

ohio_places_wide <- ohio_places_long |>
  select(
    fips,
    county,
    state_abbr,
    year,
    measure_id,
    prevalence,
    lower_ci,
    upper_ci
  ) |>
  pivot_wider(
    id_cols = c(
      fips,
      county,
      state_abbr
    ),
    
    names_from = measure_id,
    
    values_from = c(
      year,
      prevalence,
      lower_ci,
      upper_ci
    ),
    
    names_glue = "{tolower(.value)}_{tolower(measure_id)}"
  ) |>
  clean_names() |>
  arrange(
    fips
  )

if (nrow(ohio_places_wide) != 88) {
  warning(
    paste0(
      "Expected 88 Ohio counties in the PLACES wide file but found ",
      nrow(ohio_places_wide),
      "."
    )
  )
}

write_csv(
  ohio_places_wide,
  output_wide_file,
  na = ""
)

cat(
  "\nClean PLACES long file saved to:\n",
  output_long_file,
  "\n\nClean PLACES wide file saved to:\n",
  output_wide_file,
  "\n"
)