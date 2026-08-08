# ============================================================
# 19_compare_with_ohio_using_confidence_intervals.R
#
# Classify county ovarian cancer estimates relative to Ohio
# using confidence intervals rather than point estimates alone.
#
# Categories:
#   - Above Ohio
#   - Below Ohio
#   - Not clearly different from Ohio
#   - Suppressed / unavailable
#
# Input:
#   data/processed/
#   ohio_ovarian_cancer_epidemiologic_dataset.csv
#
# Outputs:
#   outputs/tables/
#   19_county_comparison_with_ohio.csv
#   19_incidence_comparison_summary.csv
#   19_mortality_comparison_summary.csv
#   19_portage_comparison_with_ohio.csv
#   19_comparison_with_ohio_summary.txt
# ============================================================

library(tidyverse)
library(janitor)

# ============================================================
# 1. FILE PATHS
# ============================================================

input_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_epidemiologic_dataset.csv"
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

if (!file.exists(input_file)) {
  stop(
    paste0(
      "Input file not found:\n",
      input_file,
      "\n\nRun R/14_create_epidemiologic_dataset.R first."
    )
  )
}

# ============================================================
# 2. STATEWIDE REFERENCE RATES
# ============================================================

ohio_incidence_rate <- 9.8
ohio_mortality_rate <- 5.8

# ============================================================
# 3. READ DATA
# ============================================================

epi <- read_csv(
  input_file,
  show_col_types = FALSE
) |>
  clean_names()

required_columns <- c(
  "county",
  "fips",
  "metro_status",
  "rurality_3_level",
  "incidence_rate",
  "incidence_lower_ci",
  "incidence_upper_ci",
  "average_annual_cases",
  "mortality_rate",
  "mortality_lower_ci",
  "mortality_upper_ci",
  "average_annual_deaths"
)

missing_columns <- setdiff(
  required_columns,
  names(epi)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The epidemiologic dataset is missing these columns:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

# ============================================================
# 4. STANDARDIZE TYPES
# ============================================================

epi <- epi |>
  mutate(
    fips = str_pad(
      as.character(fips),
      width = 5,
      side = "left",
      pad = "0"
    ),
    
    across(
      c(
        incidence_rate,
        incidence_lower_ci,
        incidence_upper_ci,
        average_annual_cases,
        mortality_rate,
        mortality_lower_ci,
        mortality_upper_ci,
        average_annual_deaths
      ),
      as.numeric
    )
  )

# ============================================================
# 5. CLASSIFY COUNTIES RELATIVE TO OHIO
# ============================================================

epi_comparison <- epi |>
  mutate(
    incidence_comparison_with_ohio = case_when(
      is.na(incidence_rate) |
        is.na(incidence_lower_ci) |
        is.na(incidence_upper_ci) ~
        "Suppressed / unavailable",
      
      incidence_lower_ci > ohio_incidence_rate ~
        "Above Ohio",
      
      incidence_upper_ci < ohio_incidence_rate ~
        "Below Ohio",
      
      TRUE ~
        "Not clearly different from Ohio"
    ),
    
    mortality_comparison_with_ohio = case_when(
      is.na(mortality_rate) |
        is.na(mortality_lower_ci) |
        is.na(mortality_upper_ci) ~
        "Suppressed / unavailable",
      
      mortality_lower_ci > ohio_mortality_rate ~
        "Above Ohio",
      
      mortality_upper_ci < ohio_mortality_rate ~
        "Below Ohio",
      
      TRUE ~
        "Not clearly different from Ohio"
    ),
    
    incidence_difference_from_ohio =
      incidence_rate -
      ohio_incidence_rate,
    
    mortality_difference_from_ohio =
      mortality_rate -
      ohio_mortality_rate,
    
    incidence_ci_overlaps_ohio = case_when(
      is.na(incidence_lower_ci) |
        is.na(incidence_upper_ci) ~
        NA,
      
      incidence_lower_ci <= ohio_incidence_rate &
        incidence_upper_ci >= ohio_incidence_rate ~
        TRUE,
      
      TRUE ~
        FALSE
    ),
    
    mortality_ci_overlaps_ohio = case_when(
      is.na(mortality_lower_ci) |
        is.na(mortality_upper_ci) ~
        NA,
      
      mortality_lower_ci <= ohio_mortality_rate &
        mortality_upper_ci >= ohio_mortality_rate ~
        TRUE,
      
      TRUE ~
        FALSE
    )
  )

# ============================================================
# 6. SUMMARY TABLES
# ============================================================

incidence_summary <- epi_comparison |>
  count(
    incidence_comparison_with_ohio,
    name = "counties"
  ) |>
  mutate(
    percent_of_all_counties = round(
      100 *
        counties /
        nrow(epi_comparison),
      1
    ),
    
    percent_of_reportable_counties = case_when(
      incidence_comparison_with_ohio ==
        "Suppressed / unavailable" ~
        NA_real_,
      
      TRUE ~ round(
        100 *
          counties /
          sum(
            epi_comparison$
              incidence_comparison_with_ohio !=
              "Suppressed / unavailable"
          ),
        1
      )
    )
  ) |>
  arrange(
    factor(
      incidence_comparison_with_ohio,
      levels = c(
        "Above Ohio",
        "Not clearly different from Ohio",
        "Below Ohio",
        "Suppressed / unavailable"
      )
    )
  )

mortality_summary <- epi_comparison |>
  count(
    mortality_comparison_with_ohio,
    name = "counties"
  ) |>
  mutate(
    percent_of_all_counties = round(
      100 *
        counties /
        nrow(epi_comparison),
      1
    ),
    
    percent_of_reportable_counties = case_when(
      mortality_comparison_with_ohio ==
        "Suppressed / unavailable" ~
        NA_real_,
      
      TRUE ~ round(
        100 *
          counties /
          sum(
            epi_comparison$
              mortality_comparison_with_ohio !=
              "Suppressed / unavailable"
          ),
        1
      )
    )
  ) |>
  arrange(
    factor(
      mortality_comparison_with_ohio,
      levels = c(
        "Above Ohio",
        "Not clearly different from Ohio",
        "Below Ohio",
        "Suppressed / unavailable"
      )
    )
  )

cat(
  "\nIncidence comparison with Ohio:\n"
)

print(
  incidence_summary
)

cat(
  "\nMortality comparison with Ohio:\n"
)

print(
  mortality_summary
)

# ============================================================
# 7. COUNTY LISTS BY CATEGORY
# ============================================================

incidence_above_ohio <- epi_comparison |>
  filter(
    incidence_comparison_with_ohio ==
      "Above Ohio"
  ) |>
  arrange(
    desc(
      incidence_rate
    )
  ) |>
  select(
    county,
    fips,
    metro_status,
    rurality_3_level,
    incidence_rate,
    incidence_lower_ci,
    incidence_upper_ci,
    average_annual_cases,
    incidence_difference_from_ohio
  )

incidence_below_ohio <- epi_comparison |>
  filter(
    incidence_comparison_with_ohio ==
      "Below Ohio"
  ) |>
  arrange(
    incidence_rate
  ) |>
  select(
    county,
    fips,
    metro_status,
    rurality_3_level,
    incidence_rate,
    incidence_lower_ci,
    incidence_upper_ci,
    average_annual_cases,
    incidence_difference_from_ohio
  )

mortality_above_ohio <- epi_comparison |>
  filter(
    mortality_comparison_with_ohio ==
      "Above Ohio"
  ) |>
  arrange(
    desc(
      mortality_rate
    )
  ) |>
  select(
    county,
    fips,
    metro_status,
    rurality_3_level,
    mortality_rate,
    mortality_lower_ci,
    mortality_upper_ci,
    average_annual_deaths,
    mortality_difference_from_ohio
  )

mortality_below_ohio <- epi_comparison |>
  filter(
    mortality_comparison_with_ohio ==
      "Below Ohio"
  ) |>
  arrange(
    mortality_rate
  ) |>
  select(
    county,
    fips,
    metro_status,
    rurality_3_level,
    mortality_rate,
    mortality_lower_ci,
    mortality_upper_ci,
    average_annual_deaths,
    mortality_difference_from_ohio
  )

# ============================================================
# 8. PORTAGE COUNTY PROFILE
# ============================================================

portage_comparison <- epi_comparison |>
  filter(
    str_to_lower(
      str_squish(
        county
      )
    ) == "portage"
  ) |>
  transmute(
    county,
    fips,
    
    ohio_incidence_rate,
    incidence_rate,
    incidence_lower_ci,
    incidence_upper_ci,
    incidence_difference_from_ohio,
    incidence_ci_overlaps_ohio,
    incidence_comparison_with_ohio,
    average_annual_cases,
    
    ohio_mortality_rate,
    mortality_rate,
    mortality_lower_ci,
    mortality_upper_ci,
    mortality_difference_from_ohio,
    mortality_ci_overlaps_ohio,
    mortality_comparison_with_ohio,
    average_annual_deaths
  )

cat(
  "\nPortage County comparison with Ohio:\n"
)

if (nrow(portage_comparison) == 0) {
  cat(
    "Portage County was not found.\n"
  )
} else {
  for (
    variable_name in names(
      portage_comparison
    )
  ) {
    value <- portage_comparison[[variable_name]][1]
    
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
        "%-42s %s\n",
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
# 9. EXPORT FILES
# ============================================================

write_csv(
  epi_comparison,
  file.path(
    output_dir,
    "19_county_comparison_with_ohio.csv"
  ),
  na = ""
)

write_csv(
  incidence_summary,
  file.path(
    output_dir,
    "19_incidence_comparison_summary.csv"
  ),
  na = ""
)

write_csv(
  mortality_summary,
  file.path(
    output_dir,
    "19_mortality_comparison_summary.csv"
  ),
  na = ""
)

write_csv(
  incidence_above_ohio,
  file.path(
    output_dir,
    "19_incidence_above_ohio.csv"
  ),
  na = ""
)

write_csv(
  incidence_below_ohio,
  file.path(
    output_dir,
    "19_incidence_below_ohio.csv"
  ),
  na = ""
)

write_csv(
  mortality_above_ohio,
  file.path(
    output_dir,
    "19_mortality_above_ohio.csv"
  ),
  na = ""
)

write_csv(
  mortality_below_ohio,
  file.path(
    output_dir,
    "19_mortality_below_ohio.csv"
  ),
  na = ""
)

write_csv(
  portage_comparison,
  file.path(
    output_dir,
    "19_portage_comparison_with_ohio.csv"
  ),
  na = ""
)

# ============================================================
# 10. PLAIN-LANGUAGE SUMMARY
# ============================================================

get_count <- function(
    summary_table,
    category_column,
    category_name
) {
  result <- summary_table |>
    filter(
      .data[[category_column]] ==
        category_name
    ) |>
    pull(
      counties
    )
  
  if (length(result) == 0) {
    return(0L)
  }
  
  result[[1]]
}

incidence_above_n <- get_count(
  incidence_summary,
  "incidence_comparison_with_ohio",
  "Above Ohio"
)

incidence_below_n <- get_count(
  incidence_summary,
  "incidence_comparison_with_ohio",
  "Below Ohio"
)

incidence_overlap_n <- get_count(
  incidence_summary,
  "incidence_comparison_with_ohio",
  "Not clearly different from Ohio"
)

mortality_above_n <- get_count(
  mortality_summary,
  "mortality_comparison_with_ohio",
  "Above Ohio"
)

mortality_below_n <- get_count(
  mortality_summary,
  "mortality_comparison_with_ohio",
  "Below Ohio"
)

mortality_overlap_n <- get_count(
  mortality_summary,
  "mortality_comparison_with_ohio",
  "Not clearly different from Ohio"
)

summary_lines <- c(
  "OHIO OVARIAN CANCER COMPARISON USING CONFIDENCE INTERVALS",
  "",
  paste0(
    "Incidence: ",
    incidence_above_n,
    " counties had confidence intervals entirely above the Ohio rate; ",
    incidence_below_n,
    " were entirely below; and ",
    incidence_overlap_n,
    " were not clearly different because their confidence intervals included the Ohio rate."
  ),
  "",
  paste0(
    "Mortality: ",
    mortality_above_n,
    " counties had confidence intervals entirely above the Ohio rate; ",
    mortality_below_n,
    " were entirely below; and ",
    mortality_overlap_n,
    " were not clearly different because their confidence intervals included the Ohio rate."
  ),
  "",
  "Interpretation note:",
  paste0(
    "This classification is more epidemiologically defensible than ranking ",
    "counties by point estimate alone. Counties whose confidence intervals ",
    "overlap the statewide rate should not be described as clearly higher or lower."
  )
)

write_lines(
  summary_lines,
  file.path(
    output_dir,
    "19_comparison_with_ohio_summary.txt"
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

# ============================================================
# 11. FINAL MESSAGE
# ============================================================

cat(
  "\nComparison-with-Ohio analysis completed successfully.\n"
)

cat(
  "\nFiles saved in:\n",
  output_dir,
  "\n",
  sep = ""
)