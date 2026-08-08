# ============================================================
# 12_clean_rucc.R
# Clean USDA 2023 Rural–Urban Continuum Codes
# ============================================================

library(tidyverse)
library(janitor)

# ------------------------------------------------------------
# File paths
# ------------------------------------------------------------

input_file <- file.path(
  "data",
  "raw",
  "RUCC_2023.csv"
)

output_file <- file.path(
  "data",
  "processed",
  "ohio_rucc_2023_clean.csv"
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "RUCC file not found:\n",
      input_file,
      "\n\nSave the downloaded file as data/raw/RUCC_2023.csv"
    )
  )
}

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

# ------------------------------------------------------------
# Import
# The USDA file is long format:
# FIPS | State | County_Name | Attribute | Value
# ------------------------------------------------------------

rucc_raw <- read_csv(
  input_file,
  col_types = cols(
    FIPS = col_character(),
    State = col_character(),
    County_Name = col_character(),
    Attribute = col_character(),
    Value = col_character()
  ),
  show_col_types = FALSE
) |>
  clean_names()

cat("\nImported RUCC rows:", nrow(rucc_raw), "\n")
cat("Imported RUCC columns:", ncol(rucc_raw), "\n\n")

print(names(rucc_raw))

# ------------------------------------------------------------
# Keep Ohio and reshape to one row per county
# ------------------------------------------------------------

ohio_rucc <- rucc_raw |>
  filter(
    state == "OH"
  ) |>
  mutate(
    fips = str_pad(
      fips,
      width = 5,
      side = "left",
      pad = "0"
    )
  ) |>
  select(
    fips,
    state,
    county_name,
    attribute,
    value
  ) |>
  pivot_wider(
    names_from = attribute,
    values_from = value
  ) |>
  clean_names()

cat("\nColumns after reshaping:\n")
print(names(ohio_rucc))

# ------------------------------------------------------------
# Standardize values and create epidemiologic rurality groups
# ------------------------------------------------------------

ohio_rucc_clean <- ohio_rucc |>
  transmute(
    fips,
    
    state,
    
    county = county_name |>
      str_remove(
        regex(
          "\\s+county$",
          ignore_case = TRUE
        )
      ) |>
      str_squish(),
    
    population_2020 = parse_number(
      population_2020
    ),
    
    rucc_2023 = parse_integer(
      rucc_2023
    ),
    
    rucc_description = description,
    
    metro_status = case_when(
      rucc_2023 %in% 1:3 ~ "Metropolitan",
      rucc_2023 %in% 4:9 ~ "Nonmetropolitan",
      TRUE ~ NA_character_
    ),
    
    rurality_3_level = case_when(
      rucc_2023 %in% 1:3 ~
        "Metropolitan",
      
      rucc_2023 %in% c(4, 6, 8) ~
        "Nonmetropolitan adjacent to metro",
      
      rucc_2023 %in% c(5, 7, 9) ~
        "Nonmetropolitan nonadjacent",
      
      TRUE ~ NA_character_
    ),
    
    rurality_ordinal = case_when(
      rucc_2023 %in% 1:3 ~ 1L,
      rucc_2023 %in% c(4, 6, 8) ~ 2L,
      rucc_2023 %in% c(5, 7, 9) ~ 3L,
      TRUE ~ NA_integer_
    )
  ) |>
  arrange(
    fips
  )

# ------------------------------------------------------------
# Validation
# ------------------------------------------------------------

cat("\nRUCC cleaning summary:\n")

print(
  ohio_rucc_clean |>
    summarise(
      counties = n(),
      unique_fips = n_distinct(fips),
      missing_rucc = sum(is.na(rucc_2023)),
      metropolitan = sum(
        metro_status == "Metropolitan",
        na.rm = TRUE
      ),
      nonmetropolitan = sum(
        metro_status == "Nonmetropolitan",
        na.rm = TRUE
      )
    )
)

if (nrow(ohio_rucc_clean) != 88) {
  warning(
    paste0(
      "Expected 88 Ohio counties but found ",
      nrow(ohio_rucc_clean),
      "."
    )
  )
}

if (anyDuplicated(ohio_rucc_clean$fips) > 0) {
  stop(
    "Duplicate FIPS values detected in the cleaned RUCC dataset."
  )
}

cat("\nPortage County RUCC record:\n")

print(
  ohio_rucc_clean |>
    filter(
      county == "Portage"
    )
)

# ------------------------------------------------------------
# Save
# ------------------------------------------------------------

write_csv(
  ohio_rucc_clean,
  output_file,
  na = ""
)

cat(
  "\nClean RUCC file saved to:\n",
  output_file,
  "\n"
)