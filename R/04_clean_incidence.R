# ============================================================
# CLEAN OHIO OVARIAN CANCER INCIDENCE DATA
# Automatically detects the true header and removes footnotes
# ============================================================

library(tidyverse)
library(janitor)
library(stringr)
library(readr)

input_file <- file.path(
  "data",
  "raw",
  "ohio_ovarian_cancer_incidence_2018_2022.csv"
)

output_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_incidence_clean.csv"
)

if (!file.exists(input_file)) {
  stop("Incidence file not found: ", input_file)
}

# ------------------------------------------------------------
# 1. FIND TRUE HEADER
# ------------------------------------------------------------

raw_lines <- read_lines(input_file)

header_candidates <- which(
  str_detect(
    str_to_lower(raw_lines),
    "^\\s*county"
  ) &
    str_detect(
      str_to_lower(raw_lines),
      "rate|incidence"
    )
)

if (length(header_candidates) == 0) {
  stop("The true incidence header row could not be detected.")
}

header_line <- header_candidates[[1]]
skip_rows <- header_line - 1
header_text <- raw_lines[[header_line]]

delimiter <- if (str_count(header_text, "\t") >= 2) {
  "\t"
} else if (str_count(header_text, ",") >= 2) {
  ","
} else {
  stop("The file delimiter could not be determined.")
}

message("True header detected on line ", header_line)
message(
  "Detected delimiter: ",
  ifelse(delimiter == "\t", "TAB", "COMMA")
)

# ------------------------------------------------------------
# 2. IMPORT
# ------------------------------------------------------------

incidence_raw <- read_delim(
  input_file,
  delim = delimiter,
  skip = skip_rows,
  show_col_types = FALSE,
  trim_ws = TRUE,
  quote = "\"",
  name_repair = "unique"
) |>
  clean_names()

cat("\nImported rows:", nrow(incidence_raw), "\n")
cat("Imported columns:", ncol(incidence_raw), "\n\n")
print(names(incidence_raw))

# ------------------------------------------------------------
# 3. IDENTIFY COLUMNS
# ------------------------------------------------------------

column_names <- names(incidence_raw)

county_candidates <- column_names[
  str_detect(
    column_names,
    "^county$|county_name|geographic_area|^area$"
  )
]

rate_candidates <- column_names[
  str_detect(
    column_names,
    "incidence_rate|age_adjusted.*rate|^rate$"
  ) &
    !str_detect(
      column_names,
      "lower|upper|confidence|trend|annual_percent|average"
    )
]

count_candidates <- column_names[
  str_detect(
    column_names,
    "average.*annual.*count|annual.*case|case.*count|average.*count"
  )
]

lower_ci_candidates <- column_names[
  str_detect(
    column_names,
    "lower_95_percent_confidence_interval_5$|lower.*rate|lower.*ci"
  )
]

upper_ci_candidates <- column_names[
  str_detect(
    column_names,
    "upper_95_percent_confidence_interval_6$|upper.*rate|upper.*ci"
  )
]

county_column <- county_candidates[[1]]
rate_column <- rate_candidates[[1]]

count_column <- if (length(count_candidates) > 0) {
  count_candidates[[1]]
} else {
  NA_character_
}

lower_ci_column <- if (length(lower_ci_candidates) > 0) {
  lower_ci_candidates[[1]]
} else {
  NA_character_
}

upper_ci_column <- if (length(upper_ci_candidates) > 0) {
  upper_ci_candidates[[1]]
} else {
  NA_character_
}

if (is.na(county_column)) {
  stop("County column was not detected.")
}

if (is.na(rate_column)) {
  stop("Incidence-rate column was not detected.")
}

message("County column detected: ", county_column)
message("Incidence-rate column detected: ", rate_column)

# ------------------------------------------------------------
# 4. CLEAN
# ------------------------------------------------------------

incidence_clean <- incidence_raw |>
  transmute(
    county_raw = as.character(.data[[county_column]]),
    incidence_rate_raw = as.character(.data[[rate_column]]),
    
    average_annual_cases_raw =
      if (!is.na(count_column)) {
        as.character(.data[[count_column]])
      } else {
        NA_character_
      },
    
    incidence_lower_ci_raw =
      if (!is.na(lower_ci_column)) {
        as.character(.data[[lower_ci_column]])
      } else {
        NA_character_
      },
    
    incidence_upper_ci_raw =
      if (!is.na(upper_ci_column)) {
        as.character(.data[[upper_ci_column]])
      } else {
        NA_character_
      }
  ) |>
  mutate(
    county = county_raw |>
      # Removes (1), (2), and similar footnotes
      str_remove_all("\\(\\d+\\)") |>
      str_replace(
        regex(",?\\s*ohio$", ignore_case = TRUE),
        ""
      ) |>
      str_replace(
        regex("\\s+county$", ignore_case = TRUE),
        ""
      ) |>
      str_squish() |>
      str_to_title(),
    
    incidence_rate = parse_number(
      incidence_rate_raw,
      na = c(
        "", "NA", "N/A", "*", "**", "***",
        "Suppressed", "Data not available", "~"
      )
    ),
    
    average_annual_cases = parse_number(
      average_annual_cases_raw,
      na = c(
        "", "NA", "N/A", "*", "**", "***",
        "Suppressed", "Data not available", "~"
      )
    ),
    
    incidence_lower_ci = parse_number(
      incidence_lower_ci_raw,
      na = c("", "NA", "N/A", "*", "**", "***", "~")
    ),
    
    incidence_upper_ci = parse_number(
      incidence_upper_ci_raw,
      na = c("", "NA", "N/A", "*", "**", "***", "~")
    ),
    
    incidence_suppressed =
      is.na(incidence_rate) &
      str_detect(
        str_to_lower(incidence_rate_raw),
        "\\*|suppressed|not available|~"
      ),
    
    cancer_site = "Ovary",
    stage = "All stages",
    period = "2018–2022",
    sex = "Female"
  )

# ------------------------------------------------------------
# 5. RETAIN OHIO'S 88 COUNTIES
# ------------------------------------------------------------

ohio_county_names <- c(
  "Adams", "Allen", "Ashland", "Ashtabula", "Athens",
  "Auglaize", "Belmont", "Brown", "Butler", "Carroll",
  "Champaign", "Clark", "Clermont", "Clinton", "Columbiana",
  "Coshocton", "Crawford", "Cuyahoga", "Darke", "Defiance",
  "Delaware", "Erie", "Fairfield", "Fayette", "Franklin",
  "Fulton", "Gallia", "Geauga", "Greene", "Guernsey",
  "Hamilton", "Hancock", "Hardin", "Harrison", "Henry",
  "Highland", "Hocking", "Holmes", "Huron", "Jackson",
  "Jefferson", "Knox", "Lake", "Lawrence", "Licking",
  "Logan", "Lorain", "Lucas", "Madison", "Mahoning",
  "Marion", "Medina", "Meigs", "Mercer", "Miami",
  "Monroe", "Montgomery", "Morgan", "Morrow", "Muskingum",
  "Noble", "Ottawa", "Paulding", "Perry", "Pickaway",
  "Pike", "Portage", "Preble", "Putnam", "Richland",
  "Ross", "Sandusky", "Scioto", "Seneca", "Shelby",
  "Stark", "Summit", "Trumbull", "Tuscarawas", "Union",
  "Van Wert", "Vinton", "Warren", "Washington", "Wayne",
  "Williams", "Wood", "Wyandot"
)

incidence_clean <- incidence_clean |>
  filter(county %in% ohio_county_names) |>
  distinct(county, .keep_all = TRUE) |>
  arrange(county)

# ------------------------------------------------------------
# 6. VALIDATE AND SAVE
# ------------------------------------------------------------

if (nrow(incidence_clean) != 88) {
  missing_counties <- setdiff(
    ohio_county_names,
    incidence_clean$county
  )
  
  warning(
    "Expected 88 counties but retained ",
    nrow(incidence_clean),
    ". Missing: ",
    paste(missing_counties, collapse = ", ")
  )
}

write_csv(
  incidence_clean,
  output_file
)

cat("\nPortage County incidence record:\n")

print(
  incidence_clean |>
    filter(county == "Portage") |>
    select(
      county,
      incidence_rate,
      incidence_lower_ci,
      incidence_upper_ci,
      average_annual_cases,
      incidence_suppressed
    )
)

cat("\nIncidence cleaning summary:\n")

print(
  incidence_clean |>
    summarise(
      counties = n(),
      available_rates = sum(!is.na(incidence_rate)),
      missing_or_suppressed = sum(is.na(incidence_rate))
    )
)

message(
  "\nClean incidence file saved to:\n",
  output_file
)