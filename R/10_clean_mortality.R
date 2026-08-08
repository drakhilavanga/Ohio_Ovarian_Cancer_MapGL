# ============================================================
# CLEAN OHIO OVARIAN CANCER MORTALITY DATA
# Automatically detects the true header row
# ============================================================

library(tidyverse)
library(janitor)
library(stringr)
library(readr)

input_file <- file.path(
  "data",
  "raw",
  "ohio_ovarian_cancer_mortality.csv"
)

output_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_mortality_clean.csv"
)

if (!file.exists(input_file)) {
  stop("Mortality file not found: ", input_file)
}

# ------------------------------------------------------------
# 1. READ RAW TEXT AND FIND THE TRUE HEADER
# ------------------------------------------------------------

raw_lines <- read_lines(input_file)

# Display the first lines for reference
cat("\nFirst 15 lines of the source file:\n\n")

cat(
  paste0(
    seq_len(min(15, length(raw_lines))),
    ": ",
    raw_lines[seq_len(min(15, length(raw_lines)))],
    collapse = "\n"
  )
)

# Find a line that begins with County and contains a rate field.
header_candidates <- which(
  str_detect(
    str_to_lower(raw_lines),
    "^\\s*county"
  ) &
    str_detect(
      str_to_lower(raw_lines),
      "rate|death"
    )
)

if (length(header_candidates) == 0) {
  stop(
    "\nThe true header row could not be detected.\n",
    "Look at the first lines printed above."
  )
}

header_line <- header_candidates[[1]]

# read_delim() uses zero-based skip:
# header on line 4 means skip = 3
skip_rows <- header_line - 1

message(
  "\nTrue header detected on line ",
  header_line,
  "."
)

message(
  "Rows skipped before import: ",
  skip_rows
)

# ------------------------------------------------------------
# 2. DETECT DELIMITER
# ------------------------------------------------------------

header_text <- raw_lines[[header_line]]

delimiter <- if (str_count(header_text, "\t") >= 2) {
  "\t"
} else if (str_count(header_text, ",") >= 2) {
  ","
} else {
  stop(
    "The file delimiter could not be determined from:\n",
    header_text
  )
}

message(
  "Detected delimiter: ",
  ifelse(delimiter == "\t", "TAB", "COMMA")
)

# ------------------------------------------------------------
# 3. IMPORT FROM THE TRUE HEADER
# ------------------------------------------------------------

mortality_raw <- read_delim(
  input_file,
  delim = delimiter,
  skip = skip_rows,
  show_col_types = FALSE,
  trim_ws = TRUE,
  quote = "\"",
  name_repair = "unique"
) |>
  clean_names()

cat("\nImported rows: ", nrow(mortality_raw), "\n")
cat("Imported columns: ", ncol(mortality_raw), "\n\n")

cat("Available columns:\n")

print(names(mortality_raw))

cat("\nFirst five imported rows:\n")

print(
  mortality_raw |>
    head(5)
)

# ------------------------------------------------------------
# 4. IDENTIFY COLUMNS
# ------------------------------------------------------------

column_names <- names(mortality_raw)

county_candidates <- column_names[
  str_detect(
    column_names,
    "^county$|county_name|geographic_area|^area$"
  )
]

rate_candidates <- column_names[
  str_detect(
    column_names,
    "death_rate|mortality_rate|age_adjusted.*rate|^rate$"
  ) &
    !str_detect(
      column_names,
      "lower|upper|confidence|trend|annual_percent|average"
    )
]

count_candidates <- column_names[
  str_detect(
    column_names,
    "average.*annual.*death|annual.*death|death.*count|average.*count"
  )
]

lower_ci_candidates <- column_names[
  str_detect(
    column_names,
    "lower.*confidence|lower.*ci|lower"
  )
]

upper_ci_candidates <- column_names[
  str_detect(
    column_names,
    "upper.*confidence|upper.*ci|upper"
  )
]

county_column <- if (length(county_candidates) > 0) {
  county_candidates[[1]]
} else {
  NA_character_
}

rate_column <- if (length(rate_candidates) > 0) {
  rate_candidates[[1]]
} else {
  NA_character_
}

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
  stop(
    "\nCounty column was not detected.\n",
    "Available columns:\n",
    paste(column_names, collapse = "\n")
  )
}

if (is.na(rate_column)) {
  stop(
    "\nMortality-rate column was not detected.\n",
    "Available columns:\n",
    paste(column_names, collapse = "\n")
  )
}

message("County column detected: ", county_column)
message("Mortality-rate column detected: ", rate_column)

if (!is.na(count_column)) {
  message("Death-count column detected: ", count_column)
} else {
  message("No average annual death-count column was detected.")
}

# ------------------------------------------------------------
# 5. CLEAN DATA
# ------------------------------------------------------------

mortality_clean <- mortality_raw |>
  transmute(
    county_raw = as.character(
      .data[[county_column]]
    ),
    
    mortality_rate_raw = as.character(
      .data[[rate_column]]
    ),
    
    average_annual_deaths_raw =
      if (!is.na(count_column)) {
        as.character(.data[[count_column]])
      } else {
        NA_character_
      },
    
    mortality_lower_ci_raw =
      if (!is.na(lower_ci_column)) {
        as.character(.data[[lower_ci_column]])
      } else {
        NA_character_
      },
    
    mortality_upper_ci_raw =
      if (!is.na(upper_ci_column)) {
        as.character(.data[[upper_ci_column]])
      } else {
        NA_character_
      }
  ) |>
  mutate(
    county = county_raw |>
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
    
    mortality_rate = parse_number(
      mortality_rate_raw,
      na = c(
        "",
        "NA",
        "N/A",
        "*",
        "**",
        "***",
        "Suppressed",
        "Data not available",
        "~"
      )
    ),
    
    average_annual_deaths = parse_number(
      average_annual_deaths_raw,
      na = c(
        "",
        "NA",
        "N/A",
        "*",
        "**",
        "***",
        "Suppressed",
        "Data not available",
        "~"
      )
    ),
    
    mortality_lower_ci = parse_number(
      mortality_lower_ci_raw,
      na = c(
        "",
        "NA",
        "N/A",
        "*",
        "**",
        "***",
        "~"
      )
    ),
    
    mortality_upper_ci = parse_number(
      mortality_upper_ci_raw,
      na = c(
        "",
        "NA",
        "N/A",
        "*",
        "**",
        "***",
        "~"
      )
    ),
    
    mortality_suppressed =
      is.na(mortality_rate) &
      str_detect(
        str_to_lower(mortality_rate_raw),
        "\\*|suppressed|not available|~"
      )
  ) |>
  filter(
    !is.na(county),
    county != ""
  )

# ------------------------------------------------------------
# 6. RETAIN ONLY OHIO'S 88 COUNTIES
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

mortality_clean <- mortality_clean |>
  filter(county %in% ohio_county_names) |>
  distinct(
    county,
    .keep_all = TRUE
  ) |>
  arrange(county)

# ------------------------------------------------------------
# 7. VALIDATION
# ------------------------------------------------------------

if (nrow(mortality_clean) != 88) {
  missing_counties <- setdiff(
    ohio_county_names,
    mortality_clean$county
  )
  
  warning(
    "Expected 88 counties but retained ",
    nrow(mortality_clean),
    ".\nMissing counties: ",
    paste(missing_counties, collapse = ", ")
  )
}

# ------------------------------------------------------------
# 8. SAVE CLEAN FILE
# ------------------------------------------------------------

dir.create(
  dirname(output_file),
  recursive = TRUE,
  showWarnings = FALSE
)

write_csv(
  mortality_clean,
  output_file
)

# ------------------------------------------------------------
# 9. PRINT RESULTS
# ------------------------------------------------------------

cat("\nPortage County mortality record:\n\n")

print(
  mortality_clean |>
    filter(county == "Portage") |>
    select(
      county,
      mortality_rate_raw,
      mortality_rate,
      average_annual_deaths_raw,
      average_annual_deaths,
      mortality_suppressed
    )
)

cat("\nMortality cleaning summary:\n\n")

print(
  mortality_clean |>
    summarise(
      counties = n(),
      available_rates = sum(
        !is.na(mortality_rate)
      ),
      missing_or_suppressed = sum(
        is.na(mortality_rate)
      )
    )
)

message(
  "\nClean mortality file saved to:\n",
  output_file
)