# ============================================================
# 18_rankings_and_percentiles.R
# ============================================================

library(tidyverse)
library(janitor)

input_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_epidemiologic_dataset.csv"
)

output_dir <- file.path(
  "outputs",
  "tables"
)

dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "Input file not found:\n",
      input_file,
      "\n\nRun R/14_create_epidemiologic_dataset.R first."
    )
  )
}

epi <- read_csv(input_file, show_col_types = FALSE) |>
  clean_names()

required_columns <- c(
  "county", "fips", "metro_status", "rurality_3_level",
  "population_2020", "incidence_rate", "incidence_lower_ci",
  "incidence_upper_ci", "average_annual_cases", "mortality_rate",
  "mortality_lower_ci", "mortality_upper_ci", "average_annual_deaths"
)

missing_columns <- setdiff(required_columns, names(epi))

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The epidemiologic dataset is missing these columns:\n",
      paste(missing_columns, collapse = ", ")
    )
  )
}

epi <- epi |>
  mutate(
    fips = str_pad(as.character(fips), width = 5, side = "left", pad = "0"),
    population_2020 = as.numeric(population_2020),
    across(
      c(
        incidence_rate, incidence_lower_ci, incidence_upper_ci,
        average_annual_cases, mortality_rate, mortality_lower_ci,
        mortality_upper_ci, average_annual_deaths
      ),
      as.numeric
    ),
    metro_status = factor(
      metro_status,
      levels = c("Metropolitan", "Nonmetropolitan")
    ),
    rurality_3_level = factor(
      rurality_3_level,
      levels = c(
        "Metropolitan",
        "Nonmetropolitan adjacent to metro",
        "Nonmetropolitan nonadjacent"
      )
    )
  )

safe_percentile <- function(x) {
  if (all(is.na(x))) return(rep(NA_real_, length(x)))
  rank(x, na.last = "keep", ties.method = "average") /
    sum(!is.na(x)) * 100
}

rank_descending <- function(x) {
  if_else(is.na(x), NA_integer_, min_rank(desc(x)))
}

rank_ascending <- function(x) {
  if_else(is.na(x), NA_integer_, min_rank(x))
}

ranked <- epi |>
  mutate(
    incidence_rank_high_to_low = rank_descending(incidence_rate),
    incidence_rank_low_to_high = rank_ascending(incidence_rate),
    incidence_percentile = safe_percentile(incidence_rate),
    mortality_rank_high_to_low = rank_descending(mortality_rate),
    mortality_rank_low_to_high = rank_ascending(mortality_rate),
    mortality_percentile = safe_percentile(mortality_rate)
  )

incidence_reportable_n <- sum(!is.na(ranked$incidence_rate))
mortality_reportable_n <- sum(!is.na(ranked$mortality_rate))

ranked <- ranked |>
  group_by(metro_status) |>
  mutate(
    incidence_rank_within_metro_status = rank_descending(incidence_rate),
    incidence_percentile_within_metro_status = safe_percentile(incidence_rate),
    mortality_rank_within_metro_status = rank_descending(mortality_rate),
    mortality_percentile_within_metro_status = safe_percentile(mortality_rate),
    incidence_reportable_within_metro_status = sum(!is.na(incidence_rate)),
    mortality_reportable_within_metro_status = sum(!is.na(mortality_rate))
  ) |>
  ungroup() |>
  group_by(rurality_3_level) |>
  mutate(
    incidence_rank_within_rurality_3_level = rank_descending(incidence_rate),
    incidence_percentile_within_rurality_3_level = safe_percentile(incidence_rate),
    mortality_rank_within_rurality_3_level = rank_descending(mortality_rate),
    mortality_percentile_within_rurality_3_level = safe_percentile(mortality_rate),
    incidence_reportable_within_rurality_3_level = sum(!is.na(incidence_rate)),
    mortality_reportable_within_rurality_3_level = sum(!is.na(mortality_rate))
  ) |>
  ungroup() |>
  mutate(
    incidence_rank_display = if_else(
      is.na(incidence_rank_high_to_low),
      "Not ranked",
      paste0(incidence_rank_high_to_low, " of ", incidence_reportable_n)
    ),
    mortality_rank_display = if_else(
      is.na(mortality_rank_high_to_low),
      "Not ranked",
      paste0(mortality_rank_high_to_low, " of ", mortality_reportable_n)
    ),
    incidence_percentile_display = if_else(
      is.na(incidence_percentile),
      "Not available",
      paste0(round(incidence_percentile, 1), "th percentile")
    ),
    mortality_percentile_display = if_else(
      is.na(mortality_percentile),
      "Not available",
      paste0(round(mortality_percentile, 1), "th percentile")
    )
  )

incidence_top10 <- ranked |>
  filter(!is.na(incidence_rate)) |>
  arrange(desc(incidence_rate), county) |>
  slice_head(n = 10) |>
  transmute(
    rank = incidence_rank_high_to_low,
    county, fips, metro_status, rurality_3_level,
    incidence_rate, incidence_lower_ci, incidence_upper_ci,
    average_annual_cases, incidence_percentile
  )

incidence_bottom10 <- ranked |>
  filter(!is.na(incidence_rate)) |>
  arrange(incidence_rate, county) |>
  slice_head(n = 10) |>
  transmute(
    rank_low_to_high = incidence_rank_low_to_high,
    statewide_rank_high_to_low = incidence_rank_high_to_low,
    county, fips, metro_status, rurality_3_level,
    incidence_rate, incidence_lower_ci, incidence_upper_ci,
    average_annual_cases, incidence_percentile
  )

mortality_top10 <- ranked |>
  filter(!is.na(mortality_rate)) |>
  arrange(desc(mortality_rate), county) |>
  slice_head(n = 10) |>
  transmute(
    rank = mortality_rank_high_to_low,
    county, fips, metro_status, rurality_3_level,
    mortality_rate, mortality_lower_ci, mortality_upper_ci,
    average_annual_deaths, mortality_percentile
  )

mortality_bottom10 <- ranked |>
  filter(!is.na(mortality_rate)) |>
  arrange(mortality_rate, county) |>
  slice_head(n = 10) |>
  transmute(
    rank_low_to_high = mortality_rank_low_to_high,
    statewide_rank_high_to_low = mortality_rank_high_to_low,
    county, fips, metro_status, rurality_3_level,
    mortality_rate, mortality_lower_ci, mortality_upper_ci,
    average_annual_deaths, mortality_percentile
  )

portage_ranking_profile <- ranked |>
  filter(str_to_lower(str_squish(county)) == "portage") |>
  transmute(
    county, fips, population_2020, metro_status, rurality_3_level,
    incidence_rate, incidence_lower_ci, incidence_upper_ci,
    average_annual_cases, incidence_rank_high_to_low,
    incidence_rank_display, incidence_percentile,
    incidence_percentile_display,
    incidence_rank_within_metro_status,
    incidence_reportable_within_metro_status,
    incidence_percentile_within_metro_status,
    incidence_rank_within_rurality_3_level,
    incidence_reportable_within_rurality_3_level,
    incidence_percentile_within_rurality_3_level,
    mortality_rate, mortality_lower_ci, mortality_upper_ci,
    average_annual_deaths, mortality_rank_high_to_low,
    mortality_rank_display, mortality_percentile,
    mortality_percentile_display,
    mortality_rank_within_metro_status,
    mortality_reportable_within_metro_status,
    mortality_percentile_within_metro_status,
    mortality_rank_within_rurality_3_level,
    mortality_reportable_within_rurality_3_level,
    mortality_percentile_within_rurality_3_level
  )

cat("\nPortage County ranking profile:\n")

if (nrow(portage_ranking_profile) == 0) {
  cat("Portage County was not found.\n")
} else {
  for (variable_name in names(portage_ranking_profile)) {
    value <- portage_ranking_profile[[variable_name]][1]
    value_text <- if (length(value) == 0 || is.na(value)) {
      "NA"
    } else {
      as.character(value)
    }
    
    cat(
      sprintf(
        "%-48s %s\n",
        paste0(variable_name, ":"),
        value_text
      )
    )
  }
}

write_csv(ranked, file.path(output_dir, "18_county_rankings.csv"), na = "")
write_csv(incidence_top10, file.path(output_dir, "18_incidence_top10.csv"), na = "")
write_csv(incidence_bottom10, file.path(output_dir, "18_incidence_bottom10.csv"), na = "")
write_csv(mortality_top10, file.path(output_dir, "18_mortality_top10.csv"), na = "")
write_csv(mortality_bottom10, file.path(output_dir, "18_mortality_bottom10.csv"), na = "")
write_csv(portage_ranking_profile, file.path(output_dir, "18_portage_ranking_profile.csv"), na = "")

cat("\nTop 10 reportable incidence counties:\n")
print(
  incidence_top10 |>
    select(rank, county, incidence_rate, incidence_lower_ci, incidence_upper_ci)
)

cat("\nTop 10 reportable mortality counties:\n")
print(
  mortality_top10 |>
    select(rank, county, mortality_rate, mortality_lower_ci, mortality_upper_ci)
)

highest_incidence_county <- incidence_top10$county[[1]]
highest_incidence_rate <- incidence_top10$incidence_rate[[1]]
highest_mortality_county <- mortality_top10$county[[1]]
highest_mortality_rate <- mortality_top10$mortality_rate[[1]]

portage_incidence_rank <- if (nrow(portage_ranking_profile) == 1) {
  portage_ranking_profile$incidence_rank_display[[1]]
} else {
  "Not available"
}

portage_mortality_rank <- if (nrow(portage_ranking_profile) == 1) {
  portage_ranking_profile$mortality_rank_display[[1]]
} else {
  "Not available"
}

summary_lines <- c(
  "OHIO OVARIAN CANCER DESCRIPTIVE RANKINGS",
  "",
  paste0(
    "Incidence estimates were reportable for ",
    incidence_reportable_n,
    " of 88 counties."
  ),
  paste0(
    "Mortality estimates were reportable for ",
    mortality_reportable_n,
    " of 88 counties."
  ),
  "",
  paste0(
    "Highest reportable incidence estimate: ",
    highest_incidence_county,
    " County, ",
    format(highest_incidence_rate, nsmall = 1),
    " per 100,000."
  ),
  paste0(
    "Highest reportable mortality estimate: ",
    highest_mortality_county,
    " County, ",
    format(highest_mortality_rate, nsmall = 1),
    " per 100,000."
  ),
  "",
  paste0("Portage incidence rank: ", portage_incidence_rank, "."),
  paste0("Portage mortality rank: ", portage_mortality_rank, "."),
  "",
  "Interpretation note:",
  paste0(
    "These rankings are descriptive and exclude counties with ",
    "suppressed or unavailable estimates. Rankings do not account ",
    "for overlapping confidence intervals and should not be treated ",
    "as evidence that neighboring ranks are statistically different."
  )
)

write_lines(
  summary_lines,
  file.path(output_dir, "18_rankings_summary.txt")
)

cat("\n", paste(summary_lines, collapse = "\n"), "\n", sep = "")
cat("\nRankings and percentile analysis completed successfully.\n")
cat("\nFiles saved in:\n", output_dir, "\n", sep = "")