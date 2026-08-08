# ============================================================
# 15_suppression_analysis.R
#
# Epidemiologic analysis of county-level data availability:
#   1. Incidence reportability by rurality
#   2. Mortality reportability by rurality
#   3. Reportability by county population group
#   4. Fisher exact tests
#   5. Logistic regression models
#   6. Export publication-ready summary tables
#
# Input:
#   data/processed/
#   ohio_ovarian_cancer_epidemiologic_dataset.csv
#
# Outputs:
#   outputs/tables/
#   15_suppression_summary.csv
#   15_suppression_by_metro_status.csv
#   15_suppression_by_rurality_3_level.csv
#   15_suppression_by_population_group.csv
#   15_fisher_test_results.csv
#   15_logistic_regression_results.csv
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
# 2. READ DATA
# ============================================================

epi <- read_csv(
  input_file,
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
  "population_group",
  "incidence_rate",
  "mortality_rate",
  "incidence_reportable",
  "mortality_reportable"
)

missing_columns <- setdiff(
  required_columns,
  names(epi)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The analytic dataset is missing these columns:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

# ============================================================
# 3. STANDARDIZE VARIABLE TYPES
# ============================================================

as_logical_safe <- function(x) {
  if (is.logical(x)) return(x)
  
  x_chr <- str_to_lower(
    str_trim(
      as.character(x)
    )
  )
  
  case_when(
    x_chr %in% c("true", "t", "1", "yes") ~ TRUE,
    x_chr %in% c("false", "f", "0", "no") ~ FALSE,
    TRUE ~ NA
  )
}

epi <- epi |>
  mutate(
    fips = str_pad(
      as.character(fips),
      width = 5,
      side = "left",
      pad = "0"
    ),
    
    population_2020 = as.numeric(
      population_2020
    ),
    
    rucc_2023 = as.integer(
      rucc_2023
    ),
    
    incidence_reportable = as_logical_safe(
      incidence_reportable
    ),
    
    mortality_reportable = as_logical_safe(
      mortality_reportable
    ),
    
    incidence_suppressed = !incidence_reportable,
    
    mortality_suppressed = !mortality_reportable,
    
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
      )
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
    )
  )

if (any(is.na(epi$incidence_reportable))) {
  stop(
    "One or more incidence_reportable values could not be converted to TRUE/FALSE."
  )
}

if (any(is.na(epi$mortality_reportable))) {
  stop(
    "One or more mortality_reportable values could not be converted to TRUE/FALSE."
  )
}

# ============================================================
# 4. OVERALL SUPPRESSION SUMMARY
# ============================================================

suppression_summary <- tibble(
  measure = c(
    "Incidence",
    "Mortality"
  ),
  
  total_counties = nrow(epi),
  
  reportable_counties = c(
    sum(epi$incidence_reportable),
    sum(epi$mortality_reportable)
  ),
  
  suppressed_counties = c(
    sum(epi$incidence_suppressed),
    sum(epi$mortality_suppressed)
  )
) |>
  mutate(
    reportable_percent = round(
      100 * reportable_counties /
        total_counties,
      1
    ),
    
    suppressed_percent = round(
      100 * suppressed_counties /
        total_counties,
      1
    )
  )

cat(
  "\nOverall surveillance-data availability:\n"
)

print(
  suppression_summary
)

write_csv(
  suppression_summary,
  file.path(
    output_dir,
    "15_suppression_summary.csv"
  )
)

# ============================================================
# 5. HELPER: GROUPED SUPPRESSION TABLE
# ============================================================

make_grouped_table <- function(
    data,
    group_variable,
    reportable_variable,
    measure_name
) {
  data |>
    filter(
      !is.na(
        .data[[group_variable]]
      )
    ) |>
    group_by(
      group = .data[[group_variable]]
    ) |>
    summarise(
      total_counties = n(),
      
      reportable_counties = sum(
        .data[[reportable_variable]],
        na.rm = TRUE
      ),
      
      suppressed_counties = sum(
        !.data[[reportable_variable]],
        na.rm = TRUE
      ),
      
      reportable_percent = round(
        100 *
          reportable_counties /
          total_counties,
        1
      ),
      
      suppressed_percent = round(
        100 *
          suppressed_counties /
          total_counties,
        1
      ),
      
      .groups = "drop"
    ) |>
    mutate(
      measure = measure_name,
      grouping_variable = group_variable,
      .before = 1
    )
}

# ============================================================
# 6. SUPPRESSION BY METRO STATUS
# ============================================================

incidence_by_metro <- make_grouped_table(
  epi,
  "metro_status",
  "incidence_reportable",
  "Incidence"
)

mortality_by_metro <- make_grouped_table(
  epi,
  "metro_status",
  "mortality_reportable",
  "Mortality"
)

suppression_by_metro <- bind_rows(
  incidence_by_metro,
  mortality_by_metro
)

cat(
  "\nSuppression by metropolitan status:\n"
)

print(
  suppression_by_metro
)

write_csv(
  suppression_by_metro,
  file.path(
    output_dir,
    "15_suppression_by_metro_status.csv"
  )
)

# ============================================================
# 7. SUPPRESSION BY THREE-LEVEL RURALITY
# ============================================================

incidence_by_rurality3 <- make_grouped_table(
  epi,
  "rurality_3_level",
  "incidence_reportable",
  "Incidence"
)

mortality_by_rurality3 <- make_grouped_table(
  epi,
  "rurality_3_level",
  "mortality_reportable",
  "Mortality"
)

suppression_by_rurality3 <- bind_rows(
  incidence_by_rurality3,
  mortality_by_rurality3
)

cat(
  "\nSuppression by three-level rurality:\n"
)

print(
  suppression_by_rurality3
)

write_csv(
  suppression_by_rurality3,
  file.path(
    output_dir,
    "15_suppression_by_rurality_3_level.csv"
  )
)

# ============================================================
# 8. SUPPRESSION BY POPULATION GROUP
# ============================================================

incidence_by_population <- make_grouped_table(
  epi,
  "population_group",
  "incidence_reportable",
  "Incidence"
)

mortality_by_population <- make_grouped_table(
  epi,
  "population_group",
  "mortality_reportable",
  "Mortality"
)

suppression_by_population <- bind_rows(
  incidence_by_population,
  mortality_by_population
)

cat(
  "\nSuppression by county population group:\n"
)

print(
  suppression_by_population
)

write_csv(
  suppression_by_population,
  file.path(
    output_dir,
    "15_suppression_by_population_group.csv"
  )
)

# ============================================================
# 9. FISHER EXACT TESTS
#
# Fisher's exact test is used because some cells may be small.
# ============================================================

run_fisher_test <- function(
    data,
    exposure_variable,
    outcome_variable,
    analysis_name
) {
  analysis_data <- data |>
    filter(
      !is.na(
        .data[[exposure_variable]]
      ),
      !is.na(
        .data[[outcome_variable]]
      )
    )
  
  contingency_table <- table(
    analysis_data[[exposure_variable]],
    analysis_data[[outcome_variable]]
  )
  
  test_result <- fisher.test(
    contingency_table
  )
  
  tibble(
    analysis = analysis_name,
    exposure = exposure_variable,
    outcome = outcome_variable,
    n_counties = nrow(analysis_data),
    p_value = unname(
      test_result$p.value
    ),
    method = test_result$method
  )
}

fisher_results <- bind_rows(
  run_fisher_test(
    epi,
    "metro_status",
    "incidence_reportable",
    "Incidence reportability by metropolitan status"
  ),
  
  run_fisher_test(
    epi,
    "metro_status",
    "mortality_reportable",
    "Mortality reportability by metropolitan status"
  ),
  
  run_fisher_test(
    epi,
    "rurality_3_level",
    "incidence_reportable",
    "Incidence reportability by three-level rurality"
  ),
  
  run_fisher_test(
    epi,
    "rurality_3_level",
    "mortality_reportable",
    "Mortality reportability by three-level rurality"
  ),
  
  run_fisher_test(
    epi,
    "population_group",
    "incidence_reportable",
    "Incidence reportability by county population group"
  ),
  
  run_fisher_test(
    epi,
    "population_group",
    "mortality_reportable",
    "Mortality reportability by county population group"
  )
) |>
  mutate(
    statistically_significant_0_05 =
      p_value < 0.05
  )

cat(
  "\nFisher exact test results:\n"
)

print(
  fisher_results
)

write_csv(
  fisher_results,
  file.path(
    output_dir,
    "15_fisher_test_results.csv"
  )
)

# ============================================================
# 10. LOGISTIC REGRESSION
#
# Outcome:
#   1 = reportable
#   0 = suppressed / unavailable
#
# Predictors:
#   metropolitan status
#   log-transformed 2020 population
#
# These models evaluate whether population size explains some
# of the rural-urban difference in reportability.
# ============================================================

epi <- epi |>
  mutate(
    log_population_2020 = log(
      population_2020
    )
  )

fit_logistic_model <- function(
    data,
    outcome_variable,
    model_name
) {
  model_data <- data |>
    filter(
      !is.na(
        .data[[outcome_variable]]
      ),
      !is.na(
        metro_status
      ),
      !is.na(
        log_population_2020
      )
    )
  
  formula_object <- as.formula(
    paste0(
      outcome_variable,
      " ~ metro_status + log_population_2020"
    )
  )
  
  model <- glm(
    formula_object,
    data = model_data,
    family = binomial(
      link = "logit"
    )
  )
  
  coefficient_table <- summary(
    model
  )$coefficients
  
  result <- as.data.frame(
    coefficient_table
  ) |>
    rownames_to_column(
      var = "term"
    ) |>
    as_tibble() |>
    clean_names() |>
    transmute(
      model = model_name,
      outcome = outcome_variable,
      n_counties = nrow(model_data),
      term,
      log_odds_estimate = estimate,
      standard_error = std_error,
      z_value = z_value,
      p_value = pr_z,
      odds_ratio = exp(
        estimate
      ),
      confidence_interval_lower = exp(
        estimate -
          1.96 *
          std_error
      ),
      confidence_interval_upper = exp(
        estimate +
          1.96 *
          std_error
      )
    )
  
  list(
    model = model,
    results = result
  )
}

incidence_model <- fit_logistic_model(
  epi,
  "incidence_reportable",
  "Incidence reportability model"
)

mortality_model <- fit_logistic_model(
  epi,
  "mortality_reportable",
  "Mortality reportability model"
)

logistic_results <- bind_rows(
  incidence_model$results,
  mortality_model$results
) |>
  mutate(
    statistically_significant_0_05 =
      p_value < 0.05
  )

cat(
  "\nLogistic regression results:\n"
)

print(
  logistic_results
)

write_csv(
  logistic_results,
  file.path(
    output_dir,
    "15_logistic_regression_results.csv"
  )
)

# ============================================================
# 11. COUNTY-LEVEL SUPPRESSION EXPORT
# ============================================================

county_suppression_data <- epi |>
  transmute(
    county,
    fips,
    population_2020,
    population_group,
    rucc_2023,
    metro_status,
    rurality_3_level,
    
    incidence_rate,
    incidence_reportable,
    incidence_suppressed,
    
    mortality_rate,
    mortality_reportable,
    mortality_suppressed,
    
    surveillance_availability
  ) |>
  arrange(
    county
  )

write_csv(
  county_suppression_data,
  file.path(
    output_dir,
    "15_county_suppression_status.csv"
  )
)

# ============================================================
# 12. AUTOMATED PLAIN-LANGUAGE SUMMARY
# ============================================================

get_group_percent <- function(
    table_data,
    measure_name,
    group_name
) {
  value <- table_data |>
    filter(
      measure == measure_name,
      as.character(group) == group_name
    ) |>
    pull(
      suppressed_percent
    )
  
  if (length(value) == 0) {
    return(NA_real_)
  }
  
  value[[1]]
}

incidence_metro_suppressed <- get_group_percent(
  suppression_by_metro,
  "Incidence",
  "Metropolitan"
)

incidence_nonmetro_suppressed <- get_group_percent(
  suppression_by_metro,
  "Incidence",
  "Nonmetropolitan"
)

mortality_metro_suppressed <- get_group_percent(
  suppression_by_metro,
  "Mortality",
  "Metropolitan"
)

mortality_nonmetro_suppressed <- get_group_percent(
  suppression_by_metro,
  "Mortality",
  "Nonmetropolitan"
)

summary_lines <- c(
  "OHIO OVARIAN CANCER SURVEILLANCE SUPPRESSION ANALYSIS",
  "",
  paste0(
    "Incidence estimates were reportable for ",
    suppression_summary$reportable_counties[
      suppression_summary$measure == "Incidence"
    ],
    " of ",
    nrow(epi),
    " counties (",
    suppression_summary$reportable_percent[
      suppression_summary$measure == "Incidence"
    ],
    "%)."
  ),
  paste0(
    "Mortality estimates were reportable for ",
    suppression_summary$reportable_counties[
      suppression_summary$measure == "Mortality"
    ],
    " of ",
    nrow(epi),
    " counties (",
    suppression_summary$reportable_percent[
      suppression_summary$measure == "Mortality"
    ],
    "%)."
  ),
  "",
  paste0(
    "Incidence suppression: ",
    incidence_metro_suppressed,
    "% among metropolitan counties versus ",
    incidence_nonmetro_suppressed,
    "% among nonmetropolitan counties."
  ),
  paste0(
    "Mortality suppression: ",
    mortality_metro_suppressed,
    "% among metropolitan counties versus ",
    mortality_nonmetro_suppressed,
    "% among nonmetropolitan counties."
  ),
  "",
  "Interpretation note:",
  paste0(
    "Suppression reflects insufficient counts or unstable estimates. ",
    "It should not be interpreted as absence of ovarian cancer."
  )
)

write_lines(
  summary_lines,
  file.path(
    output_dir,
    "15_suppression_analysis_summary.txt"
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
# 13. FINAL MESSAGE
# ============================================================

cat(
  "\nSuppression analysis completed successfully.\n"
)

cat(
  "\nFiles saved in:\n",
  output_dir,
  "\n",
  sep = ""
)