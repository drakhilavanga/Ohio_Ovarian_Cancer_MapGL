# ============================================================
# 17_uncertainty_analysis.R
#
# Analyze statistical precision of county-level ovarian cancer
# incidence and mortality estimates using confidence intervals.
#
# Analyses:
#   1. CI width and relative CI width
#   2. Precision categories
#   3. Rural–urban comparison of uncertainty
#   4. Association between annual counts and CI width
#   5. Portage County uncertainty profile
#   6. Export tables for the app and manuscript
#
# Input:
#   data/processed/
#   ohio_ovarian_cancer_epidemiologic_dataset.csv
#
# Outputs:
#   outputs/tables/
#   17_uncertainty_county_data.csv
#   17_uncertainty_summary.csv
#   17_uncertainty_by_metro_status.csv
#   17_uncertainty_by_rurality_3_level.csv
#   17_uncertainty_test_results.csv
#   17_count_ci_correlation_results.csv
#   17_portage_uncertainty_profile.csv
#   17_uncertainty_analysis_summary.txt
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
      "The analytic dataset is missing these columns:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

# ============================================================
# 3. STANDARDIZE TYPES
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
      )
    )
  )

# ============================================================
# 4. CREATE UNCERTAINTY VARIABLES
# ============================================================

epi <- epi |>
  mutate(
    incidence_ci_width = if_else(
      !is.na(incidence_lower_ci) &
        !is.na(incidence_upper_ci),
      incidence_upper_ci -
        incidence_lower_ci,
      NA_real_
    ),
    
    mortality_ci_width = if_else(
      !is.na(mortality_lower_ci) &
        !is.na(mortality_upper_ci),
      mortality_upper_ci -
        mortality_lower_ci,
      NA_real_
    ),
    
    incidence_relative_ci_width = if_else(
      !is.na(incidence_ci_width) &
        !is.na(incidence_rate) &
        incidence_rate > 0,
      incidence_ci_width /
        incidence_rate,
      NA_real_
    ),
    
    mortality_relative_ci_width = if_else(
      !is.na(mortality_ci_width) &
        !is.na(mortality_rate) &
        mortality_rate > 0,
      mortality_ci_width /
        mortality_rate,
      NA_real_
    )
  )

# ============================================================
# 5. PRECISION CATEGORIES
#
# Based on quintiles of relative CI width among reportable
# estimates. Lower relative width = more precise.
# ============================================================

make_precision_class <- function(x) {
  valid <- !is.na(x)
  out <- rep(
    "Suppressed / unavailable",
    length(x)
  )
  
  if (sum(valid) >= 5) {
    class_number <- ntile(
      x[valid],
      5
    )
    
    labels <- c(
      "Highest precision",
      "High precision",
      "Moderate precision",
      "Low precision",
      "Lowest precision"
    )
    
    out[valid] <- labels[
      class_number
    ]
  }
  
  out
}

epi <- epi |>
  mutate(
    incidence_precision_class = make_precision_class(
      incidence_relative_ci_width
    ),
    
    mortality_precision_class = make_precision_class(
      mortality_relative_ci_width
    )
  )

# ============================================================
# 6. OVERALL UNCERTAINTY SUMMARY
# ============================================================

summarise_uncertainty <- function(
    data,
    measure_name,
    rate_variable,
    ci_width_variable,
    relative_width_variable
) {
  data |>
    filter(
      !is.na(
        .data[[rate_variable]]
      ),
      !is.na(
        .data[[ci_width_variable]]
      ),
      !is.na(
        .data[[relative_width_variable]]
      )
    ) |>
    summarise(
      measure = measure_name,
      reportable_counties = n(),
      
      median_rate = median(
        .data[[rate_variable]],
        na.rm = TRUE
      ),
      
      median_ci_width = median(
        .data[[ci_width_variable]],
        na.rm = TRUE
      ),
      
      q1_ci_width = quantile(
        .data[[ci_width_variable]],
        0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      
      q3_ci_width = quantile(
        .data[[ci_width_variable]],
        0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      
      minimum_ci_width = min(
        .data[[ci_width_variable]],
        na.rm = TRUE
      ),
      
      maximum_ci_width = max(
        .data[[ci_width_variable]],
        na.rm = TRUE
      ),
      
      median_relative_ci_width = median(
        .data[[relative_width_variable]],
        na.rm = TRUE
      ),
      
      minimum_relative_ci_width = min(
        .data[[relative_width_variable]],
        na.rm = TRUE
      ),
      
      maximum_relative_ci_width = max(
        .data[[relative_width_variable]],
        na.rm = TRUE
      )
    )
}

uncertainty_summary <- bind_rows(
  summarise_uncertainty(
    epi,
    "Incidence",
    "incidence_rate",
    "incidence_ci_width",
    "incidence_relative_ci_width"
  ),
  
  summarise_uncertainty(
    epi,
    "Mortality",
    "mortality_rate",
    "mortality_ci_width",
    "mortality_relative_ci_width"
  )
)

cat(
  "\nOverall uncertainty summary:\n"
)

print(
  uncertainty_summary
)

write_csv(
  uncertainty_summary,
  file.path(
    output_dir,
    "17_uncertainty_summary.csv"
  )
)

# ============================================================
# 7. GROUPED UNCERTAINTY SUMMARY
# ============================================================

summarise_uncertainty_by_group <- function(
    data,
    group_variable,
    measure_name,
    ci_width_variable,
    relative_width_variable
) {
  data |>
    filter(
      !is.na(
        .data[[group_variable]]
      ),
      !is.na(
        .data[[ci_width_variable]]
      ),
      !is.na(
        .data[[relative_width_variable]]
      )
    ) |>
    group_by(
      group = .data[[group_variable]]
    ) |>
    summarise(
      reportable_counties = n(),
      
      median_ci_width = median(
        .data[[ci_width_variable]],
        na.rm = TRUE
      ),
      
      q1_ci_width = quantile(
        .data[[ci_width_variable]],
        0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      
      q3_ci_width = quantile(
        .data[[ci_width_variable]],
        0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      
      median_relative_ci_width = median(
        .data[[relative_width_variable]],
        na.rm = TRUE
      ),
      
      q1_relative_ci_width = quantile(
        .data[[relative_width_variable]],
        0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      
      q3_relative_ci_width = quantile(
        .data[[relative_width_variable]],
        0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      
      .groups = "drop"
    ) |>
    mutate(
      measure = measure_name,
      grouping_variable = group_variable,
      .before = 1
    )
}

uncertainty_by_metro <- bind_rows(
  summarise_uncertainty_by_group(
    epi,
    "metro_status",
    "Incidence",
    "incidence_ci_width",
    "incidence_relative_ci_width"
  ),
  
  summarise_uncertainty_by_group(
    epi,
    "metro_status",
    "Mortality",
    "mortality_ci_width",
    "mortality_relative_ci_width"
  )
)

uncertainty_by_rurality3 <- bind_rows(
  summarise_uncertainty_by_group(
    epi,
    "rurality_3_level",
    "Incidence",
    "incidence_ci_width",
    "incidence_relative_ci_width"
  ),
  
  summarise_uncertainty_by_group(
    epi,
    "rurality_3_level",
    "Mortality",
    "mortality_ci_width",
    "mortality_relative_ci_width"
  )
)

cat(
  "\nUncertainty by metropolitan status:\n"
)

print(
  uncertainty_by_metro
)

cat(
  "\nUncertainty by three-level rurality:\n"
)

print(
  uncertainty_by_rurality3
)

write_csv(
  uncertainty_by_metro,
  file.path(
    output_dir,
    "17_uncertainty_by_metro_status.csv"
  )
)

write_csv(
  uncertainty_by_rurality3,
  file.path(
    output_dir,
    "17_uncertainty_by_rurality_3_level.csv"
  )
)

# ============================================================
# 8. NONPARAMETRIC TESTS
# ============================================================

run_wilcoxon <- function(
    data,
    outcome_variable,
    group_variable,
    analysis_name
) {
  analysis_data <- data |>
    filter(
      !is.na(
        .data[[outcome_variable]]
      ),
      !is.na(
        .data[[group_variable]]
      )
    ) |>
    droplevels()
  
  if (
    n_distinct(
      analysis_data[[group_variable]]
    ) != 2
  ) {
    return(
      tibble(
        analysis = analysis_name,
        outcome = outcome_variable,
        grouping_variable = group_variable,
        n_counties = nrow(analysis_data),
        statistic = NA_real_,
        p_value = NA_real_,
        method = "Not run: grouping variable does not have exactly two levels"
      )
    )
  }
  
  test_result <- wilcox.test(
    analysis_data[[outcome_variable]] ~
      analysis_data[[group_variable]],
    exact = FALSE
  )
  
  tibble(
    analysis = analysis_name,
    outcome = outcome_variable,
    grouping_variable = group_variable,
    n_counties = nrow(analysis_data),
    statistic = unname(
      test_result$statistic
    ),
    p_value = unname(
      test_result$p.value
    ),
    method = test_result$method
  )
}

run_kruskal <- function(
    data,
    outcome_variable,
    group_variable,
    analysis_name
) {
  analysis_data <- data |>
    filter(
      !is.na(
        .data[[outcome_variable]]
      ),
      !is.na(
        .data[[group_variable]]
      )
    )
  
  if (
    n_distinct(
      analysis_data[[group_variable]]
    ) < 2
  ) {
    return(
      tibble(
        analysis = analysis_name,
        outcome = outcome_variable,
        grouping_variable = group_variable,
        n_counties = nrow(analysis_data),
        statistic = NA_real_,
        degrees_of_freedom = NA_real_,
        p_value = NA_real_,
        method = "Not run: fewer than two groups"
      )
    )
  }
  
  test_result <- kruskal.test(
    analysis_data[[outcome_variable]] ~
      analysis_data[[group_variable]]
  )
  
  tibble(
    analysis = analysis_name,
    outcome = outcome_variable,
    grouping_variable = group_variable,
    n_counties = nrow(analysis_data),
    statistic = unname(
      test_result$statistic
    ),
    degrees_of_freedom = unname(
      test_result$parameter
    ),
    p_value = unname(
      test_result$p.value
    ),
    method = test_result$method
  )
}

wilcoxon_results <- bind_rows(
  run_wilcoxon(
    epi,
    "incidence_relative_ci_width",
    "metro_status",
    "Incidence relative CI width by metropolitan status"
  ),
  
  run_wilcoxon(
    epi,
    "mortality_relative_ci_width",
    "metro_status",
    "Mortality relative CI width by metropolitan status"
  )
)

kruskal_results <- bind_rows(
  run_kruskal(
    epi,
    "incidence_relative_ci_width",
    "rurality_3_level",
    "Incidence relative CI width by three-level rurality"
  ),
  
  run_kruskal(
    epi,
    "mortality_relative_ci_width",
    "rurality_3_level",
    "Mortality relative CI width by three-level rurality"
  )
)

uncertainty_test_results <- bind_rows(
  wilcoxon_results |>
    mutate(
      degrees_of_freedom = NA_real_
    ),
  
  kruskal_results
) |>
  mutate(
    statistically_significant_0_05 =
      p_value < 0.05
  ) |>
  select(
    analysis,
    outcome,
    grouping_variable,
    n_counties,
    statistic,
    degrees_of_freedom,
    p_value,
    statistically_significant_0_05,
    method
  )

cat(
  "\nUncertainty test results:\n"
)

print(
  uncertainty_test_results
)

write_csv(
  uncertainty_test_results,
  file.path(
    output_dir,
    "17_uncertainty_test_results.csv"
  )
)

# ============================================================
# 9. CORRELATION BETWEEN ANNUAL COUNTS AND CI WIDTH
# ============================================================

run_spearman <- function(
    data,
    count_variable,
    uncertainty_variable,
    analysis_name
) {
  analysis_data <- data |>
    filter(
      !is.na(
        .data[[count_variable]]
      ),
      !is.na(
        .data[[uncertainty_variable]]
      )
    )
  
  if (nrow(analysis_data) < 3) {
    return(
      tibble(
        analysis = analysis_name,
        count_variable = count_variable,
        uncertainty_variable = uncertainty_variable,
        n_counties = nrow(analysis_data),
        spearman_rho = NA_real_,
        p_value = NA_real_,
        method = "Not run: fewer than three complete observations"
      )
    )
  }
  
  test_result <- cor.test(
    analysis_data[[count_variable]],
    analysis_data[[uncertainty_variable]],
    method = "spearman",
    exact = FALSE
  )
  
  tibble(
    analysis = analysis_name,
    count_variable = count_variable,
    uncertainty_variable = uncertainty_variable,
    n_counties = nrow(analysis_data),
    spearman_rho = unname(
      test_result$estimate
    ),
    p_value = unname(
      test_result$p.value
    ),
    method = test_result$method
  )
}

correlation_results <- bind_rows(
  run_spearman(
    epi,
    "average_annual_cases",
    "incidence_ci_width",
    "Average annual cases versus incidence CI width"
  ),
  
  run_spearman(
    epi,
    "average_annual_cases",
    "incidence_relative_ci_width",
    "Average annual cases versus incidence relative CI width"
  ),
  
  run_spearman(
    epi,
    "average_annual_deaths",
    "mortality_ci_width",
    "Average annual deaths versus mortality CI width"
  ),
  
  run_spearman(
    epi,
    "average_annual_deaths",
    "mortality_relative_ci_width",
    "Average annual deaths versus mortality relative CI width"
  )
) |>
  mutate(
    statistically_significant_0_05 =
      p_value < 0.05
  )

cat(
  "\nAnnual-count and uncertainty correlations:\n"
)

print(
  correlation_results
)

write_csv(
  correlation_results,
  file.path(
    output_dir,
    "17_count_ci_correlation_results.csv"
  )
)

# ============================================================
# 10. PORTAGE COUNTY UNCERTAINTY PROFILE
# ============================================================

portage_uncertainty <- epi |>
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
    metro_status,
    rurality_3_level,
    
    incidence_rate,
    incidence_lower_ci,
    incidence_upper_ci,
    incidence_ci_width,
    incidence_relative_ci_width,
    incidence_precision_class,
    average_annual_cases,
    
    mortality_rate,
    mortality_lower_ci,
    mortality_upper_ci,
    mortality_ci_width,
    mortality_relative_ci_width,
    mortality_precision_class,
    average_annual_deaths
  )

cat(
  "\nPortage County uncertainty profile:\n"
)

if (nrow(portage_uncertainty) == 0) {
  cat("Portage County was not found.\n")
} else {
  for (variable_name in names(portage_uncertainty)) {
    value <- portage_uncertainty[[variable_name]][1]
    
    if (length(value) == 0 || is.na(value)) {
      value_text <- "NA"
    } else {
      value_text <- as.character(value)
    }
    
    cat(
      sprintf(
        "%-38s %s\n",
        paste0(
          variable_name,
          ":"
        ),
        value_text
      )
    )
  }
}

write_csv(
  portage_uncertainty,
  file.path(
    output_dir,
    "17_portage_uncertainty_profile.csv"
  )
)

# ============================================================
# 11. COUNTY-LEVEL UNCERTAINTY EXPORT
# ============================================================

county_uncertainty <- epi |>
  transmute(
    county,
    fips,
    metro_status,
    rurality_3_level,
    
    incidence_rate,
    incidence_lower_ci,
    incidence_upper_ci,
    incidence_ci_width,
    incidence_relative_ci_width,
    incidence_precision_class,
    average_annual_cases,
    
    mortality_rate,
    mortality_lower_ci,
    mortality_upper_ci,
    mortality_ci_width,
    mortality_relative_ci_width,
    mortality_precision_class,
    average_annual_deaths
  ) |>
  arrange(
    county
  )

write_csv(
  county_uncertainty,
  file.path(
    output_dir,
    "17_uncertainty_county_data.csv"
  )
)

# ============================================================
# 12. PLAIN-LANGUAGE SUMMARY
# ============================================================

get_result <- function(
    table_data,
    measure_name,
    variable_name
) {
  result <- table_data |>
    filter(
      measure == measure_name
    ) |>
    pull(
      all_of(
        variable_name
      )
    )
  
  if (length(result) == 0) {
    return(NA_real_)
  }
  
  result[[1]]
}

incidence_median_relative_width <- get_result(
  uncertainty_summary,
  "Incidence",
  "median_relative_ci_width"
)

mortality_median_relative_width <- get_result(
  uncertainty_summary,
  "Mortality",
  "median_relative_ci_width"
)

summary_lines <- c(
  "OHIO OVARIAN CANCER UNCERTAINTY ANALYSIS",
  "",
  paste0(
    "Median relative CI width for incidence: ",
    round(
      incidence_median_relative_width,
      2
    ),
    "."
  ),
  paste0(
    "Median relative CI width for mortality: ",
    round(
      mortality_median_relative_width,
      2
    ),
    "."
  ),
  "",
  "Interpretation note:",
  paste0(
    "A larger relative confidence-interval width indicates a less precise ",
    "county estimate. Precision is expected to be lower where annual case ",
    "or death counts are smaller."
  ),
  "",
  paste0(
    "County rankings should be interpreted cautiously because estimates ",
    "with overlapping confidence intervals may not be meaningfully different."
  )
)

write_lines(
  summary_lines,
  file.path(
    output_dir,
    "17_uncertainty_analysis_summary.txt"
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
  "\nUncertainty analysis completed successfully.\n"
)

cat(
  "\nFiles saved in:\n",
  output_dir,
  "\n",
  sep = ""
) 