# ============================================================
# 16_rural_urban_rate_analysis.R
#
# Rural–urban comparison of reportable ovarian cancer rates
#
# Analyses:
#   1. Descriptive statistics by metropolitan status
#   2. Descriptive statistics by three-level rurality
#   3. Wilcoxon rank-sum tests
#   4. Kruskal–Wallis tests
#   5. Effect-size estimates
#   6. Population-weighted sensitivity summaries
#   7. Publication-ready exports
#
# Input:
#   data/processed/
#   ohio_ovarian_cancer_epidemiologic_dataset.csv
#
# Outputs:
#   outputs/tables/
#   16_incidence_by_metro_status.csv
#   16_mortality_by_metro_status.csv
#   16_incidence_by_rurality_3_level.csv
#   16_mortality_by_rurality_3_level.csv
#   16_nonparametric_test_results.csv
#   16_effect_size_results.csv
#   16_weighted_rate_summary.csv
#   16_rural_urban_analysis_summary.txt
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
  "metro_status",
  "rurality_3_level",
  "incidence_rate",
  "mortality_rate",
  "average_annual_cases",
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
    
    population_2020 = as.numeric(
      population_2020
    ),
    
    incidence_rate = as.numeric(
      incidence_rate
    ),
    
    mortality_rate = as.numeric(
      mortality_rate
    ),
    
    average_annual_cases = as.numeric(
      average_annual_cases
    ),
    
    average_annual_deaths = as.numeric(
      average_annual_deaths
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
# 4. DESCRIPTIVE SUMMARY HELPER
# ============================================================

summarise_rates <- function(
    data,
    group_variable,
    rate_variable,
    count_variable,
    measure_name
) {
  data |>
    filter(
      !is.na(
        .data[[group_variable]]
      ),
      !is.na(
        .data[[rate_variable]]
      )
    ) |>
    group_by(
      group = .data[[group_variable]]
    ) |>
    summarise(
      reportable_counties = n(),
      
      median_rate = median(
        .data[[rate_variable]],
        na.rm = TRUE
      ),
      
      q1_rate = quantile(
        .data[[rate_variable]],
        0.25,
        na.rm = TRUE,
        names = FALSE
      ),
      
      q3_rate = quantile(
        .data[[rate_variable]],
        0.75,
        na.rm = TRUE,
        names = FALSE
      ),
      
      minimum_rate = min(
        .data[[rate_variable]],
        na.rm = TRUE
      ),
      
      maximum_rate = max(
        .data[[rate_variable]],
        na.rm = TRUE
      ),
      
      mean_rate = mean(
        .data[[rate_variable]],
        na.rm = TRUE
      ),
      
      standard_deviation = sd(
        .data[[rate_variable]],
        na.rm = TRUE
      ),
      
      total_average_annual_count = sum(
        .data[[count_variable]],
        na.rm = TRUE
      ),
      
      median_average_annual_count = median(
        .data[[count_variable]],
        na.rm = TRUE
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
# 5. DESCRIPTIVE SUMMARIES BY METRO STATUS
# ============================================================

incidence_by_metro <- summarise_rates(
  epi,
  "metro_status",
  "incidence_rate",
  "average_annual_cases",
  "Incidence"
)

mortality_by_metro <- summarise_rates(
  epi,
  "metro_status",
  "mortality_rate",
  "average_annual_deaths",
  "Mortality"
)

cat(
  "\nIncidence by metropolitan status:\n"
)

print(
  incidence_by_metro
)

cat(
  "\nMortality by metropolitan status:\n"
)

print(
  mortality_by_metro
)

write_csv(
  incidence_by_metro,
  file.path(
    output_dir,
    "16_incidence_by_metro_status.csv"
  )
)

write_csv(
  mortality_by_metro,
  file.path(
    output_dir,
    "16_mortality_by_metro_status.csv"
  )
)

# ============================================================
# 6. DESCRIPTIVE SUMMARIES BY THREE-LEVEL RURALITY
# ============================================================

incidence_by_rurality3 <- summarise_rates(
  epi,
  "rurality_3_level",
  "incidence_rate",
  "average_annual_cases",
  "Incidence"
)

mortality_by_rurality3 <- summarise_rates(
  epi,
  "rurality_3_level",
  "mortality_rate",
  "average_annual_deaths",
  "Mortality"
)

cat(
  "\nIncidence by three-level rurality:\n"
)

print(
  incidence_by_rurality3
)

cat(
  "\nMortality by three-level rurality:\n"
)

print(
  mortality_by_rurality3
)

write_csv(
  incidence_by_rurality3,
  file.path(
    output_dir,
    "16_incidence_by_rurality_3_level.csv"
  )
)

write_csv(
  mortality_by_rurality3,
  file.path(
    output_dir,
    "16_mortality_by_rurality_3_level.csv"
  )
)

# ============================================================
# 7. WILCOXON RANK-SUM TESTS
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
    )
  
  group_count <- n_distinct(
    analysis_data[[group_variable]]
  )
  
  if (group_count != 2) {
    return(
      tibble(
        analysis = analysis_name,
        outcome = outcome_variable,
        grouping_variable = group_variable,
        n_counties = nrow(analysis_data),
        statistic = NA_real_,
        p_value = NA_real_,
        method = "Wilcoxon rank-sum test not run: grouping variable does not have exactly two levels"
      )
    )
  }
  
  test_result <- wilcox.test(
    analysis_data[[outcome_variable]] ~
      analysis_data[[group_variable]],
    exact = FALSE,
    conf.int = FALSE
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

wilcoxon_results <- bind_rows(
  run_wilcoxon(
    epi,
    "incidence_rate",
    "metro_status",
    "Incidence rate by metropolitan status"
  ),
  
  run_wilcoxon(
    epi,
    "mortality_rate",
    "metro_status",
    "Mortality rate by metropolitan status"
  )
)

# ============================================================
# 8. KRUSKAL–WALLIS TESTS
# ============================================================

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
  
  group_count <- n_distinct(
    analysis_data[[group_variable]]
  )
  
  if (group_count < 2) {
    return(
      tibble(
        analysis = analysis_name,
        outcome = outcome_variable,
        grouping_variable = group_variable,
        n_counties = nrow(analysis_data),
        statistic = NA_real_,
        degrees_of_freedom = NA_real_,
        p_value = NA_real_,
        method = "Kruskal-Wallis test not run: fewer than two groups"
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

kruskal_results <- bind_rows(
  run_kruskal(
    epi,
    "incidence_rate",
    "rurality_3_level",
    "Incidence rate by three-level rurality"
  ),
  
  run_kruskal(
    epi,
    "mortality_rate",
    "rurality_3_level",
    "Mortality rate by three-level rurality"
  )
)

nonparametric_results <- bind_rows(
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
  "\nNonparametric test results:\n"
)

print(
  nonparametric_results
)

write_csv(
  nonparametric_results,
  file.path(
    output_dir,
    "16_nonparametric_test_results.csv"
  )
)

# ============================================================
# 9. EFFECT SIZE: RANK-BISERIAL CORRELATION
#
# Interpretation:
#   0.10 = small
#   0.30 = moderate
#   0.50 = large
#
# The sign depends on factor ordering:
#   Metropolitan is the reference/first level.
# ============================================================

rank_biserial <- function(
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
  
  groups <- levels(
    analysis_data[[group_variable]]
  )
  
  if (length(groups) != 2) {
    return(
      tibble(
        analysis = analysis_name,
        outcome = outcome_variable,
        grouping_variable = group_variable,
        group_1 = NA_character_,
        group_2 = NA_character_,
        n_group_1 = NA_integer_,
        n_group_2 = NA_integer_,
        rank_biserial_correlation = NA_real_,
        absolute_effect_size = NA_real_,
        interpretation = "Not calculated"
      )
    )
  }
  
  group1_values <- analysis_data |>
    filter(
      .data[[group_variable]] == groups[[1]]
    ) |>
    pull(
      .data[[outcome_variable]]
    )
  
  group2_values <- analysis_data |>
    filter(
      .data[[group_variable]] == groups[[2]]
    ) |>
    pull(
      .data[[outcome_variable]]
    )
  
  n1 <- length(
    group1_values
  )
  
  n2 <- length(
    group2_values
  )
  
  combined_values <- c(
    group1_values,
    group2_values
  )
  
  combined_ranks <- rank(
    combined_values,
    ties.method = "average"
  )
  
  rank_sum_group1 <- sum(
    combined_ranks[
      seq_len(n1)
    ]
  )
  
  u1 <- rank_sum_group1 -
    n1 *
    (n1 + 1) /
    2
  
  rbc <- 1 -
    (2 * u1) /
    (n1 * n2)
  
  abs_rbc <- abs(
    rbc
  )
  
  interpretation <- case_when(
    abs_rbc < 0.10 ~ "Negligible",
    abs_rbc < 0.30 ~ "Small",
    abs_rbc < 0.50 ~ "Moderate",
    TRUE ~ "Large"
  )
  
  tibble(
    analysis = analysis_name,
    outcome = outcome_variable,
    grouping_variable = group_variable,
    group_1 = groups[[1]],
    group_2 = groups[[2]],
    n_group_1 = n1,
    n_group_2 = n2,
    rank_biserial_correlation = rbc,
    absolute_effect_size = abs_rbc,
    interpretation
  )
}

effect_size_results <- bind_rows(
  rank_biserial(
    epi,
    "incidence_rate",
    "metro_status",
    "Incidence rate by metropolitan status"
  ),
  
  rank_biserial(
    epi,
    "mortality_rate",
    "metro_status",
    "Mortality rate by metropolitan status"
  )
)

cat(
  "\nEffect-size results:\n"
)

print(
  effect_size_results
)

write_csv(
  effect_size_results,
  file.path(
    output_dir,
    "16_effect_size_results.csv"
  )
)

# ============================================================
# 10. POPULATION-WEIGHTED SENSITIVITY SUMMARY
#
# This is descriptive only.
# County rates are age-adjusted and should not be interpreted
# as if these weighted summaries were official state rates.
# ============================================================

weighted_summary <- bind_rows(
  epi |>
    filter(
      !is.na(
        incidence_rate
      ),
      !is.na(
        population_2020
      ),
      !is.na(
        metro_status
      )
    ) |>
    group_by(
      group = metro_status
    ) |>
    summarise(
      measure = "Incidence",
      reportable_counties = n(),
      
      unweighted_mean_rate = mean(
        incidence_rate,
        na.rm = TRUE
      ),
      
      population_weighted_mean_rate = weighted.mean(
        incidence_rate,
        w = population_2020,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    ),
  
  epi |>
    filter(
      !is.na(
        mortality_rate
      ),
      !is.na(
        population_2020
      ),
      !is.na(
        metro_status
      )
    ) |>
    group_by(
      group = metro_status
    ) |>
    summarise(
      measure = "Mortality",
      reportable_counties = n(),
      
      unweighted_mean_rate = mean(
        mortality_rate,
        na.rm = TRUE
      ),
      
      population_weighted_mean_rate = weighted.mean(
        mortality_rate,
        w = population_2020,
        na.rm = TRUE
      ),
      
      .groups = "drop"
    )
) |>
  select(
    measure,
    group,
    reportable_counties,
    unweighted_mean_rate,
    population_weighted_mean_rate
  )

cat(
  "\nPopulation-weighted sensitivity summary:\n"
)

print(
  weighted_summary
)

write_csv(
  weighted_summary,
  file.path(
    output_dir,
    "16_weighted_rate_summary.csv"
  )
)

# ============================================================
# 11. COUNTY-LEVEL EXPORT FOR FIGURES
# ============================================================

county_rate_data <- epi |>
  transmute(
    county,
    fips,
    population_2020,
    metro_status,
    rurality_3_level,
    incidence_rate,
    mortality_rate,
    average_annual_cases,
    average_annual_deaths
  ) |>
  arrange(
    county
  )

write_csv(
  county_rate_data,
  file.path(
    output_dir,
    "16_county_rural_urban_rate_data.csv"
  )
)

# ============================================================
# 12. PLAIN-LANGUAGE SUMMARY
# ============================================================

extract_median <- function(
    table_data,
    group_name
) {
  result <- table_data |>
    filter(
      as.character(group) == group_name
    ) |>
    pull(
      median_rate
    )
  
  if (length(result) == 0) {
    return(NA_real_)
  }
  
  result[[1]]
}

incidence_metro_median <- extract_median(
  incidence_by_metro,
  "Metropolitan"
)

incidence_nonmetro_median <- extract_median(
  incidence_by_metro,
  "Nonmetropolitan"
)

mortality_metro_median <- extract_median(
  mortality_by_metro,
  "Metropolitan"
)

mortality_nonmetro_median <- extract_median(
  mortality_by_metro,
  "Nonmetropolitan"
)

incidence_wilcoxon_p <- wilcoxon_results |>
  filter(
    outcome == "incidence_rate"
  ) |>
  pull(
    p_value
  )

mortality_wilcoxon_p <- wilcoxon_results |>
  filter(
    outcome == "mortality_rate"
  ) |>
  pull(
    p_value
  )

summary_lines <- c(
  "OHIO OVARIAN CANCER RURAL–URBAN RATE ANALYSIS",
  "",
  paste0(
    "Median incidence rate among metropolitan counties: ",
    round(
      incidence_metro_median,
      1
    ),
    " per 100,000."
  ),
  paste0(
    "Median incidence rate among nonmetropolitan counties: ",
    round(
      incidence_nonmetro_median,
      1
    ),
    " per 100,000."
  ),
  paste0(
    "Wilcoxon p-value for incidence: ",
    signif(
      incidence_wilcoxon_p,
      4
    ),
    "."
  ),
  "",
  paste0(
    "Median mortality rate among metropolitan counties: ",
    round(
      mortality_metro_median,
      1
    ),
    " per 100,000."
  ),
  paste0(
    "Median mortality rate among nonmetropolitan counties: ",
    round(
      mortality_nonmetro_median,
      1
    ),
    " per 100,000."
  ),
  paste0(
    "Wilcoxon p-value for mortality: ",
    signif(
      mortality_wilcoxon_p,
      4
    ),
    "."
  ),
  "",
  "Interpretation note:",
  paste0(
    "These comparisons include only counties with reportable rates. ",
    "Because suppression is not random and differs by county population, ",
    "rural–urban rate comparisons may be affected by selection bias."
  )
)

write_lines(
  summary_lines,
  file.path(
    output_dir,
    "16_rural_urban_analysis_summary.txt"
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
  "\nRural–urban rate analysis completed successfully.\n"
)

cat(
  "\nFiles saved in:\n",
  output_dir,
  "\n",
  sep = ""
)