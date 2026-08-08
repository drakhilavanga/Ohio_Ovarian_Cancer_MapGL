# ============================================================
# 21_contextual_association_analysis.R
#
# Explore ecological associations between county-level ovarian
# cancer estimates and selected CDC PLACES contextual measures.
#
# IMPORTANT:
#   - This is an ecological analysis.
#   - PLACES measures are contextual county characteristics.
#   - Results must not be interpreted as individual-level risk
#     or causal evidence.
#   - Incidence and mortality analyses use only counties with
#     reportable cancer estimates.
#
# Analyses:
#   1. Spearman correlations
#   2. False-discovery-rate adjustment
#   3. Simple linear models
#   4. Population- and rurality-adjusted sensitivity models
#   5. Portage County contextual comparison
#   6. App-ready county and result exports
#
# Input:
#   data/processed/
#   ohio_ovarian_cancer_epidemiologic_dataset.csv
#
# Outputs:
#   outputs/tables/
#   21_spearman_results.csv
#   21_linear_model_results.csv
#   21_adjusted_model_results.csv
#   21_portage_context_profile.csv
#   21_county_contextual_analysis_data.csv
#   21_contextual_analysis_summary.txt
# ============================================================

# ============================================================
# 1. PACKAGE CHECK
# ============================================================

required_packages <- c(
  "dplyr",
  "readr",
  "tidyr",
  "stringr",
  "purrr",
  "tibble",
  "janitor",
  "broom"
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
      ),
      "\n\nRun:\ninstall.packages(c(",
      paste0(
        '"',
        missing_packages,
        '"',
        collapse = ", "
      ),
      "))"
    )
  )
}

library(dplyr)
library(readr)
library(tidyr)
library(stringr)
library(purrr)
library(tibble)
library(janitor)
library(broom)

# ============================================================
# 2. FILE PATHS
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
  "population_2020",
  "metro_status",
  "rurality_3_level",
  "incidence_rate",
  "mortality_rate",
  "smoking_percent",
  "obesity_percent",
  "physical_inactivity_percent",
  "diabetes_percent",
  "uninsured_percent",
  "annual_checkup_percent",
  "mammography_percent",
  "high_blood_pressure_percent",
  "depression_percent",
  "frequent_mental_distress_percent",
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

context_variables <- c(
  "smoking_percent",
  "obesity_percent",
  "physical_inactivity_percent",
  "diabetes_percent",
  "uninsured_percent",
  "annual_checkup_percent",
  "mammography_percent",
  "high_blood_pressure_percent",
  "depression_percent",
  "frequent_mental_distress_percent",
  "food_insecurity_percent",
  "transportation_barriers_percent"
)

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
    
    across(
      all_of(
        context_variables
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
    ),
    
    log_population_2020 = log(
      population_2020
    )
  )

# ============================================================
# 5. CONTEXT LABELS
# ============================================================

context_labels <- c(
  smoking_percent =
    "Current cigarette smoking",
  
  obesity_percent =
    "Obesity",
  
  physical_inactivity_percent =
    "Physical inactivity",
  
  diabetes_percent =
    "Diabetes",
  
  uninsured_percent =
    "Uninsured",
  
  annual_checkup_percent =
    "Annual checkup",
  
  mammography_percent =
    "Mammography",
  
  high_blood_pressure_percent =
    "High blood pressure",
  
  depression_percent =
    "Depression",
  
  frequent_mental_distress_percent =
    "Frequent mental distress",
  
  food_insecurity_percent =
    "Food insecurity",
  
  transportation_barriers_percent =
    "Transportation barriers"
)

# ============================================================
# 6. SPEARMAN CORRELATION HELPER
# ============================================================

run_spearman <- function(
    data,
    outcome_variable,
    context_variable
) {
  analysis_data <- data |>
    filter(
      !is.na(
        .data[[outcome_variable]]
      ),
      !is.na(
        .data[[context_variable]]
      )
    )
  
  if (nrow(analysis_data) < 10) {
    return(
      tibble(
        outcome = outcome_variable,
        contextual_variable = context_variable,
        contextual_label =
          unname(
            context_labels[
              context_variable
            ]
          ),
        counties = nrow(analysis_data),
        spearman_rho = NA_real_,
        p_value = NA_real_,
        method =
          "Not run: fewer than 10 complete counties"
      )
    )
  }
  
  test_result <- cor.test(
    analysis_data[[outcome_variable]],
    analysis_data[[context_variable]],
    method = "spearman",
    exact = FALSE
  )
  
  tibble(
    outcome = outcome_variable,
    contextual_variable = context_variable,
    contextual_label =
      unname(
        context_labels[
          context_variable
        ]
      ),
    counties = nrow(analysis_data),
    spearman_rho =
      unname(
        test_result$estimate
      ),
    p_value =
      unname(
        test_result$p.value
      ),
    method =
      test_result$method
  )
}

spearman_results <- expand_grid(
  outcome = c(
    "incidence_rate",
    "mortality_rate"
  ),
  contextual_variable =
    context_variables
) |>
  mutate(
    result = map2(
      outcome,
      contextual_variable,
      ~ run_spearman(
        epi,
        .x,
        .y
      )
    )
  ) |>
  select(
    result
  ) |>
  unnest(
    result
  ) |>
  group_by(
    outcome
  ) |>
  mutate(
    p_fdr = p.adjust(
      p_value,
      method = "BH"
    ),
    
    statistically_significant_0_05 =
      p_value < 0.05,
    
    statistically_significant_fdr_0_05 =
      p_fdr < 0.05,
    
    absolute_rho =
      abs(
        spearman_rho
      ),
    
    strength = case_when(
      is.na(
        absolute_rho
      ) ~
        "Not available",
      
      absolute_rho < 0.10 ~
        "Negligible",
      
      absolute_rho < 0.30 ~
        "Weak",
      
      absolute_rho < 0.50 ~
        "Moderate",
      
      TRUE ~
        "Strong"
    )
  ) |>
  ungroup() |>
  arrange(
    outcome,
    desc(
      absolute_rho
    )
  )

cat(
  "\nSpearman correlation results:\n"
)

print(
  spearman_results
)

write_csv(
  spearman_results,
  file.path(
    output_dir,
    "21_spearman_results.csv"
  ),
  na = ""
)

# ============================================================
# 7. SIMPLE LINEAR MODELS
#
# These models estimate the county-level change in cancer rate
# per one-percentage-point difference in the contextual measure.
# ============================================================

fit_simple_model <- function(
    data,
    outcome_variable,
    context_variable
) {
  analysis_data <- data |>
    filter(
      !is.na(
        .data[[outcome_variable]]
      ),
      !is.na(
        .data[[context_variable]]
      )
    )
  
  if (nrow(analysis_data) < 10) {
    return(
      tibble(
        outcome = outcome_variable,
        contextual_variable = context_variable,
        contextual_label =
          unname(
            context_labels[
              context_variable
            ]
          ),
        counties = nrow(analysis_data),
        estimate = NA_real_,
        standard_error = NA_real_,
        confidence_interval_lower = NA_real_,
        confidence_interval_upper = NA_real_,
        p_value = NA_real_,
        r_squared = NA_real_,
        adjusted_r_squared = NA_real_
      )
    )
  }
  
  model_formula <- reformulate(
    context_variable,
    response = outcome_variable
  )
  
  model <- lm(
    model_formula,
    data = analysis_data
  )
  
  coefficient_result <- tidy(
    model,
    conf.int = TRUE
  ) |>
    filter(
      term ==
        context_variable
    )
  
  model_fit <- glance(
    model
  )
  
  tibble(
    outcome = outcome_variable,
    contextual_variable = context_variable,
    contextual_label =
      unname(
        context_labels[
          context_variable
        ]
      ),
    counties = nrow(analysis_data),
    estimate =
      coefficient_result$estimate[[1]],
    standard_error =
      coefficient_result$std.error[[1]],
    confidence_interval_lower =
      coefficient_result$conf.low[[1]],
    confidence_interval_upper =
      coefficient_result$conf.high[[1]],
    p_value =
      coefficient_result$p.value[[1]],
    r_squared =
      model_fit$r.squared[[1]],
    adjusted_r_squared =
      model_fit$adj.r.squared[[1]]
  )
}

linear_model_results <- expand_grid(
  outcome = c(
    "incidence_rate",
    "mortality_rate"
  ),
  contextual_variable =
    context_variables
) |>
  mutate(
    result = map2(
      outcome,
      contextual_variable,
      ~ fit_simple_model(
        epi,
        .x,
        .y
      )
    )
  ) |>
  select(
    result
  ) |>
  unnest(
    result
  ) |>
  group_by(
    outcome
  ) |>
  mutate(
    p_fdr = p.adjust(
      p_value,
      method = "BH"
    ),
    
    statistically_significant_0_05 =
      p_value < 0.05,
    
    statistically_significant_fdr_0_05 =
      p_fdr < 0.05
  ) |>
  ungroup() |>
  arrange(
    outcome,
    p_fdr
  )

cat(
  "\nSimple linear-model results:\n"
)

print(
  linear_model_results
)

write_csv(
  linear_model_results,
  file.path(
    output_dir,
    "21_linear_model_results.csv"
  ),
  na = ""
)

# ============================================================
# 8. ADJUSTED SENSITIVITY MODELS
#
# Each model adjusts for:
#   - log county population
#   - metropolitan status
#
# This is a sensitivity analysis, not a causal model.
# ============================================================

fit_adjusted_model <- function(
    data,
    outcome_variable,
    context_variable
) {
  analysis_data <- data |>
    filter(
      !is.na(
        .data[[outcome_variable]]
      ),
      !is.na(
        .data[[context_variable]]
      ),
      !is.na(
        log_population_2020
      ),
      !is.na(
        metro_status
      )
    )
  
  if (nrow(analysis_data) < 15) {
    return(
      tibble(
        outcome = outcome_variable,
        contextual_variable = context_variable,
        contextual_label =
          unname(
            context_labels[
              context_variable
            ]
          ),
        counties = nrow(analysis_data),
        adjusted_estimate = NA_real_,
        adjusted_standard_error = NA_real_,
        adjusted_ci_lower = NA_real_,
        adjusted_ci_upper = NA_real_,
        adjusted_p_value = NA_real_,
        adjusted_r_squared = NA_real_
      )
    )
  }
  
  model_formula <- as.formula(
    paste0(
      outcome_variable,
      " ~ ",
      context_variable,
      " + log_population_2020 + metro_status"
    )
  )
  
  model <- lm(
    model_formula,
    data = analysis_data
  )
  
  coefficient_result <- tidy(
    model,
    conf.int = TRUE
  ) |>
    filter(
      term ==
        context_variable
    )
  
  model_fit <- glance(
    model
  )
  
  tibble(
    outcome = outcome_variable,
    contextual_variable = context_variable,
    contextual_label =
      unname(
        context_labels[
          context_variable
        ]
      ),
    counties = nrow(analysis_data),
    adjusted_estimate =
      coefficient_result$estimate[[1]],
    adjusted_standard_error =
      coefficient_result$std.error[[1]],
    adjusted_ci_lower =
      coefficient_result$conf.low[[1]],
    adjusted_ci_upper =
      coefficient_result$conf.high[[1]],
    adjusted_p_value =
      coefficient_result$p.value[[1]],
    adjusted_r_squared =
      model_fit$adj.r.squared[[1]]
  )
}

adjusted_model_results <- expand_grid(
  outcome = c(
    "incidence_rate",
    "mortality_rate"
  ),
  contextual_variable =
    context_variables
) |>
  mutate(
    result = map2(
      outcome,
      contextual_variable,
      ~ fit_adjusted_model(
        epi,
        .x,
        .y
      )
    )
  ) |>
  select(
    result
  ) |>
  unnest(
    result
  ) |>
  group_by(
    outcome
  ) |>
  mutate(
    adjusted_p_fdr = p.adjust(
      adjusted_p_value,
      method = "BH"
    ),
    
    statistically_significant_adjusted_0_05 =
      adjusted_p_value < 0.05,
    
    statistically_significant_adjusted_fdr_0_05 =
      adjusted_p_fdr < 0.05
  ) |>
  ungroup() |>
  arrange(
    outcome,
    adjusted_p_fdr
  )

cat(
  "\nAdjusted sensitivity-model results:\n"
)

print(
  adjusted_model_results
)

write_csv(
  adjusted_model_results,
  file.path(
    output_dir,
    "21_adjusted_model_results.csv"
  ),
  na = ""
)

# ============================================================
# 9. PORTAGE COUNTY CONTEXT PROFILE
# ============================================================

state_context_summary <- epi |>
  summarise(
    across(
      all_of(
        context_variables
      ),
      list(
        ohio_median =
          ~ median(
            .x,
            na.rm = TRUE
          ),
        ohio_q1 =
          ~ quantile(
            .x,
            0.25,
            na.rm = TRUE,
            names = FALSE
          ),
        ohio_q3 =
          ~ quantile(
            .x,
            0.75,
            na.rm = TRUE,
            names = FALSE
          )
      ),
      .names = "{.col}_{.fn}"
    )
  )

portage_row <- epi |>
  filter(
    str_to_lower(
      str_squish(
        county
      )
    ) == "portage"
  )

if (nrow(portage_row) == 0) {
  portage_context_profile <- tibble(
    contextual_variable = character(),
    contextual_label = character(),
    portage_percent = numeric(),
    ohio_median = numeric(),
    ohio_q1 = numeric(),
    ohio_q3 = numeric(),
    difference_from_ohio_median = numeric(),
    contextual_position = character()
  )
} else {
  portage_context_profile <- map_dfr(
    context_variables,
    function(variable_name) {
      portage_value <-
        portage_row[[variable_name]][[1]]
      
      ohio_median_value <-
        state_context_summary[[paste0(
          variable_name,
          "_ohio_median"
        )]][[1]]
      
      ohio_q1_value <-
        state_context_summary[[paste0(
          variable_name,
          "_ohio_q1"
        )]][[1]]
      
      ohio_q3_value <-
        state_context_summary[[paste0(
          variable_name,
          "_ohio_q3"
        )]][[1]]
      
      contextual_position <- case_when(
        is.na(
          portage_value
        ) ~
          "Unavailable",
        
        portage_value < ohio_q1_value ~
          "Below Ohio interquartile range",
        
        portage_value > ohio_q3_value ~
          "Above Ohio interquartile range",
        
        TRUE ~
          "Within Ohio interquartile range"
      )
      
      tibble(
        contextual_variable =
          variable_name,
        
        contextual_label =
          unname(
            context_labels[
              variable_name
            ]
          ),
        
        portage_percent =
          portage_value,
        
        ohio_median =
          ohio_median_value,
        
        ohio_q1 =
          ohio_q1_value,
        
        ohio_q3 =
          ohio_q3_value,
        
        difference_from_ohio_median =
          portage_value -
          ohio_median_value,
        
        contextual_position =
          contextual_position
      )
    }
  )
}

cat(
  "\nPortage County contextual profile:\n"
)

print(
  portage_context_profile
)

write_csv(
  portage_context_profile,
  file.path(
    output_dir,
    "21_portage_context_profile.csv"
  ),
  na = ""
)

# ============================================================
# 10. APP-READY COUNTY EXPORT
# ============================================================

county_contextual_data <- epi |>
  select(
    county,
    fips,
    population_2020,
    metro_status,
    rurality_3_level,
    incidence_rate,
    mortality_rate,
    all_of(
      context_variables
    )
  ) |>
  arrange(
    county
  )

write_csv(
  county_contextual_data,
  file.path(
    output_dir,
    "21_county_contextual_analysis_data.csv"
  ),
  na = ""
)

# ============================================================
# 11. PLAIN-LANGUAGE SUMMARY
# ============================================================

top_incidence_association <- spearman_results |>
  filter(
    outcome ==
      "incidence_rate",
    !is.na(
      spearman_rho
    )
  ) |>
  arrange(
    desc(
      absolute_rho
    )
  ) |>
  slice_head(
    n = 1
  )

top_mortality_association <- spearman_results |>
  filter(
    outcome ==
      "mortality_rate",
    !is.na(
      spearman_rho
    )
  ) |>
  arrange(
    desc(
      absolute_rho
    )
  ) |>
  slice_head(
    n = 1
  )

incidence_summary_line <- if (
  nrow(
    top_incidence_association
  ) == 1
) {
  paste0(
    "Largest absolute incidence correlation: ",
    top_incidence_association$
      contextual_label[[1]],
    " (Spearman rho = ",
    round(
      top_incidence_association$
        spearman_rho[[1]],
      3
    ),
    "; FDR-adjusted p = ",
    signif(
      top_incidence_association$
        p_fdr[[1]],
      4
    ),
    ")."
  )
} else {
  "No incidence correlation result was available."
}

mortality_summary_line <- if (
  nrow(
    top_mortality_association
  ) == 1
) {
  paste0(
    "Largest absolute mortality correlation: ",
    top_mortality_association$
      contextual_label[[1]],
    " (Spearman rho = ",
    round(
      top_mortality_association$
        spearman_rho[[1]],
      3
    ),
    "; FDR-adjusted p = ",
    signif(
      top_mortality_association$
        p_fdr[[1]],
      4
    ),
    ")."
  )
} else {
  "No mortality correlation result was available."
}

summary_lines <- c(
  "OHIO OVARIAN CANCER CONTEXTUAL ASSOCIATION ANALYSIS",
  "",
  incidence_summary_line,
  mortality_summary_line,
  "",
  "Interpretation note:",
  paste0(
    "These analyses are ecological and descriptive. Associations between ",
    "county-level PLACES indicators and ovarian cancer rates do not establish ",
    "individual-level risk, biological mechanisms, or causation."
  ),
  "",
  paste0(
    "Only counties with reportable cancer estimates were included. ",
    "Because suppression is substantial, selection bias may affect these results."
  ),
  "",
  paste0(
    "False-discovery-rate-adjusted p-values should be emphasized because ",
    "multiple contextual indicators were evaluated."
  )
)

write_lines(
  summary_lines,
  file.path(
    output_dir,
    "21_contextual_analysis_summary.txt"
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
# 12. FINAL MESSAGE
# ============================================================

cat(
  "\nContextual association analysis completed successfully.\n"
)

cat(
  "\nFiles saved in:\n",
  output_dir,
  "\n",
  sep = ""
)