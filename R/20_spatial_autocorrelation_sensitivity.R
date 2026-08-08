# ============================================================
# 20_spatial_autocorrelation_sensitivity.R
#
# Spatial epidemiology analysis for the Ohio ovarian cancer
# project.
#
# PRIMARY ANALYSIS:
#   Spatial clustering of reportability/suppression across all
#   88 Ohio counties.
#
# SECONDARY SENSITIVITY ANALYSIS:
#   Global and Local Moran's I for incidence and mortality
#   among counties with reportable estimates only.
#
# IMPORTANT:
#   - Suppressed rates are NEVER replaced with zero.
#   - Rate analyses are restricted to reportable counties.
#   - Results for cancer rates must be interpreted cautiously
#     because missingness is substantial and spatially patterned.
#
# Input:
#   data/processed/
#   ohio_ovarian_cancer_epidemiologic_dataset.gpkg
#
# Outputs:
#   outputs/tables/
#   20_global_moran_results.csv
#   20_local_moran_incidence.csv
#   20_local_moran_mortality.csv
#   20_local_moran_reportability.csv
#   20_spatial_neighbor_summary.csv
#   20_spatial_analysis_summary.txt
#
#   data/processed/
#   ohio_ovarian_cancer_spatial_analysis.gpkg
# ============================================================

# ============================================================
# 1. PACKAGE CHECK
# ============================================================

required_packages <- c(
  "sf",
  "dplyr",
  "readr",
  "stringr",
  "janitor",
  "spdep",
  "tibble"
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

library(sf)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(spdep)
library(tibble)

# ============================================================
# 2. FILE PATHS
# ============================================================

input_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_epidemiologic_dataset.gpkg"
)

output_table_dir <- file.path(
  "outputs",
  "tables"
)

output_spatial_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_spatial_analysis.gpkg"
)

dir.create(
  output_table_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

dir.create(
  dirname(output_spatial_file),
  recursive = TRUE,
  showWarnings = FALSE
)

if (!file.exists(input_file)) {
  stop(
    paste0(
      "Input GeoPackage not found:\n",
      input_file,
      "\n\nRun R/14_create_epidemiologic_dataset.R first."
    )
  )
}

# ============================================================
# 3. READ AND PREPARE SPATIAL DATA
# ============================================================

ohio <- st_read(
  input_file,
  quiet = TRUE
) |>
  clean_names() |>
  st_make_valid()

required_columns <- c(
  "county",
  "fips",
  "incidence_rate",
  "mortality_rate",
  "incidence_reportable",
  "mortality_reportable"
)

missing_columns <- setdiff(
  required_columns,
  names(ohio)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The spatial dataset is missing these columns:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

as_logical_safe <- function(x) {
  if (is.logical(x)) {
    return(x)
  }
  
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

ohio <- ohio |>
  mutate(
    fips = str_pad(
      as.character(fips),
      width = 5,
      side = "left",
      pad = "0"
    ),
    
    incidence_rate = as.numeric(
      incidence_rate
    ),
    
    mortality_rate = as.numeric(
      mortality_rate
    ),
    
    incidence_reportable = as_logical_safe(
      incidence_reportable
    ),
    
    mortality_reportable = as_logical_safe(
      mortality_reportable
    ),
    
    incidence_reportable_binary = if_else(
      incidence_reportable,
      1,
      0
    ),
    
    mortality_reportable_binary = if_else(
      mortality_reportable,
      1,
      0
    )
  )

if (nrow(ohio) != 88) {
  warning(
    paste0(
      "Expected 88 Ohio counties but found ",
      nrow(ohio),
      "."
    )
  )
}

if (any(is.na(ohio$incidence_reportable_binary))) {
  stop(
    "One or more incidence reportability values could not be converted to 0/1."
  )
}

if (any(is.na(ohio$mortality_reportable_binary))) {
  stop(
    "One or more mortality reportability values could not be converted to 0/1."
  )
}

# ============================================================
# 4. NEIGHBOR AND WEIGHT HELPERS
# ============================================================

build_queen_neighbors <- function(spatial_data) {
  neighbors <- poly2nb(
    spatial_data,
    queen = TRUE,
    row.names = spatial_data$fips
  )
  
  list_weights <- nb2listw(
    neighbors,
    style = "W",
    zero.policy = TRUE
  )
  
  list(
    nb = neighbors,
    listw = list_weights
  )
}

neighbor_count <- function(nb_object) {
  lengths(
    nb_object
  )
}

connected_component_count <- function(nb_object) {
  n.comp.nb(
    nb_object
  )$nc
}

# ============================================================
# 5. GLOBAL MORAN HELPER
# ============================================================

run_global_moran <- function(
    spatial_data,
    value_variable,
    analysis_name,
    permutations = 9999
) {
  analysis_data <- spatial_data |>
    filter(
      !is.na(
        .data[[value_variable]]
      )
    )
  
  if (nrow(analysis_data) < 10) {
    return(
      tibble(
        analysis = analysis_name,
        variable = value_variable,
        counties = nrow(analysis_data),
        connected_components = NA_integer_,
        isolated_counties = NA_integer_,
        moran_i = NA_real_,
        expected_i = NA_real_,
        variance = NA_real_,
        z_score = NA_real_,
        asymptotic_p_value = NA_real_,
        permutation_p_value = NA_real_,
        permutations = permutations,
        note = "Not run: fewer than 10 complete counties"
      )
    )
  }
  
  weights <- build_queen_neighbors(
    analysis_data
  )
  
  nb_object <- weights$nb
  listw_object <- weights$listw
  
  values <- analysis_data[[value_variable]]
  
  analytical_test <- moran.test(
    values,
    listw_object,
    zero.policy = TRUE,
    randomisation = TRUE,
    alternative = "two.sided"
  )
  
  permutation_test <- moran.mc(
    values,
    listw_object,
    nsim = permutations,
    zero.policy = TRUE,
    alternative = "two.sided"
  )
  
  estimate_values <- analytical_test$estimate
  
  tibble(
    analysis = analysis_name,
    variable = value_variable,
    counties = nrow(analysis_data),
    connected_components = connected_component_count(
      nb_object
    ),
    isolated_counties = sum(
      neighbor_count(
        nb_object
      ) == 0
    ),
    moran_i = unname(
      estimate_values[["Moran I statistic"]]
    ),
    expected_i = unname(
      estimate_values[["Expectation"]]
    ),
    variance = unname(
      estimate_values[["Variance"]]
    ),
    z_score = unname(
      analytical_test$statistic
    ),
    asymptotic_p_value = unname(
      analytical_test$p.value
    ),
    permutation_p_value = unname(
      permutation_test$p.value
    ),
    permutations = permutations,
    note = if_else(
      connected_component_count(
        nb_object
      ) > 1,
      "Multiple disconnected components; interpret cautiously",
      "Single connected component"
    )
  )
  
}

# ============================================================
# 6. GLOBAL MORAN'S I
# ============================================================

set.seed(
  20260806
)

global_moran_results <- bind_rows(
  run_global_moran(
    ohio,
    "incidence_reportable_binary",
    "Spatial clustering of incidence reportability"
  ),
  
  run_global_moran(
    ohio,
    "mortality_reportable_binary",
    "Spatial clustering of mortality reportability"
  ),
  
  run_global_moran(
    ohio,
    "incidence_rate",
    "Spatial autocorrelation of reportable incidence rates"
  ),
  
  run_global_moran(
    ohio,
    "mortality_rate",
    "Spatial autocorrelation of reportable mortality rates"
  )
) |>
  mutate(
    statistically_significant_permutation_0_05 =
      permutation_p_value < 0.05
  )

cat(
  "\nGlobal Moran's I results:\n"
)

print(
  global_moran_results
)

write_csv(
  global_moran_results,
  file.path(
    output_table_dir,
    "20_global_moran_results.csv"
  ),
  na = ""
)

# ============================================================
# 7. LOCAL MORAN HELPER
#
# Quadrant interpretation:
#   High-High = high value surrounded by high values
#   Low-Low   = low value surrounded by low values
#   High-Low  = high value surrounded by low values
#   Low-High  = low value surrounded by high values
#
# False-discovery-rate adjustment is applied to local p-values.
# ============================================================

run_local_moran <- function(
    spatial_data,
    value_variable,
    analysis_name
) {
  analysis_data <- spatial_data |>
    filter(
      !is.na(
        .data[[value_variable]]
      )
    )
  
  if (nrow(analysis_data) < 10) {
    return(
      analysis_data |>
        st_drop_geometry() |>
        transmute(
          county,
          fips,
          analysis = analysis_name,
          variable = value_variable,
          value = .data[[value_variable]],
          spatial_lag = NA_real_,
          local_moran_i = NA_real_,
          local_z_score = NA_real_,
          local_p_value = NA_real_,
          local_p_fdr = NA_real_,
          local_cluster = "Not calculated",
          significant_0_05 = FALSE,
          significant_fdr_0_05 = FALSE
        )
    )
  }
  
  weights <- build_queen_neighbors(
    analysis_data
  )
  
  listw_object <- weights$listw
  
  values <- analysis_data[[value_variable]]
  
  standardized_values <- as.numeric(
    scale(
      values
    )
  )
  
  spatial_lag_values <- lag.listw(
    listw_object,
    standardized_values,
    zero.policy = TRUE
  )
  
  local_results <- localmoran(
    values,
    listw_object,
    zero.policy = TRUE,
    alternative = "two.sided"
  )
  
  local_table <- as.data.frame(
    local_results
  ) |>
    rownames_to_column(
      var = "row_id"
    ) |>
    as_tibble() |>
    clean_names()
  
  p_column <- names(local_table)[
    str_detect(
      names(local_table),
      "^pr_z"
    )
  ][1]
  
  z_column <- names(local_table)[
    str_detect(
      names(local_table),
      "^z_ii"
    )
  ][1]
  
  i_column <- names(local_table)[
    names(local_table) == "ii"
  ][1]
  
  if (
    is.na(p_column) ||
    is.na(z_column) ||
    is.na(i_column)
  ) {
    stop(
      paste0(
        "Could not detect Local Moran output columns for ",
        analysis_name,
        ". Available columns: ",
        paste(
          names(local_table),
          collapse = ", "
        )
      )
    )
  }
  
  local_p_values <- local_table[[p_column]]
  
  local_p_fdr_values <- p.adjust(
    local_p_values,
    method = "BH"
  )
  
  local_cluster <- case_when(
    standardized_values > 0 &
      spatial_lag_values > 0 ~
      "High-High",
    
    standardized_values < 0 &
      spatial_lag_values < 0 ~
      "Low-Low",
    
    standardized_values > 0 &
      spatial_lag_values < 0 ~
      "High-Low",
    
    standardized_values < 0 &
      spatial_lag_values > 0 ~
      "Low-High",
    
    TRUE ~
      "Not classified"
  )
  
  analysis_data |>
    st_drop_geometry() |>
    transmute(
      county,
      fips,
      analysis = analysis_name,
      variable = value_variable,
      value = .data[[value_variable]],
      standardized_value = standardized_values,
      spatial_lag = spatial_lag_values,
      local_moran_i = local_table[[i_column]],
      local_z_score = local_table[[z_column]],
      local_p_value = local_p_values,
      local_p_fdr = local_p_fdr_values,
      local_cluster = if_else(
        local_p_values < 0.05,
        local_cluster,
        "Not significant"
      ),
      local_cluster_fdr = if_else(
        local_p_fdr_values < 0.05,
        local_cluster,
        "Not significant"
      ),
      significant_0_05 =
        local_p_values < 0.05,
      significant_fdr_0_05 =
        local_p_fdr_values < 0.05
    )
}

# ============================================================
# 8. LOCAL MORAN ANALYSES
# ============================================================

local_incidence_reportability <- run_local_moran(
  ohio,
  "incidence_reportable_binary",
  "Local clustering of incidence reportability"
)

local_mortality_reportability <- run_local_moran(
  ohio,
  "mortality_reportable_binary",
  "Local clustering of mortality reportability"
)

local_incidence_rate <- run_local_moran(
  ohio,
  "incidence_rate",
  "Local autocorrelation of reportable incidence rates"
)

local_mortality_rate <- run_local_moran(
  ohio,
  "mortality_rate",
  "Local autocorrelation of reportable mortality rates"
)

write_csv(
  bind_rows(
    local_incidence_reportability,
    local_mortality_reportability
  ),
  file.path(
    output_table_dir,
    "20_local_moran_reportability.csv"
  ),
  na = ""
)

write_csv(
  local_incidence_rate,
  file.path(
    output_table_dir,
    "20_local_moran_incidence.csv"
  ),
  na = ""
)

write_csv(
  local_mortality_rate,
  file.path(
    output_table_dir,
    "20_local_moran_mortality.csv"
  ),
  na = ""
)

# ============================================================
# 9. NEIGHBOR STRUCTURE SUMMARY
# ============================================================

create_neighbor_summary <- function(
    spatial_data,
    analysis_name,
    value_variable
) {
  analysis_data <- spatial_data |>
    filter(
      !is.na(
        .data[[value_variable]]
      )
    )
  
  weights <- build_queen_neighbors(
    analysis_data
  )
  
  nb_counts <- neighbor_count(
    weights$nb
  )
  
  tibble(
    analysis = analysis_name,
    variable = value_variable,
    counties = nrow(analysis_data),
    connected_components = connected_component_count(
      weights$nb
    ),
    isolated_counties = sum(
      nb_counts == 0
    ),
    minimum_neighbors = min(
      nb_counts
    ),
    median_neighbors = median(
      nb_counts
    ),
    mean_neighbors = mean(
      nb_counts
    ),
    maximum_neighbors = max(
      nb_counts
    )
  )
}

neighbor_summary <- bind_rows(
  create_neighbor_summary(
    ohio,
    "Incidence reportability",
    "incidence_reportable_binary"
  ),
  
  create_neighbor_summary(
    ohio,
    "Mortality reportability",
    "mortality_reportable_binary"
  ),
  
  create_neighbor_summary(
    ohio,
    "Reportable incidence rate",
    "incidence_rate"
  ),
  
  create_neighbor_summary(
    ohio,
    "Reportable mortality rate",
    "mortality_rate"
  )
)

cat(
  "\nSpatial-neighbor summary:\n"
)

print(
  neighbor_summary
)

write_csv(
  neighbor_summary,
  file.path(
    output_table_dir,
    "20_spatial_neighbor_summary.csv"
  ),
  na = ""
)

# ============================================================
# 10. JOIN LOCAL RESULTS BACK TO THE MAP DATA
# ============================================================

incidence_reportability_join <- local_incidence_reportability |>
  transmute(
    fips,
    incidence_reportability_local_i =
      local_moran_i,
    incidence_reportability_p =
      local_p_value,
    incidence_reportability_p_fdr =
      local_p_fdr,
    incidence_reportability_cluster =
      local_cluster,
    incidence_reportability_cluster_fdr =
      local_cluster_fdr
  )

mortality_reportability_join <- local_mortality_reportability |>
  transmute(
    fips,
    mortality_reportability_local_i =
      local_moran_i,
    mortality_reportability_p =
      local_p_value,
    mortality_reportability_p_fdr =
      local_p_fdr,
    mortality_reportability_cluster =
      local_cluster,
    mortality_reportability_cluster_fdr =
      local_cluster_fdr
  )

incidence_rate_join <- local_incidence_rate |>
  transmute(
    fips,
    incidence_local_i =
      local_moran_i,
    incidence_local_p =
      local_p_value,
    incidence_local_p_fdr =
      local_p_fdr,
    incidence_local_cluster =
      local_cluster,
    incidence_local_cluster_fdr =
      local_cluster_fdr
  )

mortality_rate_join <- local_mortality_rate |>
  transmute(
    fips,
    mortality_local_i =
      local_moran_i,
    mortality_local_p =
      local_p_value,
    mortality_local_p_fdr =
      local_p_fdr,
    mortality_local_cluster =
      local_cluster,
    mortality_local_cluster_fdr =
      local_cluster_fdr
  )

ohio_spatial_results <- ohio |>
  left_join(
    incidence_reportability_join,
    by = "fips"
  ) |>
  left_join(
    mortality_reportability_join,
    by = "fips"
  ) |>
  left_join(
    incidence_rate_join,
    by = "fips"
  ) |>
  left_join(
    mortality_rate_join,
    by = "fips"
  )

if (file.exists(output_spatial_file)) {
  file.remove(
    output_spatial_file
  )
}

st_write(
  ohio_spatial_results,
  output_spatial_file,
  quiet = TRUE
)

# ============================================================
# 11. PLAIN-LANGUAGE SUMMARY
# ============================================================

get_global_result <- function(
    result_table,
    variable_name,
    result_column
) {
  value <- result_table |>
    filter(
      variable == variable_name
    ) |>
    pull(
      all_of(
        result_column
      )
    )
  
  if (length(value) == 0) {
    return(NA_real_)
  }
  
  value[[1]]
}

incidence_reportability_i <- get_global_result(
  global_moran_results,
  "incidence_reportable_binary",
  "moran_i"
)

incidence_reportability_p <- get_global_result(
  global_moran_results,
  "incidence_reportable_binary",
  "permutation_p_value"
)

mortality_reportability_i <- get_global_result(
  global_moran_results,
  "mortality_reportable_binary",
  "moran_i"
)

mortality_reportability_p <- get_global_result(
  global_moran_results,
  "mortality_reportable_binary",
  "permutation_p_value"
)

incidence_rate_i <- get_global_result(
  global_moran_results,
  "incidence_rate",
  "moran_i"
)

incidence_rate_p <- get_global_result(
  global_moran_results,
  "incidence_rate",
  "permutation_p_value"
)

mortality_rate_i <- get_global_result(
  global_moran_results,
  "mortality_rate",
  "moran_i"
)

mortality_rate_p <- get_global_result(
  global_moran_results,
  "mortality_rate",
  "permutation_p_value"
)

summary_lines <- c(
  "OHIO OVARIAN CANCER SPATIAL SENSITIVITY ANALYSIS",
  "",
  
  paste0(
    "Incidence reportability Moran's I: ",
    round(incidence_reportability_i, 3),
    "; permutation p = ",
    signif(incidence_reportability_p, 4),
    "."
  ),
  
  paste0(
    "Mortality reportability Moran's I: ",
    round(mortality_reportability_i, 3),
    "; permutation p = ",
    signif(mortality_reportability_p, 4),
    "."
  ),
  
  "",
  
  paste0(
    "Reportable incidence-rate Moran's I: ",
    round(incidence_rate_i, 3),
    "; permutation p = ",
    signif(incidence_rate_p, 4),
    "."
  ),
  
  paste0(
    "Reportable mortality-rate Moran's I: ",
    round(mortality_rate_i, 3),
    "; permutation p = ",
    signif(mortality_rate_p, 4),
    "."
  ),
  
  "",
  
  "Interpretation note:",
  
  paste0(
    "The reportability analyses include all 88 counties and are the ",
    "primary spatial analyses. Cancer-rate analyses include only counties ",
    "with reportable estimates and are sensitivity analyses because ",
    "suppression is substantial and potentially spatially patterned."
  ),
  
  "",
  
  paste0(
    "Local Moran results are provided using both unadjusted p-values and ",
    "Benjamini-Hochberg false-discovery-rate-adjusted p-values. ",
    "FDR-adjusted clusters should be emphasized in formal reporting."
  )
)

write_lines(
  summary_lines,
  file.path(
    output_table_dir,
    "20_spatial_analysis_summary.txt"
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
  "\nSpatial autocorrelation sensitivity analysis completed successfully.\n"
)

cat(
  "\nTable outputs saved in:\n",
  output_table_dir,
  "\n",
  sep = ""
)

cat(
  "\nSpatial output saved as:\n",
  output_spatial_file,
  "\n",
  sep = ""
)