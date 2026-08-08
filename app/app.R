# ============================================================
# OHIO OVARIAN CANCER ATLAS — VERSION 1.0
# Frozen feature set for public release
# Story map + epidemiologic context + methods + sources + downloads
# mapgl 0.5.0
# ============================================================

library(shiny)
library(mapgl)
library(sf)
library(dplyr)
library(readr)
library(stringr)
library(janitor)
library(tidyr)
library(htmltools)

# -------------------- VERSION 1.0 METADATA -------------------
atlas_version <- "1.0"
atlas_release <- "August 2026"

developer_name <- "Akhila Vanga, BDS, MS"
developer_role <- "Clinical Epidemiology | Spatial Epidemiology | R Shiny | GIS"

github_url <- "https://github.com/drakhilavanga"
orcid_url <- "https://orcid.org/0009-0002-4273-3221"

state_cancer_profiles_url <- "https://statecancerprofiles.cancer.gov/"
cdc_places_url <- "https://www.cdc.gov/places/tools/data-portal.html"
usda_rucc_url <- "https://www.ers.usda.gov/data-products/rural-urban-continuum-codes"
census_tiger_url <- "https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.2023.html"

# --------------------------- FILES ---------------------------

find_file <- function(paths) {
  found <- paths[file.exists(paths)]
  if (length(found) == 0) NA_character_ else found[[1]]
}

combined_file <- find_file(c(
  file.path("data", "processed", "ohio_ovarian_cancer_combined.gpkg"),
  file.path("..", "data", "processed", "ohio_ovarian_cancer_combined.gpkg")
))

if (is.na(combined_file)) {
  stop("Run R/11_join_incidence_mortality.R first.")
}

ohio_cancer <- st_read(combined_file, quiet = TRUE) |>
  st_transform(4326) |>
  mutate(
    incidence_rate = as.numeric(incidence_rate),
    mortality_rate = as.numeric(mortality_rate),
    average_annual_cases = as.numeric(average_annual_cases),
    average_annual_deaths = as.numeric(average_annual_deaths)
  )


# -------------------- ENRICHED EPIDEMIOLOGIC DATA --------------------
#
# Script 14 combines the cancer estimates with:
#   * USDA 2023 Rural-Urban Continuum Codes
#   * 2020 county population
#   * CDC PLACES contextual indicators
#
# Script 20 optionally adds Local Moran results.
# The app still runs when the optional spatial-analysis file is absent.

epi_file <- find_file(c(
  file.path("data", "processed", "ohio_ovarian_cancer_epidemiologic_dataset.csv"),
  file.path("..", "data", "processed", "ohio_ovarian_cancer_epidemiologic_dataset.csv")
))

spatial_analysis_file <- find_file(c(
  file.path("data", "processed", "ohio_ovarian_cancer_spatial_analysis.gpkg"),
  file.path("..", "data", "processed", "ohio_ovarian_cancer_spatial_analysis.gpkg")
))

if (!is.na(epi_file)) {
  epi_context <- read_csv(
    epi_file,
    show_col_types = FALSE
  ) |>
    clean_names() |>
    mutate(
      county = str_remove(
        county,
        regex("\\s+County$", ignore_case = TRUE)
      )
    ) |>
    select(
      county,
      any_of(
        c(
          "fips",
          "population_2020",
          "rucc_2023",
          "rucc_description",
          "metro_status",
          "rurality_3_level",
          "incidence_lower_ci",
          "incidence_upper_ci",
          "mortality_lower_ci",
          "mortality_upper_ci",
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
          "transportation_barriers_percent",
          "incidence_reportable",
          "mortality_reportable"
        )
      )
    )
  
  ohio_cancer <- ohio_cancer |>
    left_join(
      epi_context,
      by = "county"
    )
} else {
  warning(
    paste0(
      "The enriched epidemiologic CSV was not found. ",
      "Run R/14_create_epidemiologic_dataset.R to enable ",
      "rurality, population, confidence-interval, and CDC PLACES layers."
    )
  )
}

if (!is.na(spatial_analysis_file)) {
  spatial_context <- st_read(
    spatial_analysis_file,
    quiet = TRUE
  ) |>
    st_drop_geometry() |>
    clean_names() |>
    mutate(
      county = str_remove(
        county,
        regex("\\s+County$", ignore_case = TRUE)
      )
    ) |>
    select(
      county,
      any_of(
        c(
          "incidence_local_cluster_fdr",
          "mortality_local_cluster_fdr",
          "incidence_reportability_cluster_fdr",
          "mortality_reportability_cluster_fdr"
        )
      )
    )
  
  ohio_cancer <- ohio_cancer |>
    left_join(
      spatial_context,
      by = "county"
    )
}

# Add absent optional variables so later map code remains safe.
optional_columns <- c(
  "population_2020",
  "rucc_2023",
  "rucc_description",
  "metro_status",
  "rurality_3_level",
  "incidence_lower_ci",
  "incidence_upper_ci",
  "mortality_lower_ci",
  "mortality_upper_ci",
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
  "transportation_barriers_percent",
  "incidence_local_cluster_fdr",
  "mortality_local_cluster_fdr"
)

for (column_name in optional_columns) {
  if (!column_name %in% names(ohio_cancer)) {
    ohio_cancer[[column_name]] <- NA
  }
}

# Statewide values from the source exports
ohio_incidence_rate <- 9.8
ohio_mortality_rate <- 5.8
ohio_incidence_period <- "2018–2022"
ohio_mortality_period <- "2019–2023"

# ----------------------- RANKS / CLASSES ---------------------

incidence_ranks <- ohio_cancer |>
  st_drop_geometry() |>
  filter(!is.na(incidence_rate)) |>
  arrange(desc(incidence_rate), county) |>
  mutate(incidence_rank = row_number())

mortality_ranks <- ohio_cancer |>
  st_drop_geometry() |>
  filter(!is.na(mortality_rate)) |>
  arrange(desc(mortality_rate), county) |>
  mutate(mortality_rank = row_number())

ohio_cancer <- ohio_cancer |>
  left_join(
    incidence_ranks |> select(county, incidence_rank),
    by = "county"
  ) |>
  left_join(
    mortality_ranks |> select(county, mortality_rank),
    by = "county"
  ) |>
  mutate(
    incidence_class_no = if_else(
      is.na(incidence_rate),
      NA_integer_,
      ntile(incidence_rate, 5)
    ),
    mortality_class_no = if_else(
      is.na(mortality_rate),
      NA_integer_,
      ntile(mortality_rate, 5)
    ),
    incidence_class = case_when(
      is.na(incidence_rate) ~ "Suppressed / unavailable",
      incidence_class_no == 1 ~ "Lowest",
      incidence_class_no == 2 ~ "Low",
      incidence_class_no == 3 ~ "Moderate",
      incidence_class_no == 4 ~ "High",
      TRUE ~ "Highest"
    ),
    mortality_class = case_when(
      is.na(mortality_rate) ~ "Suppressed / unavailable",
      mortality_class_no == 1 ~ "Lowest",
      mortality_class_no == 2 ~ "Low",
      mortality_class_no == 3 ~ "Moderate",
      mortality_class_no == 4 ~ "High",
      TRUE ~ "Highest"
    ),
    incidence_display = if_else(
      is.na(incidence_rate),
      "Suppressed or unavailable",
      paste0(format(incidence_rate, nsmall = 1), " per 100,000")
    ),
    mortality_display = if_else(
      is.na(mortality_rate),
      "Suppressed or unavailable",
      paste0(format(mortality_rate, nsmall = 1), " per 100,000")
    ),
    cases_display = if_else(
      is.na(average_annual_cases),
      "Not available",
      format(round(average_annual_cases), big.mark = ",")
    ),
    deaths_display = if_else(
      is.na(average_annual_deaths),
      "Not available",
      format(round(average_annual_deaths), big.mark = ",")
    ),
    incidence_rank_display = if_else(
      is.na(incidence_rank),
      "Not ranked",
      paste0(incidence_rank, " of ", nrow(incidence_ranks))
    ),
    mortality_rank_display = if_else(
      is.na(mortality_rank),
      "Not ranked",
      paste0(mortality_rank, " of ", nrow(mortality_ranks))
    ),
    
    population_display = if_else(
      is.na(population_2020),
      "Unavailable",
      format(
        round(population_2020),
        big.mark = ",",
        scientific = FALSE
      )
    ),
    
    rurality_display = case_when(
      !is.na(rurality_3_level) ~ rurality_3_level,
      !is.na(metro_status) ~ metro_status,
      TRUE ~ "Unavailable"
    ),
    
    incidence_ci_display = if_else(
      is.na(incidence_lower_ci) | is.na(incidence_upper_ci),
      "Unavailable",
      paste0(
        format(incidence_lower_ci, nsmall = 1),
        "–",
        format(incidence_upper_ci, nsmall = 1)
      )
    ),
    
    mortality_ci_display = if_else(
      is.na(mortality_lower_ci) | is.na(mortality_upper_ci),
      "Unavailable",
      paste0(
        format(mortality_lower_ci, nsmall = 1),
        "–",
        format(mortality_upper_ci, nsmall = 1)
      )
    ),
    
    incidence_comparison = case_when(
      is.na(incidence_rate) ~ "Suppressed / unavailable",
      !is.na(incidence_lower_ci) &
        incidence_lower_ci > ohio_incidence_rate ~ "Above Ohio",
      !is.na(incidence_upper_ci) &
        incidence_upper_ci < ohio_incidence_rate ~ "Below Ohio",
      !is.na(incidence_lower_ci) &
        !is.na(incidence_upper_ci) ~ "Not clearly different from Ohio",
      incidence_rate > ohio_incidence_rate ~ "Point estimate above Ohio",
      incidence_rate < ohio_incidence_rate ~ "Point estimate below Ohio",
      TRUE ~ "Equal to Ohio"
    ),
    
    mortality_comparison = case_when(
      is.na(mortality_rate) ~ "Suppressed / unavailable",
      !is.na(mortality_lower_ci) &
        mortality_lower_ci > ohio_mortality_rate ~ "Above Ohio",
      !is.na(mortality_upper_ci) &
        mortality_upper_ci < ohio_mortality_rate ~ "Below Ohio",
      !is.na(mortality_lower_ci) &
        !is.na(mortality_upper_ci) ~ "Not clearly different from Ohio",
      mortality_rate > ohio_mortality_rate ~ "Point estimate above Ohio",
      mortality_rate < ohio_mortality_rate ~ "Point estimate below Ohio",
      TRUE ~ "Equal to Ohio"
    ),
    
    smoking_display = if_else(
      is.na(smoking_percent),
      "Unavailable",
      paste0(format(smoking_percent, nsmall = 1), "%")
    ),
    
    obesity_display = if_else(
      is.na(obesity_percent),
      "Unavailable",
      paste0(format(obesity_percent, nsmall = 1), "%")
    ),
    
    inactivity_display = if_else(
      is.na(physical_inactivity_percent),
      "Unavailable",
      paste0(format(physical_inactivity_percent, nsmall = 1), "%")
    ),
    
    uninsured_display = if_else(
      is.na(uninsured_percent),
      "Unavailable",
      paste0(format(uninsured_percent, nsmall = 1), "%")
    ),
    
    mammography_display = if_else(
      is.na(mammography_percent),
      "Unavailable",
      paste0(format(mammography_percent, nsmall = 1), "%")
    ),
    
    food_insecurity_display = if_else(
      is.na(food_insecurity_percent),
      "Unavailable",
      paste0(format(food_insecurity_percent, nsmall = 1), "%")
    ),
    
    transportation_display = if_else(
      is.na(transportation_barriers_percent),
      "Unavailable",
      paste0(format(transportation_barriers_percent, nsmall = 1), "%")
    ),
    
    incidence_cluster_display = if_else(
      is.na(incidence_local_cluster_fdr),
      "Not available",
      as.character(incidence_local_cluster_fdr)
    ),
    
    mortality_cluster_display = if_else(
      is.na(mortality_local_cluster_fdr),
      "Not available",
      as.character(mortality_local_cluster_fdr)
    )
  )

rescale_values <- function(x, new_min, new_max) {
  old_min <- min(x, na.rm = TRUE)
  old_max <- max(x, na.rm = TRUE)
  if (old_min == old_max) return(rep(mean(c(new_min, new_max)), length(x)))
  new_min + ((x - old_min) / (old_max - old_min)) * (new_max - new_min)
}

ohio_cancer$mortality_height <- 150
idx <- !is.na(ohio_cancer$mortality_rate)
ohio_cancer$mortality_height[idx] <- rescale_values(
  ohio_cancer$mortality_rate[idx],
  1200,
  11000
)

# --------------------------- PORTAGE -------------------------

portage <- ohio_cancer |>
  filter(str_to_lower(str_squish(county)) == "portage")

if (nrow(portage) != 1) stop("Portage County was not found uniquely.")

portage_row <- portage |>
  st_drop_geometry() |>
  slice(1)

comparison_text <- function(county_rate, state_rate) {
  d <- county_rate - state_rate
  if (is.na(d)) return("Comparison unavailable")
  if (d < 0) {
    paste0(format(abs(d), nsmall = 1), " points below Ohio")
  } else if (d > 0) {
    paste0(format(d, nsmall = 1), " points above Ohio")
  } else {
    "Equal to Ohio"
  }
}

# ------------------------ TOP 10 / POINTS --------------------

top10_incidence <- incidence_ranks |> slice_head(n = 10)
top10_mortality <- mortality_ranks |> slice_head(n = 10)

top10_incidence_polygons <- ohio_cancer |>
  filter(county %in% top10_incidence$county)

highest_incidence_polygon <- top10_incidence_polygons |>
  filter(incidence_rank == 1)

centroids <- ohio_cancer |>
  st_transform(5070) |>
  st_point_on_surface() |>
  st_transform(4326)

top10_incidence_points <- centroids |>
  filter(county %in% top10_incidence$county) |>
  mutate(rank_label = as.character(incidence_rank))

mortality_points <- centroids |>
  filter(!is.na(mortality_rate)) |>
  mutate(
    symbol_radius = rescale_values(mortality_rate, 5, 18),
    symbol_group = if_else(mortality_rank <= 10, "Top 10", "Other")
  )

top10_mortality_points <- mortality_points |>
  filter(mortality_rank <= 10) |>
  mutate(rank_label = as.character(mortality_rank))

# --------------------------- DOWNLOADS -----------------------

county_download <- ohio_cancer |>
  st_drop_geometry() |>
  select(
    county,
    any_of(c(
      "fips",
      "population_2020",
      "rucc_2023",
      "rucc_description",
      "metro_status",
      "rurality_3_level"
    )),
    incidence_rate,
    any_of(c("incidence_lower_ci", "incidence_upper_ci")),
    incidence_class,
    incidence_rank,
    incidence_comparison,
    average_annual_cases,
    mortality_rate,
    any_of(c("mortality_lower_ci", "mortality_upper_ci")),
    mortality_class,
    mortality_rank,
    mortality_comparison,
    average_annual_deaths,
    any_of(c(
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
      "transportation_barriers_percent",
      "incidence_local_cluster_fdr",
      "mortality_local_cluster_fdr"
    ))
  )

top10_download <- bind_rows(
  top10_incidence |>
    transmute(
      measure = "Incidence",
      rank = incidence_rank,
      county,
      rate = incidence_rate,
      average_annual_count = average_annual_cases,
      period = ohio_incidence_period
    ),
  top10_mortality |>
    transmute(
      measure = "Mortality",
      rank = mortality_rank,
      county,
      rate = mortality_rate,
      average_annual_count = average_annual_deaths,
      period = ohio_mortality_period
    )
)

portage_download <- tibble(
  measure = c("Incidence", "Mortality"),
  portage_rate = c(
    portage_row$incidence_rate[[1]],
    portage_row$mortality_rate[[1]]
  ),
  ohio_rate = c(ohio_incidence_rate, ohio_mortality_rate),
  difference_from_ohio = c(
    portage_row$incidence_rate[[1]] - ohio_incidence_rate,
    portage_row$mortality_rate[[1]] - ohio_mortality_rate
  ),
  rank = c(
    portage_row$incidence_rank[[1]],
    portage_row$mortality_rank[[1]]
  ),
  average_annual_count = c(
    portage_row$average_annual_cases[[1]],
    portage_row$average_annual_deaths[[1]]
  ),
  
  period = c(ohio_incidence_period, ohio_mortality_period)
)

dictionary <- tribble(
  ~variable, ~definition, ~unit, ~source_or_period,
  "incidence_rate", "Age-adjusted ovarian cancer incidence rate", "Cases per 100,000 women", ohio_incidence_period,
  "mortality_rate", "Age-adjusted ovarian cancer mortality rate", "Deaths per 100,000 women", ohio_mortality_period,
  "incidence_lower_ci / incidence_upper_ci", "95% confidence interval for incidence", "Rate limits", "State Cancer Profiles",
  "mortality_lower_ci / mortality_upper_ci", "95% confidence interval for mortality", "Rate limits", "State Cancer Profiles",
  "incidence_class", "Five relative groups among reportable counties", "Lowest to Highest", ohio_incidence_period,
  "mortality_class", "Five relative groups among reportable counties", "Lowest to Highest", ohio_mortality_period,
  "rank", "Descending rank among reportable counties", "Rank", "Measure-specific",
  "average_annual_count", "Average annual cases or deaths", "Count", "Measure-specific",
  "population_2020", "County population", "Persons", "2020 population context",
  "rucc_2023", "USDA Rural-Urban Continuum Code", "Code 1–9", "USDA ERS 2023",
  "metro_status", "Metropolitan or nonmetropolitan classification", "Category", "USDA ERS 2023",
  "rurality_3_level", "Three-level rurality classification", "Category", "Derived from RUCC 2023",
  "smoking_percent", "Current cigarette smoking prevalence", "Percent", "CDC PLACES",
  "obesity_percent", "Obesity prevalence", "Percent", "CDC PLACES",
  "physical_inactivity_percent", "Physical inactivity prevalence", "Percent", "CDC PLACES",
  "diabetes_percent", "Diagnosed diabetes prevalence", "Percent", "CDC PLACES",
  "uninsured_percent", "Health-insurance measure from PLACES", "Percent", "CDC PLACES",
  "annual_checkup_percent", "Annual routine checkup prevalence", "Percent", "CDC PLACES",
  "mammography_percent", "Mammography use prevalence", "Percent", "CDC PLACES",
  "food_insecurity_percent", "Food-insecurity prevalence", "Percent", "CDC PLACES",
  "transportation_barriers_percent", "Transportation-barrier prevalence", "Percent", "CDC PLACES",
  "incidence_local_cluster_fdr", "FDR-adjusted Local Moran incidence category", "Cluster category", "Spatial sensitivity analysis",
  "mortality_local_cluster_fdr", "FDR-adjusted Local Moran mortality category", "Cluster category", "Spatial sensitivity analysis"
)

# --------------------------- HELPERS -------------------------

make_top10 <- function(data, rate_col, accent) {
  rows <- lapply(seq_len(nrow(data)), function(i) {
    tags$tr(
      tags$td(i),
      tags$td(paste0(data$county[[i]], " County")),
      tags$td(
        style = paste0("color:", accent, ";font-weight:700;text-align:right;"),
        format(data[[rate_col]][[i]], nsmall = 1)
      )
    )
  })
  
  tags$div(
    class = "ranking-wrap",
    tags$table(
      class = "ranking-table",
      tags$thead(tags$tr(tags$th("Rank"), tags$th("County"), tags$th("Rate"))),
      tags$tbody(rows)
    )
  )
}

us_center <- c(-98.5, 39.5)
ohio_center <- c(-82.8, 40.3)
portage_center <- c(-81.20, 41.16)

# ----------------------------- UI ----------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      html, body {
        margin: 0;
        padding: 0;
        overflow-x: hidden;
        background: #07090d;
        font-family: Arial, Helvetica, sans-serif;
      }

      .story-section { max-width: 460px !important; }
      .story-section h2 { color:#2f455b; font-weight:700; }
      .story-section p { color:#31485e; font-size:16px; line-height:1.5; }

      .data-card {
        margin-top:12px;
        padding:14px 16px;
        border-radius:8px;
        background:rgba(10,13,20,.95);
        color:white;
      }
      .data-card * { color:white; }
      .incidence-card { border-left:5px solid #F26B38; }
      .mortality-card { border-left:5px solid #5E4FA2; }
      .portage-card { border-left:5px solid #00E6E6; }

      .metric { margin-top:7px; font-size:13px; font-weight:700; text-transform:uppercase; opacity:.8; }
      .big-value { font-size:21px; font-weight:700; margin:3px 0 7px; }

      .ranking-wrap {
        margin-top:12px;
        max-height:300px;
        overflow-y:auto;
        border-radius:8px;
        background:rgba(10,13,20,.96);
      }
      .ranking-table { width:100%; border-collapse:collapse; color:white; font-size:13px; }
      .ranking-table th { position:sticky; top:0; padding:8px; background:#242c35; text-align:left; color:white; }
      .ranking-table td { padding:6px 8px; border-top:1px solid rgba(255,255,255,.12); color:white; }

      .warning-box {
        margin-top:12px;
        padding:14px;
        border-left:5px solid #D99A28;
        border-radius:7px;
        background:rgba(255,247,229,.97);
        color:#4d3814;
        line-height:1.5;
      }

      #explore-toggle {
        position: fixed;
        top: 16px;
        right: 16px;
        z-index: 100001;
        width: 48px;
        height: 48px;
        padding: 0;
        border: 0;
        border-radius: 50%;
        background: rgba(255,255,255,.97);
        color: #26394c;
        box-shadow: 0 4px 16px rgba(0,0,0,.30);
        font-size: 21px;
        line-height: 48px;
        text-align: center;
        cursor: pointer;
      }

      #explore-toggle:hover,
      #explore-toggle:focus {
        background: #26394c;
        color: #ffffff;
        outline: none;
      }

      #reviewer-controls {
        position: fixed;
        top: 72px;
        right: 16px;
        z-index: 100000;
        width: 220px;
        max-height: calc(100vh - 92px);
        overflow-y: auto;
        padding: 13px 15px;
        border-radius: 9px;
        background: rgba(255,255,255,.97);
        box-shadow: 0 4px 16px rgba(0,0,0,.30);
        color: #26394c;
        transform: translateX(calc(100% + 30px));
        opacity: 0;
        visibility: hidden;
        transition: transform .22s ease, opacity .22s ease, visibility .22s ease;
      }

      #reviewer-controls.open {
        transform: translateX(0);
        opacity: 1;
        visibility: visible;
      }

      .panel-title { font-size:16px; font-weight:700; margin-bottom:8px; }
      .reviewer-button { width:100%; background:#26394c; color:white; font-weight:700; margin:5px 0 9px; }
      details { margin-top:8px; padding-top:8px; border-top:1px solid #d8dee4; }
      summary { cursor:pointer; font-weight:700; font-size:13px; }
      .download-button { display:block; width:100%; margin-top:6px; font-size:12px; }
      .guide { margin-top:7px; padding:8px; border-radius:5px; background:#f4f7f9; }
      .guide p { margin:4px 0; font-size:12px; color:#31485e; }

      .meta-grid {
        display:grid;
        grid-template-columns:1fr 1fr;
        gap:10px;
        margin:12px 0;
      }

      .meta-item {
        padding:11px 12px;
        border-radius:8px;
        background:#f1f4f6;
        border:1px solid #d9e0e5;
      }

      .meta-label {
        margin-bottom:3px;
        font-size:10px;
        font-weight:700;
        letter-spacing:.6px;
        text-transform:uppercase;
        color:#66788a;
      }

      .meta-value {
        font-size:14px;
        font-weight:700;
        color:#26394c;
      }

      .source-link {
        display:inline-block;
        margin-top:5px;
        color:#0b6ea8;
        font-weight:700;
        text-decoration:none;
      }

      .source-link:hover {
        text-decoration:underline;
      }

      .version-badge {
        display:inline-block;
        margin-top:8px;
        padding:5px 9px;
        border-radius:999px;
        background:#26394c;
        color:#fff;
        font-size:11px;
        font-weight:700;
      }

      #map-legend {
        position: fixed;
        left: 18px;
        bottom: 42px;
        z-index: 100000;
        width: 250px;
        max-height: 42vh;
        overflow-y: auto;
        padding: 14px 16px;
        border-radius: 10px;
        background: rgba(255,255,255,.96);
        color: #26394c;
        box-shadow: 0 4px 18px rgba(0,0,0,.28);
        backdrop-filter: blur(8px);
      }

      #map-legend.hidden {
        display: none;
      }

      .legend-title {
        margin-bottom: 9px;
        font-size: 15px;
        font-weight: 700;
        color: #26394c;
      }

      .legend-subtitle {
        margin-top: -4px;
        margin-bottom: 9px;
        font-size: 11px;
        color: #617184;
        line-height: 1.35;
      }

      .legend-row {
        display: flex;
        align-items: center;
        gap: 9px;
        margin: 6px 0;
        font-size: 12px;
        line-height: 1.25;
      }

      .legend-swatch {
        width: 18px;
        height: 18px;
        flex: 0 0 18px;
        border: 1px solid rgba(0,0,0,.25);
        border-radius: 3px;
      }

      .legend-circle {
        display: inline-block;
        border-radius: 50%;
        background: #7FDBDA;
        border: 1.5px solid #FFFFFF;
        box-shadow: 0 0 0 1px rgba(0,0,0,.28);
      }

      .legend-note {
        margin-top: 9px;
        padding-top: 8px;
        border-top: 1px solid #d9dfe5;
        font-size: 10px;
        color: #657486;
        line-height: 1.35;
      }

      @media (max-width:750px) {
        #explore-toggle {
          top: 10px;
          right: 10px;
          width: 44px;
          height: 44px;
          line-height: 44px;
        }

        #reviewer-controls {
          top: 64px;
          right: 10px;
          width: 185px;
          max-height: calc(100vh - 78px);
        }

        #map-legend {
          left: 10px;
          bottom: 34px;
          width: 205px;
          max-height: 34vh;
          padding: 11px 12px;
        }
      }
    "))),
  
  tags$button(
    id = "explore-toggle",
    type = "button",
    title = "Explore map",
    `aria-label` = "Open map tools",
    `aria-expanded` = "false",
    onclick = "
      var panel = document.getElementById('reviewer-controls');
      var open = panel.classList.toggle('open');
      this.setAttribute('aria-expanded', open ? 'true' : 'false');
      this.setAttribute('title', open ? 'Close map tools' : 'Explore map');
      this.innerHTML = open
        ? '<i class=\"fa-solid fa-xmark\"></i>'
        : '<i class=\"fa-solid fa-map\"></i>';
    ",
    HTML('<i class="fa-solid fa-map"></i>')
  ),
  
  tags$div(
    id = "reviewer-controls",
    tags$div(class = "panel-title", "Explore the Map"),
    
    radioButtons(
      "basemap_choice",
      "Map View",
      choices = c("Dark View" = "dark", "Satellite View" = "satellite"),
      selected = "dark"
    ),
    
    actionButton(
      "reset_view",
      "Reset Current View",
      icon = icon("rotate-left"),
      class = "reviewer-button"
    ),
    
    tags$details(
      tags$summary(icon("download"), " Download Data"),
      downloadButton("download_county", "County Analysis CSV", class = "download-button"),
      downloadButton("download_top10", "Top-10 Rankings CSV", class = "download-button"),
      downloadButton("download_portage", "Portage Profile CSV", class = "download-button"),
      downloadButton("download_dictionary", "Data Dictionary CSV", class = "download-button")
    ),
    
    tags$details(
      tags$summary(icon("circle-info"), " Map Controls"),
      tags$div(
        class = "guide",
        tags$p(tags$strong("Drag: "), "move the map"),
        tags$p(tags$strong("Double-click: "), "zoom in"),
        tags$p(tags$strong("+ / −: "), "zoom"),
        tags$p(tags$strong("Compass: "), "rotate or reset north"),
        tags$p(tags$strong("Reset: "), "restore the current story scene")
      )
    )
  ),
  
  tags$div(
    id = "map-legend",
    class = "hidden"
  ),
  
  tags$script(
    HTML("
      Shiny.addCustomMessageHandler('update-map-legend', function(message) {
        var legend = document.getElementById('map-legend');

        if (!legend) return;

        if (!message || message.show === false) {
          legend.classList.add('hidden');
          legend.innerHTML = '';
          return;
        }

        legend.innerHTML = message.html || '';
        legend.classList.remove('hidden');
      });
    ")
  ),
  
  story_maplibre(
    map_id = "map",
    font_family = "Arial",
    
    sections = list(
      intro = story_section(
        title = "Ohio Ovarian Cancer Atlas",
        content = list(
          tags$p(
            "Developed by ", developer_name, ", this interactive atlas integrates ",
            "ovarian cancer surveillance, spatial epidemiology, rurality, and county-level ",
            "public-health context across Ohio."
          ),
          tags$p(
            "The project translates complex epidemiologic data into an accessible visual narrative ",
            "while preserving uncertainty, suppression, and ecological-study limitations."
          ),
          tags$p(
            "Scroll through the atlas to explore incidence, mortality, county rankings, rurality, ",
            "CDC PLACES context, comparisons with Ohio, spatial sensitivity analyses, and a ",
            "focused Portage County profile."
          ),
          tags$div(
            class = "data-card portage-card",
            tags$div(class = "metric", "Developer"),
            tags$div(class = "big-value", developer_name),
            tags$div(developer_role),
            tags$span(
              class = "version-badge",
              paste0("Version ", atlas_version, " • ", atlas_release)
            )
          )
        ),
        position = "center"
      ),
      
      study_overview = story_section(
        title = "Study at a Glance",
        content = list(
          tags$p(
            "A county-level ecological spatial epidemiology study of ovarian cancer burden across Ohio."
          ),
          
          tags$div(
            class = "meta-grid",
            
            tags$div(
              class = "meta-item",
              tags$div(class = "meta-label", "Study area"),
              tags$div(class = "meta-value", "Ohio")
            ),
            
            tags$div(
              class = "meta-item",
              tags$div(class = "meta-label", "Spatial units"),
              tags$div(class = "meta-value", "88 counties")
            ),
            
            tags$div(
              class = "meta-item",
              tags$div(class = "meta-label", "Incidence period"),
              tags$div(class = "meta-value", ohio_incidence_period)
            ),
            
            tags$div(
              class = "meta-item",
              tags$div(class = "meta-label", "Mortality period"),
              tags$div(class = "meta-value", ohio_mortality_period)
            ),
            
            tags$div(
              class = "meta-item",
              tags$div(class = "meta-label", "Primary measures"),
              tags$div(class = "meta-value", "Age-adjusted rates per 100,000")
            ),
            
            tags$div(
              class = "meta-item",
              tags$div(class = "meta-label", "Reportable estimates"),
              tags$div(
                class = "meta-value",
                paste0(
                  nrow(incidence_ranks),
                  " incidence • ",
                  nrow(mortality_ranks),
                  " mortality"
                )
              )
            )
          ),
          
          tags$div(
            class = "warning-box",
            tags$strong("Ecological study"),
            tags$br(),
            tags$br(),
            "The unit of analysis is the county. County-level associations describe populations ",
            "and must not be interpreted as individual-level risk relationships."
          )
        ),
        position = "left"
      ),
      
      incidence_overview = story_section(
        title = "Incidence: Classified Choropleth",
        content = list(
          tags$div(
            class = "data-card incidence-card",
            tags$div(class = "metric", "Ohio incidence rate"),
            tags$div(class = "big-value", paste0(ohio_incidence_rate, " per 100,000")),
            tags$div(paste0("Period: ", ohio_incidence_period)),
            tags$div(class = "metric", "Reportable counties"),
            tags$div(paste0(nrow(incidence_ranks), " of 88"))
          ),
          tags$p(
            paste0(
              "Highest: ",
              incidence_ranks$county[[1]],
              " County (",
              incidence_ranks$incidence_rate[[1]],
              "). Lowest: ",
              tail(incidence_ranks$county, 1),
              " County (",
              tail(incidence_ranks$incidence_rate, 1),
              ")."
            )
          )
        ),
        position = "left"
      ),
      
      incidence_top10 = story_section(
        title = "Incidence: Top-10 Ranking Map",
        content = list(
          tags$p("Only the 10 highest reportable counties receive strong colors and numbered labels."),
          make_top10(top10_incidence, "incidence_rate", "#FF8A3D")
        ),
        position = "right"
      ),
      
      mortality_overview = story_section(
        title = "Mortality: 3D Prism Map",
        content = list(
          tags$div(
            class = "data-card mortality-card",
            tags$div(class = "metric", "Ohio mortality rate"),
            tags$div(class = "big-value", paste0(ohio_mortality_rate, " per 100,000")),
            tags$div(paste0("Period: ", ohio_mortality_period)),
            tags$div(class = "metric", "Reportable counties"),
            tags$div(paste0(nrow(mortality_ranks), " of 88"))
          ),
          tags$p("Prism height is normalized within the mortality distribution and is not physical elevation.")
        ),
        position = "left"
      ),
      
      mortality_symbols = story_section(
        title = "Mortality: Proportional-Symbol Map",
        content = list(
          tags$p("Circle size represents mortality rate. Dark teal circles indicate the top 10."),
          make_top10(top10_mortality, "mortality_rate", "#55D6C2")
        ),
        position = "right"
      ),
      
      rurality_context = story_section(
        title = "Rurality and Population Context",
        content = list(
          tags$p(
            "USDA 2023 Rural-Urban Continuum Codes classify each county's metropolitan and nonmetropolitan context."
          ),
          tags$div(
            class = "data-card portage-card",
            tags$div(class = "metric", "Portage population"),
            tags$div(class = "big-value", portage_row$population_display[[1]]),
            tags$div(class = "metric", "Portage rurality"),
            tags$div(portage_row$rurality_display[[1]]),
            tags$div(
              paste0(
                "RUCC code: ",
                ifelse(
                  is.na(portage_row$rucc_2023[[1]]),
                  "Unavailable",
                  portage_row$rucc_2023[[1]]
                )
              )
            )
          ),
          tags$p(
            "Rurality is a contextual characteristic and should not be interpreted as an individual-level cause of ovarian cancer."
          )
        ),
        position = "left"
      ),
      
      places_context = story_section(
        title = "CDC PLACES: County Health Context",
        content = list(
          tags$p(
            "The map adds behavioral, access, preventive-care, and social indicators from CDC PLACES."
          ),
          tags$div(
            class = "data-card incidence-card",
            tags$div(class = "metric", "Portage smoking"),
            tags$div(class = "big-value", portage_row$smoking_display[[1]]),
            tags$div(paste0("Obesity: ", portage_row$obesity_display[[1]])),
            tags$div(paste0("Physical inactivity: ", portage_row$inactivity_display[[1]])),
            tags$div(paste0("Uninsured: ", portage_row$uninsured_display[[1]])),
            tags$div(paste0("Mammography: ", portage_row$mammography_display[[1]])),
            tags$div(paste0("Food insecurity: ", portage_row$food_insecurity_display[[1]])),
            tags$div(paste0("Transportation barriers: ", portage_row$transportation_display[[1]]))
          ),
          tags$p(
            "These estimates describe county context. They do not establish causes of ovarian cancer."
          )
        ),
        position = "right"
      ),
      
      ohio_comparison = story_section(
        title = "Comparison With Ohio",
        content = list(
          tags$p(
            "County confidence intervals are compared with the statewide rate instead of relying on ranks alone."
          ),
          tags$div(
            class = "data-card incidence-card",
            tags$div(class = "metric", "Portage incidence"),
            tags$div(class = "big-value", portage_row$incidence_display[[1]]),
            tags$div(paste0("95% CI: ", portage_row$incidence_ci_display[[1]])),
            tags$div(paste0("Classification: ", portage_row$incidence_comparison[[1]]))
          ),
          tags$div(
            class = "data-card mortality-card",
            tags$div(class = "metric", "Portage mortality"),
            tags$div(class = "big-value", portage_row$mortality_display[[1]]),
            tags$div(paste0("95% CI: ", portage_row$mortality_ci_display[[1]])),
            tags$div(paste0("Classification: ", portage_row$mortality_comparison[[1]]))
          )
        ),
        position = "left"
      ),
      
      spatial_context = story_section(
        title = "Spatial Pattern Sensitivity Analysis",
        content = list(
          tags$p(
            "FDR-adjusted Local Moran categories are shown when Script 20 outputs are available."
          ),
          tags$div(
            class = "data-card mortality-card",
            tags$div(class = "metric", "Portage incidence cluster"),
            tags$div(class = "big-value", portage_row$incidence_cluster_display[[1]]),
            tags$div(class = "metric", "Portage mortality cluster"),
            tags$div(class = "big-value", portage_row$mortality_cluster_display[[1]])
          ),
          tags$p(
            "Because many cancer estimates are suppressed, rate-based spatial results are sensitivity analyses and must be interpreted cautiously."
          )
        ),
        position = "right"
      ),
      
      portage_profile = story_section(
        title = "Portage County Profile",
        content = list(
          tags$div(
            class = "data-card portage-card",
            
            tags$div(class = "metric", "Portage incidence"),
            tags$div(class = "big-value", portage_row$incidence_display[[1]]),
            tags$div(paste0("Ohio: ", ohio_incidence_rate)),
            tags$div(comparison_text(portage_row$incidence_rate[[1]], ohio_incidence_rate)),
            tags$div(paste0("Rank: ", portage_row$incidence_rank_display[[1]])),
            tags$div(paste0("Average annual cases: ", portage_row$cases_display[[1]])),
            
            tags$hr(),
            
            tags$div(class = "metric", "Portage mortality"),
            tags$div(class = "big-value", portage_row$mortality_display[[1]]),
            tags$div(paste0("Ohio: ", ohio_mortality_rate)),
            tags$div(comparison_text(portage_row$mortality_rate[[1]], ohio_mortality_rate)),
            tags$div(paste0("Rank: ", portage_row$mortality_rank_display[[1]])),
            tags$div(paste0("Average annual deaths: ", portage_row$deaths_display[[1]]))
          )
        ),
        position = "left"
      ),
      
      portage_limitations = story_section(
        title = "No Subcounty Cancer Rate Is Available",
        content = list(
          tags$div(
            class = "warning-box",
            tags$strong("The source provides one estimate for all of Portage County."),
            tags$br(), tags$br(),
            "It cannot identify high-rate neighborhoods, ZIP codes, municipalities, or census tracts."
          ),
          tags$p(
            "A later Portage analysis can map healthcare access and social vulnerability, but those are contextual indicators—not ovarian cancer rates."
          )
        ),
        position = "right"
      ),
      
      interpretation = story_section(
        title = "Interpretation",
        content = list(
          tags$p("Incidence and mortality use different periods, scales, and map designs."),
          tags$p(
            "Gray counties represent suppressed or unavailable public estimates—not zero cancer burden."
          ),
          tags$p(
            "A high county rank does not by itself demonstrate a statistically meaningful difference."
          ),
          tags$p(
            "Confidence intervals represent uncertainty around county estimates; wider intervals indicate less precision."
          ),
          tags$p(
            "CDC PLACES and rurality measures provide county context and should not be interpreted as causes of ovarian cancer."
          ),
          tags$p(
            "FDR-adjusted spatial results are sensitivity analyses because suppression is substantial."
          )
        ),
        position = "left"
      ),
      
      methods_reproducibility = story_section(
        title = "Methods and Reproducibility",
        content = list(
          tags$p(
            "The analytical workflow was designed to preserve county-level uncertainty and make the atlas reproducible."
          ),
          
          tags$div(
            class = "data-card incidence-card",
            
            tags$div(class = "metric", "Analytical workflow"),
            tags$div(class = "big-value", "From surveillance data to interactive atlas"),
            
            tags$div("1. Imported county-level ovarian cancer incidence and mortality estimates."),
            tags$div("2. Standardized county identifiers and joined records using county geography/FIPS."),
            tags$div("3. Preserved suppressed and unavailable estimates as missing—not zero."),
            tags$div("4. Ranked only counties with reportable estimates."),
            tags$div("5. Classified reportable incidence and mortality into five relative groups for visualization."),
            tags$div("6. Integrated 2023 USDA RUCC rurality and 2020 population context."),
            tags$div("7. Integrated selected CDC PLACES county contextual indicators."),
            tags$div("8. Evaluated spatial autocorrelation and FDR-adjusted Local Moran categories as sensitivity analyses."),
            tags$div("9. Rendered interactive 2D, 3D, proportional-symbol, contextual, and focused county views.")
          ),
          
          tags$div(
            class = "data-card mortality-card",
            tags$div(class = "metric", "Software"),
            tags$div(class = "big-value", "R-based reproducible workflow"),
            tags$div(
              "R • Shiny • mapgl • sf • dplyr • readr • tidyr • stringr • janitor • htmltools"
            ),
            tags$br(),
            tags$div(
              "Web mapping uses MapLibre through the R mapgl package. ",
              "County geometry is transformed to WGS 84 (EPSG:4326) for web display."
            )
          ),
          
          tags$div(
            class = "warning-box",
            tags$strong("Spatial-analysis caution"),
            tags$br(),
            tags$br(),
            "Global and Local Moran analyses of cancer rates are treated as sensitivity analyses because ",
            "only counties with reportable estimates can contribute to rate-based analyses."
          )
        ),
        position = "center"
      ),
      
      data_sources = story_section(
        title = "Data Sources, Citation and Contact",
        content = list(
          tags$p(
            "Version 1.0 integrates public cancer surveillance, rurality, population, ",
            "and county health-context data."
          ),
          
          tags$div(
            class = "data-card incidence-card",
            
            tags$div(class = "metric", "Cancer surveillance"),
            tags$div(class = "big-value", "State Cancer Profiles"),
            tags$div(
              "Ohio county ovarian cancer incidence (2018–2022) and mortality (2019–2023), ",
              "including age-adjusted rates, confidence intervals, average annual counts, ",
              "and public-reporting suppression."
            ),
            tags$a(
              class = "source-link",
              href = state_cancer_profiles_url,
              target = "_blank",
              "Open State Cancer Profiles"
            ),
            
            tags$hr(),
            
            tags$div(class = "metric", "Rurality"),
            tags$div(class = "big-value", "USDA Economic Research Service"),
            tags$div(
              "2023 Rural–Urban Continuum Codes used for RUCC code, metropolitan status, ",
              "and derived rurality classification."
            ),
            tags$a(
              class = "source-link",
              href = usda_rucc_url,
              target = "_blank",
              "Open USDA RUCC documentation"
            ),
            
            tags$hr(),
            
            tags$div(class = "metric", "Geographic framework"),
            tags$div(class = "big-value", "U.S. Census Bureau"),
            tags$div(
              "County geographic boundaries and standardized geographic identifiers used for spatial joins."
            ),
            tags$a(
              class = "source-link",
              href = census_tiger_url,
              target = "_blank",
              "Open Census TIGER/Line"
            )
          ),
          
          tags$div(
            class = "data-card mortality-card",
            
            tags$div(class = "metric", "County health context"),
            tags$div(class = "big-value", "CDC PLACES"),
            tags$div(
              "Selected county estimates include smoking, obesity, physical inactivity, diabetes, ",
              "health insurance, annual checkups, mammography, high blood pressure, depression, ",
              "frequent mental distress, food insecurity, and transportation barriers."
            ),
            tags$a(
              class = "source-link",
              href = cdc_places_url,
              target = "_blank",
              "Open CDC PLACES Data Portal"
            ),
            
            tags$hr(),
            
            tags$div(class = "metric", "Spatial sensitivity analysis"),
            tags$div(class = "big-value", "Global and Local Moran's I"),
            tags$div(
              "Spatial autocorrelation analyses were evaluated with suppression-aware interpretation. ",
              "FDR-adjusted Local Moran categories are emphasized for local results."
            )
          ),
          
          tags$div(
            class = "warning-box",
            tags$strong("Interpretation and use"),
            tags$br(),
            tags$br(),
            "All analyses are county-level and ecological. This atlas does not establish causation, ",
            "individual-level risk, diagnosis, prognosis, or neighborhood-level cancer patterns. ",
            "Suppressed estimates are not zero."
          ),
          
          tags$p(
            tags$strong("Recommended citation: "),
            "Vanga A. Ohio Ovarian Cancer Atlas: An Interactive County-Level Surveillance and ",
            "Spatial Epidemiology Application. Version ",
            atlas_version,
            ". ",
            atlas_release,
            "."
          ),
          
          tags$p(
            tags$strong("Developer: "),
            developer_name,
            " — ",
            developer_role
          ),
          
          tags$p(
            tags$strong("Professional profiles: "),
            tags$a(href = github_url, target = "_blank", "GitHub"),
            " • ",
            tags$a(href = orcid_url, target = "_blank", "ORCID")
          ),
          
          tags$p(
            tags$strong("Version: "),
            atlas_version,
            " | Released: ",
            atlas_release
          )
        ),
        position = "center"
      )
    )
  )
)

# ---------------------------- SERVER -------------------------

server <- function(input, output, session) {
  
  current_scene <- reactiveVal("intro")
  
  scene_camera <- list(
    intro = list(center = us_center, zoom = 2.8, pitch = 0, bearing = 0),
    study_overview = list(center = ohio_center, zoom = 5.45, pitch = 0, bearing = 0),
    incidence_overview = list(center = ohio_center, zoom = 5.75, pitch = 12, bearing = 0),
    incidence_top10 = list(center = ohio_center, zoom = 5.95, pitch = 0, bearing = 0),
    mortality_overview = list(center = c(-82.75, 40.15), zoom = 5.9, pitch = 52, bearing = -12),
    mortality_symbols = list(center = ohio_center, zoom = 5.85, pitch = 5, bearing = 0),
    rurality_context = list(center = ohio_center, zoom = 5.75, pitch = 5, bearing = 0),
    places_context = list(center = ohio_center, zoom = 5.75, pitch = 5, bearing = 0),
    ohio_comparison = list(center = ohio_center, zoom = 5.75, pitch = 5, bearing = 0),
    spatial_context = list(center = ohio_center, zoom = 5.75, pitch = 5, bearing = 0),
    portage_profile = list(center = portage_center, zoom = 9.15, pitch = 18, bearing = 0),
    portage_limitations = list(center = portage_center, zoom = 9.55, pitch = 5, bearing = 0),
    interpretation = list(center = ohio_center, zoom = 5.65, pitch = 10, bearing = 0),
    methods_reproducibility = list(center = ohio_center, zoom = 5.45, pitch = 5, bearing = 0),
    data_sources = list(center = ohio_center, zoom = 5.35, pitch = 5, bearing = 0)
  )
  
  legend_row <- function(color, label) {
    paste0(
      "<div class='legend-row'>",
      "<span class='legend-swatch' style='background:",
      color,
      ";'></span>",
      "<span>",
      label,
      "</span>",
      "</div>"
    )
  }
  
  circle_row <- function(size, color, label) {
    paste0(
      "<div class='legend-row'>",
      "<span class='legend-circle' style='width:",
      size,
      "px;height:",
      size,
      "px;background:",
      color,
      ";'></span>",
      "<span>",
      label,
      "</span>",
      "</div>"
    )
  }
  
  send_legend <- function(title, subtitle = NULL, rows, note = NULL) {
    html <- paste0(
      "<div class='legend-title'>",
      title,
      "</div>",
      if (!is.null(subtitle)) {
        paste0(
          "<div class='legend-subtitle'>",
          subtitle,
          "</div>"
        )
      } else {
        ""
      },
      paste0(rows, collapse = ""),
      if (!is.null(note)) {
        paste0(
          "<div class='legend-note'>",
          note,
          "</div>"
        )
      } else {
        ""
      }
    )
    
    session$sendCustomMessage(
      "update-map-legend",
      list(
        show = TRUE,
        html = html
      )
    )
  }
  
  hide_legend <- function() {
    session$sendCustomMessage(
      "update-map-legend",
      list(show = FALSE)
    )
  }
  
  show_incidence_legend <- function() {
    send_legend(
      title = "Incidence classification",
      subtitle = "Relative quintiles among counties with reportable incidence estimates",
      rows = c(
        legend_row("#FFF3B0", "Lowest"),
        legend_row("#FEC44F", "Low"),
        legend_row("#FE9929", "Moderate"),
        legend_row("#EC5D3A", "High"),
        legend_row("#9E1B32", "Highest"),
        legend_row("#50565C", "Suppressed / unavailable")
      ),
      note = "Classes are relative categories, not clinical risk thresholds."
    )
  }
  
  show_incidence_top10_legend <- function() {
    send_legend(
      title = "Top-10 incidence ranking",
      subtitle = "Only the 10 highest reportable counties are emphasized",
      rows = c(
        legend_row("#FFCC80", "Lower end of Top 10"),
        legend_row("#F4511E", "Middle of Top 10"),
        legend_row("#7F0000", "Highest incidence"),
        legend_row("#B7B1A7", "All other counties"),
        legend_row("#FFD700", "Highest county outline")
      ),
      note = "Numbers on the map are descending county ranks."
    )
  }
  
  show_mortality_prism_legend <- function() {
    send_legend(
      title = "Mortality 3D prism map",
      subtitle = "Color and prism height both represent mortality",
      rows = c(
        legend_row("#E0F3F8", "Lowest"),
        legend_row("#ABD9E9", "Low"),
        legend_row("#74ADD1", "Moderate"),
        legend_row("#5E4FA2", "High"),
        legend_row("#2D1457", "Highest"),
        legend_row("#484D54", "Suppressed / unavailable")
      ),
      note = "Prism height is normalized within the reportable mortality distribution and is not physical elevation."
    )
  }
  
  show_mortality_symbol_legend <- function() {
    send_legend(
      title = "Mortality proportional symbols",
      subtitle = "Larger circles represent higher mortality rates",
      rows = c(
        circle_row(10, "#7FDBDA", "Lower mortality rate"),
        circle_row(16, "#7FDBDA", "Middle mortality rate"),
        circle_row(24, "#005B5C", "Higher / Top-10 mortality rate")
      ),
      note = "Dark teal circles identify Top-10 counties. Circle size is scaled within the available mortality estimates."
    )
  }
  
  show_rurality_legend <- function() {
    send_legend(
      title = "USDA rurality classification",
      subtitle = "Derived from 2023 Rural-Urban Continuum Codes",
      rows = c(
        legend_row("#2C7FB8", "Metropolitan"),
        legend_row("#7FCDBB", "Nonmetropolitan adjacent to metro"),
        legend_row("#FEC44F", "Nonmetropolitan nonadjacent"),
        legend_row("#666666", "Unavailable")
      )
    )
  }
  
  show_places_legend <- function() {
    send_legend(
      title = "Current cigarette smoking",
      subtitle = "CDC PLACES county prevalence",
      rows = c(
        legend_row("#EDF8FB", "Lower prevalence"),
        legend_row("#66C2A4", "Middle prevalence"),
        legend_row("#006D2C", "Higher prevalence"),
        legend_row("#555B61", "Unavailable")
      ),
      note = "Hover or click a county to view additional PLACES indicators."
    )
  }
  
  show_ohio_comparison_legend <- function() {
    send_legend(
      title = "Incidence compared with Ohio",
      subtitle = "Classification uses county confidence intervals when available",
      rows = c(
        legend_row("#B2182B", "Above Ohio"),
        legend_row("#F7F7F7", "Not clearly different from Ohio"),
        legend_row("#2166AC", "Below Ohio"),
        legend_row("#EF8A62", "Point estimate above Ohio"),
        legend_row("#67A9CF", "Point estimate below Ohio"),
        legend_row("#D9D9D9", "Equal to Ohio"),
        legend_row("#4D5358", "Suppressed / unavailable")
      ),
      note = "Point-estimate categories are used only when confidence intervals are unavailable."
    )
  }
  
  show_lisa_legend <- function() {
    send_legend(
      title = "FDR-adjusted Local Moran category",
      subtitle = "Spatial sensitivity analysis",
      rows = c(
        legend_row("#B2182B", "High-High"),
        legend_row("#2166AC", "Low-Low"),
        legend_row("#EF8A62", "High-Low"),
        legend_row("#67A9CF", "Low-High"),
        legend_row("#D9D9D9", "Not significant"),
        legend_row("#4D5358", "Not available")
      ),
      note = "Rate-based cluster results should be interpreted cautiously because many county estimates are suppressed."
    )
  }
  
  show_portage_legend <- function() {
    send_legend(
      title = "Portage County focus",
      rows = c(
        legend_row("#00E6E6", "Portage County"),
        legend_row("#9AA0A6", "Other Ohio counties")
      ),
      note = "The source provides one county-level estimate for Portage County."
    )
  }
  
  
  show_sources_legend <- function() {
    send_legend(
      title = "Integrated data sources",
      subtitle = "Public surveillance and contextual datasets",
      rows = c(
        legend_row("#EC5D3A", "State Cancer Profiles"),
        legend_row("#2C7FB8", "USDA RUCC 2023"),
        legend_row("#66C2A4", "CDC PLACES"),
        legend_row("#5E4FA2", "Spatial sensitivity analyses"),
        legend_row("#D9D9D9", "Suppressed / unavailable data")
      ),
      note = "All results are county-level and ecological."
    )
  }
  
  output$map <- renderMaplibre({
    maplibre(
      style = carto_style("positron"),
      center = us_center,
      zoom = 2.8,
      pitch = 0,
      bearing = 0,
      scrollZoom = FALSE,
      dragPan = TRUE,
      dragRotate = TRUE,
      doubleClickZoom = TRUE,
      touchZoomRotate = TRUE,
      minZoom = 2,
      maxZoom = 14
    ) |>
      
      add_raster_source(
        id = "dark-source",
        tiles = c(
          "https://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
          "https://b.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
          "https://c.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png"
        ),
        tileSize = 256,
        maxzoom = 20,
        attribution = "© OpenStreetMap contributors © CARTO"
      ) |>
      
      add_raster_layer(
        id = "dark-basemap",
        source = "dark-source",
        raster_opacity = 1,
        visibility = "visible"
      ) |>
      
      add_raster_source(
        id = "satellite-source",
        tiles = c(
          paste0(
            "https://server.arcgisonline.com/",
            "ArcGIS/rest/services/World_Imagery/",
            "MapServer/tile/{z}/{y}/{x}"
          )
        ),
        tileSize = 256,
        maxzoom = 19,
        attribution = "Imagery © Esri"
      ) |>
      
      add_raster_layer(
        id = "satellite-basemap",
        source = "satellite-source",
        raster_opacity = 1,
        visibility = "none"
      ) |>
      
      add_fill_layer(
        id = "incidence-overview",
        source = ohio_cancer,
        fill_color = match_expr(
          column = "incidence_class",
          values = c("Lowest", "Low", "Moderate", "High", "Highest", "Suppressed / unavailable"),
          stops = c("#FFF3B0", "#FEC44F", "#FE9929", "#EC5D3A", "#9E1B32", "#50565C"),
          default = "#50565C"
        ),
        fill_opacity = 0.84,
        visibility = "visible",
        popup = paste0(
          "<strong>{county} County</strong><br>",
          "Incidence: {incidence_display}<br>",
          "Rating: {incidence_class}<br>",
          "Rank: {incidence_rank_display}<br>",
          "Average annual cases: {cases_display}"
        ),
        popup_style = "dark"
      ) |>
      
      add_fill_layer(
        id = "incidence-ranking-base",
        source = ohio_cancer,
        fill_color = "#B7B1A7",
        fill_opacity = 0.55,
        visibility = "none"
      ) |>
      
      add_fill_layer(
        id = "incidence-top10-fill",
        source = top10_incidence_polygons,
        fill_color = interpolate(
          column = "incidence_rate",
          values = c(
            min(top10_incidence_polygons$incidence_rate, na.rm = TRUE),
            median(top10_incidence_polygons$incidence_rate, na.rm = TRUE),
            max(top10_incidence_polygons$incidence_rate, na.rm = TRUE)
          ),
          stops = c("#FFCC80", "#F4511E", "#7F0000"),
          na_color = "#B7B1A7"
        ),
        fill_opacity = 0.92,
        visibility = "none",
        popup = paste0(
          "<strong>{county} County</strong><br>",
          "Incidence: {incidence_display}<br>",
          "Rank: {incidence_rank_display}"
        ),
        popup_style = "dark"
      ) |>
      
      add_line_layer(
        id = "highest-incidence-outline",
        source = highest_incidence_polygon,
        line_color = "#FFD700",
        line_width = 5,
        line_opacity = 1,
        visibility = "none"
      ) |>
      
      add_symbol_layer(
        id = "incidence-rank-labels",
        source = top10_incidence_points,
        text_field = get_column("rank_label"),
        text_size = 15,
        text_color = "#FFFFFF",
        text_halo_color = "#5A1000",
        text_halo_width = 2,
        text_allow_overlap = TRUE,
        text_ignore_placement = TRUE,
        visibility = "none"
      ) |>
      
      add_fill_layer(
        id = "mortality-prism-base",
        source = ohio_cancer,
        fill_color = match_expr(
          column = "mortality_class",
          values = c("Lowest", "Low", "Moderate", "High", "Highest", "Suppressed / unavailable"),
          stops = c("#E0F3F8", "#ABD9E9", "#74ADD1", "#5E4FA2", "#2D1457", "#484D54"),
          default = "#484D54"
        ),
        fill_opacity = 0.42,
        visibility = "none"
      ) |>
      
      add_fill_extrusion_layer(
        id = "mortality-prisms",
        source = ohio_cancer,
        fill_extrusion_color = match_expr(
          column = "mortality_class",
          values = c("Lowest", "Low", "Moderate", "High", "Highest", "Suppressed / unavailable"),
          stops = c("#E0F3F8", "#ABD9E9", "#74ADD1", "#5E4FA2", "#2D1457", "#484D54"),
          default = "#484D54"
        ),
        fill_extrusion_height = get_column("mortality_height"),
        fill_extrusion_base = 0,
        fill_extrusion_opacity = 0.82,
        fill_extrusion_vertical_gradient = TRUE,
        visibility = "none",
        popup = paste0(
          "<strong>{county} County</strong><br>",
          "Mortality: {mortality_display}<br>",
          "Rating: {mortality_class}<br>",
          "Rank: {mortality_rank_display}<br>",
          "Average annual deaths: {deaths_display}"
        ),
        popup_style = "dark"
      ) |>
      
      add_fill_layer(
        id = "mortality-symbol-base",
        source = ohio_cancer,
        fill_color = "#969CA2",
        fill_opacity = 0.28,
        visibility = "none"
      ) |>
      
      add_circle_layer(
        id = "mortality-symbols",
        source = mortality_points,
        circle_radius = get_column("symbol_radius"),
        circle_color = match_expr(
          column = "symbol_group",
          values = c("Top 10", "Other"),
          stops = c("#005B5C", "#7FDBDA"),
          default = "#7FDBDA"
        ),
        circle_opacity = 0.82,
        circle_stroke_color = "#FFFFFF",
        circle_stroke_width = 1.2,
        visibility = "none",
        popup = paste0(
          "<strong>{county} County</strong><br>",
          "Mortality: {mortality_display}<br>",
          "Rank: {mortality_rank_display}"
        ),
        popup_style = "dark"
      ) |>
      
      add_symbol_layer(
        id = "mortality-rank-labels",
        source = top10_mortality_points,
        text_field = get_column("rank_label"),
        text_size = 13,
        text_color = "#FFFFFF",
        text_halo_color = "#003637",
        text_halo_width = 2,
        text_allow_overlap = TRUE,
        text_ignore_placement = TRUE,
        visibility = "none"
      ) |>
      
      add_fill_layer(
        id = "rurality-context",
        source = ohio_cancer,
        fill_color = match_expr(
          column = "rurality_3_level",
          values = c(
            "Metropolitan",
            "Nonmetropolitan adjacent to metro",
            "Nonmetropolitan nonadjacent"
          ),
          stops = c(
            "#2C7FB8",
            "#7FCDBB",
            "#FEC44F"
          ),
          default = "#666666"
        ),
        fill_opacity = 0.82,
        visibility = "none",
        popup = paste0(
          "<strong>{county} County</strong><br>",
          "Population: {population_display}<br>",
          "Rurality: {rurality_display}<br>",
          "RUCC: {rucc_2023}"
        ),
        popup_style = "dark"
      ) |>
      
      add_fill_layer(
        id = "places-smoking",
        source = ohio_cancer,
        fill_color = interpolate(
          column = "smoking_percent",
          values = c(10, 16, 24),
          stops = c("#EDF8FB", "#66C2A4", "#006D2C"),
          na_color = "#555B61"
        ),
        fill_opacity = 0.84,
        visibility = "none",
        popup = paste0(
          "<strong>{county} County</strong><br>",
          "Smoking: {smoking_display}<br>",
          "Obesity: {obesity_display}<br>",
          "Physical inactivity: {inactivity_display}<br>",
          "Uninsured: {uninsured_display}<br>",
          "Mammography: {mammography_display}<br>",
          "Food insecurity: {food_insecurity_display}<br>",
          "Transportation barriers: {transportation_display}"
        ),
        popup_style = "dark"
      ) |>
      
      add_fill_layer(
        id = "incidence-ohio-comparison",
        source = ohio_cancer,
        fill_color = match_expr(
          column = "incidence_comparison",
          values = c(
            "Above Ohio",
            "Not clearly different from Ohio",
            "Below Ohio",
            "Point estimate above Ohio",
            "Point estimate below Ohio",
            "Equal to Ohio",
            "Suppressed / unavailable"
          ),
          stops = c(
            "#B2182B",
            "#F7F7F7",
            "#2166AC",
            "#EF8A62",
            "#67A9CF",
            "#D9D9D9",
            "#4D5358"
          ),
          default = "#4D5358"
        ),
        fill_opacity = 0.88,
        visibility = "none",
        popup = paste0(
          "<strong>{county} County</strong><br>",
          "Incidence: {incidence_display}<br>",
          "95% CI: {incidence_ci_display}<br>",
          "Comparison: {incidence_comparison}"
        ),
        popup_style = "dark"
      ) |>
      
      add_fill_layer(
        id = "incidence-lisa",
        source = ohio_cancer,
        fill_color = match_expr(
          column = "incidence_cluster_display",
          values = c(
            "High-High",
            "Low-Low",
            "High-Low",
            "Low-High",
            "Not significant",
            "Not available"
          ),
          stops = c(
            "#B2182B",
            "#2166AC",
            "#EF8A62",
            "#67A9CF",
            "#D9D9D9",
            "#4D5358"
          ),
          default = "#4D5358"
        ),
        fill_opacity = 0.88,
        visibility = "none",
        popup = paste0(
          "<strong>{county} County</strong><br>",
          "Incidence cluster: {incidence_cluster_display}<br>",
          "Mortality cluster: {mortality_cluster_display}<br>",
          "Incidence: {incidence_display}<br>",
          "Mortality: {mortality_display}"
        ),
        popup_style = "dark"
      ) |>
      
      add_fill_layer(
        id = "portage-context",
        source = ohio_cancer,
        fill_color = "#9AA0A6",
        fill_opacity = 0.26,
        visibility = "none"
      ) |>
      
      add_fill_layer(
        id = "portage-focus",
        source = portage,
        fill_color = "#00E6E6",
        fill_opacity = 0.34,
        visibility = "none"
      ) |>
      
      add_line_layer(
        id = "county-boundaries",
        source = ohio_cancer,
        line_color = "#FFFFFF",
        line_width = 0.85,
        line_opacity = 0.90
      ) |>
      
      add_raster_source(
        id = "satellite-labels-source",
        tiles = c(
          paste0(
            "https://server.arcgisonline.com/",
            "ArcGIS/rest/services/Reference/",
            "World_Boundaries_and_Places/",
            "MapServer/tile/{z}/{y}/{x}"
          )
        ),
        tileSize = 256,
        maxzoom = 19,
        attribution = "Reference labels © Esri"
      ) |>
      
      add_raster_layer(
        id = "satellite-labels",
        source = "satellite-labels-source",
        raster_opacity = 1,
        visibility = "none"
      ) |>
      
      add_line_layer(
        id = "portage-outline",
        source = portage,
        line_color = "#00FFFF",
        line_width = 4,
        line_opacity = 1
      ) |>
      
      add_navigation_control(
        position = "bottom-right",
        show_compass = TRUE,
        show_zoom = TRUE,
        visualize_pitch = TRUE,
        orientation = "vertical"
      ) |>
      
      add_fullscreen_control(position = "bottom-right") |>
      add_scale_control(position = "bottom-left")
  })
  
  observeEvent(input$basemap_choice, {
    proxy <- maplibre_proxy("map")
    
    if (identical(input$basemap_choice, "satellite")) {
      proxy |>
        set_layout_property("dark-basemap", "visibility", "none") |>
        set_layout_property("satellite-basemap", "visibility", "visible") |>
        set_layout_property("satellite-labels", "visibility", "visible")
    } else {
      proxy |>
        set_layout_property("dark-basemap", "visibility", "visible") |>
        set_layout_property("satellite-basemap", "visibility", "none") |>
        set_layout_property("satellite-labels", "visibility", "none")
    }
  }, ignoreInit = FALSE)
  
  hide_all <- function() {
    maplibre_proxy("map") |>
      set_layout_property("incidence-overview", "visibility", "none") |>
      set_layout_property("incidence-ranking-base", "visibility", "none") |>
      set_layout_property("incidence-top10-fill", "visibility", "none") |>
      set_layout_property("highest-incidence-outline", "visibility", "none") |>
      set_layout_property("incidence-rank-labels", "visibility", "none") |>
      set_layout_property("mortality-prism-base", "visibility", "none") |>
      set_layout_property("mortality-prisms", "visibility", "none") |>
      set_layout_property("mortality-symbol-base", "visibility", "none") |>
      set_layout_property("mortality-symbols", "visibility", "none") |>
      set_layout_property("mortality-rank-labels", "visibility", "none") |>
      set_layout_property("rurality-context", "visibility", "none") |>
      set_layout_property("places-smoking", "visibility", "none") |>
      set_layout_property("incidence-ohio-comparison", "visibility", "none") |>
      set_layout_property("incidence-lisa", "visibility", "none") |>
      set_layout_property("portage-context", "visibility", "none") |>
      set_layout_property("portage-focus", "visibility", "none")
  }
  
  show_incidence <- function() {
    hide_all() |>
      set_layout_property("incidence-overview", "visibility", "visible")
  }
  
  show_incidence_top10 <- function() {
    hide_all() |>
      set_layout_property("incidence-ranking-base", "visibility", "visible") |>
      set_layout_property("incidence-top10-fill", "visibility", "visible") |>
      set_layout_property("highest-incidence-outline", "visibility", "visible") |>
      set_layout_property("incidence-rank-labels", "visibility", "visible")
  }
  
  show_mortality_3d <- function() {
    hide_all() |>
      set_layout_property("mortality-prism-base", "visibility", "visible") |>
      set_layout_property("mortality-prisms", "visibility", "visible")
  }
  
  show_mortality_symbols <- function() {
    hide_all() |>
      set_layout_property("mortality-symbol-base", "visibility", "visible") |>
      set_layout_property("mortality-symbols", "visibility", "visible") |>
      set_layout_property("mortality-rank-labels", "visibility", "visible")
  }
  
  show_rurality <- function() {
    hide_all() |>
      set_layout_property("rurality-context", "visibility", "visible")
  }
  
  show_places <- function() {
    hide_all() |>
      set_layout_property("places-smoking", "visibility", "visible")
  }
  
  show_ohio_comparison <- function() {
    hide_all() |>
      set_layout_property("incidence-ohio-comparison", "visibility", "visible")
  }
  
  show_spatial_context <- function() {
    hide_all() |>
      set_layout_property("incidence-lisa", "visibility", "visible")
  }
  
  show_portage <- function() {
    hide_all() |>
      set_layout_property("portage-context", "visibility", "visible") |>
      set_layout_property("portage-focus", "visibility", "visible")
  }
  
  observeEvent(input$reset_view, {
    camera <- scene_camera[[current_scene()]]
    if (is.null(camera)) camera <- scene_camera$intro
    
    maplibre_proxy("map") |>
      fly_to(
        center = camera$center,
        zoom = camera$zoom,
        pitch = camera$pitch,
        bearing = camera$bearing,
        duration = 1800,
        essential = TRUE
      )
  })
  
  on_section("map", "intro", {
    current_scene("intro")
    hide_legend()
    show_incidence() |>
      fly_to(
        center = us_center,
        zoom = 2.8,
        pitch = 0,
        bearing = 0,
        duration = 2800,
        essential = TRUE
      )
  })
  
  on_section("map", "study_overview", {
    current_scene("study_overview")
    
    # Show the color key because the background map is displaying
    # the incidence-classified choropleth on this section.
    show_incidence_legend()
    
    show_incidence() |>
      fly_to(
        center = ohio_center,
        zoom = 5.45,
        pitch = 0,
        bearing = 0,
        duration = 3200,
        essential = TRUE
      )
  })
  
  on_section("map", "incidence_overview", {
    current_scene("incidence_overview")
    show_incidence_legend()
    show_incidence() |>
      fly_to(center = ohio_center, zoom = 5.75, pitch = 12, bearing = 0, duration = 3500, essential = TRUE)
  })
  
  on_section("map", "incidence_top10", {
    current_scene("incidence_top10")
    show_incidence_top10_legend()
    show_incidence_top10() |>
      fly_to(center = ohio_center, zoom = 5.95, pitch = 0, bearing = 0, duration = 3000, essential = TRUE)
  })
  
  on_section("map", "mortality_overview", {
    current_scene("mortality_overview")
    show_mortality_prism_legend()
    show_mortality_3d() |>
      fly_to(center = c(-82.75, 40.15), zoom = 5.9, pitch = 52, bearing = -12, duration = 3600, essential = TRUE)
  })
  
  on_section("map", "mortality_symbols", {
    current_scene("mortality_symbols")
    show_mortality_symbol_legend()
    show_mortality_symbols() |>
      fly_to(center = ohio_center, zoom = 5.85, pitch = 5, bearing = 0, duration = 3000, essential = TRUE)
  })
  
  on_section("map", "rurality_context", {
    current_scene("rurality_context")
    show_rurality_legend()
    show_rurality() |>
      fly_to(
        center = ohio_center,
        zoom = 5.75,
        pitch = 5,
        bearing = 0,
        duration = 3000,
        essential = TRUE
      )
  })
  
  on_section("map", "places_context", {
    current_scene("places_context")
    show_places_legend()
    show_places() |>
      fly_to(
        center = ohio_center,
        zoom = 5.75,
        pitch = 5,
        bearing = 0,
        duration = 3000,
        essential = TRUE
      )
  })
  
  on_section("map", "ohio_comparison", {
    current_scene("ohio_comparison")
    show_ohio_comparison_legend()
    show_ohio_comparison() |>
      fly_to(
        center = ohio_center,
        zoom = 5.75,
        pitch = 5,
        bearing = 0,
        duration = 3000,
        essential = TRUE
      )
  })
  
  on_section("map", "spatial_context", {
    current_scene("spatial_context")
    show_lisa_legend()
    show_spatial_context() |>
      fly_to(
        center = ohio_center,
        zoom = 5.75,
        pitch = 5,
        bearing = 0,
        duration = 3000,
        essential = TRUE
      )
  })
  
  on_section("map", "portage_profile", {
    current_scene("portage_profile")
    show_portage_legend()
    show_portage() |>
      fly_to(center = portage_center, zoom = 9.15, pitch = 18, bearing = 0, duration = 4300, essential = TRUE)
  })
  
  on_section("map", "portage_limitations", {
    current_scene("portage_limitations")
    show_portage_legend()
    show_portage() |>
      fly_to(center = portage_center, zoom = 9.55, pitch = 5, bearing = 0, duration = 2800, essential = TRUE)
  })
  
  on_section("map", "interpretation", {
    current_scene("interpretation")
    show_incidence_legend()
    show_incidence() |>
      fly_to(center = ohio_center, zoom = 5.65, pitch = 10, bearing = 0, duration = 3200, essential = TRUE)
  })
  
  
  on_section("map", "methods_reproducibility", {
    current_scene("methods_reproducibility")
    show_sources_legend()
    show_incidence() |>
      fly_to(
        center = ohio_center,
        zoom = 5.45,
        pitch = 5,
        bearing = 0,
        duration = 3200,
        essential = TRUE
      )
  })
  
  
  on_section("map", "data_sources", {
    current_scene("data_sources")
    show_sources_legend()
    show_incidence() |>
      fly_to(
        center = ohio_center,
        zoom = 5.35,
        pitch = 5,
        bearing = 0,
        duration = 3200,
        essential = TRUE
      )
  })
  
  output$download_county <- downloadHandler(
    filename = function() paste0("ohio_ovarian_cancer_county_analysis_", Sys.Date(), ".csv"),
    content = function(file) write_csv(county_download, file, na = "")
  )
  
  output$download_top10 <- downloadHandler(
    filename = function() paste0("ohio_ovarian_cancer_top10_", Sys.Date(), ".csv"),
    content = function(file) write_csv(top10_download, file, na = "")
  )
  
  output$download_portage <- downloadHandler(
    filename = function() paste0("portage_ovarian_cancer_profile_", Sys.Date(), ".csv"),
    content = function(file) write_csv(portage_download, file, na = "")
  )
  
  output$download_dictionary <- downloadHandler(
    filename = function() "ohio_ovarian_cancer_data_dictionary.csv",
    content = function(file) write_csv(dictionary, file, na = "")
  )
}

shinyApp(ui, server)