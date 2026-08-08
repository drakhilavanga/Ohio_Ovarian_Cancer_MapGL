# ============================================================
# 23_generate_county_reports.R
#
# Generate reviewer-friendly HTML county fact sheets from the
# county profile dataset created by Script 22.
#
# Default behavior:
#   - Generates a Portage County HTML report
#   - Generates a report index for all counties
#
# Optional behavior:
#   - Set generate_all_counties <- TRUE to create one report
#     for every Ohio county.
#
# Inputs:
#   outputs/tables/22_county_profiles.csv
#
# Outputs:
#   outputs/reports/
#   index.html
#   portage_county_profile.html
#   reports for all counties when enabled
#
# IMPORTANT:
#   - Cancer estimates are county-level ecological measures.
#   - PLACES indicators are contextual characteristics.
#   - Contextual indicators are not interpreted as causes of
#     ovarian cancer.
# ============================================================

# ============================================================
# 1. PACKAGE CHECK
# ============================================================

required_packages <- c(
  "dplyr",
  "readr",
  "stringr",
  "janitor",
  "htmltools",
  "scales"
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
library(stringr)
library(janitor)
library(htmltools)
library(scales)

# ============================================================
# 2. SETTINGS
# ============================================================

generate_all_counties <- FALSE

input_file <- file.path(
  "outputs",
  "tables",
  "22_county_profiles.csv"
)

output_dir <- file.path(
  "outputs",
  "reports"
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
      "\n\nRun R/22_county_profile_builder.R first."
    )
  )
}

# ============================================================
# 3. READ DATA
# ============================================================

profiles <- read_csv(
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
  "incidence_rate",
  "incidence_lower_ci",
  "incidence_upper_ci",
  "average_annual_cases",
  "mortality_rate",
  "mortality_lower_ci",
  "mortality_upper_ci",
  "average_annual_deaths",
  "smoking_percent",
  "obesity_percent",
  "physical_inactivity_percent",
  "diabetes_percent",
  "uninsured_percent",
  "annual_checkup_percent",
  "mammography_percent",
  "food_insecurity_percent",
  "transportation_barriers_percent"
)

missing_columns <- setdiff(
  required_columns,
  names(profiles)
)

if (length(missing_columns) > 0) {
  stop(
    paste0(
      "The county profile dataset is missing:\n",
      paste(
        missing_columns,
        collapse = ", "
      )
    )
  )
}

profiles <- profiles |>
  mutate(
    fips = str_pad(
      as.character(fips),
      width = 5,
      side = "left",
      pad = "0"
    )
  )

# ============================================================
# 4. FORMAT HELPERS
# ============================================================

format_number_safe <- function(
    x,
    digits = 1
) {
  if (
    length(x) == 0 ||
    is.na(x)
  ) {
    return("Unavailable")
  }
  
  format(
    round(
      as.numeric(x),
      digits
    ),
    nsmall = digits,
    big.mark = ",",
    scientific = FALSE
  )
}

format_integer_safe <- function(x) {
  if (
    length(x) == 0 ||
    is.na(x)
  ) {
    return("Unavailable")
  }
  
  comma(
    round(
      as.numeric(x)
    )
  )
}

format_rate_ci <- function(
    rate,
    lower,
    upper
) {
  if (
    is.na(rate) ||
    is.na(lower) ||
    is.na(upper)
  ) {
    return(
      "Suppressed / unavailable"
    )
  }
  
  paste0(
    format_number_safe(
      rate,
      1
    ),
    " per 100,000 (95% CI ",
    format_number_safe(
      lower,
      1
    ),
    "-",
    format_number_safe(
      upper,
      1
    ),
    ")"
  )
}

format_percent_safe <- function(x) {
  if (
    length(x) == 0 ||
    is.na(x)
  ) {
    return("Unavailable")
  }
  
  paste0(
    format_number_safe(
      x,
      1
    ),
    "%"
  )
}

safe_value <- function(
    row,
    variable_name,
    fallback = "Unavailable"
) {
  if (
    !variable_name %in%
    names(row)
  ) {
    return(fallback)
  }
  
  value <- row[[variable_name]][[1]]
  
  if (
    length(value) == 0 ||
    is.na(value)
  ) {
    return(fallback)
  }
  
  as.character(value)
}

slugify_county <- function(x) {
  x |>
    str_to_lower() |>
    str_replace_all(
      "[^a-z0-9]+",
      "_"
    ) |>
    str_remove_all(
      "^_|_$"
    )
}

comparison_badge <- function(
    comparison_text
) {
  badge_class <- case_when(
    comparison_text ==
      "Above Ohio" ~
      "badge badge-high",
    
    comparison_text ==
      "Below Ohio" ~
      "badge badge-low",
    
    comparison_text ==
      "Not clearly different from Ohio" ~
      "badge badge-neutral",
    
    TRUE ~
      "badge badge-missing"
  )
  
  tags$span(
    class = badge_class,
    comparison_text
  )
}

# ============================================================
# 5. REPORT CSS
# ============================================================

report_css <- "
  :root {
    --navy: #17324d;
    --blue: #2f6f9f;
    --light: #f5f8fb;
    --border: #d8e1e8;
    --orange: #d96a2b;
    --purple: #654ea3;
    --teal: #168a8a;
    --text: #203040;
  }

  * {
    box-sizing: border-box;
  }

  body {
    margin: 0;
    background: #edf2f6;
    color: var(--text);
    font-family: Arial, Helvetica, sans-serif;
    line-height: 1.45;
  }

  .page {
    max-width: 1120px;
    margin: 28px auto;
    background: white;
    box-shadow: 0 8px 30px rgba(18, 39, 58, 0.15);
  }

  .hero {
    padding: 34px 42px;
    background: linear-gradient(135deg, #142f49, #285f87);
    color: white;
  }

  .hero h1 {
    margin: 0 0 6px;
    font-size: 34px;
  }

  .hero p {
    margin: 3px 0;
    color: rgba(255,255,255,0.88);
  }

  .content {
    padding: 30px 42px 42px;
  }

  .grid {
    display: grid;
    grid-template-columns: repeat(2, minmax(0, 1fr));
    gap: 18px;
  }

  .card {
    padding: 20px;
    border: 1px solid var(--border);
    border-radius: 10px;
    background: white;
  }

  .card h2 {
    margin: 0 0 12px;
    color: var(--navy);
    font-size: 20px;
  }

  .incidence {
    border-top: 5px solid var(--orange);
  }

  .mortality {
    border-top: 5px solid var(--purple);
  }

  .context {
    border-top: 5px solid var(--teal);
  }

  .metric {
    margin: 12px 0;
  }

  .metric-label {
    display: block;
    margin-bottom: 3px;
    color: #5b6b79;
    font-size: 12px;
    font-weight: 700;
    letter-spacing: 0.4px;
    text-transform: uppercase;
  }

  .metric-value {
    color: var(--navy);
    font-size: 19px;
    font-weight: 700;
  }

  .context-table {
    width: 100%;
    border-collapse: collapse;
  }

  .context-table th,
  .context-table td {
    padding: 9px 8px;
    border-bottom: 1px solid var(--border);
    text-align: left;
  }

  .context-table th {
    color: #556674;
    font-size: 12px;
    text-transform: uppercase;
  }

  .context-table td:last-child {
    text-align: right;
    font-weight: 700;
  }

  .badge {
    display: inline-block;
    padding: 5px 9px;
    border-radius: 999px;
    font-size: 12px;
    font-weight: 700;
  }

  .badge-high {
    background: #fde5dc;
    color: #9b3d16;
  }

  .badge-low {
    background: #e4f2fb;
    color: #205d86;
  }

  .badge-neutral {
    background: #edf1f4;
    color: #4c5b66;
  }

  .badge-missing {
    background: #eeeeee;
    color: #666666;
  }

  .note {
    margin-top: 22px;
    padding: 16px 18px;
    border-left: 5px solid #d8a13d;
    background: #fff8e9;
    color: #5b451e;
  }

  .footer {
    padding: 18px 42px;
    border-top: 1px solid var(--border);
    background: var(--light);
    color: #62717d;
    font-size: 12px;
  }

  .index-list {
    list-style: none;
    padding: 0;
    columns: 3;
  }

  .index-list li {
    margin: 6px 0;
  }

  .index-list a {
    color: var(--blue);
    text-decoration: none;
  }

  .index-list a:hover {
    text-decoration: underline;
  }

  @media (max-width: 760px) {
    .page {
      margin: 0;
    }

    .hero,
    .content,
    .footer {
      padding-left: 20px;
      padding-right: 20px;
    }

    .grid {
      grid-template-columns: 1fr;
    }

    .index-list {
      columns: 1;
    }
  }

  @media print {
    body {
      background: white;
    }

    .page {
      margin: 0;
      box-shadow: none;
    }
  }
"

# ============================================================
# 6. COUNTY REPORT BUILDER
# ============================================================

build_county_report <- function(
    county_row,
    output_path
) {
  county_name <-
    county_row$county[[1]]
  
  incidence_comparison <- safe_value(
    county_row,
    "incidence_comparison_with_ohio",
    "Comparison unavailable"
  )
  
  mortality_comparison <- safe_value(
    county_row,
    "mortality_comparison_with_ohio",
    "Comparison unavailable"
  )
  
  incidence_rank <- safe_value(
    county_row,
    "incidence_rank_display",
    "Not available"
  )
  
  mortality_rank <- safe_value(
    county_row,
    "mortality_rank_display",
    "Not available"
  )
  
  incidence_cluster <- safe_value(
    county_row,
    "incidence_local_cluster_fdr",
    "Not available"
  )
  
  mortality_cluster <- safe_value(
    county_row,
    "mortality_local_cluster_fdr",
    "Not available"
  )
  
  context_rows <- list(
    c(
      "Current cigarette smoking",
      format_percent_safe(
        county_row$smoking_percent[[1]]
      )
    ),
    c(
      "Obesity",
      format_percent_safe(
        county_row$obesity_percent[[1]]
      )
    ),
    c(
      "Physical inactivity",
      format_percent_safe(
        county_row$physical_inactivity_percent[[1]]
      )
    ),
    c(
      "Diabetes",
      format_percent_safe(
        county_row$diabetes_percent[[1]]
      )
    ),
    c(
      "Uninsured",
      format_percent_safe(
        county_row$uninsured_percent[[1]]
      )
    ),
    c(
      "Annual checkup",
      format_percent_safe(
        county_row$annual_checkup_percent[[1]]
      )
    ),
    c(
      "Mammography",
      format_percent_safe(
        county_row$mammography_percent[[1]]
      )
    ),
    c(
      "Food insecurity",
      format_percent_safe(
        county_row$food_insecurity_percent[[1]]
      )
    ),
    c(
      "Transportation barriers",
      format_percent_safe(
        county_row$transportation_barriers_percent[[1]]
      )
    )
  )
  
  context_table_rows <- lapply(
    context_rows,
    function(item) {
      tags$tr(
        tags$td(
          item[[1]]
        ),
        tags$td(
          item[[2]]
        )
      )
    }
  )
  
  html_document <- tags$html(
    tags$head(
      tags$meta(
        charset = "utf-8"
      ),
      tags$meta(
        name = "viewport",
        content =
          "width=device-width, initial-scale=1"
      ),
      tags$title(
        paste0(
          county_name,
          " County Ovarian Cancer Profile"
        )
      ),
      tags$style(
        HTML(
          report_css
        )
      )
    ),
    
    tags$body(
      tags$div(
        class = "page",
        
        tags$div(
          class = "hero",
          tags$h1(
            paste0(
              county_name,
              " County Profile"
            )
          ),
          tags$p(
            "Ohio Ovarian Cancer Surveillance Atlas"
          ),
          tags$p(
            paste0(
              "County FIPS: ",
              county_row$fips[[1]],
              " | Population: ",
              format_integer_safe(
                county_row$population_2020[[1]]
              )
            )
          ),
          tags$p(
            paste0(
              county_row$metro_status[[1]],
              " | RUCC ",
              county_row$rucc_2023[[1]],
              " | ",
              county_row$rurality_3_level[[1]]
            )
          )
        ),
        
        tags$div(
          class = "content",
          
          tags$div(
            class = "grid",
            
            tags$div(
              class = "card incidence",
              tags$h2(
                "Incidence"
              ),
              
              tags$div(
                class = "metric",
                tags$span(
                  class = "metric-label",
                  "Age-adjusted rate"
                ),
                tags$div(
                  class = "metric-value",
                  format_rate_ci(
                    county_row$incidence_rate[[1]],
                    county_row$incidence_lower_ci[[1]],
                    county_row$incidence_upper_ci[[1]]
                  )
                )
              ),
              
              tags$div(
                class = "metric",
                tags$span(
                  class = "metric-label",
                  "Average annual cases"
                ),
                tags$div(
                  class = "metric-value",
                  format_integer_safe(
                    county_row$average_annual_cases[[1]]
                  )
                )
              ),
              
              tags$div(
                class = "metric",
                tags$span(
                  class = "metric-label",
                  "Descriptive rank"
                ),
                tags$div(
                  class = "metric-value",
                  incidence_rank
                )
              ),
              
              tags$div(
                class = "metric",
                tags$span(
                  class = "metric-label",
                  "Comparison with Ohio"
                ),
                comparison_badge(
                  incidence_comparison
                )
              ),
              
              tags$div(
                class = "metric",
                tags$span(
                  class = "metric-label",
                  "FDR-adjusted Local Moran category"
                ),
                tags$div(
                  class = "metric-value",
                  incidence_cluster
                )
              )
            ),
            
            tags$div(
              class = "card mortality",
              tags$h2(
                "Mortality"
              ),
              
              tags$div(
                class = "metric",
                tags$span(
                  class = "metric-label",
                  "Age-adjusted rate"
                ),
                tags$div(
                  class = "metric-value",
                  format_rate_ci(
                    county_row$mortality_rate[[1]],
                    county_row$mortality_lower_ci[[1]],
                    county_row$mortality_upper_ci[[1]]
                  )
                )
              ),
              
              tags$div(
                class = "metric",
                tags$span(
                  class = "metric-label",
                  "Average annual deaths"
                ),
                tags$div(
                  class = "metric-value",
                  format_integer_safe(
                    county_row$average_annual_deaths[[1]]
                  )
                )
              ),
              
              tags$div(
                class = "metric",
                tags$span(
                  class = "metric-label",
                  "Descriptive rank"
                ),
                tags$div(
                  class = "metric-value",
                  mortality_rank
                )
              ),
              
              tags$div(
                class = "metric",
                tags$span(
                  class = "metric-label",
                  "Comparison with Ohio"
                ),
                comparison_badge(
                  mortality_comparison
                )
              ),
              
              tags$div(
                class = "metric",
                tags$span(
                  class = "metric-label",
                  "FDR-adjusted Local Moran category"
                ),
                tags$div(
                  class = "metric-value",
                  mortality_cluster
                )
              )
            )
          ),
          
          tags$div(
            class = "card context",
            style = "margin-top: 18px;",
            tags$h2(
              "County Context"
            ),
            tags$table(
              class = "context-table",
              tags$thead(
                tags$tr(
                  tags$th(
                    "Indicator"
                  ),
                  tags$th(
                    "Estimate"
                  )
                )
              ),
              tags$tbody(
                context_table_rows
              )
            )
          ),
          
          tags$div(
            class = "note",
            tags$strong(
              "Interpretation limits"
            ),
            tags$br(),
            paste0(
              "Cancer estimates are county-level ecological measures. ",
              "CDC PLACES indicators describe county context and should ",
              "not be interpreted as individual-level causes or predictors ",
              "of ovarian cancer. Suppressed values do not indicate zero burden."
            )
          )
        ),
        
        tags$div(
          class = "footer",
          paste0(
            "Generated ",
            Sys.Date(),
            " | Incidence period: 2018-2022 | ",
            "Mortality period: 2019-2023"
          )
        )
      )
    )
  )
  
  save_html(
    html_document,
    file = output_path
  )
}

# ============================================================
# 7. GENERATE PORTAGE REPORT
# ============================================================

portage_row <- profiles |>
  filter(
    str_to_lower(
      str_squish(
        county
      )
    ) == "portage"
  )

if (nrow(portage_row) == 0) {
  warning(
    "Portage County was not found. No Portage report was created."
  )
} else {
  portage_output <- file.path(
    output_dir,
    "portage_county_profile.html"
  )
  
  build_county_report(
    portage_row,
    portage_output
  )
  
  cat(
    "\nPortage report created:\n",
    portage_output,
    "\n",
    sep = ""
  )
}

# ============================================================
# 8. OPTIONALLY GENERATE ALL COUNTY REPORTS
# ============================================================

if (isTRUE(generate_all_counties)) {
  for (
    row_number in seq_len(
      nrow(
        profiles
      )
    )
  ) {
    county_row <- profiles[
      row_number,
      ,
      drop = FALSE
    ]
    
    output_name <- paste0(
      slugify_county(
        county_row$county[[1]]
      ),
      "_county_profile.html"
    )
    
    build_county_report(
      county_row,
      file.path(
        output_dir,
        output_name
      )
    )
  }
  
  cat(
    "\nAll county reports were generated.\n"
  )
}

# ============================================================
# 9. GENERATE REPORT INDEX
# ============================================================

index_rows <- profiles |>
  arrange(
    county
  ) |>
  mutate(
    report_file = paste0(
      slugify_county(
        county
      ),
      "_county_profile.html"
    )
  )

index_links <- lapply(
  seq_len(
    nrow(
      index_rows
    )
  ),
  function(i) {
    county_name <-
      index_rows$county[[i]]
    
    report_file <-
      index_rows$report_file[[i]]
    
    if (
      county_name == "Portage"
    ) {
      report_file <-
        "portage_county_profile.html"
    }
    
    tags$li(
      tags$a(
        href = report_file,
        paste0(
          county_name,
          " County"
        )
      )
    )
  }
)

index_document <- tags$html(
  tags$head(
    tags$meta(
      charset = "utf-8"
    ),
    tags$meta(
      name = "viewport",
      content =
        "width=device-width, initial-scale=1"
    ),
    tags$title(
      "Ohio Ovarian Cancer County Reports"
    ),
    tags$style(
      HTML(
        report_css
      )
    )
  ),
  
  tags$body(
    tags$div(
      class = "page",
      
      tags$div(
        class = "hero",
        tags$h1(
          "Ohio Ovarian Cancer County Reports"
        ),
        tags$p(
          "Interactive county fact sheets generated from the epidemiologic atlas."
        )
      ),
      
      tags$div(
        class = "content",
        
        tags$p(
          if (
            isTRUE(
              generate_all_counties
            )
          ) {
            "All 88 county reports were generated."
          } else {
            paste0(
              "Only the Portage County report was generated in this run. ",
              "Set generate_all_counties <- TRUE at the beginning of the ",
              "script to create reports for every county."
            )
          }
        ),
        
        tags$ul(
          class = "index-list",
          index_links
        ),
        
        tags$div(
          class = "note",
          paste0(
            "Links for counties other than Portage will work only after ",
            "generate_all_counties is set to TRUE and the script is rerun."
          )
        )
      )
    )
  )
)

save_html(
  index_document,
  file = file.path(
    output_dir,
    "index.html"
  )
)

# ============================================================
# 10. SUMMARY FILE
# ============================================================

summary_lines <- c(
  "OHIO OVARIAN CANCER COUNTY REPORT GENERATOR",
  "",
  paste0(
    "Profile records available: ",
    nrow(
      profiles
    ),
    "."
  ),
  paste0(
    "Generate all counties setting: ",
    generate_all_counties,
    "."
  ),
  paste0(
    "Report folder: ",
    output_dir,
    "."
  ),
  "",
  paste0(
    "Open outputs/reports/index.html in a browser to review ",
    "the report collection."
  )
)

write_lines(
  summary_lines,
  file.path(
    output_dir,
    "23_report_generation_summary.txt"
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

cat(
  "\nCounty report generation completed successfully.\n"
)