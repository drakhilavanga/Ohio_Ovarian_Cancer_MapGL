# ============================================================
# OHIO OVARIAN CANCER — 3D INCIDENCE MAP
# mapgl 0.5.0
# ============================================================

library(sf)
library(dplyr)
library(mapgl)
library(htmlwidgets)

# ------------------------------------------------------------
# 1. LOAD DATA
# ------------------------------------------------------------

input_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_incidence.gpkg"
)

if (!file.exists(input_file)) {
  stop(
    "Input file not found:\n",
    input_file
  )
}

ohio_ovarian <- st_read(
  input_file,
  quiet = TRUE
) |>
  st_transform(4326)

# ------------------------------------------------------------
# 2. PREPARE DISPLAY AND HEIGHT VARIABLES
# ------------------------------------------------------------

ohio_ovarian <- ohio_ovarian |>
  mutate(
    incidence_rate = as.numeric(incidence_rate),
    
    incidence_display = if_else(
      is.na(incidence_rate),
      "Suppressed or unavailable",
      paste0(
        format(
          round(incidence_rate, 1),
          nsmall = 1
        ),
        " per 100,000"
      )
    ),
    
    average_cases_display = if_else(
      is.na(average_annual_cases),
      "Not available",
      format(
        round(average_annual_cases, 1),
        nsmall = 1
      )
    ),
    
    # Height is purely for visualization.
    # It does not represent physical elevation.
    extrusion_height = if_else(
      is.na(incidence_rate),
      1000,
      incidence_rate * 3000
    )
  )

# ------------------------------------------------------------
# 3. EXTRACT PORTAGE COUNTY
# ------------------------------------------------------------

portage <- ohio_ovarian |>
  filter(
    tolower(trimws(county)) == "portage"
  )

if (nrow(portage) != 1) {
  stop("Portage County was not found uniquely.")
}

# ------------------------------------------------------------
# 4. CREATE 3D MAP
# ------------------------------------------------------------

map_3d <- maplibre(
  style = carto_style("dark-matter"),
  center = c(-82.8, 40.35),
  zoom = 6.2,
  pitch = 58,
  bearing = -15
) |>
  
  # ----------------------------------------------------------
# 3D COUNTY EXTRUSIONS
# ----------------------------------------------------------

add_fill_extrusion_layer(
  id = "ovarian-incidence-3d",
  source = ohio_ovarian,
  
  fill_extrusion_color = interpolate(
    column = "incidence_rate",
    values = c(
      5,
      8,
      11,
      14,
      17
    ),
    stops = c(
      "#fff5f0",
      "#fcbba1",
      "#fb6a4a",
      "#cb181d",
      "#67000d"
    ),
    na_color = "#555555"
  ),
  
  fill_extrusion_height = get_column(
    "extrusion_height"
  ),
  
  fill_extrusion_base = 0,
  
  fill_extrusion_opacity = 0.88,
  
  fill_extrusion_vertical_gradient = TRUE,
  
  popup = paste0(
    "<strong>{county} County</strong><br>",
    "<strong>Incidence:</strong> ",
    "{incidence_display}<br>",
    "<strong>Average annual cases:</strong> ",
    "{average_cases_display}<br>",
    "<strong>Period:</strong> {period}"
  ),
  
  popup_style = "dark"
) |>
  
  # ----------------------------------------------------------
# COUNTY OUTLINES
# ----------------------------------------------------------

add_line_layer(
  id = "county-outline-3d",
  source = ohio_ovarian,
  line_color = "#e6e6e6",
  line_width = 0.8,
  line_opacity = 0.85
) |>
  
  # ----------------------------------------------------------
# PORTAGE COUNTY HIGHLIGHT
# ----------------------------------------------------------

add_fill_extrusion_layer(
  id = "portage-3d-highlight",
  source = portage,
  
  fill_extrusion_color = "#00ffff",
  
  fill_extrusion_height = get_column(
    "extrusion_height"
  ),
  
  fill_extrusion_base = 0,
  
  fill_extrusion_opacity = 0.30,
  
  fill_extrusion_vertical_gradient = TRUE,
  
  popup = paste0(
    "<strong>Portage County</strong><br>",
    "<strong>Incidence:</strong> ",
    "{incidence_display}<br>",
    "<strong>Average annual cases:</strong> ",
    "{average_cases_display}<br>",
    "<strong>Period:</strong> {period}"
  ),
  
  popup_style = "dark"
) |>
  
  add_line_layer(
    id = "portage-outline-3d",
    source = portage,
    line_color = "#00ffff",
    line_width = 4,
    line_opacity = 1
  ) |>
  
  # ----------------------------------------------------------
# LEGEND
# ----------------------------------------------------------

add_legend(
  legend_title = paste0(
    "Ovarian cancer incidence",
    "<br>",
    "<span style='font-size:12px;'>",
    "Age-adjusted rate per 100,000",
    "</span>"
  ),
  
  values = c(
    "5",
    "8",
    "11",
    "14",
    "17"
  ),
  
  colors = c(
    "#fff5f0",
    "#fcbba1",
    "#fb6a4a",
    "#cb181d",
    "#67000d"
  ),
  
  type = "continuous",
  position = "bottom-left"
) |>
  
  add_navigation_control(
    position = "bottom-right"
  ) |>
  
  add_fullscreen_control(
    position = "bottom-right"
  ) |>
  
  add_screenshot_control(
    position = "top-left",
    filename = "ohio_ovarian_cancer_3d"
  )

# ------------------------------------------------------------
# 5. DISPLAY
# ------------------------------------------------------------

print(map_3d)

# ------------------------------------------------------------
# 6. SAVE
# ------------------------------------------------------------

dir.create(
  "output/maps",
  recursive = TRUE,
  showWarnings = FALSE
)

output_file <- file.path(
  "output",
  "maps",
  "ohio_ovarian_cancer_3d.html"
)

saveWidget(
  widget = map_3d,
  file = output_file,
  selfcontained = FALSE
)

message(
  "3D map saved to:\n",
  normalizePath(output_file)
)

browseURL(
  normalizePath(output_file)
)