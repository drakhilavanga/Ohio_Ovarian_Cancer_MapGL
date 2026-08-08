# ============================================================
# OHIO OVARIAN CANCER MAPGL
# Light, Dark, and Satellite Views with Labels
# mapgl 0.5.0
# ============================================================

library(sf)
library(dplyr)
library(mapgl)
library(htmlwidgets)

# ------------------------------------------------------------
# 1. FILE PATHS
# ------------------------------------------------------------

input_file <- file.path(
  "data",
  "processed",
  "ohio_ovarian_cancer_incidence.gpkg"
)

output_directory <- file.path(
  "output",
  "maps"
)

output_file <- file.path(
  output_directory,
  "ohio_ovarian_cancer_all_views.html"
)

# ------------------------------------------------------------
# 2. CHECK INPUT FILE
# ------------------------------------------------------------

if (!file.exists(input_file)) {
  stop(
    "The spatial cancer dataset was not found:\n",
    input_file,
    "\nRun the previous cleaning and county-join scripts first."
  )
}

# ------------------------------------------------------------
# 3. READ DATA
# ------------------------------------------------------------

ohio_ovarian <- st_read(
  input_file,
  quiet = TRUE
) |>
  st_transform(4326)

required_columns <- c(
  "county",
  "incidence_rate"
)

missing_columns <- setdiff(
  required_columns,
  names(ohio_ovarian)
)

if (length(missing_columns) > 0) {
  stop(
    "The dataset is missing these columns: ",
    paste(missing_columns, collapse = ", ")
  )
}

# Add optional variables when absent
if (!"average_annual_cases" %in% names(ohio_ovarian)) {
  ohio_ovarian$average_annual_cases <- NA_real_
}

if (!"period" %in% names(ohio_ovarian)) {
  ohio_ovarian$period <- "2018–2022"
}

# ------------------------------------------------------------
# 4. CREATE POPUP DISPLAY VARIABLES
# ------------------------------------------------------------

ohio_ovarian <- ohio_ovarian |>
  mutate(
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
    
    cases_display = if_else(
      is.na(average_annual_cases),
      "Not available",
      format(
        round(average_annual_cases, 1),
        nsmall = 1
      )
    )
  )

# ------------------------------------------------------------
# 5. EXTRACT PORTAGE COUNTY
# ------------------------------------------------------------

portage <- ohio_ovarian |>
  filter(
    tolower(trimws(county)) == "portage"
  )

if (nrow(portage) != 1) {
  stop(
    "Portage County was not found uniquely."
  )
}

cat("\nPortage County profile:\n")

print(
  portage |>
    st_drop_geometry() |>
    select(
      county,
      incidence_rate,
      average_annual_cases,
      period
    )
)

# ------------------------------------------------------------
# 6. CREATE MAP
#
# Positron is the default Light View.
# Dark and Satellite are optional raster overlays.
# ------------------------------------------------------------

map_all_views <- maplibre(
  style = carto_style("positron"),
  center = c(-82.8, 40.4),
  zoom = 5.8,
  pitch = 25,
  bearing = 0
) |>
  
  # ==========================================================
# DARK BASEMAP
# ==========================================================

add_raster_source(
  id = "dark-source",
  
  tiles = c(
    "https://a.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
    "https://b.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
    "https://c.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png",
    "https://d.basemaps.cartocdn.com/dark_all/{z}/{x}/{y}.png"
  ),
  
  tileSize = 256,
  maxzoom = 20,
  
  attribution = paste0(
    "© OpenStreetMap contributors ",
    "© CARTO"
  )
) |>
  
  add_raster_layer(
    id = "dark-basemap",
    source = "dark-source",
    raster_opacity = 1,
    visibility = "none"
  ) |>
  
  # ==========================================================
# ESRI SATELLITE IMAGERY
# ==========================================================

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
  
  # ==========================================================
# OVARIAN CANCER INCIDENCE
# ==========================================================

add_fill_layer(
  id = "ovarian-incidence",
  source = ohio_ovarian,
  
  fill_color = interpolate(
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
    
    na_color = "#777777"
  ),
  
  fill_opacity = 0.62,
  
  popup = paste0(
    "<div style='min-width:220px;'>",
    "<strong>{county} County</strong>",
    "<hr style='margin:6px 0;'>",
    "<strong>Incidence rate:</strong> ",
    "{incidence_display}<br>",
    "<strong>Average annual cases:</strong> ",
    "{cases_display}<br>",
    "<strong>Study period:</strong> ",
    "{period}",
    "</div>"
  ),
  
  popup_style = "light"
) |>
  
  # ==========================================================
# COUNTY BOUNDARIES
# ==========================================================

add_line_layer(
  id = "county-boundaries",
  source = ohio_ovarian,
  line_color = "#ffffff",
  line_width = 0.9,
  line_opacity = 0.9
) |>
  
  # ==========================================================
# ESRI SATELLITE LABELS
#
# This reference layer is specifically designed to overlay
# roads, cities, states, and place names on satellite imagery.
# It is added after the cancer layer so labels remain visible.
# ==========================================================

add_raster_source(
  id = "satellite-labels-source",
  
  tiles = c(
    paste0(
      "https://server.arcgisonline.com/",
      "ArcGIS/rest/services/",
      "Reference/World_Boundaries_and_Places/",
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
  
  # ==========================================================
# PORTAGE COUNTY HIGHLIGHT
# ==========================================================

add_fill_layer(
  id = "portage-fill",
  source = portage,
  fill_color = "#00ffff",
  fill_opacity = 0.08
) |>
  
  add_line_layer(
    id = "portage-highlight",
    source = portage,
    line_color = "#00ffff",
    line_width = 4,
    line_opacity = 1
  ) |>
  
  # ==========================================================
# LEGEND
# ==========================================================

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
  
  # ==========================================================
# LAYER CONTROL
#
# Light view:
# Dark OFF + Satellite OFF
#
# Satellite view activates:
# imagery + geographic labels
# ==========================================================

add_layers_control(
  position = "top-right",
  
  layers = list(
    "Dark View" =
      "dark-basemap",
    
    "Satellite View" = c(
      "satellite-basemap",
      "satellite-labels"
    ),
    
    "Ovarian Cancer Incidence" =
      "ovarian-incidence",
    
    "County Boundaries" =
      "county-boundaries",
    
    "Portage County" = c(
      "portage-fill",
      "portage-highlight"
    )
  ),
  
  collapsible = TRUE,
  use_icon = TRUE,
  
  background_color = "#ffffff",
  active_color = "#d9edf7",
  hover_color = "#eeeeee",
  active_text_color = "#111111",
  inactive_text_color = "#333333"
) |>
  
  # ==========================================================
# MAP CONTROLS
# ==========================================================

add_navigation_control(
  position = "bottom-right",
  show_compass = TRUE,
  show_zoom = TRUE
) |>
  
  add_fullscreen_control(
    position = "bottom-right"
  ) |>
  
  add_scale_control(
    position = "bottom-left"
  )

# ------------------------------------------------------------
# 7. DISPLAY MAP
# ------------------------------------------------------------

print(map_all_views)

# ------------------------------------------------------------
# 8. SAVE MAP
# ------------------------------------------------------------

dir.create(
  output_directory,
  recursive = TRUE,
  showWarnings = FALSE
)

saveWidget(
  widget = map_all_views,
  file = output_file,
  selfcontained = FALSE
)

message(
  "\nInteractive map saved to:\n",
  normalizePath(output_file)
)

# ------------------------------------------------------------
# 9. OPEN MAP
# ------------------------------------------------------------

browseURL(
  normalizePath(output_file)
)