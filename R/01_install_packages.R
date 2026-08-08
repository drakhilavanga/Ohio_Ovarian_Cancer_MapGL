packages <- c(
  "tidyverse",
  "readr",
  "dplyr",
  "stringr",
  "janitor",
  "sf",
  "tigris",
  "tidycensus",
  "mapgl",
  "htmltools",
  "scales",
  "ggplot2",
  "glue",
  "DT",
  "shiny",
  "bslib"
)

new_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(new_packages) > 0) {
  install.packages(
    new_packages,
    dependencies = TRUE
  )
}

message("All required packages are installed.")