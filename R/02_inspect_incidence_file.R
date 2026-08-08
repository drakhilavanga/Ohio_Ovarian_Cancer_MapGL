library(readr)
library(tidyverse)

file_path <- "data/raw/ohio_ovarian_cancer_incidence_2018_2022.csv"

if (!file.exists(file_path)) {
  stop(
    "The incidence file was not found at: ",
    file_path
  )
}

# Display the first 30 raw lines exactly as they appear.
raw_lines <- read_lines(
  file_path,
  n_max = 30
)

cat(
  paste0(
    seq_along(raw_lines),
    ": ",
    raw_lines,
    collapse = "\n"
  )
)