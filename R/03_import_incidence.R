library(tidyverse)
library(janitor)
library(stringr)

file_path <- "data/raw/ohio_ovarian_cancer_incidence_2018_2022.csv"

if (!file.exists(file_path)) {
  stop("Incidence file is missing.")
}

# First import attempt.
incidence_raw <- read_csv(
  file_path,
  show_col_types = FALSE,
  name_repair = "unique"
)

cat("\nImported rows:", nrow(incidence_raw), "\n")
cat("Imported columns:", ncol(incidence_raw), "\n\n")

print(names(incidence_raw))
print(head(incidence_raw, 10))