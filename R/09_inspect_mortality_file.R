# ============================================================
# INSPECT OHIO OVARIAN CANCER MORTALITY EXPORT
# ============================================================

library(readr)

input_file <- file.path(
  "data",
  "raw",
  "ohio_ovarian_cancer_mortality.csv"
)

if (!file.exists(input_file)) {
  stop(
    "Mortality file not found:\n",
    input_file
  )
}

raw_lines <- read_lines(
  input_file,
  n_max = 35
)

cat(
  paste0(
    seq_along(raw_lines),
    ": ",
    raw_lines,
    collapse = "\n"
  )
)