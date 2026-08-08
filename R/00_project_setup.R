folders <- c(
  "R",
  "data/raw",
  "data/processed",
  "output/tables",
  "output/figures",
  "output/maps",
  "app",
  "www"
)

for (folder in folders) {
  dir.create(
    folder,
    recursive = TRUE,
    showWarnings = FALSE
  )
}

message("Project folders created successfully.")