# ============================================================
# COMP8410 Assignment 2
# Shared setup
# ============================================================

set.seed(8410)

required_packages <- c(
  "tidyverse",
  "janitor",
  "rpart",
  "rpart.plot",
  "caret",
  "cluster",
  "factoextra",
  "arules"
)

installed_packages <- rownames(installed.packages())

for (pkg in required_packages) {
  if (!(pkg %in% installed_packages)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

data_path <- "data/2_IndigenousVoters_2025_CSV_100297_GENERAL.csv"

output_rq1_dir <- "outputs/rq1"
output_rq2_dir <- "outputs/rq2"

figure_rq1_dir <- "figures/rq1"
figure_rq2_dir <- "figures/rq2"

dir.create(output_rq1_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(output_rq2_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_rq1_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(figure_rq2_dir, recursive = TRUE, showWarnings = FALSE)

if (!file.exists(data_path)) {
  stop(
    paste0(
      "Data file not found at: ", data_path, "\n",
      "Place the course-provided CSV file in data/, or update data_path in scripts/00_setup.R."
    )
  )
}