# COMP8410 Assignment 2 Code

This repository contains the R code for COMP8410 Assignment 2.

The code is organised so that the tutor can reproduce the analysis.  
The original course data file is not included because of course data-use restrictions.

## Environment

This project was run with R 4.5.3.

RStudio is recommended, but not required. The code can also be run from Terminal with `Rscript`.

The working directory should be the project root folder. This is the folder that contains:

```text
README.md
run_all.r
scripts/
data/
outputs/
figures/
```

## Required R Packages

The scripts use these R packages:

```text
tidyverse 2.0.0
janitor 2.2.1
rpart 4.1.27
rpart.plot 3.1.4
caret 7.0-1
arules
cluster 2.1.8.2
factoextra 2.0.0
```

The setup script `scripts/00_setup.r` checks these packages and installs missing packages if needed.

If package installation fails, install them manually:

```r
install.packages(c(
  "tidyverse",
  "janitor",
  "rpart",
  "rpart.plot",
  "caret",
  "arules",
  "cluster",
  "factoextra"
))
```

## Folder Structure

```text
COMP8410_Assignment2_Code/
├── README.md
├── .gitignore
├── run_all.r
├── data/
│   ├── .gitkeep
│   └── README.md
├── scripts/
│   ├── 00_setup.r
│   ├── 01_data_quality_check.r
│   ├── 02_rq1_descriptive_summary.r
│   ├── 03_rq1_decision_tree_full_model.r
│   ├── 04_rq1_decision_tree_reduced_model.r
│   ├── 05_rq2_kmeans_clustering.r
│   └── 06_association_rules.r
├── outputs/
│   ├── rq1/
│   └── rq2/
└── figures/
    ├── rq1/
    └── rq2/
```

## Data

The original dataset is not uploaded to this repository because it is course-provided data subject to data-use restrictions.

To run the code, put the course-provided CSV file in the `data/` folder.

The expected path is:

```text
data/2_IndigenousVoters_2025_CSV_100297_GENERAL.csv
```

If the file name is different, edit this line in `scripts/00_setup.r`:

```r
data_path <- "data/2_IndigenousVoters_2025_CSV_100297_GENERAL.csv"
```

## How to Run

Open R or RStudio from the project root folder.

Then run:

```r
source("run_all.r")
```

This runs all scripts in order.

The same workflow can also be run from Terminal:

```bash
Rscript run_all.r
```

## Script Order

```text
01_data_quality_check.r
02_rq1_descriptive_summary.r
03_rq1_decision_tree_full_model.r
04_rq1_decision_tree_reduced_model.r
05_rq2_kmeans_clustering.r
06_association_rules.r
```

## Script Descriptions

`00_setup.r`  
Sets the seed, checks packages, defines paths, and creates output folders.

`01_data_quality_check.r`  
Checks missing values, special codes, and structural missingness in the original dataset.

`02_rq1_descriptive_summary.r`  
Creates descriptive summaries for RQ1, including the original Q27 distribution, the binary Q27 target, selected predictors, missingness, and the constructed political knowledge score.

`03_rq1_decision_tree_full_model.r`  
Fits the full decision tree model for RQ1. It runs manual grid search with cross-validation, evaluates the final model on the held-out test set, and saves model results, metrics, confusion matrices, variable importance, threshold analysis, and tree figures.

`04_rq1_decision_tree_reduced_model.r`  
Fits the reduced decision tree model for RQ1. This model excludes Q28, Q29, Q32, and Q33 to test whether the result depends heavily on variables conceptually close to Q27.

`05_rq2_kmeans_clustering.r`  
Runs k-means clustering for RQ2. It constructs and standardises clustering features, evaluates different values of k using elbow and silhouette results, creates cluster profiles, and compares clusters by Q27 satisfaction.

`06_association_rules.r`  
Runs supplementary association rule mining for RQ1 using variables identified by the decision tree results. It uses the Apriori algorithm to find high-confidence co-occurrence rules associated with `Q27_binary`, and saves rule tables with support, confidence, lift, and count.

## Outputs

RQ1 output tables are saved in:

```text
outputs/rq1/
```

RQ2 output tables are saved in:

```text
outputs/rq2/
```

RQ1 figures are saved in:

```text
figures/rq1/
```

RQ2 figures are saved in:

```text
figures/rq2/
```

Main RQ1 outputs include:

```text
outputs/rq1/full_model_grid_search_results.csv
outputs/rq1/full_model_best_params.csv
outputs/rq1/full_model_test_metrics.csv
outputs/rq1/full_model_confusion_matrix.csv
outputs/rq1/full_model_variable_importance.csv
outputs/rq1/reduced_model_test_metrics.csv
outputs/rq1/reduced_model_confusion_matrix.csv
outputs/rq1/association_rules_report_table.csv
```

Main RQ2 outputs include:

```text
outputs/rq2/elbow_results.csv
outputs/rq2/silhouette_results.csv
outputs/rq2/cluster_profiles_original_scales.csv
outputs/rq2/q27_by_cluster_wide.csv
```

Exact output file names may vary slightly depending on the current script version, but all generated tables are saved under `outputs/` and all generated figures are saved under `figures/`.

## Notes

The repository does not include raw data.

Some outputs with row-level predictions are not saved, because they may reveal information from the course dataset.

The main results can be reproduced by placing the original CSV file in `data/` and running `run_all.r`.
