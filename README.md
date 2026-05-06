# COMP8410 Assignment 2 Code

This repository contains the R code for COMP8410 Assignment 2.

The code is organised so that the tutor can reproduce the analysis.  
The original course data file is not included because of course data-use restrictions.

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
│   └── 05_rq2_kmeans_clustering.r
├── outputs/
│   ├── rq1/
│   └── rq2/
└── figures/
    ├── rq1/
    └── rq2/
```

## Data

The original dataset is not uploaded to this repository due to data privacy issue.

To run the code, put the course-provided CSV file in the `data/` folder.

The expected path is:

```text
data/2_IndigenousVoters_2025_CSV_100297_GENERAL.csv
```

If the file name is different, edit this line in `scripts/00_setup.r`:

```r
data_path <- "data/2_IndigenousVoters_2025_CSV_100297_GENERAL.csv"
```

## Required R Packages

The code uses these R packages:

```r
tidyverse
janitor
rpart
rpart.plot
caret
cluster
factoextra
```

The setup script checks these packages and installs missing ones if needed.

## How to Run

Open R or RStudio from the project root folder.

Then run:

```r
source("run_all.r")
```

This runs all scripts in order.

## Script Order

```text
01_data_quality_check.r
02_rq1_descriptive_summary.r
03_rq1_decision_tree_full_model.r
04_rq1_decision_tree_reduced_model.r
05_rq2_kmeans_clustering.r
```

## Script Descriptions

`00_setup.r`  
Sets the seed, loads packages, defines paths, and creates output folders.

`01_data_quality_check.r`  
Checks missing values, special codes, and structural missingness.

`02_rq1_descriptive_summary.r`  
Creates descriptive summaries for RQ1 and the Q27 target variable.

`03_rq1_decision_tree_full_model.r`  
Fits the full decision tree model for RQ1. It also runs grid search and saves model results.

`04_rq1_decision_tree_reduced_model.r`  
Fits the reduced decision tree model for RQ1. This model excludes Q28, Q29, Q32, and Q33.

`05_rq2_kmeans_clustering.r`  
Runs K-means clustering for RQ2. It also compares clusters by Q27 satisfaction.

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

## Notes

The repository does not include raw data.

Some outputs with row-level predictions are not saved, because they may reveal information from the course dataset.

The main results can be reproduced by placing the original CSV file in `data/` and running `run_all.r`.
