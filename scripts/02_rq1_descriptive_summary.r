# ============================================================
# COMP8410 Assignment 2
# Script: 02_rq1_descriptive_summary.R
# Purpose: Produce RQ1 descriptive summaries for the Q27 target,
#          selected predictors, political knowledge score, missingness,
#          frequency tables, cross-tabulations, and complete-case count.
# ============================================================

source("scripts/00_setup.R")

library(tidyverse)
library(janitor)

data <- read.csv(
  data_path,
  stringsAsFactors = FALSE,
  na.strings = c("", " ", "NA")
)

cat("Number of rows:", nrow(data), "\n")
cat("Number of columns:", ncol(data), "\n")

data <- data %>%
  mutate(across(
    everything(),
    ~ ifelse(trimws(as.character(.x)) == "", NA, as.character(.x))
  ))

data <- data %>%
  mutate(across(
    everything(),
    ~ type.convert(.x, as.is = TRUE)
  ))

target_var <- "Q27"

political_engagement <- c("Q1", "Q3", "Q4_1", "Q7")
party_voting <- c("Q8", "Q10", "Q13", "Q15", "Q21")
party_leader_eval <- c("Q25_2", "Q25_4", "Q26_1", "Q26_4")
trust_efficacy <- c("Q28", "Q29", "Q32", "Q33")
knowledge_items <- c("Q47_1", "Q47_2", "Q47_3", "Q47_4", "Q47_5", "Q47_6")
social_background <- c("age_group", "gender", "location", "education")

rq1_predictors <- c(
  political_engagement,
  party_voting,
  party_leader_eval,
  trust_efficacy,
  knowledge_items,
  social_background
)

all_rq1_vars <- c(target_var, rq1_predictors)
missing_vars <- setdiff(all_rq1_vars, names(data))

if (length(missing_vars) > 0) {
  cat("\nWARNING: These variables were not found in the dataset:\n")
  print(missing_vars)
  cat("\nPlease check the data dictionary and update variable names.\n")
}

existing_rq1_vars <- intersect(all_rq1_vars, names(data))

cat("\n============================\n")
cat("Q27 original distribution\n")
cat("============================\n")

q27_original <- data %>%
  count(.data[[target_var]], name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  arrange(.data[[target_var]])

print(q27_original)

write.csv(
  q27_original,
  file.path(output_rq1_dir, "rq1_q27_original_distribution.csv"),
  row.names = FALSE
)

data <- data %>%
  mutate(
    Q27_binary = case_when(
      Q27 %in% c(1, "1", "Very satisfied", "very satisfied") ~ "Satisfied",
      Q27 %in% c(2, "2", "Fairly satisfied", "fairly satisfied") ~ "Satisfied",
      Q27 %in% c(3, "3", "Not very satisfied", "not very satisfied") ~ "Dissatisfied",
      Q27 %in% c(4, "4", "Not at all satisfied", "not at all satisfied") ~ "Dissatisfied",
      TRUE ~ NA_character_
    ),
    Q27_binary = factor(Q27_binary, levels = c("Satisfied", "Dissatisfied"))
  )

cat("\n============================\n")
cat("Q27 binary distribution\n")
cat("============================\n")

q27_binary_dist <- data %>%
  count(Q27_binary, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 2))

print(q27_binary_dist)

write.csv(
  q27_binary_dist,
  file.path(output_rq1_dir, "rq1_q27_binary_distribution.csv"),
  row.names = FALSE
)

q27_non_missing <- data %>%
  filter(!is.na(Q27_binary))

majority_baseline <- q27_non_missing %>%
  count(Q27_binary, name = "n") %>%
  mutate(percent = n / sum(n)) %>%
  arrange(desc(n)) %>%
  slice(1)

cat("\nMajority-class baseline accuracy:\n")
print(majority_baseline)

write.csv(
  majority_baseline,
  file.path(output_rq1_dir, "rq1_majority_class_baseline.csv"),
  row.names = FALSE
)

correct_answers <- list(
  Q47_1 = c(1, "1", "True", "true", "Yes", "yes"),
  Q47_2 = c(2, "2", "False", "false", "No", "no"),
  Q47_3 = c(2, "2", "False", "false", "No", "no"),
  Q47_4 = c(1, "1", "True", "true", "Yes", "yes"),
  Q47_5 = c(1, "1", "True", "true", "Yes", "yes"),
  Q47_6 = c(2, "2", "False", "false", "No", "no")
)

for (item in knowledge_items) {
  if (item %in% names(data)) {
    new_col <- paste0(item, "_correct")
    data[[new_col]] <- ifelse(data[[item]] %in% correct_answers[[item]], 1, 0)
    data[[new_col]][is.na(data[[item]])] <- NA
  }
}

knowledge_correct_cols <- paste0(knowledge_items, "_correct")
knowledge_correct_cols <- intersect(knowledge_correct_cols, names(data))

data <- data %>%
  mutate(
    political_knowledge_score = ifelse(
      rowSums(is.na(across(all_of(knowledge_correct_cols)))) > 0,
      NA,
      rowSums(across(all_of(knowledge_correct_cols)), na.rm = FALSE)
    )
  )

cat("\n============================\n")
cat("Political knowledge score summary\n")
cat("============================\n")

knowledge_summary <- data %>%
  summarise(
    n_non_missing = sum(!is.na(political_knowledge_score)),
    n_missing = sum(is.na(political_knowledge_score)),
    mean = mean(political_knowledge_score, na.rm = TRUE),
    median = median(political_knowledge_score, na.rm = TRUE),
    sd = sd(political_knowledge_score, na.rm = TRUE),
    min = min(political_knowledge_score, na.rm = TRUE),
    max = max(political_knowledge_score, na.rm = TRUE)
  )

print(knowledge_summary)

knowledge_dist <- data %>%
  count(political_knowledge_score, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 2)) %>%
  arrange(political_knowledge_score)

print(knowledge_dist)

write.csv(
  knowledge_summary,
  file.path(output_rq1_dir, "rq1_political_knowledge_score_summary.csv"),
  row.names = FALSE
)

write.csv(
  knowledge_dist,
  file.path(output_rq1_dir, "rq1_political_knowledge_score_distribution.csv"),
  row.names = FALSE
)

rq1_predictors_constructed <- c(
  political_engagement,
  party_voting,
  party_leader_eval,
  trust_efficacy,
  "political_knowledge_score",
  social_background
)

rq1_predictors_constructed <- intersect(rq1_predictors_constructed, names(data))
rq1_analysis_vars <- c("Q27_binary", rq1_predictors_constructed)

missing_summary <- data %>%
  summarise(across(
    all_of(rq1_analysis_vars),
    ~ sum(is.na(.x))
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable",
    values_to = "n_missing"
  ) %>%
  mutate(
    n_total = nrow(data),
    missing_percent = round(100 * n_missing / n_total, 2)
  ) %>%
  arrange(desc(missing_percent))

cat("\n============================\n")
cat("Missingness summary for RQ1 variables\n")
cat("============================\n")

print(missing_summary)

write.csv(
  missing_summary,
  file.path(output_rq1_dir, "rq1_missingness_summary.csv"),
  row.names = FALSE
)

possible_numeric_vars <- c(
  "Q7", "Q13",
  "Q25_2", "Q25_4",
  "Q26_1", "Q26_4",
  "political_knowledge_score"
)

numeric_vars <- intersect(possible_numeric_vars, names(data))

numeric_summary <- data %>%
  summarise(across(
    all_of(numeric_vars),
    list(
      n_non_missing = ~ sum(!is.na(.x)),
      mean = ~ mean(as.numeric(.x), na.rm = TRUE),
      median = ~ median(as.numeric(.x), na.rm = TRUE),
      sd = ~ sd(as.numeric(.x), na.rm = TRUE),
      min = ~ min(as.numeric(.x), na.rm = TRUE),
      max = ~ max(as.numeric(.x), na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  pivot_longer(
    cols = everything(),
    names_to = "variable_stat",
    values_to = "value"
  )

cat("\n============================\n")
cat("Numeric / score-like variable summary\n")
cat("============================\n")

print(numeric_summary)

write.csv(
  numeric_summary,
  file.path(output_rq1_dir, "rq1_numeric_summary.csv"),
  row.names = FALSE
)

categorical_vars <- setdiff(rq1_predictors_constructed, numeric_vars)
frequency_tables <- list()

for (var in categorical_vars) {
  if (var %in% names(data)) {
    tab <- data %>%
      count(.data[[var]], name = "n") %>%
      mutate(
        variable = var,
        percent = round(100 * n / sum(n), 2)
      ) %>%
      rename(value = all_of(var)) %>%
      select(variable, value, n, percent)

    frequency_tables[[var]] <- tab
  }
}

all_frequency_tables <- bind_rows(frequency_tables)

cat("\n============================\n")
cat("Frequency tables for categorical predictors\n")
cat("============================\n")

print(all_frequency_tables)

write.csv(
  all_frequency_tables,
  file.path(output_rq1_dir, "rq1_categorical_frequency_tables.csv"),
  row.names = FALSE
)

cross_tabs <- list()

for (var in categorical_vars) {
  if (var %in% names(data)) {
    tab <- data %>%
      filter(!is.na(Q27_binary)) %>%
      count(Q27_binary, .data[[var]], name = "n") %>%
      group_by(Q27_binary) %>%
      mutate(row_percent = round(100 * n / sum(n), 2)) %>%
      ungroup() %>%
      rename(value = all_of(var)) %>%
      mutate(variable = var) %>%
      select(variable, Q27_binary, value, n, row_percent)

    cross_tabs[[var]] <- tab
  }
}

all_cross_tabs <- bind_rows(cross_tabs)

cat("\n============================\n")
cat("Cross-tabs: categorical predictors by Q27_binary\n")
cat("============================\n")

print(all_cross_tabs)

write.csv(
  all_cross_tabs,
  file.path(output_rq1_dir, "rq1_categorical_by_q27_binary.csv"),
  row.names = FALSE
)

numeric_by_target <- data %>%
  filter(!is.na(Q27_binary)) %>%
  group_by(Q27_binary) %>%
  summarise(across(
    all_of(numeric_vars),
    list(
      n_non_missing = ~ sum(!is.na(.x)),
      mean = ~ mean(as.numeric(.x), na.rm = TRUE),
      median = ~ median(as.numeric(.x), na.rm = TRUE),
      sd = ~ sd(as.numeric(.x), na.rm = TRUE)
    ),
    .names = "{.col}_{.fn}"
  )) %>%
  ungroup()

cat("\n============================\n")
cat("Numeric predictors by Q27_binary\n")
cat("============================\n")

print(numeric_by_target)

write.csv(
  numeric_by_target,
  file.path(output_rq1_dir, "rq1_numeric_by_q27_binary.csv"),
  row.names = FALSE
)

rq1_complete_cases <- data %>%
  select(all_of(rq1_analysis_vars)) %>%
  drop_na()

cat("\n============================\n")
cat("RQ1 complete-case sample size\n")
cat("============================\n")

cat("Complete cases for Q27_binary + selected predictors:", nrow(rq1_complete_cases), "\n")
cat("Original rows:", nrow(data), "\n")
cat("Retention rate:", round(100 * nrow(rq1_complete_cases) / nrow(data), 2), "%\n")

complete_case_summary <- tibble(
  original_rows = nrow(data),
  complete_case_rows = nrow(rq1_complete_cases),
  retention_percent = round(100 * nrow(rq1_complete_cases) / nrow(data), 2)
)

write.csv(
  complete_case_summary,
  file.path(output_rq1_dir, "rq1_complete_case_summary.csv"),
  row.names = FALSE
)

cat("\nSaved RQ1 descriptive summary outputs to:", output_rq1_dir, "\n")