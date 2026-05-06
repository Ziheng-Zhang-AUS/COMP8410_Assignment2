# ============================================================
# COMP8410 Assignment 2
# Script: 03_rq1_decision_tree_full_model.R
# Purpose: Fit the full RQ1 decision tree model with manual grid search,
#          evaluate it on a held-out test set, and save model summaries.
# ============================================================

source("scripts/00_setup.R")

library(tidyverse)
library(rpart)
library(rpart.plot)
library(caret)
library(janitor)

data <- read.csv(
  data_path,
  stringsAsFactors = FALSE,
  na.strings = c("", " ", "NA")
)

cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")

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
social_background <- c()

final_predictors_raw <- c(
  political_engagement,
  party_voting,
  party_leader_eval,
  trust_efficacy,
  knowledge_items,
  social_background
)

needed_vars <- c(target_var, final_predictors_raw)
missing_vars <- setdiff(needed_vars, names(data))

if (length(missing_vars) > 0) {
  cat("\nWARNING: These variables were not found in the dataset:\n")
  print(missing_vars)
  cat("\nThe script will continue using variables that exist.\n")
}

data <- data %>%
  mutate(
    Q27_binary = case_when(
      Q27 %in% c(1, "1", "Very satisfied", "very satisfied") ~ "Satisfied",
      Q27 %in% c(2, "2", "Fairly satisfied", "fairly satisfied") ~ "Satisfied",
      Q27 %in% c(3, "3", "Not very satisfied", "not very satisfied") ~ "Dissatisfied",
      Q27 %in% c(4, "4", "Not at all satisfied", "not at all satisfied") ~ "Dissatisfied",
      TRUE ~ NA_character_
    )
  )

data$Q27_binary <- factor(
  data$Q27_binary,
  levels = c("Dissatisfied", "Satisfied")
)

cat("\nQ27 binary distribution:\n")
print(table(data$Q27_binary, useNA = "ifany"))
print(round(prop.table(table(data$Q27_binary)) * 100, 2))

majority_baseline <- max(prop.table(table(data$Q27_binary)))
cat("\nMajority-class baseline accuracy:", round(majority_baseline, 4), "\n")

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

    data[[new_col]] <- ifelse(
      data[[item]] %in% correct_answers[[item]],
      1,
      0
    )

    data[[new_col]][is.na(data[[item]])] <- 0
  }
}

knowledge_correct_cols <- paste0(knowledge_items, "_correct")
knowledge_correct_cols <- intersect(knowledge_correct_cols, names(data))

data <- data %>%
  mutate(
    political_knowledge_score = rowSums(
      across(all_of(knowledge_correct_cols)),
      na.rm = TRUE
    )
  )

cat("\nPolitical knowledge score distribution:\n")
print(table(data$political_knowledge_score, useNA = "ifany"))

final_predictors <- c(
  political_engagement,
  party_voting,
  party_leader_eval,
  trust_efficacy,
  "political_knowledge_score",
  social_background
)

final_predictors <- intersect(final_predictors, names(data))

model_data <- data %>%
  select(Q27_binary, all_of(final_predictors)) %>%
  filter(!is.na(Q27_binary))

cat("\nFinal predictors used:\n")
print(final_predictors)

cat("\nInitial model data dimensions:\n")
print(dim(model_data))

numeric_vars <- c(
  "Q7",
  "Q13",
  "Q25_2",
  "Q25_4",
  "Q26_1",
  "Q26_4",
  "political_knowledge_score"
)

numeric_vars <- intersect(numeric_vars, names(model_data))
categorical_vars <- setdiff(final_predictors, numeric_vars)

model_data <- model_data %>%
  mutate(across(
    all_of(numeric_vars),
    ~ as.numeric(.x)
  ))

for (v in numeric_vars) {
  med <- median(model_data[[v]], na.rm = TRUE)
  model_data[[v]][is.na(model_data[[v]])] <- med
}

for (v in categorical_vars) {
  model_data[[v]] <- as.character(model_data[[v]])
  model_data[[v]][is.na(model_data[[v]])] <- "Missing_or_not_asked"
  model_data[[v]] <- factor(model_data[[v]])
}

cat("\nMissing values after preprocessing:\n")
print(colSums(is.na(model_data)))

set.seed(8410)

train_index <- createDataPartition(
  model_data$Q27_binary,
  p = 0.70,
  list = FALSE
)

train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

cat("\nTraining data dimensions:\n")
print(dim(train_data))

cat("\nTest data dimensions:\n")
print(dim(test_data))

cat("\nTraining class distribution:\n")
print(prop.table(table(train_data$Q27_binary)))

cat("\nTest class distribution:\n")
print(prop.table(table(test_data$Q27_binary)))

evaluate_tree <- function(model, newdata, actual, positive_class = "Satisfied") {
  pred_class <- predict(model, newdata = newdata, type = "class")

  cm <- confusionMatrix(
    pred_class,
    actual,
    positive = positive_class
  )

  tibble(
    accuracy = as.numeric(cm$overall["Accuracy"]),
    kappa = as.numeric(cm$overall["Kappa"]),
    balanced_accuracy = as.numeric(cm$byClass["Balanced Accuracy"]),
    precision_satisfied = as.numeric(cm$byClass["Precision"]),
    recall_satisfied = as.numeric(cm$byClass["Recall"]),
    f1_satisfied = as.numeric(cm$byClass["F1"]),
    specificity = as.numeric(cm$byClass["Specificity"])
  )
}

param_grid <- expand.grid(
  cp = c(0.0005, 0.001, 0.002, 0.005, 0.01, 0.02),
  minsplit = c(30, 40, 60, 80),
  minbucket = c(10, 20, 30, 40),
  maxdepth = c(3, 4, 5, 6)
)

param_grid <- param_grid %>%
  filter(minbucket <= minsplit / 2)

cat("\nNumber of parameter combinations:", nrow(param_grid), "\n")

set.seed(8410)

folds <- createFolds(
  train_data$Q27_binary,
  k = 10,
  returnTrain = FALSE
)

grid_results <- list()

for (i in seq_len(nrow(param_grid))) {
  params <- param_grid[i, ]
  fold_metrics <- list()

  for (fold_id in seq_along(folds)) {
    val_idx <- folds[[fold_id]]

    cv_train <- train_data[-val_idx, ]
    cv_val <- train_data[val_idx, ]

    tree_cv <- rpart(
      Q27_binary ~ .,
      data = cv_train,
      method = "class",
      parms = list(split = "gini"),
      control = rpart.control(
        cp = params$cp,
        minsplit = params$minsplit,
        minbucket = params$minbucket,
        maxdepth = params$maxdepth,
        xval = 0
      )
    )

    metrics <- evaluate_tree(
      model = tree_cv,
      newdata = cv_val,
      actual = cv_val$Q27_binary,
      positive_class = "Satisfied"
    )

    fold_metrics[[fold_id]] <- metrics
  }

  avg_metrics <- bind_rows(fold_metrics) %>%
    summarise(
      accuracy = mean(accuracy, na.rm = TRUE),
      kappa = mean(kappa, na.rm = TRUE),
      balanced_accuracy = mean(balanced_accuracy, na.rm = TRUE),
      precision_satisfied = mean(precision_satisfied, na.rm = TRUE),
      recall_satisfied = mean(recall_satisfied, na.rm = TRUE),
      f1_satisfied = mean(f1_satisfied, na.rm = TRUE),
      specificity = mean(specificity, na.rm = TRUE)
    )

  grid_results[[i]] <- bind_cols(params, avg_metrics)

  if (i %% 20 == 0) {
    cat("Completed", i, "of", nrow(param_grid), "parameter combinations\n")
  }
}

grid_results_df <- bind_rows(grid_results)

write.csv(
  grid_results_df,
  file.path(output_rq1_dir, "full_model_grid_search_results.csv"),
  row.names = FALSE
)

cat("\nTop 10 parameter settings by balanced accuracy:\n")
top_by_balanced <- grid_results_df %>%
  arrange(desc(balanced_accuracy), desc(f1_satisfied), desc(accuracy)) %>%
  head(10)

print(top_by_balanced)

cat("\nTop 10 parameter settings by F1 for Satisfied:\n")
top_by_f1 <- grid_results_df %>%
  arrange(desc(f1_satisfied), desc(balanced_accuracy), desc(accuracy)) %>%
  head(10)

print(top_by_f1)

best_params <- grid_results_df %>%
  arrange(desc(balanced_accuracy), desc(f1_satisfied), desc(accuracy)) %>%
  slice(1)

cat("\nSelected best parameters:\n")
print(best_params)

write.csv(
  best_params,
  file.path(output_rq1_dir, "full_model_best_params.csv"),
  row.names = FALSE
)

final_tree <- rpart(
  Q27_binary ~ .,
  data = train_data,
  method = "class",
  parms = list(split = "gini"),
  control = rpart.control(
    cp = best_params$cp,
    minsplit = best_params$minsplit,
    minbucket = best_params$minbucket,
    maxdepth = best_params$maxdepth,
    xval = 10
  )
)

cat("\nFinal tree summary:\n")
print(final_tree)

cat("\nFinal tree CP table:\n")
printcp(final_tree)

best_cp_internal <- final_tree$cptable[
  which.min(final_tree$cptable[, "xerror"]),
  "CP"
]

cat("\nInternal best cp from final tree:", best_cp_internal, "\n")

final_tree_pruned <- prune(final_tree, cp = best_cp_internal)

cat("\nPruned final tree summary:\n")
print(final_tree_pruned)

pred_class <- predict(
  final_tree_pruned,
  newdata = test_data,
  type = "class"
)

pred_prob <- predict(
  final_tree_pruned,
  newdata = test_data,
  type = "prob"
)

conf_mat <- confusionMatrix(
  pred_class,
  test_data$Q27_binary,
  positive = "Satisfied"
)

cat("\nTest-set confusion matrix and evaluation:\n")
print(conf_mat)

test_metrics <- tibble(
  metric = c(
    "Accuracy",
    "Majority baseline accuracy",
    "Balanced accuracy",
    "Precision for Satisfied",
    "Recall for Satisfied",
    "F1 for Satisfied",
    "Kappa"
  ),
  value = c(
    as.numeric(conf_mat$overall["Accuracy"]),
    as.numeric(majority_baseline),
    as.numeric(conf_mat$byClass["Balanced Accuracy"]),
    as.numeric(conf_mat$byClass["Precision"]),
    as.numeric(conf_mat$byClass["Recall"]),
    as.numeric(conf_mat$byClass["F1"]),
    as.numeric(conf_mat$overall["Kappa"])
  )
)

cat("\nSelected test metrics:\n")
print(test_metrics)

write.csv(
  test_metrics,
  file.path(output_rq1_dir, "full_model_test_metrics.csv"),
  row.names = FALSE
)

confusion_table <- as.data.frame(conf_mat$table)

write.csv(
  confusion_table,
  file.path(output_rq1_dir, "full_model_confusion_matrix.csv"),
  row.names = FALSE
)

var_importance <- final_tree_pruned$variable.importance

if (!is.null(var_importance)) {
  var_importance_df <- tibble(
    variable = names(var_importance),
    importance = as.numeric(var_importance)
  ) %>%
    arrange(desc(importance))

  cat("\nVariable importance from pruned final tree:\n")
  print(var_importance_df)

  write.csv(
    var_importance_df,
    file.path(output_rq1_dir, "full_model_variable_importance.csv"),
    row.names = FALSE
  )
} else {
  cat("\nNo variable importance available. The tree may have no split.\n")
}

png(
  file.path(figure_rq1_dir, "full_model_final_tree.png"),
  width = 1500,
  height = 950
)

rpart.plot(
  final_tree_pruned,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Full Decision Tree for Democratic Satisfaction"
)

dev.off()

png(
  file.path(figure_rq1_dir, "full_model_unpruned_selected_tree.png"),
  width = 1500,
  height = 950
)

rpart.plot(
  final_tree,
  type = 2,
  extra = 104,
  fallen.leaves = TRUE,
  main = "Unpruned Full Decision Tree"
)

dev.off()

prediction_summary <- tibble(
  actual = test_data$Q27_binary,
  predicted_class = pred_class,
  prob_dissatisfied = pred_prob[, "Dissatisfied"],
  prob_satisfied = pred_prob[, "Satisfied"]
)

prediction_summary <- prediction_summary %>%
  group_by(actual, predicted_class) %>%
  summarise(
    n = n(),
    mean_prob_satisfied = mean(prob_satisfied),
    mean_prob_dissatisfied = mean(prob_dissatisfied),
    .groups = "drop"
  )

write.csv(
  prediction_summary,
  file.path(output_rq1_dir, "full_model_prediction_summary.csv"),
  row.names = FALSE
)

thresholds <- seq(0.30, 0.70, by = 0.05)

threshold_results <- lapply(thresholds, function(th) {
  pred_th <- factor(
    ifelse(pred_prob[, "Satisfied"] >= th, "Satisfied", "Dissatisfied"),
    levels = c("Dissatisfied", "Satisfied")
  )

  cm_th <- confusionMatrix(
    pred_th,
    test_data$Q27_binary,
    positive = "Satisfied"
  )

  tibble(
    threshold = th,
    accuracy = as.numeric(cm_th$overall["Accuracy"]),
    balanced_accuracy = as.numeric(cm_th$byClass["Balanced Accuracy"]),
    precision_satisfied = as.numeric(cm_th$byClass["Precision"]),
    recall_satisfied = as.numeric(cm_th$byClass["Recall"]),
    f1_satisfied = as.numeric(cm_th$byClass["F1"])
  )
}) %>%
  bind_rows()

cat("\nThreshold analysis:\n")
print(threshold_results)

write.csv(
  threshold_results,
  file.path(output_rq1_dir, "full_model_threshold_analysis.csv"),
  row.names = FALSE
)

cat("\nSaved full decision tree outputs to:", output_rq1_dir, "\n")
cat("Saved full decision tree figures to:", figure_rq1_dir, "\n")