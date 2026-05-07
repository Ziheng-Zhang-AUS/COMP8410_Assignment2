# ============================================================
# COMP8410 Assignment 2
# Script: 06_rq1_association_rules.R
# Purpose: Supplementary association rule mining for RQ1.
#          Uses key variables identified by the RQ1 decision trees
#          to find interpretable co-occurrence rules associated
#          with Q27_binary.
# ============================================================

source("scripts/00_setup.R")

library(tidyverse)
library(arules)
library(janitor)

# ------------------------------------------------------------
# 1. Load data
# ------------------------------------------------------------

data <- read.csv(
  data_path,
  stringsAsFactors = FALSE,
  na.strings = c("", " ", "NA")
)

cat("Rows:", nrow(data), "\n")
cat("Columns:", ncol(data), "\n")

# Convert blank / whitespace missing values to NA
data <- data %>%
  mutate(across(
    everything(),
    ~ ifelse(trimws(as.character(.x)) == "", NA, as.character(.x))
  ))

# Convert numeric-looking columns back to numeric where possible
data <- data %>%
  mutate(across(
    everything(),
    ~ type.convert(.x, as.is = TRUE)
  ))

# ------------------------------------------------------------
# 2. Recode Q27 into binary target
# ------------------------------------------------------------
# Q27:
# 1 = Very satisfied
# 2 = Fairly satisfied
# 3 = Not very satisfied
# 4 = Not at all satisfied

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

# ------------------------------------------------------------
# 3. Construct political knowledge score from Q47
# ------------------------------------------------------------
# Same construction as RQ1 decision tree scripts.
# Correct = 1, Incorrect / IDK / missing = 0.

knowledge_items <- c("Q47_1", "Q47_2", "Q47_3", "Q47_4", "Q47_5", "Q47_6")

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

# ------------------------------------------------------------
# 4. Prepare variables for association rule mining
# ------------------------------------------------------------
# Variables selected from:
# - Full RQ1 decision tree split variables:
#   Q28, Q26_1, Q3, Q15, Q7, Q32, Q33
# - Reduced RQ1 tree key variable:
#   Q25_2
#
# The goal is not to build another classifier.
# The goal is to express key decision-tree patterns as
# non-hierarchical association rules.

needed_vars <- c(
  "Q27_binary",
  "Q28",
  "Q26_1",
  "Q3",
  "Q15",
  "Q7",
  "Q32",
  "Q33",
  "Q25_2",
  "political_knowledge_score"
)

missing_vars <- setdiff(needed_vars, names(data))

if (length(missing_vars) > 0) {
  cat("\nWARNING: These variables were not found:\n")
  print(missing_vars)
  stop("Please check variable names before running association rule mining.")
}

rule_data <- data %>%
  select(all_of(needed_vars)) %>%
  filter(!is.na(Q27_binary))

# Convert numeric variables safely
numeric_vars <- c("Q26_1", "Q7", "Q25_2", "political_knowledge_score")

for (v in numeric_vars) {
  rule_data[[v]] <- as.numeric(rule_data[[v]])
}

# Median imputation for numeric variables, if any missing
for (v in numeric_vars) {
  med <- median(rule_data[[v]], na.rm = TRUE)
  rule_data[[v]][is.na(rule_data[[v]])] <- med
}

# Treat categorical/factor variables as character first
categorical_vars <- c("Q28", "Q3", "Q15", "Q32", "Q33")

for (v in categorical_vars) {
  rule_data[[v]] <- as.character(rule_data[[v]])
  rule_data[[v]][is.na(rule_data[[v]])] <- "Missing_or_not_asked"
}

# ------------------------------------------------------------
# 5. Discretise variables into interpretable items
# ------------------------------------------------------------
# These categories are inspired by the actual decision tree splits.
# They are intentionally simple to keep association rules interpretable.

rule_items <- rule_data %>%
  mutate(
    # Target item
    Q27_item = case_when(
      Q27_binary == "Satisfied" ~ "Q27=Satisfied",
      Q27_binary == "Dissatisfied" ~ "Q27=Dissatisfied",
      TRUE ~ NA_character_
    ),
    
    # Q28 was the root split in the full decision tree.
    # In the tree, Q28 = 1 formed the most dissatisfied branch.
    Q28_item = case_when(
      Q28 == "1" ~ "Q28=LowTrust",
      Q28 %in% c("2", "3", "4") ~ "Q28=NotLowTrust",
      TRUE ~ "Q28=Missing"
    ),
    
    # Q26_1 split at < 7 in the full decision tree.
    Q26_1_item = case_when(
      Q26_1 < 7 ~ "Q26_1=AlbaneseLowMid",
      Q26_1 >= 7 ~ "Q26_1=AlbaneseHigh",
      TRUE ~ "Q26_1=Missing"
    ),
    
    # Q3 was used as a categorical split in the full tree.
    # Keep it as broad categories rather than over-interpreting.
    Q3_item = case_when(
      Q3 == "1" ~ "Q3=HighCampaignInterest",
      Q3 == "2" ~ "Q3=SomeCampaignInterest",
      Q3 == "3" ~ "Q3=LowCampaignInterest",
      Q3 == "4" ~ "Q3=NoCampaignInterest",
      TRUE ~ "Q3=Missing"
    ),
    
    # Q15 was an important split.
    # Based on observed tree grouping:
    # Q15 = 2,96 appeared on the more satisfied branch in the full tree.
    # This label is deliberately descriptive rather than substantive.
    Q15_item = case_when(
      Q15 == "2" ~ "Q15=Group2",
      Q15 == "96" ~ "Q15=Group96",
      Q15 %in% c("1", "3", "4", "97") ~ "Q15=OtherListedOrNoPreference",
      Q15 == "Missing_or_not_asked" ~ "Q15=MissingOrNotAsked",
      TRUE ~ "Q15=OtherOrMissing"
    ),
    
    # Q7 split at < 3 in the full tree.
    Q7_item = case_when(
      Q7 < 3 ~ "Q7=LowSocialMediaAttention",
      Q7 >= 3 ~ "Q7=HigherSocialMediaAttention",
      TRUE ~ "Q7=Missing"
    ),
    
    # Q32 and Q33 are kept in simple category labels.
    # Their precise substantive direction should be interpreted using the data dictionary.
    # These labels are intentionally cautious.
    Q32_item = case_when(
      Q32 %in% c("1", "2") ~ "Q32=Group1_2",
      Q32 %in% c("3", "4", "5") ~ "Q32=Group3_4_5",
      TRUE ~ "Q32=Missing"
    ),
    
    Q33_item = case_when(
      Q33 %in% c("1", "2") ~ "Q33=Group1_2",
      Q33 %in% c("3", "4", "5") ~ "Q33=Group3_4_5",
      TRUE ~ "Q33=Missing"
    ),
    
    # Q25_2 was the main split in the reduced tree.
    # Reduced tree split: Q25_2 < 8 vs Q25_2 >= 8.
    Q25_2_item = case_when(
      Q25_2 < 8 ~ "Q25_2=LaborLowMid",
      Q25_2 >= 8 ~ "Q25_2=LaborHigh",
      TRUE ~ "Q25_2=Missing"
    ),
    
    # Political knowledge score: keep simple.
    # 0-1 is low because many respondents had 0 or 1.
    # 2+ indicates relatively higher factual knowledge in this sample.
    Knowledge_item = case_when(
      political_knowledge_score <= 1 ~ "Knowledge=Low",
      political_knowledge_score >= 2 ~ "Knowledge=Higher",
      TRUE ~ "Knowledge=Missing"
    )
  ) %>%
  select(
    Q27_item,
    Q28_item,
    Q26_1_item,
    Q3_item,
    Q15_item,
    Q7_item,
    Q32_item,
    Q33_item,
    Q25_2_item,
    Knowledge_item
  )

cat("\nAssociation rule item data preview:\n")
print(head(rule_items))

cat("\nItem frequency preview:\n")
print(summary(rule_items))

# ------------------------------------------------------------
# 6. Convert to transactions
# ------------------------------------------------------------

# arules expects a data frame of factors for transactions.
rule_items <- rule_items %>%
  mutate(across(everything(), as.factor))

transactions <- as(rule_items, "transactions")

cat("\nTransaction summary:\n")
print(summary(transactions))

# Save item frequency plot
png(
  file.path(figure_rq1_dir, "association_rules_item_frequency.png"),
  width = 1200,
  height = 800
)

itemFrequencyPlot(
  transactions,
  topN = 25,
  type = "absolute",
  main = "Top Association Rule Items"
)

dev.off()

# ------------------------------------------------------------
# 7. Run Apriori algorithm
# ------------------------------------------------------------
# We generate rules whose RHS is Q27=Satisfied or Q27=Dissatisfied.
# Thresholds are deliberately moderate:
# - support >= 0.05 means the full rule covers at least about 40 respondents.
# - confidence >= 0.65 keeps reasonably strong rules.
# - minlen = 2 ensures at least one antecedent and one consequent.
# - maxlen = 4 keeps rules interpretable.

rules_satisfied <- apriori(
  transactions,
  parameter = list(
    supp = 0.05,
    conf = 0.60,
    minlen = 2,
    maxlen = 4
  ),
  appearance = list(
    rhs = "Q27_item=Q27=Satisfied",
    default = "lhs"
  ),
  control = list(verbose = TRUE)
)

rules_dissatisfied <- apriori(
  transactions,
  parameter = list(
    supp = 0.05,
    conf = 0.70,
    minlen = 2,
    maxlen = 4
  ),
  appearance = list(
    rhs = "Q27_item=Q27=Dissatisfied",
    default = "lhs"
  ),
  control = list(verbose = TRUE)
)

cat("\nNumber of satisfied rules:", length(rules_satisfied), "\n")
cat("Number of dissatisfied rules:", length(rules_dissatisfied), "\n")

# ------------------------------------------------------------
# 8. Filter and sort rules
# ------------------------------------------------------------

# Remove redundant rules to avoid reporting overly repetitive patterns.
rules_satisfied_nr <- rules_satisfied[!is.redundant(rules_satisfied)]
rules_dissatisfied_nr <- rules_dissatisfied[!is.redundant(rules_dissatisfied)]

# Filter by lift > 1.1 for meaningful positive association.
rules_satisfied_strong <- subset(rules_satisfied_nr, subset = lift > 1.10)
rules_dissatisfied_strong <- subset(rules_dissatisfied_nr, subset = lift > 1.10)

rules_satisfied_sorted <- sort(rules_satisfied_strong, by = c("lift", "confidence", "support"), decreasing = TRUE)
rules_dissatisfied_sorted <- sort(rules_dissatisfied_strong, by = c("lift", "confidence", "support"), decreasing = TRUE)

cat("\nTop satisfied rules:\n")
inspect(head(rules_satisfied_sorted, 15))

cat("\nTop dissatisfied rules:\n")
inspect(head(rules_dissatisfied_sorted, 15))

# ------------------------------------------------------------
# 9. Convert rules to data frames and save
# ------------------------------------------------------------

rules_to_df <- function(rules_object, top_n = 30) {
  if (length(rules_object) == 0) {
    return(tibble())
  }
  
  rules_head <- head(rules_object, top_n)
  
  as(rules_head, "data.frame") %>%
    as_tibble() %>%
    arrange(desc(lift), desc(confidence), desc(support))
}

satisfied_rules_df <- rules_to_df(rules_satisfied_sorted, top_n = 30)
dissatisfied_rules_df <- rules_to_df(rules_dissatisfied_sorted, top_n = 30)

write.csv(
  satisfied_rules_df,
  file.path(output_rq1_dir, "association_rules_satisfied_top.csv"),
  row.names = FALSE
)

write.csv(
  dissatisfied_rules_df,
  file.path(output_rq1_dir, "association_rules_dissatisfied_top.csv"),
  row.names = FALSE
)

# Combine top rules for easier reporting
top_rules_combined <- bind_rows(
  satisfied_rules_df %>% mutate(target = "Satisfied"),
  dissatisfied_rules_df %>% mutate(target = "Dissatisfied")
) %>%
  select(target, rules, support, confidence, coverage, lift, count)

write.csv(
  top_rules_combined,
  file.path(output_rq1_dir, "association_rules_top_combined.csv"),
  row.names = FALSE
)

cat("\nCombined top rules:\n")
print(top_rules_combined)

# ------------------------------------------------------------
# 10. Create a simplified reporting table
# ------------------------------------------------------------
# This table is easier to read in the report / appendix.
# You can manually select 3-6 rules from this output for the main text.

report_rules <- top_rules_combined %>%
  group_by(target) %>%
  slice_max(order_by = lift, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(
    support = round(support, 3),
    confidence = round(confidence, 3),
    lift = round(lift, 3),
    coverage = round(coverage, 3)
  )

write.csv(
  report_rules,
  file.path(output_rq1_dir, "association_rules_report_table.csv"),
  row.names = FALSE
)

cat("\nSuggested report table rules:\n")
print(report_rules)

# ------------------------------------------------------------
# 11. Save basic explanation of baseline rates
# ------------------------------------------------------------

baseline_rates <- tibble(
  class = names(prop.table(table(rule_data$Q27_binary))),
  proportion = as.numeric(prop.table(table(rule_data$Q27_binary))),
  percent = round(100 * proportion, 2)
)

write.csv(
  baseline_rates,
  file.path(output_rq1_dir, "association_rules_q27_baseline_rates.csv"),
  row.names = FALSE
)

cat("\nQ27 baseline rates used to interpret lift:\n")
print(baseline_rates)

cat("\nSaved association rule outputs to:", output_rq1_dir, "\n")
cat("Saved association rule figures to:", figure_rq1_dir, "\n")