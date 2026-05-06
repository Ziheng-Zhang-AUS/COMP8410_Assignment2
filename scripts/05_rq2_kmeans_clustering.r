# ============================================================
# COMP8410 Assignment 2
# Script: 05_rq2_kmeans_clustering.R
# Purpose: Identify political engagement and attitude profiles using
#          K-means clustering, then compare clusters by Q27 satisfaction.
# ============================================================

source("scripts/00_setup.R")

library(tidyverse)
library(cluster)
library(factoextra)
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

data <- data %>%
  mutate(
    Q27_binary = case_when(
      Q27 %in% c(1, "1", "Very satisfied", "very satisfied") ~ "Satisfied",
      Q27 %in% c(2, "2", "Fairly satisfied", "fairly satisfied") ~ "Satisfied",
      Q27 %in% c(3, "3", "Not very satisfied", "not very satisfied") ~ "Dissatisfied",
      Q27 %in% c(4, "4", "Not at all satisfied", "not at all satisfied") ~ "Dissatisfied",
      TRUE ~ NA_character_
    ),
    Q27_binary = factor(Q27_binary, levels = c("Dissatisfied", "Satisfied"))
  )

cat("\nQ27_binary distribution, for external interpretation only:\n")
print(table(data$Q27_binary, useNA = "ifany"))

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

required_vars <- c(
  "Q1", "Q3", "Q4_1", "Q7",
  "Q28", "Q29", "Q32", "Q33",
  "Q25_2", "Q25_4", "Q26_1", "Q26_4",
  "political_knowledge_score",
  "Q27_binary"
)

missing_vars <- setdiff(required_vars, names(data))

if (length(missing_vars) > 0) {
  cat("\nWARNING: These required variables were not found:\n")
  print(missing_vars)
  cat("\nPlease check names before interpreting results.\n")
}

rq2_data <- data %>%
  transmute(
    respondent_id = row_number(),
    political_interest = 5 - as.numeric(Q1),
    campaign_interest = 5 - as.numeric(Q3),
    political_discussion = 5 - as.numeric(Q4_1),
    social_media_attention_days = as.numeric(Q7) - 1,
    government_trust = as.numeric(Q28),
    government_for_all = as.numeric(Q29),
    power_makes_difference = 6 - as.numeric(Q32),
    vote_makes_difference = 6 - as.numeric(Q33),
    labor_rating = as.numeric(Q25_2),
    greens_rating = as.numeric(Q25_4),
    albanese_rating = as.numeric(Q26_1),
    bandt_rating = as.numeric(Q26_4),
    political_knowledge_score = as.numeric(political_knowledge_score),
    Q27_binary = Q27_binary
  )

cat("\nRQ2 data dimensions before missing handling:\n")
print(dim(rq2_data))

cat("\nMissing values in RQ2 variables before imputation:\n")
print(colSums(is.na(rq2_data)))

cluster_vars <- c(
  "political_interest",
  "campaign_interest",
  "political_discussion",
  "social_media_attention_days",
  "government_trust",
  "government_for_all",
  "power_makes_difference",
  "vote_makes_difference",
  "labor_rating",
  "greens_rating",
  "albanese_rating",
  "bandt_rating",
  "political_knowledge_score"
)

for (v in cluster_vars) {
  med <- median(rq2_data[[v]], na.rm = TRUE)
  rq2_data[[v]][is.na(rq2_data[[v]])] <- med
}

cat("\nMissing values in RQ2 variables after imputation:\n")
print(colSums(is.na(rq2_data)))

cluster_matrix_raw <- rq2_data %>%
  select(all_of(cluster_vars))

cluster_matrix_scaled <- scale(cluster_matrix_raw)

cat("\nScaled clustering matrix dimensions:\n")
print(dim(cluster_matrix_scaled))

k_values <- 2:8

wss <- map_dbl(k_values, function(k) {
  set.seed(8410)
  km <- kmeans(cluster_matrix_scaled, centers = k, nstart = 50, iter.max = 100)
  km$tot.withinss
})

elbow_df <- tibble(
  k = k_values,
  total_withinss = wss
)

cat("\nElbow method results:\n")
print(elbow_df)

write.csv(
  elbow_df,
  file.path(output_rq2_dir, "elbow_results.csv"),
  row.names = FALSE
)

png(
  file.path(figure_rq2_dir, "elbow_plot.png"),
  width = 1000,
  height = 700
)

plot(
  elbow_df$k,
  elbow_df$total_withinss,
  type = "b",
  xlab = "Number of clusters (k)",
  ylab = "Total within-cluster sum of squares",
  main = "RQ2 Elbow Plot for K-means"
)

dev.off()

silhouette_scores <- map_dbl(k_values, function(k) {
  set.seed(8410)
  km <- kmeans(cluster_matrix_scaled, centers = k, nstart = 50, iter.max = 100)
  sil <- silhouette(km$cluster, dist(cluster_matrix_scaled))
  mean(sil[, 3])
})

silhouette_df <- tibble(
  k = k_values,
  avg_silhouette_width = silhouette_scores
)

cat("\nSilhouette results:\n")
print(silhouette_df)

write.csv(
  silhouette_df,
  file.path(output_rq2_dir, "silhouette_results.csv"),
  row.names = FALSE
)

png(
  file.path(figure_rq2_dir, "silhouette_plot.png"),
  width = 1000,
  height = 700
)

plot(
  silhouette_df$k,
  silhouette_df$avg_silhouette_width,
  type = "b",
  xlab = "Number of clusters (k)",
  ylab = "Average silhouette width",
  main = "RQ2 Average Silhouette Width"
)

dev.off()

chosen_k <- 3

set.seed(8410)

kmeans_final <- kmeans(
  cluster_matrix_scaled,
  centers = chosen_k,
  nstart = 100,
  iter.max = 100
)

rq2_data <- rq2_data %>%
  mutate(
    cluster = factor(kmeans_final$cluster)
  )

cat("\nFinal k-means cluster sizes:\n")
print(table(rq2_data$cluster))

cluster_size_summary <- rq2_data %>%
  count(cluster, name = "n") %>%
  mutate(percent = round(100 * n / sum(n), 2))

write.csv(
  cluster_size_summary,
  file.path(output_rq2_dir, "cluster_size_summary.csv"),
  row.names = FALSE
)

cluster_profile <- rq2_data %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    percent = round(100 * n() / nrow(rq2_data), 2),
    political_interest = round(mean(political_interest, na.rm = TRUE), 2),
    campaign_interest = round(mean(campaign_interest, na.rm = TRUE), 2),
    political_discussion = round(mean(political_discussion, na.rm = TRUE), 2),
    social_media_attention_days = round(mean(social_media_attention_days, na.rm = TRUE), 2),
    government_trust = round(mean(government_trust, na.rm = TRUE), 2),
    government_for_all = round(mean(government_for_all, na.rm = TRUE), 2),
    power_makes_difference = round(mean(power_makes_difference, na.rm = TRUE), 2),
    vote_makes_difference = round(mean(vote_makes_difference, na.rm = TRUE), 2),
    labor_rating = round(mean(labor_rating, na.rm = TRUE), 2),
    greens_rating = round(mean(greens_rating, na.rm = TRUE), 2),
    albanese_rating = round(mean(albanese_rating, na.rm = TRUE), 2),
    bandt_rating = round(mean(bandt_rating, na.rm = TRUE), 2),
    political_knowledge_score = round(mean(political_knowledge_score, na.rm = TRUE), 2),
    .groups = "drop"
  )

cat("\nCluster profile table, original scales:\n")
print(cluster_profile)

write.csv(
  cluster_profile,
  file.path(output_rq2_dir, "cluster_profiles_original_scales.csv"),
  row.names = FALSE
)

scaled_centroids <- as.data.frame(kmeans_final$centers)
scaled_centroids$cluster <- factor(seq_len(chosen_k))

scaled_centroids <- scaled_centroids %>%
  select(cluster, everything())

cat("\nCluster centroids on scaled variables:\n")
print(scaled_centroids)

write.csv(
  scaled_centroids,
  file.path(output_rq2_dir, "cluster_centroids_scaled.csv"),
  row.names = FALSE
)

q27_by_cluster <- rq2_data %>%
  filter(!is.na(Q27_binary)) %>%
  count(cluster, Q27_binary, name = "n") %>%
  group_by(cluster) %>%
  mutate(
    cluster_total = sum(n),
    percent_within_cluster = round(100 * n / cluster_total, 2)
  ) %>%
  ungroup()

cat("\nQ27_binary distribution by cluster:\n")
print(q27_by_cluster)

write.csv(
  q27_by_cluster,
  file.path(output_rq2_dir, "q27_by_cluster.csv"),
  row.names = FALSE
)

q27_by_cluster_wide <- q27_by_cluster %>%
  select(cluster, Q27_binary, percent_within_cluster) %>%
  pivot_wider(
    names_from = Q27_binary,
    values_from = percent_within_cluster
  )

cat("\nQ27_binary distribution by cluster, wide:\n")
print(q27_by_cluster_wide)

write.csv(
  q27_by_cluster_wide,
  file.path(output_rq2_dir, "q27_by_cluster_wide.csv"),
  row.names = FALSE
)

centroids_long <- scaled_centroids %>%
  pivot_longer(
    cols = -cluster,
    names_to = "variable",
    values_to = "scaled_mean"
  )

png(
  file.path(figure_rq2_dir, "scaled_centroid_heatmap.png"),
  width = 1200,
  height = 800
)

heatmap_plot <- ggplot(centroids_long, aes(x = variable, y = cluster, fill = scaled_mean)) +
  geom_tile() +
  geom_text(aes(label = round(scaled_mean, 2)), size = 3) +
  labs(
    title = "RQ2 Cluster Profiles: Scaled Centroids",
    x = "Clustering variable",
    y = "Cluster"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

print(heatmap_plot)

dev.off()

pca <- prcomp(
  cluster_matrix_scaled,
  center = FALSE,
  scale. = FALSE
)

pca_df <- as.data.frame(pca$x[, 1:2]) %>%
  mutate(cluster = rq2_data$cluster)

png(
  file.path(figure_rq2_dir, "cluster_pca_plot.png"),
  width = 1000,
  height = 700
)

pca_plot <- ggplot(pca_df, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "RQ2 K-means Clusters Visualised by First Two Principal Components",
    x = "PC1",
    y = "PC2"
  ) +
  theme_minimal()

print(pca_plot)

dev.off()

png(
  file.path(figure_rq2_dir, "cluster_fviz_plot.png"),
  width = 1000,
  height = 700
)

fviz_plot <- fviz_cluster(
  kmeans_final,
  data = cluster_matrix_scaled,
  geom = "point",
  ellipse.type = "convex",
  main = "RQ2 K-means Cluster Plot"
)

print(fviz_plot)

dev.off()

cat("\nSaved RQ2 clustering outputs to:", output_rq2_dir, "\n")
cat("Saved RQ2 clustering figures to:", figure_rq2_dir, "\n")