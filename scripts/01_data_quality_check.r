# ============================================================
# COMP8410 Assignment 2
# Script: 01_rq1_data_summary.R
# Purpose: Check missingness, structural missingness, special response codes,
#          and the distribution of the RQ1 target variable.
# Outputs:
#   outputs/rq1/data_quality_summary.csv
# ============================================================

source("scripts/00_setup.R")

d <- read.csv(
  data_path,
  stringsAsFactors = FALSE,
  na.strings = c("", " ", "NA")
)

cat("Loaded data:\n")
print(dim(d))

cat("\nColumn names:\n")
print(names(d))

cat("\nFirst rows:\n")
print(head(d[, 1:min(10, ncol(d))]))

cat("\nTotal raw NA count:\n")
print(sum(is.na(d)))

special_codes_all <- c(96, 97, 98, 99, -1, -2)
true_missing_codes <- c(-1, -2)
substantive_special_codes <- c(96, 97, 98, 99)

num_cols <- sapply(d, function(x) is.numeric(x) || is.integer(x))

raw_na_count <- colSums(is.na(d))

count_code_set <- function(df, cols_mask, code_set) {
  out <- rep(0, ncol(df))
  names(out) <- names(df)

  out[cols_mask] <- sapply(df[, cols_mask, drop = FALSE], function(x) {
    sum(x %in% code_set, na.rm = TRUE)
  })

  out
}

all_special_count <- count_code_set(d, num_cols, special_codes_all)
true_missing_code_count <- count_code_set(d, num_cols, true_missing_codes)
substantive_special_count <- count_code_set(d, num_cols, substantive_special_codes)

cat("\nTop variables by all special-code count:\n")
print(sort(all_special_count, decreasing = TRUE)[1:min(30, length(all_special_count))])

structural_mask <- as.data.frame(
  matrix(FALSE, nrow = nrow(d), ncol = ncol(d))
)
names(structural_mask) <- names(d)

q6_vars <- intersect(c("Q6_1", "Q6_2", "Q6_3", "Q6_4", "Q6_96"), names(d))
q5_yes_vars <- intersect(c("Q5_2", "Q5_3", "Q5_4", "Q5_5", "Q5_6", "Q5_7"), names(d))
q5_no_var <- intersect("Q5_1", names(d))

if (length(q6_vars) > 0 && length(q5_yes_vars) > 0) {
  contacted_any <- apply(d[, q5_yes_vars, drop = FALSE], 1, function(x) {
    any(x == 1, na.rm = TRUE)
  })

  no_contact <- rep(FALSE, nrow(d))
  if (length(q5_no_var) == 1) {
    no_contact <- d[[q5_no_var]] == 1
    no_contact[is.na(no_contact)] <- FALSE
  } else {
    no_contact <- !contacted_any
  }

  for (v in q6_vars) {
    structural_mask[[v]] <- no_contact
  }
}

vote_dep_vars <- intersect(
  c("Q15", "Q15_OthRES", "Q16", "Q16_OthRES", "Q17", "Q18", "Q19", "Q20", "Q21"),
  names(d)
)

if ("Q14" %in% names(d) && length(vote_dep_vars) > 0) {
  not_enrolled <- d$Q14 != 1 | is.na(d$Q14)
  for (v in vote_dep_vars) {
    structural_mask[[v]] <- not_enrolled
  }
}

q22_vars <- intersect(paste0("Q22_", 1:7), names(d))

if ("Q14" %in% names(d) && length(q22_vars) > 0) {
  not_enrolled <- d$Q14 != 1 | is.na(d$Q14)
  for (v in q22_vars) {
    structural_mask[[v]] <- not_enrolled
  }
}

q53_name <- NULL
if ("Q53" %in% names(d)) q53_name <- "Q53"
if ("Q53RES" %in% names(d)) q53_name <- "Q53RES"

if (!is.null(q53_name) && "Q54" %in% names(d)) {
  allowed <- d[[q53_name]] %in% c(1, 2, 3)
  allowed[is.na(allowed)] <- FALSE
  structural_mask$Q54 <- !allowed
}

other_text_pairs <- list(
  Q8_OthRES = "Q8",
  Q15_OthRES = "Q15",
  Q16_OthRES = "Q16",
  Q49_OthRES = "Q49",
  Q53_Oth = "Q53RES",
  Q55_OthRES = "Q55",
  Q56_OthRES = "Q56"
)

for (txt_var in names(other_text_pairs)) {
  src_var <- other_text_pairs[[txt_var]]
  if (txt_var %in% names(d) && src_var %in% names(d)) {
    structural_mask[[txt_var]] <- is.na(d[[src_var]]) | d[[src_var]] != 96
  }
}

structural_missing_count <- sapply(names(d), function(v) {
  sum(is.na(d[[v]]) & structural_mask[[v]], na.rm = TRUE)
})

special_response_count <- all_special_count
true_missing_code_obs_count <- true_missing_code_count
substantive_special_obs_count <- substantive_special_count

residual_blank_count <- raw_na_count - structural_missing_count
missing_like_for_modelling <- residual_blank_count + true_missing_code_obs_count

quality_summary <- data.frame(
  variable = names(d),
  raw_na = raw_na_count,
  structural_missing = structural_missing_count,
  residual_blank = residual_blank_count,
  special_response_total = special_response_count,
  substantive_special = substantive_special_obs_count,
  true_missing_codes = true_missing_code_obs_count,
  missing_like_for_modelling = missing_like_for_modelling,
  raw_na_rate = round(raw_na_count / nrow(d), 4),
  structural_missing_rate = round(structural_missing_count / nrow(d), 4),
  residual_blank_rate = round(residual_blank_count / nrow(d), 4),
  special_response_rate = round(special_response_count / nrow(d), 4),
  substantive_special_rate = round(substantive_special_obs_count / nrow(d), 4),
  true_missing_code_rate = round(true_missing_code_obs_count / nrow(d), 4),
  missing_like_for_modelling_rate = round(missing_like_for_modelling / nrow(d), 4)
)

cat("\nTop variables by raw NA:\n")
print(head(quality_summary[order(-quality_summary$raw_na), ], 20))

cat("\nTop variables by structural missing:\n")
print(head(quality_summary[order(-quality_summary$structural_missing), ], 20))

cat("\nTop variables by substantive special responses (96/97/98/99):\n")
print(head(quality_summary[order(-quality_summary$substantive_special), ], 20))

cat("\nTop variables by missing-like for modelling:\n")
print(head(quality_summary[order(-quality_summary$missing_like_for_modelling), ], 20))

cat("\nCheck Q14 vs Q15 missing:\n")
if ("Q14" %in% names(d) && "Q15" %in% names(d)) {
  print(table(Q14 = d$Q14, Q15_is_NA = is.na(d$Q15), useNA = "ifany"))
}

cat("\nCheck Q14 vs Q21 missing:\n")
if ("Q14" %in% names(d) && "Q21" %in% names(d)) {
  print(table(Q14 = d$Q14, Q21_is_NA = is.na(d$Q21), useNA = "ifany"))
}

cat("\nCheck Q5 vs Q6 missing:\n")
if ("Q5_1" %in% names(d) && "Q6_1" %in% names(d)) {
  print(table(Q5_1 = d$Q5_1, Q6_1_is_NA = is.na(d$Q6_1), useNA = "ifany"))
}

cat("\nCheck Q53/Q53RES vs Q54 missing:\n")
if (!is.null(q53_name) && "Q54" %in% names(d)) {
  print(table(Q53 = d[[q53_name]], Q54_is_NA = is.na(d$Q54), useNA = "ifany"))
} else {
  cat("Q54 not present in this file.\n")
}

if ("Q27" %in% names(d)) {
  cat("\nQ27 distribution:\n")
  print(table(d$Q27, useNA = "ifany"))
  print(round(prop.table(table(d$Q27, useNA = "no")), 4))

  q27_bin <- ifelse(
    d$Q27 %in% c(1, 2), "Satisfied",
    ifelse(d$Q27 %in% c(3, 4), "Dissatisfied", NA)
  )

  cat("\nQ27 binary distribution:\n")
  print(table(q27_bin, useNA = "ifany"))
  print(round(prop.table(table(q27_bin, useNA = "no")), 4))
}

quality_summary_path <- file.path(output_rq1_dir, "data_quality_summary.csv")

write.csv(
  quality_summary,
  quality_summary_path,
  row.names = FALSE
)

cat("\nSaved:", quality_summary_path, "\n")