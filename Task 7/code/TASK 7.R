library(tidyverse)
library(janitor)
library(psych)         
library(corrplot)       
library(factoextra)     

setwd("D:/Notes/3Y/Semester 1/SM/Assignment/Task 3")

household_level_data <- read_csv("household_level_data.csv") %>%
  clean_names()

cat("Households loaded from canonical file:", nrow(household_level_data), "\n")

# 1. VARIABLE SETS

full_vars <- c(
  "avg_monthly_consumption",
  "household_size",
  "average_age",
  "employed_members",
  "average_hours_at_home",
  "total_appliances",
  "total_weekly_hours",
  "floor_area",
  "total_monthly_expenditure_of_last_month",
  "no_of_electricity_meters"
)

usage_vars <- c(
  "total_appliances",
  "total_weekly_hours",
  "floor_area",
  "total_monthly_expenditure_of_last_month"
)

demographic_vars <- c(
  "household_size",
  "average_age",
  "employed_members",
  "average_hours_at_home",
  "no_of_electricity_meters"
)


# 2. HELPER FUNCTION — RUNS THE FULL PCA SUITABILITY CHAIN

run_pca_diagnostics <- function(data, vars, label) {
  
  cat("\n============================================\n")
  cat("PCA DIAGNOSTICS —", label, "\n")
  cat("============================================\n")
  
  numeric_vars <- data %>%
    dplyr::select(all_of(vars)) %>%
    drop_na()
  
  cat("Complete cases used:", nrow(numeric_vars), "of", nrow(data), "households\n")
  
  scaled_vars <- scale(numeric_vars)
  corr_matrix <- cor(scaled_vars)
  
  cat("\nCorrelation matrix:\n")
  print(round(corr_matrix, 3))
  
  off_diag <- corr_matrix[upper.tri(corr_matrix)]
  cat("\nShare of variable pairs with |r| > 0.3:",
      round(mean(abs(off_diag) > 0.3), 3), "\n")
  cat("Strongest pairwise correlation:", round(max(abs(off_diag)), 3), "\n")
  
  kmo_result <- KMO(corr_matrix)
  cat("\nOverall KMO:", round(kmo_result$MSA, 3), "\n")
  cat("Per-variable KMO:\n")
  print(round(kmo_result$MSAi, 3))
  
  bartlett_result <- cortest.bartlett(corr_matrix, n = nrow(numeric_vars))
  cat("\nBartlett's test: chi-sq =", round(bartlett_result$chisq, 1),
      " df =", bartlett_result$df,
      " p =", format.pval(bartlett_result$p.value, digits = 3), "\n")
  
  det_corr <- det(corr_matrix)
  cat("Determinant of correlation matrix:", round(det_corr, 4), "\n")
  
  pca_model <- prcomp(scaled_vars, center = TRUE, scale. = TRUE)
  eigenvalues <- (pca_model$sdev)^2
  variance_explained <- eigenvalues / sum(eigenvalues) * 100
  cumulative_variance <- cumsum(variance_explained)
  
  eig_table <- tibble(
    Component = paste0("PC", seq_along(eigenvalues)),
    Eigenvalue = round(eigenvalues, 3),
    Variance_pct = round(variance_explained, 1),
    Cumulative_pct = round(cumulative_variance, 1)
  )
  cat("\nEigenvalues / variance explained:\n")
  print(eig_table)
  
  n_kaiser <- sum(eigenvalues > 1)
  cat("\nComponents with eigenvalue > 1 (Kaiser criterion):", n_kaiser, "\n")
  
  n_loadings_to_show <- min(4, ncol(scaled_vars))
  loadings_table <- as.data.frame(pca_model$rotation[, 1:n_loadings_to_show, drop = FALSE])
  cat("\nLoadings (first", n_loadings_to_show, "components):\n")
  print(round(loadings_table, 3))
  
  invisible(list(
    pca_model = pca_model,
    kmo = kmo_result,
    bartlett = bartlett_result,
    eig_table = eig_table,
    numeric_vars = numeric_vars,
    scaled_vars = scaled_vars
  ))
}

# 3. RUN DIAGNOSTICS ON ALL THREE SETS

full_result        <- run_pca_diagnostics(household_level_data, full_vars,        "FULL SET (exploratory only)")
usage_result       <- run_pca_diagnostics(household_level_data, usage_vars,       "USAGE-INTENSITY SUBSET (safe for regression)")
demographic_result <- run_pca_diagnostics(household_level_data, demographic_vars, "DEMOGRAPHIC SUBSET")

# 4. VISUALS FOR THE FULL SET (for the report figures)

display_labels <- c(
  "avg_monthly_consumption"                 = "Avg Consumption",
  "household_size"                          = "Household Size",
  "average_age"                             = "Avg Age",
  "employed_members"                        = "Employed Members",
  "average_hours_at_home"                   = "Avg Hours Home",
  "total_appliances"                        = "Total Appliances",
  "total_weekly_hours"                      = "Total Weekly Hrs",
  "floor_area"                              = "Floor Area",
  "total_monthly_expenditure_of_last_month" = "Monthly Expenditure",
  "no_of_electricity_meters"                = "Electricity Meters"
)

full_corr_matrix <- cor(full_result$scaled_vars)
colnames(full_corr_matrix) <- display_labels[colnames(full_corr_matrix)]
rownames(full_corr_matrix) <- display_labels[rownames(full_corr_matrix)]

png("figure1_correlation_matrix.png", width = 1400, height = 1200, res = 150)
corrplot(full_corr_matrix, method = "color", addCoef.col = "black",
         tl.col = "black", tl.cex = 0.85, tl.srt = 45, number.cex = 0.7,
         title = "Correlation Matrix - Candidate Numeric Variables",
         mar = c(0, 0, 2, 0))
dev.off()

png("figure2_scree_plot.png", width = 1200, height = 900, res = 150)
print(
  fviz_eig(full_result$pca_model, addlabels = TRUE, barfill = "#2c7fb8",
           barcolor = "#2c7fb8", linecolor = "red") +
    labs(title = "Scree Plot - Household-Level Numeric Variables") +
    theme_minimal()
)
dev.off()

png("figure3_pca_loadings.png", width = 1200, height = 1000, res = 150)
print(
  fviz_pca_var(full_result$pca_model, col.var = "contrib",
               gradient.cols = c("#2c7fb8", "#f4a6a6", "#d7191c"),
               repel = TRUE) +
    labs(title = "PCA Variable Loadings (PC1 vs PC2) - Full Set")
)
dev.off()

cat("\nSaved figure1_correlation_matrix.png, figure2_scree_plot.png and figure3_pca_loadings.png\n")

# 5. THE CORRECTED, LEAKAGE-FREE REGRESSION PREDICTOR

household_level_data_with_pc1 <- household_level_data %>%
  filter(complete.cases(dplyr::select(., all_of(usage_vars)))) %>%
  mutate(usage_intensity_pc1 = usage_result$pca_model$x[, 1])

cat("\n============================================\n")
cat("SANITY CHECK — PC1 vs. the excluded target variable\n")
cat("============================================\n")

cat("Correlation between usage-intensity PC1 and avg_monthly_consumption:",
    round(cor(household_level_data_with_pc1$usage_intensity_pc1,
              household_level_data_with_pc1$avg_monthly_consumption,
              use = "complete.obs"), 3), "\n")

write_csv(household_level_data_with_pc1, "household_level_data_with_pc1.csv")