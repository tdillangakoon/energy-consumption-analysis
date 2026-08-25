# TASK 5 — REGRESSION MODELLING

# 0. LOAD PACKAGES
library(tidyverse)   # Data manipulation
library(janitor)     # Data cleaning
library(lubridate)   # Date handling
library(broom)       # Model results
library(car)         # Model diagnostics
library(lmtest)      # Model tests
library(sandwich)    # Robust errors
library(MASS)        # Statistical models

# 1. FILE PATHS
data_dir <- "C:/Users/MSI/Desktop/Academic/3rd year/1st sem/SM/Assignment/energy-consumption-analysis/datasets"

output_dir <- "C:/Users/MSI/Desktop/Academic/3rd year/1st sem/SM/Assignment/energy-consumption-analysis/task5/outputs/tables"

fig_dir <- "C:/Users/MSI/Desktop/Academic/3rd year/1st sem/SM/Assignment/energy-consumption-analysis/task5/outputs/figures"

cat("============================================\n")
cat("TASK 5 — REGRESSION MODELLING\n")
cat("============================================\n\n")

cat("Output directory:\n", output_dir, "\n\n")


# 2. TARGET VARIABLE — MONTHLY ELECTRICITY CONSUMPTION
consumption <- read_csv(
  file.path(data_dir, "monthly_consumption.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  transmute(
    household_id = household_id,
    month = as.Date(month),
    monthly_consumption = parse_number(consumption)
  )


# Basic validation checks
stopifnot(!anyDuplicated(consumption[c("household_id", "month")]))

stopifnot(!any(consumption$monthly_consumption < 0,na.rm = TRUE))


cat("============================================\n")
cat("TARGET VARIABLE\n")
cat("============================================\n")

cat(
  "Total household-month observations:",
  nrow(consumption),
  "\n"
)

cat(
  "Missing target values:",
  sum(is.na(consumption$monthly_consumption)),
  "\n"
)

cat(
  "Zero-consumption months retained:",
  sum(
    consumption$monthly_consumption == 0,
    na.rm = TRUE
  ),
  "\n"
)

cat(
  "Date range:",
  format(min(consumption$month, na.rm = TRUE)),
  "to",
  format(max(consumption$month, na.rm = TRUE)),
  "\n\n"
)


# 3. DEMOGRAPHIC DATA
demographics <- read_csv(
  file.path(data_dir, "w1_demographics.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    age = parse_number(age),
    
    hours_at_home =
      parse_number(
        no_of_hours_stayed_at_home_during_last_week
      )
  )


# 4. APPLIANCE DATA
appliances <- read_csv(
  file.path(data_dir, "w1_appliances.csv"),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    hours_used =
      parse_number(
        no_of_hours_used_during_last_week
      )
  )


# 5. HOUSEHOLD / HOUSING DATA
housing <- read_csv(
  file.path(
    data_dir,
    "w1_household_information_and_history.csv"
  ),
  col_types = cols(.default = "c")
) %>%
  clean_names() %>%
  mutate(
    
    floor_area =
      parse_number(floor_area),
    
    total_monthly_expenditure =
      parse_number(
        total_monthly_expenditure_of_last_month
      ),
    
    electricity_meters =
      parse_number(
        no_of_electricity_meters
      )
  )


# 6. CREATE HOUSEHOLD-LEVEL PREDICTORS

# 6.1 Household composition
household_composition <- demographics %>%
  group_by(household_id) %>%
  summarise(
    
    household_size =
      n(),
    
    average_age =
      ifelse(
        all(is.na(age)),
        NA_real_,
        mean(age, na.rm = TRUE)
      ),
    
    employed_members =
      sum(
        main_activity_engaged_in ==
          "Engaged in economic activity/ currently employed/ engaged in own business",
        na.rm = TRUE
      ),
    
    average_hours_at_home =
      ifelse(
        all(is.na(hours_at_home)),
        NA_real_,
        mean(hours_at_home, na.rm = TRUE)
      ),
    
    .groups = "drop"
  )


# 6.2 Household head characteristics
household_head <- demographics %>%
  filter(
    relationship_to_the_head_of_household ==
      "Head of the household"
  ) %>%
  transmute(
    
    household_id,
    
    head_gender =
      gender,
    
    head_education =
      highest_level_of_education,
    
    head_occupation =
      main_occupation,
    
    head_employment_status =
      employment_status_of_the_main_occupation,
    
    head_main_activity =
      main_activity_engaged_in
  )


stopifnot(
  !anyDuplicated(
    household_head$household_id
  )
)


# 6.3 Appliance characteristics
household_appliances <- appliances %>%
  group_by(household_id) %>%
  summarise(
    
    total_appliances =
      n(),
    
    total_weekly_appliance_hours =
      if (
        all(is.na(hours_used))
      ) {
        NA_real_
      } else {
        sum(
          hours_used,
          na.rm = TRUE
        )
      },
    
    missing_any_appliance_hours =
      any(
        is.na(hours_used)
      ),
    
    .groups = "drop"
  )


# 6.4 Housing characteristics
household_housing <- housing %>%
  transmute(
    
    household_id,
    
    floor_area,
    
    electricity_meters,
    
    socio_economic_class,
    
    type_of_house,
    
    housing_tenure =
      own_the_house_or_living_on_rent,
    
    total_monthly_expenditure,
    
    type_of_electricity_meter
  )


stopifnot(
  !anyDuplicated(
    household_housing$household_id
  )
)


# 7. COMBINE HOUSEHOLD-LEVEL PREDICTORS
household_predictors <-
  household_composition %>%
  
  left_join(
    household_head,
    by = "household_id"
  ) %>%
  
  left_join(
    household_appliances,
    by = "household_id"
  ) %>%
  
  left_join(
    household_housing,
    by = "household_id"
  ) %>%
  
  mutate(
    
    missing_head_record =
      is.na(head_gender),
    
    missing_appliance_survey =
      is.na(total_appliances),
    
    missing_any_appliance_hours =
      replace_na(
        missing_any_appliance_hours,
        TRUE
      )
  )


cat(
  "\nHouseholds without identifiable head:",
  sum(
    is.na(
      household_predictors$head_gender
    )
  ),
  "\n"
)


# 8. CREATE HOUSEHOLD-MONTH MODELLING DATA
first_month <-
  min(
    consumption$month,
    na.rm = TRUE
  )


model_data <- consumption %>%
  
  filter(
    !is.na(monthly_consumption)
  ) %>%
  
  left_join(
    household_predictors,
    by = "household_id"
  ) %>%
  
  mutate(
    
    month_of_year =
      factor(
        month(
          month,
          label = TRUE,
          abbr = TRUE
        ),
        levels = month.abb
      ),
    
    month_index =
      12 *
      (
        year(month) -
          year(first_month)
      ) +
      month(month) -
      month(first_month)
  )


cat(
  "Final modelling observations:",
  nrow(model_data),
  "\n"
)


write_csv(
  model_data,
  file.path(
    output_dir,
    "task5_modelling_data.csv"
  )
)


# 9. TIME-BASED TRAIN / TEST SPLIT

# Training period:
# Before May 2024

# Testing period:
# May 2024 onwards

train_raw <-
  model_data %>%
  filter(
    month < as.Date("2024-05-01")
  )


test_raw <-
  model_data %>%
  filter(
    month >= as.Date("2024-05-01")
  )


stopifnot(
  nrow(train_raw) > 0
)

stopifnot(
  nrow(test_raw) > 0
)

stopifnot(
  max(train_raw$month) <
    min(test_raw$month)
)


cat("\n============================================\n")
cat("TRAIN / TEST SPLIT\n")
cat("============================================\n")

cat(
  "Training observations:",
  nrow(train_raw),
  "\n"
)

cat(
  "Training period:",
  format(min(train_raw$month)),
  "to",
  format(max(train_raw$month)),
  "\n"
)

cat(
  "Testing observations:",
  nrow(test_raw),
  "\n"
)

cat(
  "Testing period:",
  format(min(test_raw$month)),
  "to",
  format(max(test_raw$month)),
  "\n\n"
)

# 10. DEFINE CANDIDATE PREDICTORS
numeric_predictors <- c(
  
  "household_size",
  
  "average_age",
  
  "employed_members",
  
  "average_hours_at_home",
  
  "total_appliances",
  
  "total_weekly_appliance_hours",
  
  "floor_area",
  
  "electricity_meters",
  
  "total_monthly_expenditure",
  
  "month_index"
)


categorical_predictors <- c(
  
  "head_gender",
  
  "head_education",
  
  "head_occupation",
  
  "head_employment_status",
  
  "head_main_activity",
  
  "socio_economic_class",
  
  "type_of_house",
  
  "housing_tenure",
  
  "type_of_electricity_meter",
  
  "missing_head_record",
  
  "missing_appliance_survey",
  
  "missing_any_appliance_hours",
  
  "month_of_year"
)


predictors <- c(
  numeric_predictors,
  categorical_predictors
)


head_predictors <- c(
  
  "head_gender",
  
  "head_education",
  
  "head_occupation",
  
  "head_employment_status",
  
  "head_main_activity"
)


# 11. TRAINING-ONLY PREPROCESSING
most_common_level <- function(values) {
  
  values <-
    values[
      !is.na(values) &
        values != ""
    ]
  
  if (length(values) == 0) {
    return("Unknown")
  }
  
  names(
    sort(
      table(values),
      decreasing = TRUE
    )
  )[1]
}


prepare_data <- function(
    data,
    reference = NULL
) {
  
  result <-
    data %>%
    dplyr::select(
      household_id,
      month,
      monthly_consumption,
      all_of(predictors)
    )
  
   # Numeric variables
  for (
    variable in numeric_predictors
  ) {
    
    replacement <-
      if (is.null(reference)) {
        
        median(
          result[[variable]],
          na.rm = TRUE
        )
        
      } else {
        
        median(
          reference[[variable]],
          na.rm = TRUE
        )
      }
    
    
    if (is.na(replacement)) {
      replacement <- 0
    }
    
    
    result[[variable]][
      is.na(
        result[[variable]]
      )
    ] <- replacement
  }
  
  
  # Categorical variables
  for (
    variable in categorical_predictors
  ) {
    
    values <-
      as.character(
        result[[variable]]
      )
    
    
    if (
      variable %in%
      head_predictors
    ) {
      
      if (
        is.null(reference)
      ) {
        
        replacement <-
          most_common_level(
            values
          )
        
      } else {
        
        replacement <-
          most_common_level(
            as.character(
              reference[[variable]]
            )
          )
      }
      
      
      values[
        is.na(values) |
          values == ""
      ] <- replacement
      
    } else {
      
      values[
        is.na(values) |
          values == ""
      ] <- "Unknown"
    }
    
    
    if (
      is.null(reference)
    ) {
      
      factor_levels <-
        sort(
          unique(
            c(
              values,
              "Unknown"
            )
          )
        )
      
    } else {
      
      factor_levels <-
        levels(
          reference[[variable]]
        )
      
      if (
        !("Unknown" %in%
          factor_levels)
      ) {
        
        factor_levels <-
          c(
            factor_levels,
            "Unknown"
          )
      }
      
      
      values[
        !(values %in%
            factor_levels)
      ] <- "Unknown"
    }
    
    
    result[[variable]] <-
      factor(
        values,
        levels = factor_levels
      )
  }
  
  
  return(result)
}


# Prepare training and test data
train <-
  prepare_data(
    train_raw
  )


test <-
  prepare_data(
    test_raw,
    train
  )


cat(
  "Prepared training observations:",
  nrow(train),
  "\n"
)

cat(
  "Prepared testing observations:",
  nrow(test),
  "\n\n"
)


# 12. BASELINE MODEL
baseline_lm <-
  lm(
    monthly_consumption ~ household_size,
    data = train
  )

# 13. EXPANDED MULTIPLE LINEAR REGRESSION
expanded_formula <-
  reformulate(
    predictors,
    response =
      "monthly_consumption"
  )

expanded_lm <-
  lm(
    expanded_formula,
    data = train
  )

# 14. SELECTED MULTIPLE LINEAR REGRESSION
selected_lm <-
  MASS::stepAIC(
    expanded_lm,
    direction = "both",
    trace = FALSE
  )


cat("\n============================================\n")
cat("SELECTED LINEAR MODEL\n")
cat("============================================\n")

print(
  formula(selected_lm)
)


# 15. LOG-TRANSFORMED MULTIPLE LINEAR REGRESSION
log_train <-
  train %>%
  mutate(
    log_consumption =
      log1p(
        monthly_consumption
      )
  )

# Expanded log model
log_expanded_formula <-
  reformulate(
    predictors,
    response =
      "log_consumption"
  )


log_expanded_lm <-
  lm(
    log_expanded_formula,
    data = log_train
  )

# Selected log model
log_selected_lm <-
  MASS::stepAIC(
    log_expanded_lm,
    direction = "both",
    trace = FALSE
  )


cat("\n============================================\n")
cat("SELECTED LOG MODEL\n")
cat("============================================\n")

print(
  formula(log_selected_lm)
)


# Duan smearing estimator
smearing_factor <-
  mean(
    exp(
      residuals(
        log_selected_lm
      )
    )
  )


cat(
  "\nDuan smearing factor:",
  smearing_factor,
  "\n"
)


# 16. MODEL EVALUATION FUNCTIONS
calculate_r2 <- function(
    actual,
    predicted
) {
  
  valid <-
    complete.cases(
      actual,
      predicted
    )
  
  if (
    sum(valid) < 2
  ) {
    return(NA_real_)
  }
  
  cor(
    actual[valid],
    predicted[valid]
  )^2
}


evaluate_lm <-
  function(
    model,
    new_data,
    model_name
  ) {
    
    predicted <-
      as.numeric(
        predict(
          model,
          newdata =
            new_data
        )
      )
    
    actual <-
      new_data$monthly_consumption
    
    
    tibble(Model =
        model_name,
      
      Test_rows =
        length(actual),
      
      RMSE =
        sqrt(
          mean(
            (
              actual -
                predicted
            )^2,
            na.rm = TRUE
          )
        ),
      
      MAE =
        mean(
          abs(
            actual -
              predicted
          ),
          na.rm = TRUE
        ),
      
      Test_R_squared =
        calculate_r2(
          actual,
          predicted
        ),
      
      Training_R_squared =
        summary(model)$r.squared,
      
      Adjusted_R_squared_training =
        summary(model)$adj.r.squared
    )
  }


evaluate_log_lm <-
  function(
    model,
    new_data,
    smearing,
    model_name
  ) {
    
    predicted_log <-
      predict(
        model,
        newdata =
          new_data
      )
    
    
    predicted <-
      pmax(
        0,
        smearing *
          exp(
            predicted_log
          ) -
          1
      )
    
    
    actual <-
      new_data$monthly_consumption
    
    
    tibble(
      
      Model =
        model_name,
      
      Test_rows =
        length(actual),
      
      RMSE =
        sqrt(
          mean(
            (
              actual -
                predicted
            )^2,
            na.rm = TRUE
          )
        ),
      
      MAE =
        mean(
          abs(
            actual -
              predicted
          ),
          na.rm = TRUE
        ),
      
      Test_R_squared =
        calculate_r2(
          actual,
          predicted
        ),
      
      Training_R_squared =
        summary(model)$r.squared,
      
      Adjusted_R_squared_training =
        summary(model)$adj.r.squared
    )
  }


# 17. MODEL COMPARISON
model_comparison <-
  bind_rows(
    
    evaluate_lm(
      baseline_lm,
      test,
      "Model 1: Baseline — Household Size"
    ),
    
    evaluate_lm(
      expanded_lm,
      test,
      "Model 2: Expanded MLR"
    ),
    
    evaluate_lm(
      selected_lm,
      test,
      "Model 3: Selected MLR"
    ),
    
    evaluate_log_lm(
      log_expanded_lm,
      test,
      smearing_factor,
      "Model 4: Expanded Log MLR"
    ),
    
    evaluate_log_lm(
      log_selected_lm,
      test,
      smearing_factor,
      "Model 5: Selected Log MLR"
    )
    
  ) %>%
  arrange(RMSE)


cat("\n============================================\n")
cat("MODEL COMPARISON\n")
cat("============================================\n")

print(
  model_comparison
)


write_csv(
  model_comparison,
  file.path(
    output_dir,
    "model_comparison.csv"
  )
)

# 18. SELECT FINAL MODEL BASED ON TEST PERFORMANCE
best_model_row <-
  model_comparison %>%
  slice_min(
    RMSE,
    n = 1,
    with_ties = FALSE
  )


final_model_name <-
  best_model_row$Model


cat("\n============================================\n")
cat("FINAL MODEL SELECTION\n")
cat("============================================\n")

cat(
  "Selected final model:",
  final_model_name,
  "\n"
)

cat(
  "Lowest test RMSE:",
  best_model_row$RMSE,
  "\n\n"
)


# Store the actual final model object
if (
  final_model_name ==
  "Model 1: Baseline — Household Size"
) {
  
  final_model <- baseline_lm
  
  final_model_type <- "linear"
  
} else if (
  final_model_name ==
  "Model 2: Expanded MLR"
) {
  
  final_model <- expanded_lm
  
  final_model_type <- "linear"
  
} else if (
  final_model_name ==
  "Model 3: Selected MLR"
) {
  
  final_model <- selected_lm
  
  final_model_type <- "linear"
  
} else if (
  final_model_name ==
  "Model 4: Expanded Log MLR"
) {
  
  final_model <- log_expanded_lm
  
  final_model_type <- "log"
  
} else {
  
  final_model <- log_selected_lm
  
  final_model_type <- "log"
}

# 19. TEST-SET PREDICTIONS
if (
  final_model_type ==
  "linear"
) {
  
  final_predictions <-
    as.numeric(
      predict(
        final_model,
        newdata = test
      )
    )
  
} else {
  
  final_predictions <-
    pmax(
      0,
      smearing_factor *
        exp(
          predict(
            final_model,
            newdata = test
          )
        ) -
        1
    )
}


test_predictions <-
  test %>%
  
  transmute(
    
    household_id,
    
    month,
    
    actual_monthly_consumption =
      monthly_consumption,
    
    predicted_monthly_consumption =
      final_predictions
    
  ) %>%
  
  mutate(
    
    residual =
      actual_monthly_consumption -
      predicted_monthly_consumption,
    
    absolute_error =
      abs(residual),
    
    squared_error =
      residual^2
  )


write_csv(
  test_predictions,
  file.path(
    output_dir,
    "test_predictions.csv"
  )
)

# 20. FINAL MODEL COEFFICIENTS
final_coefficients <-
  tidy(
    final_model,
    conf.int = TRUE
  )

write_csv(
  final_coefficients,
  file.path(
    output_dir,
    "final_model_coefficients.csv"
  )
)


# 21. FINAL MODEL FIT STATISTICS
final_fit_statistics <-
  glance(
    final_model
  )


write_csv(
  final_fit_statistics,
  file.path(
    output_dir,
    "final_model_fit_statistics.csv"
  )
)


# 22. CLUSTER-ROBUST STANDARD ERRORS
cluster_robust <-
  lmtest::coeftest(
    final_model,
    vcov. =
      sandwich::vcovCL(
        final_model,
        cluster =
          train$household_id,
        type = "HC1"
      )
  )

write.csv(
  cluster_robust,
  file.path(
    output_dir,
    "cluster_robust_coefficients.csv"
  )
)

# 23. NAIVE VS CLUSTER-ROBUST SIGNIFICANCE
robust_tidy <-
  tibble(
    
    term =
      rownames(
        cluster_robust
      ),
    
    robust_estimate =
      cluster_robust[, "Estimate"],
    
    robust_std_error =
      cluster_robust[, "Std. Error"],
    
    robust_t_value =
      cluster_robust[, "t value"],
    
    robust_p_value =
      cluster_robust[, "Pr(>|t|)"]
  )


significance_comparison <-
  final_coefficients %>%
  
  dplyr::select(
    term,
    estimate,
    naive_std_error = std.error,
    naive_p_value = p.value
  ) %>%
  
  left_join(
    robust_tidy,
    by = "term"
  ) %>%
  
  mutate(
    
    significant_naive = naive_p_value < 0.05,
    
    significant_robust = robust_p_value < 0.05,
    
    conclusion_disagrees = significant_naive != significant_robust
  ) %>%
  
  arrange(
    desc(conclusion_disagrees),
    robust_p_value
  )


write_csv(
  significance_comparison,
  file.path(
    output_dir,
    "naive_vs_cluster_robust_significance.csv"
  )
)

# 24. MULTICOLLINEARITY — VIF
alias_check <-
  alias(
    final_model
  )


if (
  !is.null(
    alias_check$Complete
  )
) {
  
  cat(
    "\nWARNING: Perfect multicollinearity detected.\n"
  )
  
  print(
    alias_check$Complete
  )
  
}


vif_raw <-
  car::vif(
    final_model
  )


if (
  is.matrix(vif_raw)
) {
  
  vif_results <-
    as.data.frame(
      vif_raw
    ) %>%
    
    rownames_to_column(
      "term"
    ) %>%
    
    mutate(
      
      adjusted_GVIF =
        `GVIF^(1/(2*Df))`
      
    )
  
} else {
  
  vif_results <-
    tibble(
      
      term =
        names(vif_raw),
      
      VIF =
        as.numeric(vif_raw)
    )
}


write_csv(
  vif_results,
  file.path(
    output_dir,
    "vif_results.csv"
  )
)


# 25. BREUSCH-PAGAN TEST
bp_result <-
  lmtest::bptest(
    final_model
  )

# 26. SHAPIRO-WILK TEST
set.seed(3081)

residuals_final <-
  residuals(
    final_model
  )

residual_sample <-
  sample(
    residuals_final,
    min(
      5000,
      length(
        residuals_final
      )
    )
  )

shapiro_result <-
  shapiro.test(
    residual_sample
  )


# 27. ASSUMPTION TEST SUMMARY
assumption_tests <-
  tibble(
    test = c(
      "Breusch-Pagan",
      "Shapiro-Wilk"
    ),
    
    statistic = c(
      
      unname(
        bp_result$statistic
      ),
      
      unname(
        shapiro_result$statistic
      )
    ),
    
    p_value = c(
      
      bp_result$p.value,
      
      shapiro_result$p.value
    ),
    
    interpretation = c(
      
      "Small p-value indicates evidence of non-constant residual variance.",
      
      "Small p-value indicates departure from normality of residuals."
    )
  )


write_csv(
  assumption_tests,
  file.path(
    output_dir,
    "assumption_tests.csv"
  )
)

# 28. COOK'S DISTANCE
cooks <-
  cooks.distance(
    final_model
  )

cook_table <-
  tibble(
    
    row =
      seq_along(cooks),
    
    household_id =
      train$household_id,
    
    month =
      train$month,
    
    cooks_distance =
      cooks
    
  ) %>%
  
  arrange(
    desc(cooks_distance)
  ) %>%
  
  slice_head(
    n = 20
  )


write_csv(
  cook_table,
  file.path(
    output_dir,
    "top_20_cooks_distance.csv"
  )
)


cook_threshold <-
  4 /
  nrow(train)


write_csv(
  tibble(
    cooks_threshold =
      cook_threshold
  ),
  file.path(
    output_dir,
    "cooks_distance_threshold.csv"
  )
)

# 29. MODEL DIAGNOSTIC PLOTS
png(
  file.path(
    fig_dir,
    "lm_diagnostic_plots.png"
  ),
  width = 1600,
  height = 1200
)


par(
  mfrow = c(2, 2)
)


plot(
  final_model
)


dev.off()


# 30. ACTUAL VS PREDICTED PLOT
png(
  file.path(
    fig_dir,
    "test_actual_vs_predicted.png"
  ),
  width = 1100,
  height = 800
)

ggplot(
  test_predictions,
  aes(
    x =
      actual_monthly_consumption,
    y =
      predicted_monthly_consumption
  )
) +
  
  geom_point(
    alpha = 0.20
  ) +
  
  geom_abline(
    slope = 1,
    intercept = 0,
    linetype = 2
  ) +
  
  labs(
    
    title =
      paste(
        "Actual vs Predicted Monthly Electricity Consumption —",
        final_model_name
      ),
    
    x =
      "Actual consumption (kWh)",
    
    y =
      "Predicted consumption (kWh)"
  ) +
  
  theme_minimal()


dev.off()

# 31. TEST-SET RESIDUAL PLOT
png(
  file.path(
    fig_dir,
    "test_residuals.png"
  ),
  width = 1100,
  height = 800
)


ggplot(
  test_predictions,
  aes(
    x =
      predicted_monthly_consumption,
    y =
      residual
  )
) +
  
  geom_point(
    alpha = 0.20
  ) +
  
  geom_hline(
    yintercept = 0,
    linetype = 2
  ) +
  
  labs(
    
    title =
      "Test-set Residuals vs Predicted Consumption",
    
    x =
      "Predicted consumption (kWh)",
    
    y =
      "Residual (kWh)"
  ) +
  
  theme_minimal()


dev.off()

# 32. FINAL MODEL FORMULA
formula_text <-
  paste(
    deparse(
      formula(final_model)
    ),
    collapse = " "
  )


write_lines(
  formula_text,
  file.path(
    output_dir,
    "final_model_formula.txt"
  )
)

# 33. FINAL TASK 5 SUMMARY
final_row <-
  model_comparison %>%
  filter(
    Model ==
      final_model_name
  )

cat("\n\n============================================\n")
cat("FINAL TASK 5 SUMMARY\n")
cat("============================================\n\n")


cat(
  "Final model:",
  final_model_name,
  "\n\n"
)


cat(
  "Final model formula:\n",
  formula_text,
  "\n\n"
)


cat(
  "Training observations:",
  nrow(train),
  "\n"
)


cat(
  "Testing observations:",
  nrow(test),
  "\n\n"
)


cat(
  "Test RMSE:",
  final_row$RMSE,
  "\n"
)


cat(
  "Test MAE:",
  final_row$MAE,
  "\n"
)


cat(
  "Test R-squared:",
  final_row$Test_R_squared,
  "\n"
)


cat(
  "Training R-squared:",
  final_row$Training_R_squared,
  "\n"
)


cat(
  "Training adjusted R-squared:",
  final_row$Adjusted_R_squared_training,
  "\n\n"
)


cat(
  "Breusch-Pagan p-value:",
  bp_result$p.value,
  "\n"
)


cat(
  "Shapiro-Wilk p-value:",
  shapiro_result$p.value,
  "\n\n"
)


cat(
  "Duan smearing factor:",
  smearing_factor,
  "\n\n"
)


cat(
  "All Task 5 outputs saved to:\n",
  output_dir,
  "\n"
)


cat(
  "\n============================================\n"
)

cat(
  "TASK 5 COMPLETED SUCCESSFULLY\n"
)

cat(
  "============================================\n"
)