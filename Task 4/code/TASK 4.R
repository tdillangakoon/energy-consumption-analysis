# ============================================================
# TASK 4 – STATISTICAL INFERENCE
# Energy Consumption Analysis
# ============================================================

# --- Package setup (install only if missing) ---
required_pkgs <- c("tidyverse", "car", "rstatix", "readr", "bit", "bit64", "vroom")
for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

library(tidyverse)
library(car)
library(rstatix)


# ============================================================
# FOLDER SETUP — Task 4 project structure
# ============================================================

base_dir    <- "C:/Users/Sithumini/Documents/3rd year/SM/Assignment/Task 4"
data_dir    <- file.path(base_dir, "used_datasets")
tables_dir  <- file.path(base_dir, "outputs", "tables")
figures_dir <- file.path(base_dir, "outputs", "figures")
stats_dir   <- file.path(base_dir, "outputs", "statistics")

for (d in c(data_dir, tables_dir, figures_dir, stats_dir)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

data_path <- file.path(data_dir, "household_level_data.csv")

# fail fast with a clear message instead of a cryptic read_csv error
if (!file.exists(data_path)) {
  stop(paste0(
    "Dataset not found at: ", data_path,
    "\nCopy household_level_data.csv into: ", data_dir
  ))
}


# ============================================================
# LOAD HOUSEHOLD-LEVEL DATA
# ============================================================

hh <- read_csv(data_path) %>%
  mutate(
    # Gender of household head
    head_gender = factor(head_gender),

    # Collapse education into manageable categories
    head_education = case_when(
      str_detect(head_education, "No Schooling|Special Education|Studying") ~ "No schooling",
      str_detect(head_education, "Grade [1-9]$") ~ "Primary (Grade 1-9)",
      str_detect(head_education, "Grade 10|Grade 12|GAQ|GSQ") ~ "Secondary (Grade 10-12)",
      str_detect(head_education, "O/L") ~ "O/Level",
      str_detect(head_education, "A/L") ~ "A/Level",
      str_detect(head_education, "post Graduate|PhD") ~ "Postgraduate/PhD",
      str_detect(head_education, "Degree") ~ "Degree/Diploma",
      TRUE ~ NA_character_
    ) %>%
      factor(
        levels = c("No schooling", "Primary (Grade 1-9)", "Secondary (Grade 10-12)",
                   "O/Level", "A/Level", "Degree/Diploma", "Postgraduate/PhD"),
        ordered = TRUE
      ),

    # Socio-economic class
    socio_economic_class = factor(
      socio_economic_class,
      levels = c("SEC E", "SEC D", "SEC C", "SEC B", "SEC A"),
      ordered = TRUE
    ),

    # Household tenure
    tenure = case_when(
      str_starts(own_the_house_or_living_on_rent, "Yes") ~ "Owner",
      str_starts(own_the_house_or_living_on_rent, "No, I am") ~ "Renter",
      TRUE ~ NA_character_
    ) %>%
      factor()
  )


# ============================================================
# CONSUMPTION TRANSFORMATION
# ============================================================

hh <- hh %>%
  mutate(log_consumption = log1p(avg_monthly_consumption))

cat("Number of households:", nrow(hh), "\n")


# ============================================================
# 4.1 COMPARISON OF MEANS – GENDER OF HOUSEHOLD HEAD
# ============================================================

cat("\n==== 4.1 GENDER OF HOUSEHOLD HEAD ====\n")

print(hh %>% count(head_gender))

gender_variance_test <- var.test(avg_monthly_consumption ~ head_gender, data = hh)
print(gender_variance_test)

t_gender <- t.test(avg_monthly_consumption ~ head_gender, data = hh, var.equal = FALSE)
print(t_gender)

t_gender_log <- t.test(log_consumption ~ head_gender, data = hh, var.equal = FALSE)
print(t_gender_log)

wilcox_gender <- wilcox.test(avg_monthly_consumption ~ head_gender, data = hh)
print(wilcox_gender)

gender_summary <- hh %>%
  group_by(head_gender) %>%
  summarise(
    mean = mean(avg_monthly_consumption, na.rm = TRUE),
    median = median(avg_monthly_consumption, na.rm = TRUE),
    sd = sd(avg_monthly_consumption, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  )
print(gender_summary)


# ============================================================
# 4.2 COMPARISON OF VARIANCES – OWNERS VS RENTERS
# ============================================================

cat("\n==== 4.2 TENURE - VARIANCE COMPARISON ====\n")

print(hh %>% count(tenure))

tenure_data <- hh %>% filter(!is.na(tenure))

var_tenure <- var.test(avg_monthly_consumption ~ tenure, data = tenure_data)
print(var_tenure)

levene_tenure <- leveneTest(avg_monthly_consumption ~ tenure, data = tenure_data)
print(levene_tenure)


# ============================================================
# 4.3 COMPARISON OF PROPORTIONS – HIGH-CONSUMPTION OUTLIERS BY GENDER
# ============================================================

cat("\n==== 4.3 HIGH-CONSUMPTION OUTLIERS BY GENDER ====\n")

prop_table <- table(hh$head_gender, hh$consumption_outlier)
print(prop_table)

prop_table_percent <- prop.table(prop_table, margin = 1)
print(prop_table_percent)

outlier_by_gender <- prop_table[, "TRUE"]
n_by_gender        <- rowSums(prop_table)

prop_test_gender <- prop.test(x = outlier_by_gender, n = n_by_gender)
print(prop_test_gender)

chisq_gender <- chisq.test(prop_table)
print(chisq_gender)


# ============================================================
# 4.4 ANOVA – SOCIO-ECONOMIC CLASS
# ============================================================

cat("\n==== 4.4 SOCIO-ECONOMIC CLASS ====\n")

print(hh %>% count(socio_economic_class))

levene_sec <- leveneTest(avg_monthly_consumption ~ socio_economic_class, data = hh)
print(levene_sec)

anova_sec <- aov(avg_monthly_consumption ~ socio_economic_class, data = hh)
print(summary(anova_sec))

welch_sec <- oneway.test(avg_monthly_consumption ~ socio_economic_class, data = hh, var.equal = FALSE)
print(welch_sec)

kruskal_sec <- kruskal.test(avg_monthly_consumption ~ socio_economic_class, data = hh)
print(kruskal_sec)

games_howell_sec <- hh %>% games_howell_test(avg_monthly_consumption ~ socio_economic_class)
print(games_howell_sec)

sec_summary <- hh %>%
  group_by(socio_economic_class) %>%
  summarise(
    mean = mean(avg_monthly_consumption, na.rm = TRUE),
    median = median(avg_monthly_consumption, na.rm = TRUE),
    sd = sd(avg_monthly_consumption, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(socio_economic_class)
print(sec_summary)


# ============================================================
# 4.5 ANOVA – EDUCATION LEVEL OF HOUSEHOLD HEAD
# ============================================================

cat("\n==== 4.5 EDUCATION LEVEL OF HOUSEHOLD HEAD ====\n")

print(hh %>% count(head_education, sort = TRUE))

levene_edu <- leveneTest(avg_monthly_consumption ~ head_education, data = hh)
print(levene_edu)

anova_edu <- aov(avg_monthly_consumption ~ head_education, data = hh)
print(summary(anova_edu))

welch_edu <- oneway.test(avg_monthly_consumption ~ head_education, data = hh, var.equal = FALSE)
print(welch_edu)

kruskal_edu <- kruskal.test(avg_monthly_consumption ~ head_education, data = hh)
print(kruskal_edu)

games_howell_edu <- hh %>% games_howell_test(avg_monthly_consumption ~ head_education)
print(games_howell_edu)

education_summary <- hh %>%
  group_by(head_education) %>%
  summarise(
    mean = mean(avg_monthly_consumption, na.rm = TRUE),
    median = median(avg_monthly_consumption, na.rm = TRUE),
    sd = sd(avg_monthly_consumption, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(mean))
print(education_summary)


# ============================================================
# RESULTS SUMMARY
# ============================================================

results_summary <- tibble(
  test = c(
    "Welch t-test: gender",
    "Wilcoxon: gender",
    "Levene: tenure",
    "Proportion test: outlier rate by gender",
    "Chi-square: outlier rate by gender",
    "Standard ANOVA: socio-economic class",
    "Welch ANOVA: socio-economic class",
    "Kruskal-Wallis: socio-economic class",
    "Welch ANOVA: education",
    "Standard ANOVA: education",
    "Kruskal-Wallis: education"
  ),
  p_value = c(
    t_gender$p.value,
    wilcox_gender$p.value,
    levene_tenure[1, "Pr(>F)"],
    prop_test_gender$p.value,
    chisq_gender$p.value,
    summary(anova_sec)[[1]][["Pr(>F)"]][1],
    welch_sec$p.value,
    kruskal_sec$p.value,
    welch_edu$p.value,
    summary(anova_edu)[[1]][["Pr(>F)"]][1],
    kruskal_edu$p.value
  )
)
print(results_summary)


# ============================================================
# SAVE RESULTS — statistics/ and tables/
# ============================================================

write_csv(results_summary, file.path(stats_dir, "task4_results_summary.csv"))
write_csv(games_howell_sec, file.path(stats_dir, "task4_games_howell_sec.csv"))
write_csv(games_howell_edu, file.path(stats_dir, "task4_games_howell_education.csv"))

write_csv(gender_summary, file.path(tables_dir, "task4_gender_descriptive_statistics.csv"))
write_csv(sec_summary, file.path(tables_dir, "task4_sec_descriptive_statistics.csv"))
write_csv(education_summary, file.path(tables_dir, "task4_education_descriptive_statistics.csv"))


# ============================================================
# FIGURES — figures/
# ============================================================

sec_boxplot <- ggplot(hh, aes(x = socio_economic_class, y = avg_monthly_consumption)) +
  geom_boxplot(fill = "#4C72B0") +
  labs(title = "Monthly Consumption by Socio-Economic Class",
       x = "Socio-Economic Class", y = "Avg Monthly Consumption (kWh)") +
  theme_minimal()
ggsave(file.path(figures_dir, "consumption_by_sec_boxplot.png"),
       plot = sec_boxplot, width = 8, height = 5, dpi = 300)

gender_boxplot <- ggplot(hh, aes(x = head_gender, y = avg_monthly_consumption)) +
  geom_boxplot(fill = "#DD8452") +
  labs(title = "Monthly Consumption by Household Head Gender",
       x = "Gender", y = "Avg Monthly Consumption (kWh)") +
  theme_minimal()
ggsave(file.path(figures_dir, "consumption_by_gender_boxplot.png"),
       plot = gender_boxplot, width = 8, height = 5, dpi = 300)

education_boxplot <- ggplot(hh, aes(x = head_education, y = avg_monthly_consumption)) +
  geom_boxplot(fill = "#55A868") +
  labs(title = "Monthly Consumption by Education Level",
       x = "Education Level", y = "Avg Monthly Consumption (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))
ggsave(file.path(figures_dir, "consumption_by_education_boxplot.png"),
       plot = education_boxplot, width = 9, height = 5, dpi = 300)

cat("\n============================================\n")
cat("Task 4 results saved successfully to:\n")
cat(" -", tables_dir, "\n")
cat(" -", figures_dir, "\n")
cat(" -", stats_dir, "\n")
cat("============================================\n")
