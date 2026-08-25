install.packages(c("tidyverse","skimr","janitor","corrplot","scales"))

library(tidyverse)   # dplyr, ggplot2, readr, stringr, tidyr
library(skimr)       # quick summary tables
library(janitor)     # clean_names()
library(corrplot)    # correlation heatmap
library(scales)      # nicer axis labels

setwd("D:/Notes/3Y/Semester 1/SM/Assignment/datasets")

consumption_raw   <- read_csv("monthly_consumption.csv", col_types = cols(.default = "c")) %>% clean_names()
demographics_raw  <- read_csv("w1_demographics.csv",     col_types = cols(.default = "c")) %>% clean_names()

consumption <- consumption_raw %>%
  mutate(
    consumption_raw_text = consumption,                        
    consumption = as.numeric(str_remove_all(consumption, ",")),
    month = as.Date(month)
  )

n_missing_before <- sum(is.na(consumption_raw$consumption) | consumption_raw$consumption == "")
n_missing_after  <- sum(is.na(consumption$consumption))
cat("Missing consumption values - before cleaning:", n_missing_before, "\n")
cat("Missing consumption values - after cleaning: ", n_missing_after, "\n")

n_total_rows   <- nrow(consumption)
n_zero_rows    <- sum(consumption$consumption == 0, na.rm = TRUE)
n_missing_rows <- sum(is.na(consumption$consumption))

cat("\n--- Consumption data quality summary ---\n")
cat("Total household-month records:", n_total_rows, "\n")
cat("Zero-consumption records:     ", n_zero_rows, " (",
    round(100 * n_zero_rows / n_total_rows, 2), "%)\n", sep = "")
cat("Genuinely missing records:    ", n_missing_rows, " (",
    round(100 * n_missing_rows / n_total_rows, 2), "%)\n", sep = "")

zero_concentration <- consumption %>%
  filter(consumption == 0) %>%
  count(household_id, sort = TRUE)

all_zero_households <- consumption %>%
  group_by(household_id) %>%
  summarise(all_zero = all(consumption == 0, na.rm = TRUE), .groups = "drop") %>%
  filter(all_zero)

cat("\nHouseholds with zero consumption in ALL recorded months:",
    nrow(all_zero_households), "\n")

cat("\nHouseholds with at least one zero-consumption month:", nrow(zero_concentration), "\n")
cat("Max zero-months recorded by a single household:", max(zero_concentration$n), "\n")
print(head(zero_concentration, 10))

appliances_raw <- read_csv("w1_appliances.csv", col_types = cols(.default = "c")) %>% clean_names()
housing_raw    <- read_csv("w1_household_information_and_history.csv", col_types = cols(.default = "c")) %>% clean_names()

appliances <- appliances_raw %>%
  mutate(no_of_hours_used_during_last_week = as.numeric(no_of_hours_used_during_last_week))

housing <- housing_raw %>%
  mutate(
    floor_area = as.numeric(floor_area),
    total_monthly_expenditure_of_last_month = as.numeric(total_monthly_expenditure_of_last_month),
    no_of_electricity_meters = as.numeric(no_of_electricity_meters)
  )

demographics <- demographics_raw %>%
  mutate(
    age = as.numeric(age),
    no_of_hours_stayed_at_home_during_last_week =
      as.numeric(no_of_hours_stayed_at_home_during_last_week)
  )

cat("\n--- Demographics structural missingness (expected, not an error) ---\n")
cat("main_occupation missing:", sum(is.na(demographics$main_occupation)),
    " (", round(100*mean(is.na(demographics$main_occupation)),1), "%)\n", sep = "")

household_composition <- demographics %>%
  group_by(household_id) %>%
  summarise(
    household_size        = n(),
    average_age            = mean(age, na.rm = TRUE),
    median_age             = median(age, na.rm = TRUE),
    youngest_age            = min(age, na.rm = TRUE),
    oldest_age               = max(age, na.rm = TRUE),
    average_hours_at_home  = mean(no_of_hours_stayed_at_home_during_last_week, na.rm = TRUE),
    employed_members       = sum(main_activity_engaged_in ==
                                   "Engaged in economic activity/ currently employed/ engaged in own business",
                                 na.rm = TRUE),
    male_members            = sum(gender == "Male", na.rm = TRUE),
    female_members          = sum(gender == "Female", na.rm = TRUE),
    .groups = "drop"
  )

household_head <- demographics %>%
  filter(relationship_to_the_head_of_household == "Head of the household") %>%
  select(
    household_id,
    head_gender    = gender,
    head_ethnicity = ethnicity,
    head_religion  = religion,
    head_marital_status = marital_status,
    head_education = highest_level_of_education,
    head_occupation = main_occupation,
    head_employment_status = employment_status_of_the_main_occupation
  )

stopifnot(!any(duplicated(household_head$household_id)))

n_households_total <- n_distinct(demographics$household_id)
n_households_with_head <- n_distinct(household_head$household_id)
cat("\nHouseholds total:", n_households_total, "\n")
cat("Households with an identifiable head:", n_households_with_head, "\n")
cat("Households WITHOUT a head record (will be excluded):",
    n_households_total - n_households_with_head, "\n")

key_appliances <- c(
  "Refrigerator", "Washing Machine",
  "Geyser / Hot water systems for bathrooms which operate from electricity",
  "Electric water heater to heat water for drinking purposes",
  "Electric Oven", "Microwave",
  "Electric Iron including electric steam iron",
  "Electric Water pump", "Air fryer",
  "Electric cook tops (induction cookers, Infra-red cookers, hot plates)"
)

appliance_flags <- appliances %>%
  filter(appliance_type %in% key_appliances) %>%
  mutate(owns = 1) %>%
  distinct(household_id, appliance_type, owns) %>%
  pivot_wider(names_from = appliance_type, values_from = owns, values_fill = 0) %>%
  clean_names()   # turns long appliance names into valid, snake_case column names

appliance_totals <- appliances %>%
  group_by(household_id) %>%
  summarise(
    total_appliances   = n(),
    total_weekly_hours = sum(no_of_hours_used_during_last_week, na.rm = TRUE),
    .groups = "drop"
  )

household_appliances <- appliance_totals %>%
  left_join(appliance_flags, by = "household_id")

cat("\nHouseholds missing from appliance survey:",
    n_households_total - n_distinct(appliances$household_id), "\n")

household_housing <- housing %>%
  select(
    household_id,
    floor_area,
    no_of_electricity_meters,
    own_the_house_or_living_on_rent,
    type_of_house,
    socio_economic_class,
    total_monthly_expenditure_of_last_month,
    main_material_used_for_walls_of_the_house,
    main_material_used_for_roof_of_the_house,
    type_of_electricity_meter
  )

household_consumption <- consumption %>%
  filter(!household_id %in% all_zero_households$household_id) %>%
  group_by(household_id) %>%
  summarise(
    avg_monthly_consumption    = mean(consumption, na.rm = TRUE),
    median_monthly_consumption = median(consumption, na.rm = TRUE),
    sd_monthly_consumption     = sd(consumption, na.rm = TRUE),
    n_months_recorded          = n(),
    n_valid_months             = sum(!is.na(consumption)),
    n_zero_months               = sum(consumption == 0, na.rm = TRUE),
    .groups = "drop"
  )

household_level_data <- household_consumption %>%
  inner_join(household_composition, by = "household_id") %>%
  inner_join(household_head, by = "household_id") %>%
  left_join(household_appliances, by = "household_id") %>%
  left_join(household_housing, by = "household_id") %>%
  mutate(missing_appliance_survey = is.na(total_appliances)) %>%
  mutate(across(c(total_appliances, total_weekly_hours,
                  ends_with("_machine"), ends_with("_pump"), ends_with("_oven"),
                  ends_with("_fryer"), contains("microwave"), ends_with("_iron"),
                  contains("refrigerator"), contains("geyser"), contains("cook_tops"),
                  contains("water_heater")),
                ~replace_na(., 0)))

cat("Final rows after excluding all-zero households:", nrow(household_level_data), "\n")
cat("Columns:", ncol(household_level_data), "\n")
cat("Households missing appliance survey (flagged, not dropped):",
    sum(household_level_data$missing_appliance_survey), "\n")
cat("Households missing housing survey:",
    sum(is.na(household_level_data$floor_area)), "\n")

q1  <- quantile(household_level_data$avg_monthly_consumption, 0.25)
q3  <- quantile(household_level_data$avg_monthly_consumption, 0.75)
iqr <- q3 - q1
lower_fence <- q1 - 1.5 * iqr
upper_fence <- q3 + 1.5 * iqr

household_level_data <- household_level_data %>%
  mutate(
    consumption_outlier = avg_monthly_consumption < lower_fence |
      avg_monthly_consumption > upper_fence
  )

cat("\n--- Outlier detection (IQR method) ---\n")
cat("Lower fence:", round(lower_fence, 1), " | Upper fence:", round(upper_fence, 1), "\n")
cat("Number of outlier households:", sum(household_level_data$consumption_outlier),
    " (", round(100*mean(household_level_data$consumption_outlier), 2), "%)\n", sep = "")

household_level_data %>%
  select(avg_monthly_consumption, household_size, average_age,
         employed_members, average_hours_at_home) %>%
  skim()

household_level_data %>% count(head_gender, sort = TRUE)
household_level_data %>% count(head_ethnicity, sort = TRUE)
household_level_data %>% count(head_education, sort = TRUE)

ggplot(household_level_data, aes(x = avg_monthly_consumption)) +
  geom_histogram(bins = 40, fill = "#2c7fb8", colour = "white") +
  labs(title = "Distribution of Average Monthly Household Electricity Consumption",
       x = "Average monthly consumption (kWh)", y = "Number of households") +
  theme_minimal()

ggplot(household_level_data, aes(x = head_gender, y = avg_monthly_consumption, fill = head_gender)) +
  geom_boxplot() +
  scale_fill_manual(values = c(Female = "#f4a6a6", Male = "#a6c8f4")) +
  labs(title = "Average Household Consumption by Gender of Head",
       x = "Head of household - gender", y = "Average monthly consumption (kWh)") +
  theme_minimal() +
  theme(legend.position = "none")

ggplot(household_level_data, aes(x = reorder(head_education, avg_monthly_consumption, median),
                                 y = avg_monthly_consumption)) +
  geom_boxplot(fill = "#74c476") +
  coord_flip() +
  labs(title = "Average Household Consumption by Head's Education Level",
       x = NULL, y = "Average monthly consumption (kWh)") +
  theme_minimal()

numeric_vars <- household_level_data %>%
  select(avg_monthly_consumption, household_size, average_age,
         employed_members, average_hours_at_home,
         total_appliances, total_weekly_hours,
         floor_area, total_monthly_expenditure_of_last_month)
corrplot(cor(numeric_vars, use = "complete.obs"), method = "color",
         addCoef.col = "black", tl.col = "black", number.cex = 0.7)

ggplot(household_level_data %>% filter(!is.na(socio_economic_class)),
       aes(x = socio_economic_class, y = avg_monthly_consumption, fill = socio_economic_class)) +
  geom_boxplot() +
  labs(title = "Average Household Consumption by Socio-Economic Class",
       x = "Socio-economic class", y = "Average monthly consumption (kWh)") +
  theme_minimal() + theme(legend.position = "none")

monthly_trend <- consumption %>%
  group_by(month) %>%
  summarise(total_consumption = sum(consumption, na.rm = TRUE),
            avg_consumption   = mean(consumption, na.rm = TRUE))

ggplot(monthly_trend, aes(x = month, y = avg_consumption)) +
  geom_line(colour = "#2c7fb8", linewidth = 1) +
  geom_point(colour = "#2c7fb8") +
  scale_x_date(date_labels = "%b %Y", date_breaks = "3 months") +
  labs(title = "Average Household Electricity Consumption Over Time (Oct 2022 - Oct 2024)",
       x = NULL, y = "Average consumption (kWh)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

write_csv(household_level_data, "household_level_data.csv")

cleaned_monthly_panel <- consumption %>%
  select(household_id, month, consumption)
write_csv(cleaned_monthly_panel, "cleaned_monthly_panel.csv")

wide_panel <- cleaned_monthly_panel %>%
  pivot_wider(names_from = month, values_from = consumption)
write_csv(wide_panel, "monthly_consumption_wide.csv")

cat("\nSaved: household_level_data.csv, cleaned_monthly_panel.csv, monthly_consumption_wide.csv\n")