#Install Dependencies
install.packages(c("tidyverse","janitor","reshape2","ggplot2"))

#Libraries
library(tidyverse)
library(janitor)
library(reshape2)
library(ggplot2)

#Import
raw_data <- read.csv("diabetes_dataset_with_notes.csv")

# 4. Data Cleaning

#Check Dataset
dim(raw_data)
glimpse(raw_data)
colSums(is.na(raw_data))

#Clean Dataset
clean_data <- raw_data %>%
  clean_names()

#Remove Duplicates
initial_rows <- nrow(clean_data)
clean_data <- clean_data %>%
  distinct()
removed_duplicates <- initial_rows - nrow(clean_data)
cat("Duplicates removed:", removed_duplicates, "\n")

#Standardize Categorical Variables
clean_data <- clean_data %>%
  mutate(
    gender          = as.factor(gender),
    smoking_history = as.factor(smoking_history),
    location        = as.factor(location),
    hypertension    = factor(hypertension, levels = c(0, 1), labels = c("No", "Yes")),
    heart_disease   = factor(heart_disease, levels = c(0, 1), labels = c("No", "Yes")),
    diabetes        = factor(diabetes, levels = c(0, 1), labels = c("Non-Diabetic", "Diabetic"))
  )

#Compacting Race Categories into one
clean_data <- clean_data %>%
  mutate(
    race = case_when(
      race_african_american == 1 ~ "African American",
      race_asian == 1            ~ "Asian",
      race_caucasian == 1        ~ "Caucasian",
      race_hispanic == 1         ~ "Hispanic",
      race_other == 1            ~ "Other",
      TRUE                       ~ "Unknown"
    ),
    race = as.factor(race)
  ) %>%
  # Drop the 5 old race columns
  select(-starts_with("race_"))

#Reformat Smoking History
clean_data <- clean_data %>%
  mutate(
    smoking_history = case_when(
      smoking_history %in% c("former", "not current", "ever") ~ "Former",
      smoking_history == "current"                             ~ "Current",
      smoking_history == "never"                               ~ "Never",
      smoking_history %in% c("No Info", "unknown") | is.na(smoking_history) ~ "Unknown",
      TRUE                                                     ~ smoking_history
    )
  )

#Check Missing Data
na_check <- colSums(is.na(clean_data))
print(na_check)

#Check if Values fall in Possible Ranges
summary(clean_data %>% select(age, bmi, hb_a1c_level, blood_glucose_level))

clean_data

# 5. Exploratory Data Analysis (EDA)

# Format all character columns: place race after location, capitalize words, and replace underscores with spaces
clean_data <- clean_data %>%
  relocate(race, .after = location) %>%
  mutate(
    across(where(is.character), ~ str_to_sentence(str_replace_all(., "_", " ")))
  )

#Check Missing Data
na_check <- colSums(is.na(clean_data))
print(na_check)

#Check if Values fall in Possible Ranges
summary(clean_data %>% select(age, bmi, hb_a1c_level, blood_glucose_level))

summary_stats <- clean_data %>%
  select(age, bmi, hb_a1c_level, blood_glucose_level) %>%
  summary()

# 6. Data Visualization
# Figure 1: Distribution of Blood Glucose Levels by Diabetes Status
ggplot(clean_data, aes(x = blood_glucose_level, fill = diabetes)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  labs(
    title = "Distribution of Blood Glucose Levels by Diabetes Status",
    subtitle = "Comparing blood glucose concentration between diabetic and non-diabetic patients",
    x = "Blood Glucose Level (mg/dL)",
    y = "Patient Count",
    fill = "Diabetes Status"
  ) +
  theme_minimal()

# Figure 2: Relationship Between BMI and Blood Glucose Levels colored by Diabetes Status
# Using base plot() as an alternative visualization method
plot(clean_data$bmi, clean_data$blood_glucose_level,
     col = ifelse(clean_data$diabetes == "Diabetic", "salmon", "skyblue"),
     pch = 16, cex = 0.6,
     main = "BMI vs. Blood Glucose Level by Diabetes Status",
     xlab = "Body Mass Index (BMI)",
     ylab = "Blood Glucose Level (mg/dL)")

legend("topright", legend = levels(clean_data$diabetes),
       col = c("skyblue", "salmon"), pch = 16)


# Figure 3: Distribution of Blood Glucose Levels by Heart Disease and Diabetes Status
ggplot(clean_data, aes(x = heart_disease, y = blood_glucose_level, fill = diabetes)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.8) +
  labs(
    title = "Blood Glucose Distribution Across Heart Disease and Diabetes Cohorts",
    subtitle = "Evaluating cardiovascular risk groups against glycemic levels",
    x = "History of Heart Disease",
    y = "Blood Glucose Level (mg/dL)",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  scale_fill_brewer(palette = "Set2")

text(x = c(0.7, 1.9), y = diabetes_counts + 3000, 
     labels = diabetes_counts, font = 2)

# 7. Statistical Analysis/Predictive Modeling

diabetes_model <- glm(
  diabetes ~ age +
    bmi +
    hb_a1c_level +
    blood_glucose_level +
    hypertension +
    heart_disease +
    smoking_history,
  data = clean_data,
  family = binomial()
)

summary(diabetes_model)

# Contingency table and percentage proportions
smoking_diabetes_table <- table(clean_data$smoking_history, clean_data$diabetes)
prop_table <- prop.table(smoking_diabetes_table, margin = 1) * 100

print("Smoking History vs. Diabetes Counts:")
print(smoking_diabetes_table)

print("Smoking History vs. Diabetes Prevalence (%):")
print(prop_table)

# Chi-Square Test of Independence
chi_sq_result <- chisq.test(smoking_diabetes_table)
print(chi_sq_result)

# Visualization: Proportional bar chart of diabetes status across smoking categories
ggplot(clean_data, aes(x = smoking_history, fill = diabetes)) +
  geom_bar(position = "fill", alpha = 0.8) +
  labs(
    title = "Diabetes Prevalence by Smoking History",
    subtitle = "Proportion of diabetic vs. non-diabetic patients across smoking categories",
    x = "Smoking History",
    y = "Proportion",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2")

# Visualization: Proportional bar chart of diabetes status across smoking categories
ggplot(clean_data, aes(x = smoking_history, fill = diabetes)) +
  geom_bar(position = "fill", alpha = 0.8) +
  labs(
    title = "Diabetes Prevalence by Smoking History",
    subtitle = "Proportion of diabetic vs. non-diabetic patients across smoking categories",
    x = "Smoking History",
    y = "Proportion",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2")

# 1. STANDALONE HbA1c DISTRIBUTION BY DIABETES STATUS
ggplot(clean_data, aes(x = hb_a1c_level, fill = diabetes)) +
  geom_histogram(bins = 25, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("Non-Diabetic" = "#66c2a5", "Diabetic" = "#fc8d62")) +
  labs(
    title = "Distribution of HbA1c Levels by Diabetes Status",
    subtitle = "Comparing glycated hemoglobin concentration between diabetic and non-diabetic cohorts",
    x = "HbA1c Level (%)",
    y = "Patient Count",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))


# 2A. DIABETES PREVALENCE BY HYPERTENSION STATUS (PROPORTION BAR PLOT)
# (Visualizes the high comorbidity effect of hypertension)
ggplot(clean_data, aes(x = hypertension, fill = diabetes)) +
  geom_bar(position = "fill", alpha = 0.85, width = 0.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(values = c("Non-Diabetic" = "#66c2a5", "Diabetic" = "#fc8d62")) +
  labs(
    title = "Diabetes Prevalence by Hypertension Status",
    subtitle = "Proportion of diabetic vs. non-diabetic patients across hypertension groups",
    x = "History of Hypertension",
    y = "Proportion",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))


# 2B. BLOOD GLUCOSE DISTRIBUTION ACROSS HYPERTENSION COHORTS (BOXPLOT)
# (Direct companion to Figure 9 for Heart Disease)
ggplot(clean_data, aes(x = hypertension, y = blood_glucose_level, fill = diabetes)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.8) +
  scale_fill_manual(values = c("Non-Diabetic" = "#66c2a5", "Diabetic" = "#fc8d62")) +
  labs(
    title = "Blood Glucose Distribution Across Hypertension Cohorts",
    subtitle = "Evaluating blood pressure comorbidity against glycemic levels",
    x = "History of Hypertension",
    y = "Blood Glucose Level (mg/dL)",
    fill = "Diabetes Status"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))


# 3. LOGISTIC REGRESSION COEFFICIENT PLOT (FOREST PLOT)
# (Visually displays all Beta coefficients to show effect sizes on Slide 17)
coef_df <- as.data.frame(summary(diabetes_model)$coefficients)
coef_df$Variable <- rownames(coef_df)
colnames(coef_df) <- c("Estimate", "StdError", "z_value", "p_value", "Variable")

# Remove Intercept for clearer comparison across predictors
coef_plot_data <- coef_df %>% 
  filter(Variable != "(Intercept)") %>%
  mutate(Variable = factor(Variable, levels = Variable[order(Estimate)]))

ggplot(coef_plot_data, aes(x = Estimate, y = Variable)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(color = "#2b5c8f", size = 3.5) +
  geom_errorbarh(aes(xmin = Estimate - (1.96 * StdError), 
                     xmax = Estimate + (1.96 * StdError)), 
                 height = 0.25, color = "#2b5c8f", linewidth = 0.8) +
  labs(
    title = "Predictor Effect Sizes in Logistic Regression",
    subtitle = "Log-odds estimates (Beta) with 95% Confidence Intervals",
    x = "Coefficient Estimate (Log-Odds)",
    y = "Predictor Variable"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text = element_text(size = 10)
  )
