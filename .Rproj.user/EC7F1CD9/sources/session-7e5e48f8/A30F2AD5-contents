# Dependencies & Libraries
install.packages(c("tidyverse", "janitor", "reshape2", "ggplot2", "caret", "pROC", "car", "shiny", "plotly"))

library(tidyverse)
library(janitor)
library(reshape2)
library(ggplot2)
library(caret)
library(pROC)
library(car)
library(shiny)
library(plotly)

# --- Data Cleaning ---

# Modular function to clean and prepare the raw dataset
clean_diabetes_data <- function(raw_df) {
  
  # Standardize column headers and remove duplicate rows
  initial_rows <- nrow(raw_df)
  
  df_clean <- raw_df %>%
    clean_names() %>%
    distinct()
  
  removed_duplicates <- initial_rows - nrow(df_clean)
  cat("Duplicates removed:", removed_duplicates, "\n")
  
  # Compact 5 race columns into 1, standardize smoking history, and create comorbidity profile
  df_clean <- df_clean %>%
    mutate(
      race = case_when(
        race_african_american == 1 ~ "African American",
        race_asian == 1            ~ "Asian",
        race_caucasian == 1        ~ "Caucasian",
        race_hispanic == 1         ~ "Hispanic",
        race_other == 1            ~ "Other",
        TRUE                       ~ "Unknown"
      ),
      smoking_history = case_when(
        smoking_history %in% c("former", "not current", "ever")               ~ "Former",
        smoking_history == "current"                                          ~ "Current",
        smoking_history == "never"                                            ~ "Never",
        smoking_history %in% c("No Info", "unknown") | is.na(smoking_history) ~ "Unknown",
        TRUE                                                                  ~ smoking_history
      ),
      comorbidity_profile = case_when(
        hypertension == 0 & heart_disease == 0 ~ "None",
        hypertension == 1 & heart_disease == 0 ~ "Hypertension Only",
        hypertension == 0 & heart_disease == 1 ~ "Heart Disease Only",
        hypertension == 1 & heart_disease == 1 ~ "Both Comorbidities"
      ),
      age_group = cut(
        age,
        breaks = c(0, 18, 35, 50, 65, 100),
        labels = c("0-18", "19-35", "36-50", "51-65", "66+"),
        include.lowest = TRUE
      )
    ) %>%
    select(-starts_with("race_")) %>%
    relocate(race, .after = location)
  
  # Convert text and binary columns to factors
  df_clean <- df_clean %>%
    mutate(
      gender              = as.factor(gender),
      location            = as.factor(location),
      race                = as.factor(race),
      smoking_history     = factor(smoking_history, levels = c("Never", "Former", "Current", "Unknown")),
      hypertension        = factor(hypertension, levels = c(0, 1), labels = c("No", "Yes")),
      heart_disease       = factor(heart_disease, levels = c(0, 1), labels = c("No", "Yes")),
      diabetes            = factor(diabetes, levels = c(0, 1), labels = c("Non-Diabetic", "Diabetic")),
      comorbidity_profile = factor(comorbidity_profile, levels = c("None", "Hypertension Only", "Heart Disease Only", "Both Comorbidities"))
    )
  
  return(df_clean)
}

# Import raw dataset and inspect structure
raw_data <- read.csv("diabetes_dataset_with_notes.csv", stringsAsFactors = FALSE)
dim(raw_data)
str(raw_data)
glimpse(raw_data)
colSums(is.na(raw_data))

# Run the cleaning function
clean_data <- clean_diabetes_data(raw_data)

# Verify missing values and cleaned structure
na_check <- colSums(is.na(clean_data))
print(na_check)
str(clean_data)

# Save cleaned dataset as optimized RDS file
saveRDS(clean_data, "cleaned_diabetes_data.rds")


# --- Exploratory Data Analysis (EDA) ---

# Descriptive statistics for continuous clinical variables
summary_stats <- clean_data %>%
  select(age, bmi, hb_a1c_level, blood_glucose_level) %>%
  summary()
print(summary_stats)

# Target variable class balance
table(clean_data$diabetes)
round(prop.table(table(clean_data$diabetes)), 3)

# Group summary by diabetes status
cohort_summary <- clean_data %>%
  group_by(diabetes) %>%
  summarise(
    n            = n(),
    avg_age      = mean(age),
    avg_bmi      = mean(bmi),
    avg_hba1c    = mean(hb_a1c_level),
    avg_glucose  = mean(blood_glucose_level)
  )
print(cohort_summary)

# Correlation matrix of numeric variables
cor_matrix <- cor(clean_data %>% select(age, bmi, hb_a1c_level, blood_glucose_level))
round(cor_matrix, 3)

# Categorical frequency tables for Appendix B
race_freq <- clean_data %>%
  count(race, name = "n") %>%
  mutate(percentage = round((n / sum(n)) * 100, 2))
print(race_freq)

smoking_freq <- clean_data %>%
  count(smoking_history, name = "n") %>%
  mutate(percentage = round((n / sum(n)) * 100, 2))
print(smoking_freq)


# --- Data Visualization ---

# Figure 1: Distribution of Blood Glucose Levels by Diabetes Status
ggplot(clean_data, aes(x = blood_glucose_level, fill = diabetes)) +
  geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("Non-Diabetic" = "#66c2a5", "Diabetic" = "#fc8d62")) +
  labs(
    title = "Distribution of Blood Glucose Levels by Diabetes Status",
    subtitle = "Comparing blood glucose concentration between diabetic and non-diabetic patients",
    x = "Blood Glucose Level (mg/dL)",
    y = "Patient Count",
    fill = "Diabetes Status"
  ) +
  theme_minimal()

# Figure 2: Age vs. Mean BMI with Linear Regression Trend Line
age_bmi_summary <- clean_data %>%
  group_by(age, diabetes) %>%
  summarise(mean_bmi = mean(bmi), .groups = "drop")

ggplot(age_bmi_summary, aes(x = age, y = mean_bmi, color = diabetes)) +
  geom_point(alpha = 0.7, size = 2) +
  geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
  scale_color_manual(values = c("Non-Diabetic" = "steelblue", "Diabetic" = "darkred")) +
  labs(
    title = "Age vs. Mean BMI by Diabetes Status",
    subtitle = "Scatterplot and linear regression trend line across patient age (0 to 80 years)",
    x = "Age (Years)",
    y = "Mean Body Mass Index (BMI)",
    color = "Diabetes Status"
  ) +
  theme_minimal()

# Figure 3: Diabetes Prevalence Across Cardiovascular Comorbidity Profiles
comorbidity_summary <- clean_data %>%
  group_by(comorbidity_profile) %>%
  summarise(
    total_patients = n(),
    diabetic_cases = sum(diabetes == "Diabetic"),
    prevalence_pct = round((diabetic_cases / total_patients) * 100, 2)
  )
print(comorbidity_summary)

ggplot(comorbidity_summary, aes(x = comorbidity_profile, y = prevalence_pct, fill = comorbidity_profile)) +
  geom_col(width = 0.6, alpha = 0.85) +
  geom_text(aes(label = paste0(prevalence_pct, "%")), vjust = -0.5, fontface = "bold") +
  scale_fill_brewer(palette = "Set2") +
  ylim(0, 45) +
  labs(
    title = "Diabetes Prevalence Across Cardiovascular Comorbidity Profiles",
    subtitle = "Percentage of patients diagnosed with diabetes by hypertension and heart disease status",
    x = "Comorbidity Profile",
    y = "Diabetes Prevalence (%)",
    fill = "Comorbidity Profile"
  ) +
  theme_minimal()

# Additional Visualizations (Included in Appendix B)

# Extra 1: HbA1c Distribution by Diabetes Status
ggplot(clean_data, aes(x = hb_a1c_level, fill = diabetes)) +
  geom_histogram(bins = 25, alpha = 0.7, position = "identity") +
  scale_fill_manual(values = c("Non-Diabetic" = "#66c2a5", "Diabetic" = "#fc8d62")) +
  labs(
    title = "Distribution of HbA1c Levels by Diabetes Status",
    subtitle = "Comparing glycated hemoglobin concentration between cohorts",
    x = "HbA1c Level (%)",
    y = "Patient Count",
    fill = "Diabetes Status"
  ) +
  theme_minimal()

# Extra 2: Age Distribution Across Smoking History and Diabetes Status
ggplot(clean_data, aes(x = smoking_history, y = age, fill = diabetes)) +
  geom_boxplot(alpha = 0.8, outlier.size = 0.8) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Patient Age Distribution Across Smoking History and Diabetes Status",
    subtitle = "Comparing age quartiles across smoking and diabetes groups",
    x = "Smoking History",
    y = "Age (Years)",
    fill = "Diabetes Status"
  ) +
  theme_minimal()

# Extra 3: Proportional Bar Chart of Diabetes by Smoking History
ggplot(clean_data, aes(x = smoking_history, fill = diabetes)) +
  geom_bar(position = "fill", alpha = 0.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Diabetes Prevalence by Smoking History",
    subtitle = "Proportion of diabetic vs. non-diabetic patients across smoking categories",
    x = "Smoking History",
    y = "Proportion",
    fill = "Diabetes Status"
  ) +
  theme_minimal()

# Extra 4: Correlation Heatmap
melted_cor <- melt(round(cor_matrix, 3))
ggplot(melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), color = "black", fontface = "bold") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, limit = c(-1, 1), name = "Pearson\nCorrelation") +
  labs(
    title = "Correlation Heatmap of Continuous Variables",
    subtitle = "Evaluating linear relationships among clinical metrics",
    x = "",
    y = ""
  ) +
  theme_minimal()


# --- 7. Statistical Analysis / Predictive Modeling ---

# Contingency table and Chi-Square test for Smoking History vs. Diabetes
smoking_diabetes_table <- table(clean_data$smoking_history, clean_data$diabetes)
prop_table <- round(prop.table(smoking_diabetes_table, margin = 1) * 100, 2)
print(smoking_diabetes_table)
print(prop_table)
print(chisq.test(smoking_diabetes_table))

# 1. Fit Logistic Regression on Full Cleaned Dataset
diabetes_model <- glm(
  diabetes ~ age +
    bmi +
    hb_a1c_level +
    blood_glucose_level +
    hypertension +
    heart_disease +
    smoking_history,
  data = clean_data,
  family = binomial
)
summary(diabetes_model)

# Exponentiate coefficients to get Odds Ratios
round(exp(coef(diabetes_model)), 3)

# Check Multicollinearity using VIF
vif(diabetes_model)

# 2. Train/Test Split & Model Evaluation
set.seed(123)
idx   <- createDataPartition(clean_data$diabetes, p = 0.70, list = FALSE)
train <- clean_data[idx, ]
test  <- clean_data[-idx, ]

logit_fit <- glm(
  diabetes ~ age + bmi + hb_a1c_level + blood_glucose_level +
    hypertension + heart_disease + smoking_history,
  data = train,
  family = binomial
)

# Predicted probabilities and 0.5 cut-off class predictions on test set
logit_prob <- predict(logit_fit, newdata = test, type = "response")
logit_pred <- factor(ifelse(logit_prob > 0.5, "Diabetic", "Non-Diabetic"),
                     levels = c("Non-Diabetic", "Diabetic"))

# Confusion matrix and accuracy/precision/recall metrics
cm <- confusionMatrix(logit_pred, test$diabetes, positive = "Diabetic")
print(cm)

# ROC curve and AUC score
roc_obj <- roc(test$diabetes, logit_prob, levels = c("Non-Diabetic", "Diabetic"), quiet = TRUE)
cat("Test Set AUC:", round(as.numeric(auc(roc_obj)), 4), "\n")
plot(roc_obj, col = "blue", lwd = 2, main = "ROC Curve: Logistic Regression on Test Set")

# Forest Plot of Predictor Effect Sizes
coef_df <- as.data.frame(summary(diabetes_model)$coefficients)
coef_df$Variable <- rownames(coef_df)
colnames(coef_df) <- c("Estimate", "StdError", "z_value", "p_value", "Variable")

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
  theme_minimal()


# --- 8. Interactive Dashboard (Shiny App) ---

# Load optimized RDS dataset
dataset_file <- "cleaned_diabetes_data.rds"
if (file.exists(dataset_file)) {
  diabetes_df <- readRDS(dataset_file)
} else {
  diabetes_df <- clean_data
}

gender_choices  <- c("All", sort(unique(as.character(diabetes_df$gender))))
race_choices    <- c("All", sort(unique(as.character(diabetes_df$race))))
smoking_choices <- c("All", levels(diabetes_df$smoking_history))
binary_choices  <- c("All", "No", "Yes")

ui <- fluidPage(
  titlePanel("Diabetes Risk Analytics Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "chart_type",
        "Select Graph:",
        choices = c(
          "Fig 1: Blood Glucose Histogram"      = "glucose_hist",
          "Fig 2: Age vs. Mean BMI Trend"       = "age_bmi_trend",
          "Fig 3: Comorbidity Prevalence"       = "comorbidity_bar",
          "Extra: HbA1c Histogram"              = "hba1c_hist",
          "Extra: Age by Smoking Boxplot"       = "smoking_box",
          "Extra: BMI vs. Glucose Scatter"      = "bmi_glucose_scatter"
        )
      ),
      hr(),
      sliderInput("age_range", "Select Age Range:", min = 0, max = 80, value = c(0, 80)),
      selectInput("gender_select", "Select Gender:", choices = gender_choices),
      selectInput("race_select", "Select Race:", choices = race_choices),
      selectInput("smoking_select", "Select Smoking History:", choices = smoking_choices),
      selectInput("hyp_select", "Select Hypertension:", choices = binary_choices),
      selectInput("hd_select", "Select Heart Disease:", choices = binary_choices)
    ),
    
    mainPanel(
      fluidRow(
        column(6, wellPanel(h4("Total Patients"), h2(textOutput("total_patients_text"), style = "color: #2b5c8f; font-weight: bold;"))),
        column(6, wellPanel(h4("Diabetes Prevalence"), h2(textOutput("prevalence_text"), style = "color: #d95f02; font-weight: bold;")))
      ),
      fluidRow(
        column(12, plotlyOutput("dashboard_plot", height = "480px"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  filtered_data <- reactive({
    df <- diabetes_df %>%
      filter(age >= input$age_range[1], age <= input$age_range[2])
    
    if (input$gender_select != "All")  df <- df %>% filter(as.character(gender) == input$gender_select)
    if (input$race_select != "All")    df <- df %>% filter(as.character(race) == input$race_select)
    if (input$smoking_select != "All") df <- df %>% filter(as.character(smoking_history) == input$smoking_select)
    if (input$hyp_select != "All")     df <- df %>% filter(as.character(hypertension) == input$hyp_select)
    if (input$hd_select != "All")      df <- df %>% filter(as.character(heart_disease) == input$hd_select)
    
    return(df)
  })
  
  output$total_patients_text <- renderText({
    format(nrow(filtered_data()), big.mark = ",")
  })
  
  output$prevalence_text <- renderText({
    df <- filtered_data()
    if (nrow(df) == 0) return("0.0%")
    paste0(round(mean(df$diabetes == "Diabetic") * 100, 2), "%")
  })
  
  output$dashboard_plot <- renderPlotly({
    df <- filtered_data()
    if (nrow(df) == 0) {
      return(plot_ly() %>% layout(title = "No data available for selected filters"))
    }
    
    if (input$chart_type == "glucose_hist") {
      p <- ggplot(df, aes(x = blood_glucose_level, fill = diabetes)) +
        geom_histogram(bins = 30, alpha = 0.7, position = "identity") +
        scale_fill_manual(values = c("Non-Diabetic" = "#66c2a5", "Diabetic" = "#fc8d62")) +
        labs(title = "Distribution of Blood Glucose Levels", x = "Blood Glucose Level (mg/dL)", y = "Patient Count") +
        theme_minimal()
      return(ggplotly(p))
      
    } else if (input$chart_type == "age_bmi_trend") {
      age_df <- df %>%
        group_by(age, diabetes) %>%
        summarise(mean_bmi = mean(bmi), .groups = "drop")
      
      p <- ggplot(age_df, aes(x = age, y = mean_bmi, color = diabetes)) +
        geom_point(alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE) +
        scale_color_manual(values = c("Non-Diabetic" = "steelblue", "Diabetic" = "darkred")) +
        labs(title = "Age vs. Mean BMI by Diabetes Status", x = "Age (Years)", y = "Mean BMI") +
        theme_minimal()
      return(ggplotly(p))
      
    } else if (input$chart_type == "comorbidity_bar") {
      comorb_df <- df %>%
        group_by(comorbidity_profile) %>%
        summarise(prevalence_pct = round(mean(diabetes == "Diabetic") * 100, 2), .groups = "drop")
      
      p <- ggplot(comorb_df, aes(x = comorbidity_profile, y = prevalence_pct, fill = comorbidity_profile)) +
        geom_col(width = 0.6) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Diabetes Prevalence Across Comorbidity Profiles", x = "Comorbidity Profile", y = "Prevalence (%)") +
        theme_minimal()
      return(ggplotly(p))
      
    } else if (input$chart_type == "hba1c_hist") {
      p <- ggplot(df, aes(x = hb_a1c_level, fill = diabetes)) +
        geom_histogram(bins = 25, alpha = 0.7, position = "identity") +
        scale_fill_manual(values = c("Non-Diabetic" = "#66c2a5", "Diabetic" = "#fc8d62")) +
        labs(title = "Distribution of HbA1c Levels", x = "HbA1c Level (%)", y = "Patient Count") +
        theme_minimal()
      return(ggplotly(p))
      
    } else if (input$chart_type == "smoking_box") {
      p <- ggplot(df, aes(x = smoking_history, y = age, fill = diabetes)) +
        geom_boxplot(alpha = 0.8) +
        scale_fill_brewer(palette = "Set2") +
        labs(title = "Age Distribution Across Smoking History", x = "Smoking History", y = "Age (Years)") +
        theme_minimal()
      return(ggplotly(p))
      
    } else if (input$chart_type == "bmi_glucose_scatter") {
      p <- ggplot(df, aes(x = bmi, y = blood_glucose_level, color = diabetes)) +
        geom_point(alpha = 0.5, size = 1.5) +
        scale_color_manual(values = c("Non-Diabetic" = "#66c2a5", "Diabetic" = "#fc8d62")) +
        labs(title = "BMI vs. Blood Glucose Level", x = "Body Mass Index (BMI)", y = "Blood Glucose Level (mg/dL)") +
        theme_minimal()
      return(ggplotly(p) %>% toWebGL())
    }
  })
}

shinyApp(ui, server)