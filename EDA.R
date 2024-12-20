library(tidyverse)
library(janitor)
library(ggplot2)
library(GGally)
library(dplyr)
library(gridExtra)

## 1
## load the data
project_2_data = 
  read_csv("Project_2_data.csv", na = c("NA", "", ".")) |>
  janitor::clean_names() 
view(project_2_data)

## Summary statistics for numeric variables
numerical_vars <- project_2_data[c("age","tumor_size","regional_node_examined","reginol_node_positive","survival_months")]
summary(numerical_vars)

## Summary for categorical variables
project_2_data %>%
  select(where(~!is.numeric(.))) %>%
  pivot_longer(everything(), names_to = "variable", values_to = "category") %>%
  count(variable, category) %>%
  group_by(variable) %>%
  mutate(proportion = n / sum(n)) %>%
  ungroup() %>%
  print()


## 2
# Distribution of Survival Months
ggplot(project_2_data, aes(x = survival_months)) +
  geom_histogram(bins = 30, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Survival Months", x = "Survival Months", y = "Frequency")
# left - skewed

# Count plot for Status (Dead/Alive)
ggplot(project_2_data, aes(x = status)) +
  geom_bar(fill = "lightcoral") +
  labs(title = "Distribution of Status", x = "Status", y = "Count")

ggplot(project_2_data, aes(x = tumor_size)) +
  geom_histogram()
ggplot(project_2_data, aes(x = log2(tumor_size+1))) +
  geom_histogram()
ggplot(project_2_data, aes(x = sqrt(tumor_size+1))) +
  geom_histogram()

ggplot(project_2_data, aes(x = regional_node_examined)) +
  geom_histogram()
ggplot(project_2_data, aes(x = log2(regional_node_examined+1))) +
  geom_histogram()
ggplot(project_2_data, aes(x = sqrt(regional_node_examined+1))) +
  geom_histogram()

ggplot(project_2_data, aes(x = log(regional_node_examined+1))) +
  geom_boxplot()
ggplot(project_2_data, aes(x = sqrt(regional_node_examined+1))) +
  geom_boxplot()

# significantly more alive than dead (might need data imputations)
project_2_data %>% 
  select(where(is.numeric)) %>% 
  GGally::ggpairs() +
  labs(title = "Pairwise Relationships Between Numeric Variables")

# Correlation matrix
cor_matrix <- project_2_data %>% 
  select(where(is.numeric)) %>% 
  cor(use = "complete.obs")
cor_matrix

# QQ plot for Survival Months
ggplot(project_2_data, aes(sample = survival_months)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "QQ Plot for Survival Months", x = "Theoretical Quantiles", y = "Sample Quantiles")

# QQ plot for Tumor Size
ggplot(project_2_data, aes(sample = tumor_size)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "QQ Plot for Tumor Size", x = "Theoretical Quantiles", y = "Sample Quantiles")

# QQ plot for age
ggplot(project_2_data, aes(sample = age)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "QQ Plot for age", x = "Theoretical Quantiles", y = "Sample Quantiles")

# QQ plot for regional_node_examined
ggplot(project_2_data, aes(sample = regional_node_examined)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "QQ Plot for regional_node_examined", x = "Theoretical Quantiles", y = "Sample Quantiles")

# QQ plot for reginol_node_positive
ggplot(project_2_data, aes(sample =reginol_node_positive)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(title = "QQ Plot for reginol_node_positive", x = "Theoretical Quantiles", y = "Sample Quantiles")

# Loop to create histograms for each variable
for (var in numerical_vars) {
  print(ggplot(project_2_data, aes_string(x = var)) +
          geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
          labs(title = paste("Histogram for", var), x = var, y = "Frequency") +
          theme_minimal() )
  
}

### survival months, tumor size and regional_node positive might need transformation
# square root transformation for survival months
project_2_data <- project_2_data %>%
  mutate(sqrt_survival_months = sqrt(survival_months))

# Visualization after log transformation
ggplot(project_2_data, aes(x = sqrt_survival_months)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black") +
  labs(title = "Square root-Transformed Survival Months", x = "Square Root(Survival Months)", y = "Frequency")

ggplot(project_2_data, aes(sample = sqrt_survival_months)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(
    title = "QQ Plot for Square Root-Transformed Survival Months",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Log transformation for survival Months
project_2_data <- project_2_data %>%
  mutate(log_survival_months = log(survival_months + 1), # Adding 1 to avoid log(0)
         log_tumor_size = log(tumor_size + 1))

# Visualization after log transformation
ggplot(project_2_data, aes(x = log_survival_months)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black") +
  labs(title = "Log-Transformed Survival Months", x = "Log(Survival Months)", y = "Frequency")

## QQ-plot in survival months after log transformation
ggplot(project_2_data, aes(sample = log_survival_months)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(
    title = "QQ Plot for Log-Transformed Survival Months",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )
### do not involve transformation for survival months

# Square root transformation for Tumor Size
project_2_data <- project_2_data %>%
  mutate(sqrt_tumor_size = sqrt(tumor_size))

# Visualization after square root transformation
ggplot(project_2_data, aes(x = sqrt_tumor_size)) +
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  labs(title = "Square Root Transformed Tumor Size", x = "Sqrt(Tumor Size)", y = "Frequency")

# QQ Plot: Square root-transformed data
ggplot(project_2_data, aes(sample = sqrt_tumor_size)) +
  stat_qq() +
  stat_qq_line(color = "green") +
  labs(
    title = "QQ Plot for Square Root Transformed Tumor Size",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )
## Square root transformation for tumor size is needed

# Square root transformation for regional_node_positive
project_2_data <- project_2_data %>%
  mutate(sqrt_reginol_node_positive = sqrt(reginol_node_positive))

# Visualization after square root transformation
ggplot(project_2_data, aes(x = sqrt_reginol_node_positive)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Square Root Transformed Regional Node Positive", x = "Sqrt(Regional Node Positive)", y = "Frequency")

# QQ Plot: Square root-transformed data
ggplot(project_2_data, aes(sample = sqrt_reginol_node_positive)) +
  stat_qq() +
  stat_qq_line(color = "green") +
  labs(
    title = "QQ Plot for Square Root Transformed Regional Node Positive",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Inverse transformation for regional_node_positive
project_2_data <- project_2_data %>%
  mutate(inverse_reginol_node_positive = 1/reginol_node_positive)

# Visualization after square root transformation
ggplot(project_2_data, aes(x = inverse_reginol_node_positive)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  labs(title = "Square Root Transformed Regional Node Positive", x = "Sqrt(Regional Node Positive)", y = "Frequency"
       )

# QQ Plot: Square root-transformed data
ggplot(project_2_data, aes(sample = inverse_reginol_node_positive)) +
  stat_qq() +
  stat_qq_line(color = "green") +
  labs(
    title = "QQ Plot for Square Root Transformed Regional Node Positive",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

# Log transformation for regional_node_positive
project_2_data <- project_2_data %>%
  mutate(log_reginol_node_positive = log(reginol_node_positive + 1), # Adding 1 to avoid log(0)
         log_reginol_node_positive = log(reginol_node_positive + 1))

# Visualization after log transformation
ggplot(project_2_data, aes(x = log_reginol_node_positive)) +
  geom_histogram(bins = 30, fill = "darkgreen", color = "black") +
  labs(title = "Log-Transformed Regional Node Positive", x = "Log(Survival Months)", y = "Frequency")

# QQ Plot: Square log-transformed data
ggplot(project_2_data, aes(sample = log_reginol_node_positive)) +
  stat_qq() +
  stat_qq_line(color = "blue") +
  labs(
    title = "QQ Plot for Log-Transformed Regional Node Positive",
    x = "Theoretical Quantiles",
    y = "Sample Quantiles"
  )

## Do not involve any transformation
### 3
# scatter plot for Y and numerical variable 
numerical_vars <- c("age","tumor_size","regional_node_examined","reginol_node_positive")
plot_list1 <- list()
for (var in numerical_vars) {
  p<- ggplot(project_2_data, aes_string(x = var, y = "survival_months")) + 
    geom_point(color = "blue", alpha = 0.6) +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(title = paste (var, "vs Survival Months"), x = var, y = "Survival Months")+    theme_minimal() 
  plot_list1[[var]] <- p
}
grid.arrange(grobs = plot_list1, ncol = 2)

### box plot for Y and categorical variable
categorical_vars <- c("race","marital_status","t_stage","n_stage","x6th_stage","differentiate","grade","a_stage","estrogen_status", "progesterone_status")
plot_list2 <- list()
for (var in categorical_vars) {
  m<- ggplot(project_2_data, aes_string(x = var, y = "survival_months")) +
    geom_boxplot(fill = "lightblue", color = "darkblue") +
    labs(title = paste (var, "vs Survival Months"), x = var, y = "Survival Months") + 
    theme_minimal()
  plot_list2[[var]] <- m
}
grid.arrange(grobs= plot_list2, ncol = 5)


### 1. We get the summary statistics and make the table in excel to have a better format
### 2. We suggest tumor size to have a square root transformation, further transformation are still needed after finish the modeling using box-cox 
### 3. there are potential outliers identified but we might ignore them because they are quite limited and we have large sample size




