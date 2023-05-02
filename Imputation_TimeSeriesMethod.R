
# Step 0: Import libraries
library(dplyr)
library(mitml)
library(imputeTS)


# Step 1: Import raw csv data
raw_data <- read.csv(file.choose(), header = TRUE)

# Step 2: Select rows with no missing values
completed_data <- raw_data[complete.cases(raw_data),]

# Step 3: Create random-NA by replacing values with NA in random_completed
set.seed(456) # set seed for reproducibility
random_NA <- completed_data %>% 
  mutate(across(3:10, ~ifelse(runif(length(.)) < 0.4, NA, .)))
Missing_rate <- sapply(random_NA, function(x) sum(is.na(x)))

# Step 4: Impute missing values using Interpolation
# ********Linear Interpolation********
linear_imputed_data <- na_interpolation(random_NA)
RMSE_calculation(completed_data,linear_imputed_data)
# ********Spline Interpolation********
spline_imputed_data <- na_interpolation(random_NA, option = "spline")
RMSE_calculation(completed_data,spline_imputed_data)
# Impute missing values using Last Observation
locf_imputed_data <- na_locf(random_NA, option = "locf", na_remaining = "rev", maxgap = Inf)
RMSE_calculation(completed_data,locf_imputed_data)
# Impute missing values using weighted moving average(Exponential Weighted Moving Average (EWMA))
weighted_imputed_data <- na_ma(random_NA, k = 4, weighting = "exponential", maxgap = Inf)
RMSE_calculation(completed_data,weighted_imputed_data)

RMSE_calculation <- function(completed_dataset,imputed_dataset) {
  cols <- 3:10
  completed_data_subset <- as.matrix(completed_dataset[, cols])
  imputed_data_subset <- as.matrix(imputed_dataset[, cols])
  
  mse <- mean((completed_data_subset - imputed_data_subset)^2)
  rmse <- sqrt(mse)
  
  return(rmse)
}