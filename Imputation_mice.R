
# Step 0: Import libraries
library(dplyr)
library(mice)
library(jomo)
library(mitml)

# Step 1: Import raw csv data
raw_data <- read.csv(file.choose(), header = TRUE)

# Step 2: Select rows with no missing values
completed_data <- raw_data[complete.cases(raw_data),]

# Step 3: Create random-NA by replacing values with NA in random_completed
set.seed(456) # set seed for reproducibility
random_NA <- completed_data %>% 
  mutate(across(3:10, ~ifelse(runif(length(.)) < 0.4, NA, .)))
Missing_rate <- sapply(random_NA, function(x) sum(is.na(x)))

# Step 4: Impute the missing values in random-NA using mice
mice_method <- list("pmm", "cart", "2l.lmer", "norm", "norm.boot", "rf", "norm.predict", "norm.nob")
result <- list()
for (mt in 1:length(mice_method)) {
  imp <- mice(random_NA, m=5, maxit=50, meth=mice_method[mt], seed=789)
  imputed_data <- complete(imp)
  rmse <- RMSE_calculation(completed_data,imputed_data)
  result <- append(result, c(mice_method[mt],rmse),after = length(result))
}

RMSE_calculation <- function(completed_dataset,imputed_dataset) {
  cols <- 3:10
  completed_data_subset <- as.matrix(completed_dataset[, cols])
  imputed_data_subset <- as.matrix(imputed_dataset[, cols])
  
  mse <- mean((completed_data_subset - imputed_data_subset)^2)
  rmse <- sqrt(mse)
  
  return(rmse)
}