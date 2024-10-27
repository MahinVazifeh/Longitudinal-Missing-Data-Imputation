
# Step 0: Import libraries
library(dplyr)
library(mitml)
library(imputeTS)
<<<<<<< HEAD

=======
library(mice)
library(jomo)

RMSE_calculation <- function(completed_dataset,imputed_dataset) {
  cols <- 3:13
  completed_data_subset <- as.matrix(completed_dataset[, cols])
  imputed_data_subset <- as.matrix(imputed_dataset[, cols])
  
  mse <- mean((completed_data_subset - imputed_data_subset)^2)
  rmse <- sqrt(mse)
  
  return(rmse)
}
>>>>>>> 2d3f493 (Add R codes)

# Step 1: Import raw csv data
raw_data <- read.csv(file.choose(), header = TRUE)

# Step 2: Select rows with no missing values
completed_data <- raw_data[complete.cases(raw_data),]

# Step 3: Create random-NA by replacing values with NA in random_completed
set.seed(456) # set seed for reproducibility
random_NA <- completed_data %>% 
<<<<<<< HEAD
  mutate(across(3:10, ~ifelse(runif(length(.)) < 0.4, NA, .)))
=======
  mutate(across("Pyramidal":"Deambulation", ~ifelse(runif(length(.)) < 0.1, NA, .)))
>>>>>>> 2d3f493 (Add R codes)
Missing_rate <- sapply(random_NA, function(x) sum(is.na(x)))

# Step 4: Impute missing values using Interpolation
# ********Linear Interpolation********
linear_imputed_data <- na_interpolation(random_NA)
<<<<<<< HEAD
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
=======
RMSE_calculation(completed_data,round(linear_imputed_data))
# ********Spline Interpolation********
spline_imputed_data <- na_interpolation(random_NA, option = "spline")
RMSE_calculation(completed_data,round(spline_imputed_data))
# Impute missing values using Last Observation
locf_imputed_data <- na_locf(random_NA, option = "locf", na_remaining = "rev", maxgap = Inf)
RMSE_calculation(completed_data,round(locf_imputed_data))
# Impute missing values using weighted moving average(Exponential Weighted Moving Average (EWMA))
weighted_imputed_data <- na_ma(random_NA, k = 4, weighting = "exponential", maxgap = Inf)
RMSE_calculation(completed_data,round(weighted_imputed_data))

# Impute missing value using MICE method
mice_method <- list("cart", "rf", "norm.nob")
result <- list()
for (mt in 1:length(mice_method)) {
  imp <- mice(random_NA, m=5, maxit=50, meth=mice_method[mt])
  imputed_data <- complete(imp)
  rmse <- RMSE_calculation(completed_data,round(imputed_data))
  result <- append(result, c(mice_method[mt],rmse),after = length(result))}


# Imputation with Jomo
Y <- random_NA[,c(3:10)]
Y.numcat <- c(8,7,7,6,8,8,6,16)
X <- random_NA[,c(11:13)]
clus <- random_NA[,c("Patient_ID")]
nburn <- as.integer(10)
nbetween <- as.integer(10)
nimp <- as.integer(3)
formula <- as.formula(EDSS_score_assessed_by_clinician~Pyramidal+Cerebellar+
                        Thronchioencephalic+Sensitive+Sphincteric+Visual+Mental+Deambulation)

imp <- jomo1rancon(Y = Y, X = X, clus = clus, nburn = nburn, nbetween = nbetween, nimp = nimp)

empty_list <- vector(mode='list', length=nimp)
completed_data_subset <- completed_data[,3:10] %>%  as.data.frame()
completed_data_subset <- completed_data_subset %>% 
  mutate_if(is.integer,as.numeric)
str(completed_data_subset)
for (i in 1:nimp) {
  imputed_data <- imp[imp$Imputation==i,]
  imputed_data_subset <- round(imputed_data[,1:8]) %>%  as.data.frame()
  imputed_data_subset <- imputed_data_subset %>% 
    mutate_if(is.factor,as.numeric)
  str(imputed_data_subset)
  completed_data_subset <- as.matrix(completed_data_subset)
  imputed_data_subset <- as.matrix(imputed_data_subset)
  mse <- mean((completed_data_subset - imputed_data_subset)^2)
  rmse <- sqrt(mse)
  empty_list[i] <- rmse
}
print(min(unlist(empty_list)))
print(which.min(unlist(empty_list)))





>>>>>>> 2d3f493 (Add R codes)
