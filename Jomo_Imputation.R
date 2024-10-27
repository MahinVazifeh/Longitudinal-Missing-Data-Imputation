
# Step 0: Import libraries
library(dplyr)
library(mice)
library(jomo)
library(mitml)

<<<<<<< HEAD
=======
# RMSE Calculation
RMSE_calculation <- function(completed_dataset,imputed_dataset) {
  cols <- 3:10
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

# Step 4: Create random-NA by replacing values with NA in random_completed
set.seed(456) # set seed for reproducibility
random_NA <- completed_data %>% 
  mutate(across(3:10, ~ifelse(runif(length(.)) < 0.4, NA, .)))
Missing_rate <- sapply(random_NA, function(x) sum(is.na(x)))

# Solution 1
# Set parameters
set.seed(1569)
Y <- random_NA[,c(3:10)]
Y.numcat= c(8,7,7,6,7,8,6,16)
<<<<<<< HEAD
X <- random_NA[,c(11:14)]
=======
X <- random_NA[,c(11:13)]
>>>>>>> 2d3f493 (Add R codes)
clus <- random_NA[,c("Patient_ID")]
nburn <- as.integer(10)
nbetween <- as.integer(10)
nimp <- as.integer(3)
formula <- as.formula(EDSS_score_assessed_by_clinician~Pyramidal+Cerebellar+
                        Thronchioencephalic+Sensitive+Sphincteric+Visual+Mental+Deambulation+
<<<<<<< HEAD
                        Sex+MS.in.pediatric.age+Age_Patient)
=======
                        Sex+MS.in.pediatric.age+Age)
>>>>>>> 2d3f493 (Add R codes)

# Run the jomo functions
imp <- jomo1rancat(Y = Y, Y.numcat= Y.numcat, X = X, clus = clus, nburn = nburn, nbetween = nbetween, nimp = nimp)
imp <- jomo1rancon(Y = Y, X = X, clus = clus, nburn = nburn, nbetween = nbetween, nimp = nimp)
imp <-jomo.lm(formula = formula, data = random_NA[, -c(1)])
imp <-jomo1con(random_NA)
imp <- jomo1ranconhr(Y = Y, X = X, clus = clus, nburn = nburn, nbetween = nbetween, nimp = nimp)
# Make a list for keeping the final results
empty_list <- vector(mode='list', length=nimp)
completed_data_subset <- completed_data[,3:10] %>%  as.data.frame()
completed_data_subset <- completed_data_subset %>% 
  mutate_if(is.integer,as.numeric)
str(completed_data_subset)

for (i in 1:nimp) {
  
  imputed_data <- imp[imp$Imputation==i,]
  imputed_data_subset <- imputed_data[,1:8] %>%  as.data.frame()
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

<<<<<<< HEAD
# RMSE Calculation
RMSE_calculation <- function(completed_dataset,imputed_dataset) {
  cols <- 3:10
  completed_data_subset <- as.matrix(completed_dataset[, cols])
  imputed_data_subset <- as.matrix(imputed_dataset[, cols])
  
  mse <- mean((completed_data_subset - imputed_data_subset)^2)
  rmse <- sqrt(mse)
  
  return(rmse)
}


=======
>>>>>>> 2d3f493 (Add R codes)

# set.seed(456) # for reproducibility
# na_rows <- sample(nrow(random_completed), round(nrow(random_completed) * 0.4))
# na_cols <- sample(4:11, round((11-4+1) * 0.4), replace = FALSE)
# random_NA <- random_completed
# random_NA[na_rows, na_cols] <- NA















