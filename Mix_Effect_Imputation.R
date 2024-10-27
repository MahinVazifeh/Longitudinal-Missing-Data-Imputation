#library(momentuHMM)

library(dplyr)
library(nlme)
library(emmeans)


RMSE_calculation <- function(data_selected,Imputed_data) {
  cols <- 1:8
  completed_data_subset <- as.matrix(data_selected[, cols])
  imputed_data_subset <- as.matrix(Imputed_data[, cols])
  
  mse <- mean((completed_data_subset - imputed_data_subset)^2)
  rmse <- sqrt(mse)
  
  return(rmse)
}


# Step 1: Import raw csv data (Complete_Clinical_Data_12000)
data <- read.csv(file.choose(), header = TRUE)

# Create Patient number
data <- data %>%
  group_by(Patient_ID) %>%
  mutate(Patient_number = (cur_group_id() - 1) + 1)

# Create number of observation for each patient
data <- data %>%
  group_by(Patient_number) %>%                
  arrange(Patient_number, Date_of_visit) %>%
  mutate(real_time = row_number())

data$real_time <- factor(data$real_time)

# Delete Date of Visit
data = subset(data, select = -c(Date_of_visit,Patient_ID))

# Get the last 10 observations for each patient (451 Patients)
data_selected <- data %>%
  group_by(Patient_number) %>% 
  filter(n() > 6) %>% 
  slice(tail(row_number(), 7)) %>% 
  arrange(Patient_number, real_time) %>% 
  mutate(time = row_number())

Imputed_data <- data.frame()
Imputed_data <- data.frame(matrix(nrow = 3990, ncol = 15))
colnames(Imputed_data) <- colnames(data_selected)

# Select the range of columns and assign to Imputed_data
start_col <- which(names(data_selected) == "EDSS_score_assessed_by_clinician")
end_col <- which(names(data_selected) == "time")
Imputed_data[, start_col:end_col] <- data_selected[, start_col:end_col]
Imputed_data <- Imputed_data %>% arrange(Patient_number, time)

for (col in colnames(data_selected)[1:8]) {
  set.seed(456) # set seed for reproducibility
  random_NA <- data_selected %>% mutate(across(col, ~ifelse(runif(length(.)) < 0.1, NA, .)))
  Missing_rate <- sapply(random_NA, function(x) sum(is.na(x)))
  Missing_rate
  
  Completed_index <- which(!is.na(random_NA[[col]]))
  Completed_index
  
  Completed_data <- random_NA[complete.cases(random_NA[ , col]), ]
  
  Missing_index <- which(is.na(random_NA[[col]])) #Missing rows for specific feature
  Missing_index
  
  Missing_rows <- random_NA[which(is.na(random_NA[[col]])),] #Missing rows for specific feature
  View(Missing_rows)
  
  Missing_rows <- Missing_rows[, !(names(Missing_rows) %in% col)]
  View(Missing_rows)
  
  if (col == 'Pyramidal') {
    print(col)
    
    LMM_u <-lme(Pyramidal~ -1 + time + EDSS_score_assessed_by_clinician + Cerebellar + Deambulation,
                random = ~1 | Patient_number,
                data = Completed_data,
                method = "ML",
                correlation = corSymm(form = ~ as.numeric(time)|Patient_number),
                weights = varIdent(form = ~1|as.numeric(time)),
                control =list(msMaxIter = 1000, msMaxEval = 1000),
                na.action = na.omit)
    predictions  <- predict(LMM_u, newdata=Missing_rows)
    Missing_rows$Pyramidal = predictions
    Merged_data <- rbind(Completed_data,Missing_rows) %>% arrange(Patient_number, time)
    Imputed_data$Pyramidal <- Merged_data$Pyramidal}
  
  else if (col == 'Cerebellar'){
    print(col)
    
    LMM_u <-lme(Cerebellar~ -1 + time + EDSS_score_assessed_by_clinician + Pyramidal + Thronchioencephalic + Sphincteric,
                random = ~1 | Patient_number,
                data = Completed_data,
                method = "ML",
                correlation = corSymm(form = ~ as.numeric(time)|Patient_number),
                weights = varIdent(form = ~1|as.numeric(time)),
                control =list(msMaxIter = 1000, msMaxEval = 1000),
                na.action = na.omit)
    predictions  <- predict(LMM_u, newdata=Missing_rows)
    Missing_rows$Cerebellar = predictions
    Merged_data <- rbind(Completed_data,Missing_rows) %>% arrange(Patient_number, time)
    Imputed_data$Cerebellar <- Merged_data$Cerebellar} 
  
  else if (col == 'Thronchioencephalic'){
    print(col)
    
    LMM_u <-lme(Thronchioencephalic~ -1 + time + EDSS_score_assessed_by_clinician + Deambulation + Sphincteric + Cerebellar,
                random = ~1 | Patient_number,
                data = Completed_data,
                method = "ML",
                correlation = corSymm(form = ~ as.numeric(time)|Patient_number),
                weights = varIdent(form = ~1|as.numeric(time)),
                control =list(msMaxIter = 1000, msMaxEval = 1000),
                na.action = na.omit)
    predictions  <- predict(LMM_u, newdata=Missing_rows)
    Missing_rows$Thronchioencephalic = predictions
    Merged_data <- rbind(Completed_data,Missing_rows) %>% arrange(Patient_number, time)
    Imputed_data$Thronchioencephalic <- Merged_data$Thronchioencephalic
    } 
  
  else if (col == 'Sensitive'){
    print(col)
    
    LMM_u <-lme(Sensitive~ -1 + time + EDSS_score_assessed_by_clinician,
                random = ~1 | Patient_number,
                data = Completed_data,
                method = "ML",
                correlation = corSymm(form = ~ as.numeric(time)|Patient_number),
                weights = varIdent(form = ~1|as.numeric(time)),
                control =list(msMaxIter = 1000, msMaxEval = 1000),
                na.action = na.omit)
    predictions  <- predict(LMM_u, newdata=Missing_rows)
    Missing_rows$Sensitive = predictions
    Merged_data <- rbind(Completed_data,Missing_rows) %>% arrange(Patient_number, time)
    Imputed_data$Sensitive <- Merged_data$Sensitive} 
  
  else if (col == 'Sphincteric'){
    print(col)
    
    LMM_u <-lme(Sphincteric~ -1 + time + EDSS_score_assessed_by_clinician + Deambulation + Thronchioencephalic + Cerebellar,
                random = ~1 | Patient_number,
                data = Completed_data,
                method = "ML",
                correlation = corSymm(form = ~ as.numeric(time)|Patient_number),
                weights = varIdent(form = ~1|as.numeric(time)),
                control =list(msMaxIter = 1000, msMaxEval = 1000),
                na.action = na.omit)
    predictions  <- predict(LMM_u, newdata=Missing_rows)
    Missing_rows$Sphincteric = predictions
    Merged_data <- rbind(Completed_data,Missing_rows) %>% arrange(Patient_number, time)
    Imputed_data$Sphincteric <- Merged_data$Sphincteric} 
  
  else if (col == 'Visual'){
    print(col)
    
    LMM_u <-lme(Visual~ -1 + time + EDSS_score_assessed_by_clinician + Sphincteric + Cerebellar,
                random = ~1 | Patient_number,
                data = Completed_data,
                method = "ML",
                correlation = corSymm(form = ~ as.numeric(time)|Patient_number),
                weights = varIdent(form = ~1|as.numeric(time)),
                control =list(msMaxIter = 1000, msMaxEval = 1000),
                na.action = na.omit)
    predictions  <- predict(LMM_u, newdata=Missing_rows)
    Missing_rows$Visual = predictions
    Merged_data <- rbind(Completed_data,Missing_rows) %>% arrange(Patient_number, time)
    Imputed_data$Visual <- Merged_data$Visual} 
  
  else if (col == 'Mental'){
    print(col)
    
    LMM_u <-lme(Mental~ -1 + time + EDSS_score_assessed_by_clinician + Sphincteric + Cerebellar,
                random = ~1 | Patient_number,
                data = Completed_data,
                method = "ML",
                correlation = corSymm(form = ~ as.numeric(time)|Patient_number),
                weights = varIdent(form = ~1|as.numeric(time)),
                control =list(msMaxIter = 1000, msMaxEval = 1000),
                na.action = na.omit)
    predictions  <- predict(LMM_u, newdata=Missing_rows)
    Missing_rows$Mental = predictions
    Merged_data <- rbind(Completed_data,Missing_rows) %>% arrange(Patient_number, time)
    Imputed_data$Mental <- Merged_data$Mental} 
  
  else{
    print('Deambulation')
    
    LMM_u <-lme(Deambulation~ -1 + time + EDSS_score_assessed_by_clinician + Cerebellar + Pyramidal+Sphincteric,
                random = ~1 | Patient_number,
                data = Completed_data,
                method = "ML",
                correlation = corSymm(form = ~ as.numeric(time)|Patient_number),
                weights = varIdent(form = ~1|as.numeric(time)),
                control =list(msMaxIter = 1000, msMaxEval = 1000),
                na.action = na.omit)
    predictions  <- predict(LMM_u, newdata=Missing_rows)
    Missing_rows$Deambulation = predictions
    Merged_data <- rbind(Completed_data,Missing_rows) %>% arrange(Patient_number, time)
    Imputed_data$Deambulation <- Merged_data$Deambulation}}


RMSE_calculation(data_selected,Imputed_data)






