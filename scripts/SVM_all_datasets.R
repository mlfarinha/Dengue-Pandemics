####################################
###### SUPPORT VECTOR MACHINE ######
####################################

library(ggplot2)
library(viridis)
library(RColorBrewer)
library(scales)
library(PerformanceAnalytics)
library(reshape2)
library(hrbrthemes)
library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
library(DataExplorer)
library(purrr)
library(kedd)
library(MASS)
library(caret)
library(e1071)

###### IMPORT DATA ######
setwd("C:/Users/Carlos Farinha/Desktop/IST/4º Ano/2º Semestre/MEDM/dengue-mosquito-spreading")

train <- read.table("train_without_na.csv", sep = ",", header = TRUE)
train_labels <- read.table("Train_Set_Labels.csv", sep = ",", header = TRUE)
test <- read.table("Test_Set.csv", sep = ",", header = TRUE)

summary(train)

# Selecting only San Juan city
train_labels <- train_labels %>% filter(city == 'sj')
test <- test %>% filter(city == 'sj')

# Remove identification of the X because it isn't required
train$X <- NULL
train_labels$city <- NULL
train_labels$year <- NULL
train_labels$weekofyear <- NULL
train$week_start_date <- NULL

summary(train)
summary(train_labels)

# Summary of dataset
cat("Dimensions of the training set: ", dim(train), "\n")
cat("Dimensions of the training_label set: ", dim(train_labels), "\n")
summary(train)
summary(train_labels)

# Define classes
class_funct <- function(x) {
  if(x <= 8) { # first quantile without oultiers
    return(1)
  } else if(x <= 17) { # first quantile without oultiers
    return(2)
  } else if(x <= 31) { # median without oultiers
    return(3)
  } else if(x <= 79) { # third quantile without oultiers
    return(4)
  }
  return(5) # outliers
}

train_labels <- train_labels %>% mutate(class = map_dbl(total_cases, class_funct))
train_labels$class <- as.factor(train_labels$class)
summary(train_labels)
summary(train_labels$class) # number of observations per class

# Apply log to precipitation covariates
train_log <- update_columns(train, c("precipitation_amt_mm","reanalysis_precip_amt_kg_per_m2","station_precip_mm"),
                            function(x) log(x + 1))
summary(train_log)

which(colnames(train_log)=="precipitation_amt_mm") # 7
which(colnames(train_log)=="reanalysis_precip_amt_kg_per_m2") # 13
which(colnames(train_log)=="station_precip_mm") # 22

colnames(train_log)[7] <- "log(precipitation_amt_mm + 1)"
colnames(train_log)[13] <- "log(reanalysis_precip_amt_kg_per_m2 + 1)"
colnames(train_log)[22] <- "log(station_precip_mm + 1)"

summary(train_log)

# Define datasets to analyse
which(colnames(train_log)=="year") # 1
which(colnames(train_log)=="weekofyear") # 2
which(colnames(train_log)=="ndvi_ne") # 3
which(colnames(train_log)=="ndvi_nw") # 4
which(colnames(train_log)=="ndvi_se") # 5
which(colnames(train_log)=="ndvi_sw") # 6
which(colnames(train_log)=="reanalysis_avg_temp_k") # 9
which(colnames(train_log)=="reanalysis_dew_point_temp_k") # 10
which(colnames(train_log)=="reanalysis_max_air_temp_k") # 11
which(colnames(train_log)=="reanalysis_min_air_temp_k") # 12
which(colnames(train_log)=="reanalysis_tdtr_k") # 17

# Create 3 different datasets
train_precip <- train_log[,c(1,2,3,4,5,6,7,9,10,11,12,17)]
train_precip <- cbind(train_precip, train_labels$class)
colnames(train_precip)[13] <- "class"
summary(train_precip)

train_reanalysis <- train_log[,c(1,2,3,4,5,6,9,10,11,12,13,17)]
train_reanalysis <- cbind(train_reanalysis, train_labels$class)
colnames(train_reanalysis)[13] <- "class"
summary(train_reanalysis)

train_station <- train_log[,c(1,2,3,4,5,6,9,10,11,12,17,22)]
train_station <- cbind(train_station, train_labels$class)
colnames(train_station)[13] <- "class"
summary(train_station)

####################################################################################################################

### SVM for PRECIPITATION_AMT_MM

svm_precip <- svm(class~., data = train_precip, method = "C-classification", kernel = "radial",
                  gamma = 0.1, cost = 10)
summary(svm_precip)
#svm_precip$
table(svm_precip$fitted, train_precip$class)
tune_svm_precip <- tune(svm, class~., data = train_precip, kernel='poly',
                        ranges = list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune_svm_precip) # best performance: 0.6378375

train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.svm_precip_lin <- train(class ~., method = "svmLinear", trControl  = train.control,  metric = "Accuracy",
                             data = train_precip)
print(pred.svm_precip_lin) # 0.32644
set.seed(2020)
pred.svm_precip_rad <- train(class ~., method = "svmRadial", trControl  = train.control,  metric = "Accuracy",
                         data = train_precip)
print(pred.svm_precip_rad) # 0.3756
set.seed(2020)
pred.svm_precip_poly <- train(class ~., method = "svmPoly", trControl  = train.control,  metric = "Accuracy",
                             data = train_precip)
print(pred.svm_precip_poly) # 0.394


### SVM for REANALYSIS_PRECIP_AMT_KG_PER_M2

svm_reanalysis <- svm(class~., data = train_reanalysis, method = "C-classification", kernel = "radial",
                  gamma = 0.1, cost = 10)
summary(svm_reanalysis)
#svm_precip$
tune_svm_reanalysis <- tune(svm, class~., data = train_reanalysis, kernel='radial',
                        ranges = list(cost=c(0.01,0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune_svm_reanalysis) # best performance: 0.6148356 

train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.svm_reanalysis_lin <- train(class ~., method = "svmLinear", trControl  = train.control,  metric = "Accuracy",
                             data = train_reanalysis)
print(pred.svm_reanalysis_lin) #  0.3341453
set.seed(2020)
pred.svm_reanalysis_rad <- train(class ~., method = "svmRadial", trControl  = train.control,  metric = "Accuracy",
                             data = train_reanalysis)
print(pred.svm_reanalysis_rad) # 0.3794
set.seed(2020)
pred.svm_reanalysis_poly <- train(class ~., method = "svmPoly", trControl  = train.control,  metric = "Accuracy",
                              data = train_reanalysis)
print(pred.svm_reanalysis_poly) # 0.3914556

### SVM for STATION_PRECIP_MM

svm_station <- svm(class~., data = train_station, method = "C-classification", kernel = "radial",
                      gamma = 0.5, cost = 10)
summary(svm_station)
#svm_precip$
tune_svm_station <- tune(svm, class~., data = train_reanalysis, kernel='radial',
                            ranges = list(cost=c(0.01,0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune_svm_station) # best performance: 0.6252339
table(predict(), train_station$class)
sum(diag(table(predict(svm_station),train_station$class)))/sum(table(predict(svm_station), train_station$class)) # 0.3806228

train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.svm_station_lin <- train(class ~., method = "svmLinear", trControl  = train.control,  metric = "Accuracy",
                                 data = train_station)
print(pred.svm_station_lin)  # 0.3333703
set.seed(2020)
pred.svm_station_rad <- train(class ~., method = "svmRadial", trControl  = train.control,  metric = "Accuracy",
                                 data = train_station)
print(pred.svm_station_rad) # 0.3836976
set.seed(2020)
pred.svm_station_poly <- train(class ~., method = "svmPoly", trControl  = train.control,  metric = "Accuracy",
                                  data = train_station)
print(pred.svm_station_poly) # 0.4071498










