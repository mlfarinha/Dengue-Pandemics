######################################################
###### RANDOM FOREST OF STATION ON TEST DATASET ######
######################################################

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
library(rpart)
library(rpart.plot)
library(MASS)
library(caret)
library(randomForest)

###### IMPORT DATA ######
setwd("C:/Users/Carlos Farinha/Desktop/IST/4º Ano/2º Semestre/MEDM/dengue-mosquito-spreading")

train <- read.table("train_without_na.csv", sep = ",", header = TRUE)
train_labels <- read.table("Train_Set_Labels.csv", sep = ",", header = TRUE)
test <- read.table("test_without_na.csv", sep = ",", header = TRUE)

summary(train)

# Selecting only San Juan city
train_labels <- train_labels %>% filter(city == 'sj')

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

# STATION_PRECIP_MM
train_station <- train_log[,c(1,2,3,4,5,6,9,10,11,12,17,22)]
train_station <- cbind(train_station, train_labels$class)
colnames(train_station)[13] <- "class"
summary(train_station)

####################################################################################################################

### Decision Tree for STATION_PRECIP_MM

# Use the implemented method of K-fold cross validation
rfcv_station <- rfcv(train_station[,c(1:12)], train_station$class, cv.fold = 5, mtry=function(p) max(1, floor(sqrt(p))))
rfcv_station
confusionMatrix(train_station$class, rfcv_station$predicted$`3`)
# We cannot predict with the test dataset using this function so we use the repeated K-fold cross validation


# Repeated K-fold cross validation
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3, selectionFunction = "best")
set.seed(2020)
pred.rf_station <- train(train_station[,c(1:12)],train_station$class, method = "rf",
                         trControl  = train.control,  metric = "Accuracy")
print(pred.rf_station) # 0.5508833
pred.rf_station$bestTune #mtry = 12
pred.rf_station$resample
pred.rf_station$finalModel
pred.rf_station$finalModel$ntree # 500
pred.rf_station$finalModel$mtry # 12
pred.rf_station$finalModel$importance # importance of covariables




confusionMatrix(train_station$class, pred.rf_station$finalModel$predicted)
# Sensitivity:  0.6977   0.4428  0.43367   0.6338  0.78571
# Specificity:  0.9202   0.8333  0.83010   0.9021  0.95879

# Custom RF
customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"),
                                  class = rep("numeric", 2), 
                                  label = c("mtry", "ntree"))

customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev,
                         last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


# train model
control <- trainControl(method="repeatedcv",
                        number=5, repeats=3)
tunegrid <- expand.grid(.mtry=c(1:12),
                        .ntree=c(500, 1000, 1500, 2000))
set.seed(2020)
custom <- train(class~., data=train_station, 
                method=customRF, metric="Accuracy",
                tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)

####################################################################################################################

# Eliminate covariables
summary(test)
test$X <- NULL
test$city <- NULL
test$week_start_date <- NULL
test$precipitation_amt_mm <- NULL
test$reanalysis_air_temp_k <- NULL
test$reanalysis_precip_amt_kg_per_m2 <- NULL
test$reanalysis_relative_humidity_percent <- NULL
test$reanalysis_sat_precip_amt_mm <- NULL
test$reanalysis_specific_humidity_g_per_kg <- NULL
test$station_avg_temp_c <- NULL
test$station_diur_temp_rng_c <- NULL
test$station_max_temp_c <- NULL
test$station_min_temp_c <- NULL
summary(test)

# Apply log to station_precip_mm
test_log <- update_columns(test, c("station_precip_mm"), function(x) log(x + 1))
summary(test_log)

which(colnames(test_log)=="station_precip_mm") # 12
colnames(test_log)[12] <- "log(station_precip_mm + 1)"
summary(test_log)

####################################################################################################################

### PREDICT CLASSES FOR OBSERVATIONS OF TEST DATASET

dim(test_log) # 69 observations
summary(train_station)
classes_test <- predict(pred.rf_station, newdata = test_log)
classes_test

test_log_class <- cbind(test_log, classes_test)
test_log_class <- as.data.frame(test_log_class)
View(test_log_class)

test_log_class$classes_test <- as.factor(test_log_class$classes_test)
test_log_class$class <- factor(test_log_class$classes_test, levels = c(1,2,3,4,5), 
                              labels = c("Low", "Low-Medium", "Medium","High","Very High"))

# Plots to put in report
ggplot(data=test_log_class, aes(x = class)) +
  geom_bar(fill = brewer.pal(length(unique(test_log_class$class)), "Set1")) +
  geom_text(stat='count', aes(label=..count..), vjust=-1, size = 3.5) +
  xlab("Class") +
  ylab("Number of observations per class") + 
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35,40)) +
  theme(text = element_text(size = 15, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"))


####################################################################################################################
####################################################################################################################
# NOT USED

# Serch = random 
control <- trainControl(method="repeatedcv", number=10, repeats=3, search="random")
set.seed(2020)
mtry <- sqrt(ncol(train_station[,c(1:12)]))
rf_random <- train(class~., data=train_station, method="rf", metric="Accuracy", tuneLength=15, trControl=control)
print(rf_random)
plot(rf_random)

# Search = grid
control <- trainControl(method="repeatedcv", number=5, repeats=3, search="grid")
set.seed(2020)
tunegrid <- expand.grid(.mtry=c(1:12))
rf_gridsearch <- train(class~., data=train_station, method="rf", metric="Accuracy", tuneGrid=tunegrid, trControl=control)
print(rf_gridsearch)
plot(rf_gridsearch)