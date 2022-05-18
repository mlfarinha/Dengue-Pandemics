#######################
###### LDA & QDA ######
#######################

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

###### LDA FOR UNSCALED VARIABLES ######
library(MASS)
library(caret)

# DATASET WITH PRECIPITATION_AMT_MM

summary(train_precip)
lda_precip <- lda(class ~ ., data = train_precip)
names(lda_precip)
lda_precip
lda.train_precip <- predict(lda_precip)
train_precip.lda <- lda.train_precip$class
table(train_precip.lda, train_precip$class)
sum(diag(table(train_precip.lda, train_precip$class)))/sum(table(train_precip.lda, train_precip$class)) # 0.3679354
sum(predict(lda_precip,train_precip)$class!=train_precip[,13])/dim(train_precip)[1] # 0.6320646

### Recall
recall_lda_precip <- as.vector(diag(table(train_precip.lda, train_precip[,13]))/table(train_precip[,13]))
round(recall_lda_precip,3) # 0.520 0.240 0.281 0.472 0.179

### Precision
precision_lda_precip <- as.vector(diag(table(train_precip.lda, train_precip$class))/table(train_precip.lda))
round(precision_lda_precip,3) # 0.399 0.314 0.331 0.366 0.480

# Repeated k-fold cross validation
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.lda_precip <- train(class ~., method = "lda", trControl  = train.control,  metric = "Accuracy",
                         data = train_precip)
print(pred.lda_precip) # 0.3252574

# DATASET WITH REANALYSIS_PRECIP_AMT_KG_PER_M2
summary(train_reanalysis)
lda_reanalysis <- lda(class ~ ., data = train_reanalysis)
names(lda_reanalysis)
lda_reanalysis
lda.train_reanalysis <- predict(lda_reanalysis)
train_reanalysis.lda <- lda.train_reanalysis$class
table(train_reanalysis.lda, train_reanalysis$class)
sum(diag(table(train_reanalysis.lda, train_reanalysis$class)))/sum(table(train_reanalysis.lda, train_reanalysis$class)) # 0.3806228
sum(predict(lda_reanalysis,train_reanalysis)$class!=train_reanalysis[,13])/dim(train_reanalysis)[1] # 0.6193772

### Recall
recall_lda_reanalysis <- as.vector(diag(table(train_reanalysis.lda, train_reanalysis$class))/table(train_reanalysis[,13]))
round(recall_lda_reanalysis,3) # 0.520 0.220 0.281 0.497 0.194

### Precision
precision_lda_reanalysis <- as.vector(diag(table(train_reanalysis.lda, train_reanalysis$class))/table(train_reanalysis.lda))
round(precision_lda_reanalysis,3) # 0.398 0.344 0.318 0.364 0.481

# Repeated k-fold cross validation
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.lda_reanalysis <- train(class ~., method = "lda", trControl  = train.control,  metric = "Accuracy",
                         data = train_reanalysis)
print(pred.lda_reanalysis) # 0.3349691


# DATASET WITH STATION_PRECIP_MM
summary(train_station)
lda_station <- lda(class ~ ., data = train_station)
names(lda_station)
lda_station
lda.train_station <- predict(lda_station)
train_station.lda <- lda.train_station$class
table(train_station.lda, train_station$class)
sum(diag(table(train_station.lda, train_station$class)))/sum(table(train_station.lda, train_station$class)) # 0.3656286
sum(predict(lda_station,train_station)$class!=train_station[,13])/dim(train_station)[1] # 0.6343714

### Recall
recall_lda_station <- as.vector(diag(table(train_station.lda, train_station$class))/table(train_station[,13]))
round(recall_lda_station,3) # 0.515 0.230 0.302 0.477 0.179

### Precision
precision_lda_station <- as.vector(diag(table(train_station.lda, train_station$class))/table(train_station.lda))
round(precision_lda_station,3) #  0.402 0.324 0.326 0.374 0.429

# Repeated k-fold cross validation
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.lda_station <- train(class ~., method = "lda", trControl  = train.control,  metric = "Accuracy",
                             data = train_station)
print(pred.lda_station) # 0.331842


###### QDA ######
library(MASS)

# DATASET WITH PRECIPITATION_AMT_MM
summary(train_precip)
qda_precip <- qda(class ~ ., data = train_precip)
names(qda_precip)
qda_precip
qda.train_precip <- predict(qda_precip)
train_precip.qda <- qda.train_precip$class
table(train_precip.qda, train_precip$class)
sum(diag(table(train_precip.qda, train_precip$class)))/sum(table(train_precip.qda, train_precip$class)) # 0.467128
sum(predict(qda_precip,train_precip)$class!=train_precip[,13])/dim(train_precip)[1] # 0.532872

### Recall
recall_qda_precip <- as.vector(diag(table(train_precip.qda, train_precip$class))/table(train_precip[,13]))
round(recall_qda_precip,3) # 0.698 0.220 0.377 0.568 0.478

### Precision
precision_qda_precip <- as.vector(diag(table(train_precip.qda, train_precip$class))/table(train_precip.qda))
round(precision_qda_precip,3) # 0.493 0.436 0.410 0.528 0.386

# Repeated k-fold cross validation
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.qda_precip <- train(class ~., method = "qda", trControl  = train.control,  metric = "Accuracy",
                         data = train_precip)
print(pred.qda_precip) # 0.3363599


# DATASET WITH REANALYSIS_PRECIP_AMT_KG_PER_M2
summary(train_reanalysis)
qda_reanalysis <- qda(class ~ ., data = train_reanalysis)
names(qda_reanalysis)
qda_reanalysis
qda.train_reanalysis <- predict(qda_reanalysis)
train_reanalysis.qda <- qda.train_reanalysis$class
table(train_reanalysis.qda, train_reanalysis$class)
sum(diag(table(train_reanalysis.qda, train_reanalysis$class)))/sum(table(train_reanalysis.qda, train_reanalysis$class)) # 0.4602076
sum(predict(qda_reanalysis,train_reanalysis)$class!=train_reanalysis[,13])/dim(train_reanalysis)[1] # 0.5397924

### Recall
recall_qda_reanalysis <- as.vector(diag(table(train_reanalysis.qda, train_reanalysis$class))/table(train_reanalysis[,13]))
round(recall_qda_reanalysis,3) # 0.723 0.190 0.407 0.497 0.522

### Precision
precision_qda_reanalysis <- as.vector(diag(table(train_reanalysis.qda, train_reanalysis$class))/table(train_reanalysis.qda))
round(precision_qda_reanalysis,3) # 0.459 0.432 0.420 0.503 0.493

# Repeated k-fold cross validation
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.qda_reanalysis <- train(class ~., method = "qda", trControl  = train.control,  metric = "Accuracy",
                             data = train_reanalysis)
print(pred.qda_reanalysis) # 0.3356833


# DATASET WITH STATION_PRECIP_AMT
summary(train_station)
qda_station <- qda(class ~ ., data = train_station)
names(qda_station)
qda_station
qda.train_station <- predict(qda_station)
train_station.qda <- qda.train_station$class
table(train_station.qda, train_station$class)
sum(diag(table(train_station.qda, train_station$class)))/sum(table(train_station.qda, train_station$class)) # 0.4659746
sum(predict(qda_station,train_station)$class!=train_station[,13])/dim(train_station)[1] # 0.5340254

### Recall
recall_qda_station <- as.vector(diag(table(train_station.qda, train_station$class))/table(train_station[,13]))
round(recall_qda_station,3) # 0.673 0.235 0.432 0.492 0.552

### Precision
precision_qda_station <- as.vector(diag(table(train_station.qda, train_station$class))/table(train_station.qda))
round(precision_qda_station,3) # 0.489 0.435 0.441 0.492 0.425

# Repeated k-fold cross validation
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.qda_station <- train(class ~., method = "qda", trControl  = train.control,  metric = "Accuracy",
                          data = train_station)
print(pred.qda_station) #  0.3363667



