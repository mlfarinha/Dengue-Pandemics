###################
###### LINDA ######
###################

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
library(rrcov)

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

### LINDA for PRECIPITATION_AMT_MM

#mcd
rlda_precip <- Linda(class~., data = train_precip, method = "mcd", tol = 1.0e-4)
rlda_precip
rlda.train_precip <- predict(rlda_precip)
rlda.train_precip@ct
table(rlda.train_precip@classification, train_precip$class)
cat("\nApparent error rate: ", round(rrcov:::.AER(rlda.train_precip@ct),4)) # 0.6113

#fsa
rlda_precip_fsa <- Linda(class~., data = train_precip, method = "fsa", tol = 1.0e-4)
rlda_precip_fsa
rlda.train_precip_fsa <- predict(rlda_precip_fsa)
rlda.train_precip_fsa@ct
table(rlda.train_precip_fsa@classification, train_precip$class)
cat("\nApparent error rate: ", round(rrcov:::.AER(rlda.train_precip_fsa@ct),4)) # 0.6332

train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.rlda_precip <- train(class ~., method = "Linda", trControl  = train.control,  metric = "Accuracy",
                         data = train_precip)
print(pred.rlda_precip) # 0.3579469

### LINDA for REANALYSIS_PRECIP_AMT_KG_PER_M2

#mcd
rlda_reanalysis <- Linda(class~., data = train_reanalysis, method = "mcd", tol = 1.0e-4)
rlda_reanalysis
rlda.train_reanalysis <- predict(rlda_reanalysis)
rlda.train_reanalysis@ct
table(rlda.train_reanalysis@classification, train_reanalysis$class)
cat("\nApparent error rate: ", round(rrcov:::.AER(rlda.train_reanalysis@ct),4)) # 0.6078

#fsa
rlda_reanalysis_fsa <- Linda(class~., data = train_reanalysis, method = "fsa", tol = 1.0e-4)
rlda_reanalysis_fsa
rlda.train_reanalysis_fsa <- predict(rlda_reanalysis_fsa)
rlda.train_reanalysis_fsa@ct
table(rlda.train_reanalysis_fsa@classification, train_reanalysis$class)
cat("\nApparent error rate: ", round(rrcov:::.AER(rlda.train_reanalysis_fsa@ct),4)) # 0.6136

train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.rlda_reanalysis <- train(class ~., method = "Linda", trControl  = train.control,  metric = "Accuracy",
                          data = train_reanalysis)
print(pred.rlda_reanalysis) # 0.3552828

### LINDA for STATION_PRECIP_MM

#mcd
rlda_station <- Linda(class~., data = train_station, method = "mcd", tol = 1.0e-4)
rlda_station
rlda.train_station <- predict(rlda_station)
rlda.train_station@ct
table(rlda.train_station@classification, train_station$class)
cat("\nApparent error rate: ", round(rrcov:::.AER(rlda.train_station@ct),4)) # 0.5986

#fsa
rlda_station_fsa <- Linda(class~., data = train_station, method = "fsa", tol = 1.0e-4)
rlda_station_fsa
rlda.train_station_fsa <- predict(rlda_station_fsa)
rlda.train_station_fsa@ct
table(rlda.train_station_fsa@classification, train_station$class)
cat("\nApparent error rate: ", round(rrcov:::.AER(rlda.train_station_fsa@ct),4)) # 0.6136

train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.rlda_station <- train(class ~., method = "Linda", trControl  = train.control,  metric = "Accuracy",
                              data = train_station)
print(pred.rlda_station) # 0.3494291




