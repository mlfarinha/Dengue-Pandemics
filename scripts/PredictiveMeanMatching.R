######################################
###### PREDICTIVE MEAN MATCHING ######
######################################

library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(mice) # for imputation
library(sjmisc) # for imputation

setwd("C:/Users/Carlos Farinha/Desktop/IST/4º Ano/2º Semestre/MEDM/dengue-mosquito-spreading")

# we read the 3 datasets provided
train <- read.table("Train_Set.csv", sep = ",", header = TRUE)
train_labels <- read.table("Train_Set_Labels.csv", sep = ",", header = TRUE)
test <- read.table("Test_Set.csv", sep = ",", header = TRUE)

summary(train)

train <- train %>% filter(city == 'sj')
train_labels <- train_labels %>% filter(city == 'sj')
test <- test %>% filter(city == 'sj')

train$city <- NULL
train_labels$city <- NULL
test$city <- NULL

cat("Dimensions of the training set: ", dim(train), "\n")
summary(train)
summary(train_labels)

# Predictive Mean Matching
train.ori <- train
train_mice <- mice(train, method="pmm", m=3)
train <- merge_imputations(train, train_mice)

summary(train)

train <- cbind(train.ori[,c(1:3)], train[,c(1:12)], train.ori[,c(16)], train[,c(13:19)])
colnames(train)[16] <- colnames(train.ori)[16]
train$reanalysis_sat_precip_amt_mm[is.na(train$reanalysis_sat_precip_amt_mm)] <- 
  mean(na.omit(train$reanalysis_sat_precip_amt_mm))
cat(dim(train), "\n")
summary(train)

# write.csv(train, "train_without_na.csv")

test.ori <- test
test_mice <- mice(test, method="pmm", m=3)
test <- merge_imputations(test, test_mice)
test <- cbind(test.ori[,c(1:3)], test, test.ori[,c(6:23)])

summary(test)

write.csv(test, "test_without_na.csv")