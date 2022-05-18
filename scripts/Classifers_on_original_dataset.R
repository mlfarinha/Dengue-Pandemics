#############################################
###### CLASSIFIERS ON ORIGINAL DATASET ######
#############################################

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
library(klaR)

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

original_data <- as.data.frame(cbind(train, train_labels$class))
colnames(original_data)[23] <- "class"
original_data$class <- as.factor(original_data$class)


####################################################################################################################

### KNN
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.knn <- train(class~.,
                     method     = "knn",
                     tuneGrid   = expand.grid(k = 1:20),
                     trControl  = train.control,
                     metric     = "Accuracy",
                     data       = original_data)
# Summarize the results
print(pred.knn) # 0.31

### Naive Bayes
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.nb <- train(class~.,
                  method     = "nb",
                  #tuneGrid   = expand.grid(k = 1:20),
                  trControl  = train.control,
                  metric     = "Accuracy",
                  data       = original_data)
# Summarize the results
print(pred.nb) # 0.31

### LDA
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.lda <- train(class ~., method = "lda", trControl  = train.control,  metric = "Accuracy",
                         data = original_data)
# Summarize the results
print(pred.lda) # 0.38


### QDA
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.qda <- train(class ~., method = "qda", trControl  = train.control,  metric = "Accuracy",
                         data = original_data)
print(pred.qda)
#Summarize the results
print(pred.qda) # 0.33

### Indicator Matrix
ntrain <- dim(original_data)[1]
k<-5
set.seed(2020)

# we shuffle the data and partition it in 5 folds
original_data <- original_data[sample(ntrain),]

set1.1 <- original_data[c(1:174),]
set1.2 <- original_data[c(174:348),]
set1.3 <- original_data[c(349:521),]
set1.4 <- original_data[c(522:694),]
set1.5 <- original_data[c(695:867),]


# we have to do 5 iterations changing the test set to do k fold cv
train1 <- rbind(set1.1, set1.2, set1.5, set1.4)
ntrain1 <- dim(train1)[1]
Y<-matrix(rep(0,ntrain1*k),nrow=ntrain1)
test1 <- set1.3

for (i in 1:ntrain1) {
  Y[i,train1$class[i]]<-1
  # for dataset 1
  regfits <- lm(Y~.,data=train1)}

ntest1<-dim(test1)[1]
yhattest <- numeric(ntest1)
yfitstest <- matrix(rep(0,ntest1*k),nrow=ntest1)
for (i in 1:ntest1) {
  yfitstest[i,] <- as.matrix(test1[i,1:22])%*%regfits$coef[1:22,]+regfits$coef[23,]
  yhattest[i]<-which(yfitstest[i,]==max(yfitstest[i,]))
}
# misclassification error estimate
print("Error: ")
sum(yhattest[1:ntest1]!=test1[,23])/ntest1

# accuracy estimate
print("Accuracy: ")
sum(yhattest[1:ntest1]==test1[,23])/ntest1

### Logistic Regression
pred.logis <- nnet::multinom(class~., data = original_data)
predicted.classes <- pred.logis %>% predict(test1)
mean(predicted.classes == test1$class) # accuracy

### Decision Tree
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.dec_tree_orig <- train(original_data[,c(1:22)],original_data$class, method = "rpart",
                            trControl  = train.control,  metric = "Accuracy")
print(pred.dec_tree_orig)

### Conditional Tree
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.cond_tree_orig <- train(original_data[,c(1:22)],original_data$class, method = "ctree",
                            trControl  = train.control,  metric = "Accuracy")
print(pred.cond_tree_orig)

### Random Forest
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.rf_orig <- train(original_data[,c(1:22)],original_data$class, method = "rf",
                             trControl  = train.control,  metric = "Accuracy")
print(pred.rf_orig) # 0.54

### SVM
#Linear
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_linear_orig <- train(original_data[,c(1:22)],original_data$class, method = "svmLinear",
                      trControl  = train.control,  metric = "Accuracy")
print(pred.svm_linear_orig) # 0.3699
#Radial
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_radial_orig <- train(original_data[,c(1:22)],original_data$class, method = "svmRadial",
                              trControl  = train.control,  metric = "Accuracy")
print(pred.svm_radial_orig) # 0.3710363
#POlynomial
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_poly_orig <- train(original_data[,c(1:22)],original_data$class, method = "svmPoly",
                              trControl  = train.control,  metric = "Accuracy")
print(pred.svm_poly_orig) # 0.4002326


