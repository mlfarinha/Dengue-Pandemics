###########################################
###### ROC SPACE FOR EACH CLASSIFIER ######
###########################################

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
library(rpart)
library(randomForest)

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

train_station <- train_log[,c(1,2,3,4,5,6,9,10,11,12,17,22)]
train_station <- cbind(train_station, train_labels$class)
colnames(train_station)[13] <- "class"
summary(train_station)
ntrain <- dim(train_station)[1]

####################################################################################################################

nclass1 <- length(which(train_station$class == 1))
nclass1 # 202
prop1 <- nclass1/(dim(train_station)[1])
prop1 # 0.2329873
nclass2 <- length(which(train_station$class == 2))
nclass2 # 200
prop2 <- nclass2/(dim(train_station)[1])
prop2 # 0.2306805
nclass3 <- length(which(train_station$class == 3))
nclass3 # 199
prop3 <- nclass3/(dim(train_station)[1])
prop3 # 0.2295271
nclass4 <- length(which(train_station$class == 4))
nclass4 # 199
prop4 <- nclass4/(dim(train_station)[1])
prop4 # 0.2295271
nclass5 <- length(which(train_station$class == 5))
nclass5 # 67
prop5 <- nclass5/(dim(train_station)[1])
prop5 # 0.07727797

####################################################################################################################

### KNN
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.knn_station <- train(class~., method = "knn", tuneGrid = expand.grid(k = 1:20),metric = "Accuracy",
                      data = trainstation_hold)
print(pred.knn_station) # 0.4192268

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.knn_station, teststation_hold))
# True positive rate
tpr_knn <- 0.5490*prop1 + 0.44444*prop2 + 0.4390*prop3 + 0.4091*prop4 + 0.50000*prop5
tpr_knn # 0.4637346
fpr_knn <- (1-0.8770)*prop1 + (1-0.78082)*prop2 + (1-0.8182)*prop3 + (1-0.8992)*prop4 + (1-0.95092)*prop5
fpr_knn # 0.1478752

#############################################################################################

### Naive Bayes
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.nb <- train(class~., method = "nb", metric = "Accuracy", data = trainstation_hold)
print(pred.nb) # 0.3774900

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.nb, teststation_hold))
# True positive rate
tpr_nb <- 0.3607*prop1 + 0.23077*prop2 + 0.33333*prop3 + 0.3051*prop4 + 0.28571*prop5
tpr_nb # 0.3058887
fpr_nb <- (1-0.8571)*prop1 + (1-0.7500)*prop2 + (1-0.82143)*prop3 + (1-0.7895)*prop4 + (1-0.92771)*prop5
fpr_nb # 0.1858525

###############################################################################################

### LDA
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.lda_station <- train(class ~., method = "lda",  metric = "Accuracy", data = trainstation_hold)
print(pred.lda_station) #  0.30964

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.lda_station, teststation_hold))
# True positive rate
tpr_lda <- 0.3607*prop1 + 0.23077*prop2 + 0.33333*prop3 + 0.3051*prop4 + 0.28571*prop5
tpr_lda # 0.3058887
fpr_lda <- (1-0.8571)*prop1 + (1-0.7500)*prop2 + (1-0.82143)*prop3 + (1-0.7895)*prop4 + (1-0.92771)*prop5
fpr_lda # 0.1858525


###############################################################################################

### LINDA (Robust LDA)  -> DONE AND READY TO GO
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.rlda_station <- train(class ~., method = "Linda", metric = "Accuracy", data = trainstation_hold)
print(pred.rlda_station) #  0.3198978

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.rlda_station, teststation_hold))
# True positive rate
tpr_rlda <- 0.6341*prop1 + 0.2778*prop2 + 0.24074*prop3 + 0.33333*prop4 + 0.22222*prop5
tpr_rlda # 0.3607576
fpr_rlda <- (1-0.8712)*prop1 + (1-0.7548)*prop2 + (1-0.84034)*prop3 + (1-0.79508)*prop4 + (1-0.93293)*prop5
fpr_rlda # 0.1754357

#################################################################################

### QDA -> DONE AND READY TO GO
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.qda_station <- train(class ~., method = "qda", metric = "Accuracy", data = trainstation_hold)
print(pred.qda_station) # 0.3306424

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.qda_station, teststation_hold))
# True positive rate
tpr_qda <- 0.3273*prop1 + 0.2632*prop2 + 0.30233*prop3 + 0.29787*prop4 + 0.22222*prop5
tpr_qda # 0.2919067
fpr_qda <- (1-0.8559)*prop1 + (1-0.7727)*prop2 + (1-0.80000)*prop3 + (1-0.80159)*prop4 + (1-0.89024)*prop5
fpr_qda # 0.1859351

################################################################################

### QdaCov (Robust QDA) -> DONE AND READY TO GO
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.rqda_station <- train(class ~., method = "QdaCov", metric = "Accuracy", data = trainstation_hold)
print(pred.rqda_station) # 0.3255744

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.rqda_station, teststation_hold))
# True positive rate
tpr_rqda <- 0.4286*prop1 + 0.2703*prop2 + 0.25806*prop3 + 0.3529*prop4 + 0.4000*prop5
tpr_rqda # 0.3333544
fpr_rqda <- (1-0.8306)*prop1 + (1-0.7574)*prop2 + (1-0.77465)*prop3 + (1-0.8770)*prop4 + (1-0.92262)*prop5
fpr_rqda # 0.1813667

##############################################################################

### Indicator Matrix
ntrain <- dim(train_station)[1]
k<-5
set.seed(2020)

# we shuffle the data and partition it in 5 folds
train_station <- train_station[sample(ntrain),]

set1.1 <- train_station[c(1:174),]
set1.2 <- train_station[c(174:348),]
set1.3 <- train_station[c(349:521),]
set1.4 <- train_station[c(522:694),]
set1.5 <- train_station[c(695:867),]


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
  yfitstest[i,] <- as.matrix(test1[i,1:9])%*%regfits$coef[1:9,]+regfits$coef[10,]
  yhattest[i]<-which(yfitstest[i,]==max(yfitstest[i,]))
}
# misclassification error estimate
print("Error: ")
sum(yhattest[1:ntest1]!=test1[,10])/ntest1

# accuracy estimate
print("Accuracy: ") # 0.2369942
sum(yhattest[1:ntest1]==test1[,10])/ntest1

### Logistic Regression
pred.logis <- nnet::multinom(class~., data = train_station)
predicted.classes <- pred.logis %>% predict(test1)
mean(predicted.classes == test1$class) # accuracy 0.3757225

#################################################################################################

### Decision Tree
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.dec_tree_station <- train(trainstation_hold[,c(1:12)], trainstation_hold$class, method = "rpart",
                               metric = "Accuracy")
print(pred.dec_tree_station) # 0.3802011

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.dec_tree_station, teststation_hold))
# True positive rate
tpr_dec_tree <- 0.4093*prop1 + 0.33884*prop2 + 0.39744*prop3 + 0.4488*prop4 + 0.66038*prop5
tpr_dec_tree # 0.4858159
fnr_dec_tree <- (1-0.8889)*prop1 + (1-0.82698)*prop2 + (1-0.80731)*prop3 + (1-0.8613)*prop4 + (1-0.96069)*prop5
fnr_dec_tree # 0.144898

#################################################################################################

### Conditional Tree -> DONE AND READY TO GO
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.cond_tree_station <- train(class~., method = "ctree", metric = "Accuracy", data = trainstation_hold)
print(pred.cond_tree_station) # 0.4013212

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.cond_tree_station, teststation_hold))
# True positive rate
tpr_cond_tree <- 0.6000*prop1 + 0.38235*prop2 + 0.21053*prop3 + 0.4255*prop4 + 0.55556*prop5
tpr_cond_tree # 0.4169118
fpr_cond_tree <- (1-0.8359)*prop1 + (1-0.79137)*prop2 + (1-0.8000)*prop3 + (1-0.8651)*prop4 + (1-0.96341)*prop5
fpr_cond_tree # 0.1660563

##################################################################################################

### Random Forest -> DONE AND READY TO GO
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.rf_station <- train(class~., method = "rf", metric = "Accuracy", data = trainstation_hold)
print(pred.rf_station) # 0.4573733

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.rf_station, teststation_hold))
# True positive rate
tpr_rf <- 0.6316*prop1 + 0.4348*prop2 + 0.3333*prop3 + 0.6667*prop4 + 0.625000*prop5
tpr_rf # 0.5252805
fpr_rf <- (1-0.8963)*prop1 + (1-0.8110)*prop2 + (1-0.8042)*prop3 + (1-0.9262)*prop4 + (1-0.96970)*prop5
fpr_rf # 0.1319814

####################################################################################################

### Linear SVM
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.svm_linear_station <- train(class ~., method = "svmLinear", metric = "Accuracy", data = trainstation_hold)
print(pred.svm_linear_station) # 0.3157358

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.svm_linear_station, teststation_hold))
# True positive rate
tpr_svmL <- 0.3500*prop1 + 0.25000*prop2 + 0.35000*prop3 + 0.2769*prop4 + NA*prop5
tpr_svmL # NA
fpr_svmL <- (1-0.8496)*prop1 + (1-0.74483)*prop2 + (1-0.77124)*prop3 + (1-0.8426)*prop4 + (1-0.91908)*prop5
fpr_svmL # 0.1887916

# No object in predict was assigned to class 5 so the svm with linear kernel cannot predict class 5 objects

###################################################################################################

### Radial SVM -> DONE
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.svm_radial_station <- train(class ~., method = "svmRadial", metric = "Accuracy", data = trainstation_hold)
print(pred.svm_radial_station) # 0.3657976

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.svm_radial_station, teststation_hold))
# True positive rate
tpr_svmR <- 0.4000*prop1 + 0.17143*prop2 + 0.35556*prop3 + 0.4615*prop4 + 1*prop5
tpr_svmR # 0.3975559
fpr_svmR <- (1-0.8699)*prop1 + (1-0.81159)*prop2 + (1-0.82031)*prop3 + (1-0.8209)*prop4 + (1-0.88166)*prop5
fpr_svmR # 0.1652713

####################################################################################################

### Polynomial SVM -> DONE
set.seed(2020)
train_station <- train_station[sample(ntrain),]

set_trainstation.1 <- train_station[c(1:174),]
set_trainstation.2 <- train_station[c(174:348),]
set_trainstation.3 <- train_station[c(349:521),]
set_trainstation.4 <- train_station[c(522:694),]
set_trainstation.5 <- train_station[c(695:867),]

trainstation_hold <- rbind(set_trainstation.1, set_trainstation.2, set_trainstation.3, set_trainstation.4)
teststation_hold <- set_trainstation.5

pred.svm_pol_station <- train(class ~., method = "svmPoly", metric = "Accuracy", data = trainstation_hold)
print(pred.svm_pol_station) # 0.3905469

# ROC SPACE
confusionMatrix(teststation_hold$class, predict(pred.svm_pol_station, teststation_hold))
# True positive rate
tpr_svmP <- 0.3774*prop1 + 0.20588*prop2 + 0.2500*prop3 + 0.4737*prop4 + 0.75000*prop5
tpr_svmP # 0.3594892
fpr_svmP <- (1-0.8667)*prop1 + (1-0.82014)*prop2 + (1-0.7820)*prop3 + (1-0.8222)*prop4 + (1-0.89091)*prop5
fpr_svmP # 0.1718245

####################################################################################################################
####################################################################################################################

# Create Dataframe with values of TPR and FPR for each classifier
TPR <- c(0.4637346, 0.354, 0.3058887, 0.3607576, 0.2919067, 0.3333544, 0.196, 0.247, 0.4169118, 0.5252805,
         0.3975559, 0.3594892, 1)
FPR <- c(0.1478752, 0.178, 0.1858525, 0.1754357, 0.1859351, 0.1813667, 0.195, 0.210, 0.1660563, 0.1319814,
         0.1652713, 0.1718245, 0)
Classifier <- c("kNN", "Naive Bayes", "LDA", "Robust LDA", "QDA", "Robust QDA",
                "Linear Regression of an Indicator Matrix", "Logistic Regression", "Conditional Tree",
                "Random Forest", "SVM (Radial Kernel)", "SVM (Polynomial Kernel)", "Perfect Classifier")

roc_dataset <- data.frame(TPR,FPR,Classifier, stringsAsFactors = FALSE)

# Build the plot for each classifier
ggplot(data = roc_dataset, aes(x = FPR, y = TPR, colour = Classifier)) +
  geom_point(stat="identity", alpha=1.5, size = 2) +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank(),
        text = element_text(size = 13, face = "italic")) +
  labs(x="FPR", y="TPR") +
  xlim(0,1) +
  ylim(0,1) +
  scale_color_manual(breaks = c("kNN","Naive Bayes", "LDA", "Robust LDA", "QDA", "Robust QDA",
                               "Linear Regression of an Indicator Matrix", "Logistic Regression",
                               "Conditional Tree", "Random Forest", "SVM (Radial Kernel)",
                               "SVM (Polynomial Kernel)", "Perfect Classifier"), 
                    values = c("red", "blue", "orange", "green", "brown", "darkorange1","darkorchid",
                               "hotpink", "gold", "forestgreen", "firebrick4", "black", "purple")) +
  geom_abline(intercept = 0, slope = 1, color = "blue", linetype = "dashed", size = 1.5)
