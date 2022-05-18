###################################################################
##### ROBUST PRINCIPAL COMPONENT ANALYSIS: STATION_PRECIP_MM ######
###################################################################

library(gdata)
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(RColorBrewer)
library(viridis)
library(scales)
library(PerformanceAnalytics)
library(reshape2)
library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
library(DataExplorer)
library(purrr)
library(kedd)
library(rrcov)
library(MASS)
library(caret)

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

# STATION_PRECIP_AMT
train_station <- train_log[,c(1,2,3,4,5,6,9,10,11,12,17,22)]
train_station <- cbind(train_station, train_labels$class)
colnames(train_station)[13] <- "class"
summary(train_station)


####################
###### ROBPCA ######
####################

robust_station <- PcaHubert(train_station[,c(-13)], scale = TRUE)
print(robust_station, print.x = TRUE) # k = 4, 1-alpha = 0.975, SD = 3.643721, OD = 2.63047
robust_station$eigenvalues
robust_station$cutoff.sd 
robust_station$cutoff.od 
robust_station$crit.pca.distances
summary(robust_station) # we see that the number of PC to retain is 4

is_over_sd <- robust_station$sd > robust_station$cutoff.sd
which_is_over_sd <- which(is_over_sd)
which_is_over_sd 
# 14 27 257 258 396 507 508 514 517 545 564 565 566 592 620 656 669 712 715 726 770 774 775 776 781 800 822 825 862

is_over_od <- robust_station$od > robust_station$cutoff.od
which_is_over_od <- which(is_over_od)
which_is_over_od 

outliers <- robust_station$sd > robust_station$cutoff.sd | robust_station$od > robust_station$cutoff.od
which_outliers <- which(outliers)
which_outliers 
length(which_outliers) # 82

# Plot of SD vs OD
par(mfrow=c(1,2))
plot(PcaClassic(train_station[,c(1:12)], scale = TRUE, k = 9), sub = "Standardized variables")
plot(PcaHubert(train_station[,c(1:12)], scale = TRUE, k = 4), sub="Standardized variables")

# Dataframe of SD vs OD
data_robust <- cbind(robust_station$sd, robust_station$od, train_station$class)
data_robust <- as.data.frame(data_robust)
colnames(data_robust)[1] <- "Score distance"
colnames(data_robust)[2] <- "Orthogonal distance"
colnames(data_robust)[3] <- "Class"
summary(data_robust)
score_intercept <- robust_station$cutoff.sd
orthogonal_intercept <- robust_station$cutoff.od

ggplot(data = data_robust, aes(x = `Score distance`, y = `Orthogonal distance`, color=Class)) +
  geom_point(stat="identity", alpha=0.4) +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank(), text = element_text(size = 13, face = "italic")) +
  labs(x="Score distance", y="Orthogonal distance") +
  scale_color_gradientn(colours = rainbow(6)) +
  geom_hline(yintercept = orthogonal_intercept, linetype = "dashed", color = "black", size = 1) +
  geom_vline(xintercept = score_intercept, linetype = "dashed", color = "black", size = 1)

####################################################################################################################

# NEW DATASET OF STATION_PRECIP_MM WITHOUT OUTLIERS
train_station <- train_log[,c(1,2,3,4,5,6,9,10,11,12,17,22)]
train_station <- cbind(train_station, train_labels$class)
colnames(train_station)[13] <- "class"
summary(train_station)

train_station_out <- train_station[-which_outliers,]
dim(train_station_out)
summary(train_station_out)
train_station_out <- as.data.frame(train_station_out)
summary(train_station_out)

####################################################################################################################

### KNN
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.knn_out <- train(class~.,
                      method     = "knn",
                      tuneGrid   = expand.grid(k = 1:20),
                      trControl  = train.control,
                      metric     = "Accuracy",
                      data       = train_station_out)
# Summarize the results
print(pred.knn_out) # 0.4506409

### Naive Bayes
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.nb_out <- train(class~.,
                     method     = "nb",
                     #tuneGrid   = expand.grid(k = 1:20),
                     trControl  = train.control,
                     metric     = "Accuracy",
                     data       = train_station_out)
# Summarize the results
print(pred.nb_out) # 0.3774900

### LDA
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.lda_station_out <- train(class ~., method = "lda", trControl = train.control,  metric = "Accuracy",
                              data = train_station_out)
print(pred.lda_station_out) #  0.3384831

### LINDA (Robust LDA)
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.rlda_station_out <- train(class ~., method = "Linda", trControl = train.control,  metric = "Accuracy",
                               data = train_station_out)
print(pred.rlda_station_out) #  0.3720221

### QDA
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.qda_station_out <- train(class ~., method = "qda", trControl = train.control,  metric = "Accuracy",
                              data = train_station_out)
print(pred.qda_station_out) # 0.3614361

### QdaCov (Robust QDA)
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.rqda_station_out <- train(class ~., method = "QdaCov", trControl = train.control,  metric = "Accuracy",
                               data = train_station_out)
print(pred.rqda_station_out) # 0.3724465

### Indicator Matrix
ntrain <- dim(train_station_out)[1]
k<-5
set.seed(2020)
dim(train_station_out)
ntrain
# we shuffle the data and partition it in 5 folds
train_station_out <- train_station_out[sample(ntrain),]

set1.1 <- train_station_out[c(1:157),]
set1.2 <- train_station_out[c(157:314),]
set1.3 <- train_station_out[c(314:471),]
set1.4 <- train_station_out[c(471:628),]
set1.5 <- train_station_out[c(628:785),]


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
  yfitstest[i,] <- as.matrix(test1[i,1:12])%*%regfits$coef[1:12,]+regfits$coef[13,]
  yhattest[i]<-which(yfitstest[i,]==max(yfitstest[i,]))
}
# misclassification error estimate
print("Error: ")
sum(yhattest[1:ntest1]!=test1[,13])/ntest1

# accuracy estimate
print("Accuracy: ") # 0.278481
sum(yhattest[1:ntest1]==test1[,13])/ntest1

### Logistic Regression
pred.logis_out <- nnet::multinom(class~., data = train_station_out)
predicted.classes_out <- pred.logis_out %>% predict(test1)
mean(predicted.classes_out == test1$class) # accuracy 0.3607595

### Decision Tree
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.dec_tree_station_out <- train(class ~., method = "rpart", trControl = train.control,  metric = "Accuracy",
                                   data = train_station_out)
print(pred.dec_tree_station_out) # 0.3318400

### Conditional Tree
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.cond_tree_station_out <- train(class ~., method = "ctree", trControl = train.control,  metric = "Accuracy",
                                    data = train_station_out)
print(pred.cond_tree_station_out) # 0.4306614

### Random Forest
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.rf_station_out <- train(class ~., method = "rf", trControl = train.control,  metric = "Accuracy",
                             data = train_station_out)
print(pred.rf_station_out) # 0.5376081

### Linear SVM
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_linear_station_out <- train(class ~., method = "svmLinear", trControl = train.control, metric = "Accuracy",
                                     data = train_station_out)
print(pred.svm_linear_station_out) # 0.3358919

### Radial SVM
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_radial_station_out <- train(class ~., method = "svmRadial", trControl = train.control, metric = "Accuracy",
                                     data = train_station_out)
print(pred.svm_radial_station_out) # 0.3957862

### Polynomial SVM
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_pol_station_out <- train(class ~., method = "svmPoly", trControl = train.control, metric = "Accuracy",
                                  data = train_station_out)
print(pred.svm_pol_station_out) # 0.4072601

####################################################################################################################

# PCA WITHOUT OUTLIERS OF STATION_PRECIP_MM
prcomp_station_out <- prcomp(train_station_out[,-c(13)], scale. = TRUE, retx = TRUE)
summary(prcomp_station_out)

summary(prcomp_station_out) # select 5 PC -> 9 PC not to lose so much information
train_station_pca_out <- as.data.frame(cbind(prcomp_station_out$x[,c(1:9)], train_station_out[,13]))
colnames(train_station_pca_out)[10] <- "class"
summary(train_station_pca_out)
train_station_pca_out <- as.data.frame(train_station_pca_out)
train_station_pca_out$class <- as.factor(train_station_pca_out$class)
summary(train_station_pca_out)

### KNN
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.knn_pca_out <- train(class~.,
                      method     = "knn",
                      tuneGrid   = expand.grid(k = 1:20),
                      trControl  = train.control,
                      metric     = "Accuracy",
                      data       = train_station_pca_out)
# Summarize the results
print(pred.knn_pca_out) # 0.3591403

### Naive Bayes
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.nb_pca_out <- train(class~.,
                     method     = "nb",
                     #tuneGrid   = expand.grid(k = 1:20),
                     trControl  = train.control,
                     metric     = "Accuracy",
                     data       = train_station_pca_out)
# Summarize the results
print(pred.nb_pca_out) # 0.3626371

### LDA
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.lda_station_pca_out <- train(class ~., method = "lda", trControl = train.control,  metric = "Accuracy",
                              data = train_station_pca_out)
print(pred.lda_station_pca_out) # 0.3537218

### LINDA (Robust LDA)
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.rlda_station_pca_out <- train(class ~., method = "Linda", trControl = train.control,  metric = "Accuracy",
                               data = train_station_pca_out)
print(pred.rlda_station_pca_out) #  0.3864712

### QDA
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.qda_station_pca_out <- train(class ~., method = "qda", trControl = train.control,  metric = "Accuracy",
                              data = train_station_pca_out)
print(pred.qda_station_pca_out) # 0.3559021

### QdaCov (Robust QDA)
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.rqda_station_pca_out <- train(class ~., method = "QdaCov", trControl = train.control,  metric = "Accuracy",
                               data = train_station_pca_out)
print(pred.rqda_station_pca_out) # 0.362301

### Indicator Matrix
ntrain <- dim(train_station_pca_out)[1]
k<-5
set.seed(2020)

# we shuffle the data and partition it in 5 folds
train_station_pca_out <- train_station_pca_out[sample(ntrain),]

set1.1 <- train_station_pca_out[c(1:174),]
set1.2 <- train_station_pca_out[c(174:348),]
set1.3 <- train_station_pca_out[c(349:521),]
set1.4 <- train_station_pca_out[c(522:694),]
set1.5 <- train_station_pca_out[c(695:867),]


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
print("Accuracy: ") # 0.2254335
sum(yhattest[1:ntest1]==test1[,10])/ntest1

### Logistic Regression
pred.logis_pca_out <- nnet::multinom(class~., data = train_station_pca_out)
predicted.classes_pca_out <- pred.logis_pca_out %>% predict(test1)
mean(predicted.classes_pca_out == test1$class) # accuracy 0.3815029

### Decision Tree
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.dec_tree_station_pca_out <- train(class ~., method = "rpart", trControl = train.control,  metric = "Accuracy",
                                   data = train_station_pca_out)
print(pred.dec_tree_station_pca_out) # 0.2841517

### Conditional Tree
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.cond_tree_station_pca_out <- train(class ~., method = "ctree", trControl = train.control,  metric = "Accuracy",
                                    data = train_station_pca_out)
print(pred.cond_tree_station_pca_out) # 0.3176773

### Random Forest
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.rf_station_pca_out <- train(class ~., method = "rf", trControl = train.control,  metric = "Accuracy",
                             data = train_station_pca_out)
print(pred.rf_station_pca_out) # 0.3954211

### Linear SVM
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_linear_station_pca_out <- train(class ~., method = "svmLinear", trControl = train.control, metric = "Accuracy",
                                     data = train_station_pca_out)
print(pred.svm_linear_station_pca_out) # 0.3392945

### Radial SVM
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_radial_station_pca_out <- train(class ~., method = "svmRadial", trControl = train.control, metric = "Accuracy",
                                     data = train_station_pca_out)
print(pred.svm_radial_station_pca_out) # 0.3898924

### Polynomial SVM
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_pol_station_pca_out <- train(class ~., method = "svmPoly", trControl = train.control, metric = "Accuracy",
                                  data = train_station_pca_out)
print(pred.svm_pol_station_pca_out) # 0.3988588



