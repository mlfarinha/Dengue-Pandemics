#############################################################
##### PRINCIPAL COMPONENT ANALYSIS: STATION_PRECIP_AMT ######
#############################################################

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

# STATION_PRECIP_AMT
train_station <- train_log[,c(1,2,3,4,5,6,9,10,11,12,17,22)]
train_station <- cbind(train_station, train_labels$class)
colnames(train_station)[13] <- "class"
summary(train_station)

#############################
###### FUNCTION PRCOMP ######
#############################

################################################################
###### PCA ON STANDARDIZED VARIABLES (CORRELATION MATRIX) ######
################################################################

prcomp_station <- prcomp(train_station[,-c(13)], scale. = TRUE, retx = TRUE)
summary(prcomp_station)

# NUMBER OF PC's TO RETAIN
screeplot(prcomp_station, main="PCA based on Standardized Variables", type="lines")
abline(h=mean(prcomp_station$sdev^2),col="green")
plot(prcomp_station) # clear how the 1 PC dominates
biplot(prcomp_station)

# LOADINGS
load_station <- prcomp_station$r
round(load_station,3)

pc1 <- prcomp_station$rotation[,1]
pc1 # loadings of PC1

pc2 <- prcomp_station$rotation[,2]
pc2 # loadings of PC2

# SCORES
score_station <- prcomp_station$x # scores for each PC for the observations
score_station
predict(prcomp_station)[,1] # score for each observation on the PC 1

# PLOT OF PC1 VS PC2
plot(score_station[,1:2],xlab="CP1",ylab="CP2",lwd=2)
abline(h=0,v=0,col="green")

# PLOT OF EACH PC WITH IT'S ASSOCIATED EXPLAINED PERCENTAGE
variance <- prcomp_station$sdev^2
round(variance,3)
percentage <- round(variance/sum(variance)*100,2)
coul <- brewer.pal(9, "Set1")
barplot(percentage, ylim = c(0,50), names.arg = c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12"),
        ylab = "Percentage of variability explained by each PC", font.axis=3, cex.axis = 1, cex.lab = 0.9, font.lab = 2,
        col= coul, width = 0.2, space = 0.2, border = NA, xlab = "Principal Components")

# PLOT OF EACH PC VERSUS IT'S CORRESPONDING VARIANCE
barplot(variance, ylim = c(0,5),  names.arg = c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11","PC12"),
        ylab = "Variance of each PC", font.axis=3, cex.axis = 1, cex.lab = 0.9, font.lab = 2,
        col= coul, width = 0.2, space = 0.2, border = NA, xlab = "Principal Components")

# SCATTERPLOT OF THE PC1 VS PC2
pc_plot_prcomp_station <- cbind(prcomp_station$x[,c(1,2)],train_station$class)
pc_plot_prcomp_station <- as.data.frame(pc_plot_prcomp_station)
colnames(pc_plot_prcomp_station)[3] <- "Class"

ggplot(data = pc_plot_prcomp_station, aes(x = PC1, y = PC2, colour = Class)) +
  geom_point(stat="identity", alpha=0.4) +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank(), text = element_text(size = 13, face = "italic")) +
  labs(x="PC1", y="PC2") +
  scale_color_gradientn(colours = rainbow(6))

######################################## CLASSIFIERS ON PCA ########################################################

# DATASET WITH PCA OF STATION_PRECIP_MM

summary(prcomp_station) # select 5 PC >- 9 components not to lose so much information
train_station_pca <- as.data.frame(cbind(prcomp_station$x[,c(1:9)], train_station[,13]))
colnames(train_station_pca)[10] <- "class"
summary(train_station_pca)
train_station_pca <- as.data.frame(train_station_pca)
train_station_pca$class <- as.factor(train_station_pca$class)
summary(train_station_pca)

### KNN
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.knn_pca <- train(class~.,
                  method     = "knn",
                  tuneGrid   = expand.grid(k = 1:20),
                  trControl  = train.control,
                  metric     = "Accuracy",
                  data       = train_station_pca)
# Summarize the results
print(pred.knn_pca) # 0.3621437

### Naive Bayes
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
set.seed(2020)
pred.nb_pca <- train(class~.,
                 method     = "nb",
                 #tuneGrid   = expand.grid(k = 1:20),
                 trControl  = train.control,
                 metric     = "Accuracy",
                 data       = train_station_pca)
# Summarize the results
print(pred.nb_pca) # 0.3774900

### LDA
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.lda_station_pca <- train(class ~., method = "lda", trControl = train.control,  metric = "Accuracy",
                                 data = train_station_pca)
print(pred.lda_station_pca) # 0.3424972

### LINDA (Robust LDA)
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.rlda_station_pca <- train(class ~., method = "Linda", trControl = train.control,  metric = "Accuracy",
                              data = train_station_pca)
print(pred.rlda_station_pca) #  0.3636941

### QDA
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.qda_station_pca <- train(class ~., method = "qda", trControl = train.control,  metric = "Accuracy",
                                 data = train_station_pca)
print(pred.qda_station_pca) # 0.353316

### QdaCov (Robust QDA)
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.rqda_station_pca <- train(class ~., method = "QdaCov", trControl = train.control,  metric = "Accuracy",
                              data = train_station_pca)
print(pred.rqda_station_pca) # 0.3636851

### Indicator Matrix
ntrain <- dim(train_station_pca)[1]
k<-5
set.seed(2020)

# we shuffle the data and partition it in 5 folds
train_station_pca <- train_station_pca[sample(ntrain),]

set1.1 <- train_station_pca[c(1:174),]
set1.2 <- train_station_pca[c(174:348),]
set1.3 <- train_station_pca[c(349:521),]
set1.4 <- train_station_pca[c(522:694),]
set1.5 <- train_station_pca[c(695:867),]


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
pred.logis_pca <- nnet::multinom(class~., data = train_station_pca)
predicted.classes_pca <- pred.logis_pca %>% predict(test1)
mean(predicted.classes_pca == test1$class) # accuracy 0.3757225

### Decision Tree
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.dec_tree_station_pca <- train(class ~., method = "rpart", trControl = train.control,  metric = "Accuracy",
                               data = train_station_pca)
print(pred.dec_tree_station_pca) # 0.3318400

### Conditional Tree
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.cond_tree_station_pca <- train(class ~., method = "ctree", trControl = train.control,  metric = "Accuracy",
                                   data = train_station_pca)
print(pred.cond_tree_station_pca) # 0.3429891 

### Random Forest
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.rf_station_pca <- train(class ~., method = "rf", trControl = train.control,  metric = "Accuracy",
                                    data = train_station_pca)
print(pred.rf_station_pca) # 0.4126132 

### Linear SVM
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_linear_station_pca <- train(class ~., method = "svmLinear", trControl = train.control, metric = "Accuracy",
                             data = train_station_pca)
print(pred.svm_linear_station_pca) # 0.332947 

### Radial SVM
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_radial_station_pca <- train(class ~., method = "svmRadial", trControl = train.control, metric = "Accuracy",
                                     data = train_station_pca)
print(pred.svm_radial_station_pca) # 0.3952233

### Polynomial SVM
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)

set.seed(2020)
pred.svm_pol_station_pca <- train(class ~., method = "svmPoly", trControl = train.control, metric = "Accuracy",
                                     data = train_station_pca)
print(pred.svm_pol_station_pca) # 0.4129499


