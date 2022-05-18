##########################################################################
##### PRINCIPAL COMPONENT ANALYSIS: REANALYSIS_PRECIP_AMT_KG_PER_M2 ######
##########################################################################

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

# REANALYSIS_PRECIP_AMT_KG_PER_M2
train_reanalysis <- train_log[,c(1,2,3,4,5,6,9,10,11,12,13)]
train_reanalysis <- cbind(train_reanalysis, train_labels$class)
colnames(train_reanalysis)[12] <- "class"
summary(train_reanalysis)

#############################
###### FUNCTION PRCOMP ######
#############################

################################################################
###### PCA ON STANDARDIZED VARIABLES (CORRELATION MATRIX) ######
################################################################

prcomp_reanalysis <- prcomp(train_reanalysis[,-c(12)], scale. = TRUE, retx = TRUE)
summary(prcomp_reanalysis)

# NUMBER OF PC's TO RETAIN
screeplot(prcomp_reanalysis, main="PCA based on Standardized Variables", type="lines")
abline(h=mean(prcomp_reanalysis$sdev^2),col="green")
plot(prcomp_reanalysis) # clear how the 1 PC dominates
biplot(prcomp_reanalysis)

# LOADINGS
load_reanalysis <- prcomp_reanalysis$r
round(load_reanalysis,5)

pc1 <- prcomp_reanalysis$rotation[,1]
pc1 # loadings of PC1

pc2 <- prcomp_reanalysis$rotation[,2]
pc2 # loadings of PC2

# SCORES
score_reanalysis <- prcomp_reanalysis$x # scores for each PC for the observations
score_reanalysis
predict(prcomp_reanalysis)[,1] # score for each observation on the PC 1

# PLOT OF PC1 VS PC2
plot(score_reanalysis[,1:2],xlab="CP1",ylab="CP2",lwd=2)
abline(h=0,v=0,col="green")

# PLOT OF EACH PC WITH IT'S ASSOCIATED EXPLAINED PERCENTAGE
variance <- prcomp_reanalysis$sdev^2
percentage <- round(variance/sum(variance)*100,2)
coul <- brewer.pal(9, "Set1")
barplot(percentage, ylim = c(0,50), names.arg = c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11"),
        ylab = "Percentage of variability explained by each PC", font.axis=3, cex.axis = 1, cex.lab = 0.9, font.lab = 2,
        col= coul, width = 0.2, space = 0.2, border = NA, xlab = "Principal Components")

# PLOT OF EACH PC VERSUS IT'S CORRESPONDING VARIANCE
barplot(variance, ylim = c(0,5),  names.arg = c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9","PC10","PC11"),
        ylab = "Variance of each PC", font.axis=3, cex.axis = 1, cex.lab = 0.9, font.lab = 2,
        col= coul, width = 0.2, space = 0.2, border = NA, xlab = "Principal Components")

# NUMBER OF PC's TO RETAIN
screeplot(prcomp_reanalysis, main="PCA based on Standardized Variables", type="lines",cex=0.8)
abline(h=1,col="green")

# SCATTERPLOT OF THE PC1 VS PC2
pc_plot_prcomp_reanalysis <- cbind(prcomp_reanalysis$x[,c(1,2)],train_reanalysis$class)
pc_plot_prcomp_reanalysis <- as.data.frame(pc_plot_prcomp_reanalysis)
colnames(pc_plot_prcomp_reanalysis)[3] <- "Class"
pc_plot_prcomp_reanalysis

ggplot(data = pc_plot_prcomp_reanalysis, aes(x = PC1, y = PC2, colour = Class)) +
  geom_point(stat="identity", alpha=0.4) +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank(), text = element_text(size = 13, face = "italic")) +
  labs(x="PC1", y="PC2") +
  scale_color_gradientn(colours = rainbow(6))


# DATASET WITH PCA OF REANALYSIS_PRECIP_AMT_KG_PER_M2
library(MASS)
library(caret)
summary(prcomp_reanalysis) # select 5 PC
train_reanalysis_pca <- as.data.frame(cbind(prcomp_reanalysis$x[,c(1:5)], train_reanalysis[,12]))
colnames(train_reanalysis_pca)[6] <- "class"
summary(train_reanalysis_pca)
train_reanalysis_pca <- as.data.frame(train_reanalysis_pca)

### LDA

lda_reanalysis_pca <- lda(class ~ ., data = train_reanalysis_pca)
names(lda_reanalysis_pca)
lda_reanalysis_pca
lda.train_reanalysis_pca <- predict(lda_reanalysis_pca)
train_reanalysis_pca.lda <- lda.train_reanalysis_pca$class
table(train_reanalysis_pca.lda, train_reanalysis_pca$class)
sum(diag(table(train_reanalysis_pca.lda, train_reanalysis_pca$class)))/sum(table(train_reanalysis_pca.lda, train_reanalysis_pca$class)) # 0.3587082
sum(predict(lda_reanalysis_pca,train_reanalysis_pca)$class!=train_reanalysis_pca[,6])/dim(train_reanalysis_pca)[1] # 0.6412918

# Repeated k-fold cross validation
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
train_reanalysis_pca$class <- as.factor(train_reanalysis_pca$class)

set.seed(2020)
pred.lda_reanalysis_pca <- train(class ~., method = "lda", trControl  = train.control,  metric = "Accuracy",
                             data = train_reanalysis_pca)
print(pred.lda_reanalysis_pca) # 0.3595244

### QDA

qda_reanalysis_pca <- qda(class ~ ., data = train_reanalysis_pca)
names(qda_reanalysis_pca)
qda_reanalysis_pca
qda.train_reanalysis_pca <- predict(qda_reanalysis_pca)
train_reanalysis_pca.qda <- qda.train_reanalysis_pca$class
table(train_reanalysis_pca.qda, train_reanalysis_pca$class)
sum(diag(table(train_reanalysis_pca.qda, train_reanalysis_pca$class)))/sum(table(train_reanalysis_pca.qda, train_reanalysis_pca$class)) # 0.3587082
sum(predict(qda_reanalysis_pca,train_reanalysis_pca)$class!=train_reanalysis_pca[,6])/dim(train_reanalysis_pca)[1] # 0.6412918

# Repeated k-fold cross validation
train.control <- trainControl(method = "repeatedcv", number = 5, repeats = 3)
train_reanalysis_pca$class <- as.factor(train_reanalysis_pca$class)

set.seed(2020)
pred.qda_reanalysis_pca <- train(class ~., method = "qda", trControl  = train.control,  metric = "Accuracy",
                                 data = train_reanalysis_pca)
print(pred.qda_reanalysis_pca) # 0.3287212