##################################
###### EDA 2.1 (WITHOUT NA) ######
##################################

# CORRELATION MATRIX

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
train_labels$X <- NULL
test$X <- NULL

# Summary of dataset
cat("Dimensions of the training set: ", dim(train), "\n")
summary(train)
summary(train_labels)

####################################
######## CORRELATION MATRIX ########
####################################

cor(train[,c(4:23)]) # correlation matrix
cor(train$ndvi_sw,train$ndvi_se) # 0.8091525
cor(train$precipitation_amt_mm, train$reanalysis_sat_precip_amt_mm) # 0.9988104
cor(train$precipitation_amt_mm, train$reanalysis_precip_amt_kg_per_m2) # 0.4875255 low but they both say the same
cor(train$reanalysis_avg_temp_k, train$reanalysis_air_temp_k) # 0.9973875
cor(train$reanalysis_avg_temp_k, train$reanalysis_dew_point_temp_k) # 0.8842569
cor(train$reanalysis_avg_temp_k, train$reanalysis_max_air_temp_k) # 0.926691
cor(train$reanalysis_avg_temp_k, train$reanalysis_min_air_temp_k) # 0.9356675
cor(train$reanalysis_specific_humidity_g_per_kg, train$reanalysis_dew_point_temp_k) # 0.99823
cor(train$reanalysis_tdtr_k, train$station_diur_temp_rng_c) # 0.3666345

# these variables have the highest correlation and cor.test stars
# reanalysis_air_temp_k = reanalysis_avg_temp_k -> keep reanalysis_avg_temp_k
# precipitation_amt_mm = reanalysis_sat_precip_amt_mm -> different measure methods, check https://www.drivendata.org/competitions/44/dengai-predicting-disease-spread/page/82/
# reanalysis_dew_point_temp_k = reanalysis_specific_humidity_g_per_kg -> keep reanalysis_specific_humidity_g_per_kg

chart.Correlation(train[,c(4:23)], histogram = TRUE, pch = 19)

###### CONSTRUCTION OF THE CORRELATION MATRIX IN A SUITABLE FORMAT ######

cor_mat <- round(cor(train[,c(4:23)]),4)
print(cor_mat)
melted_cor_mat <- melt(cor_mat)
print(melted_cor_mat)
colnames(melted_cor_mat) <- c("Variable1", "Variable2", "Corr")

#HEATMAP OF THE NEW CORRELATION MATRIX
ggplot(data = melted_cor_mat, aes(x=Variable1, y=Variable2)) +
  geom_tile(aes(fill = Corr), colour="White") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())