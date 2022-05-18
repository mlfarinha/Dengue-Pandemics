########################
###### EDA 1 (NA) ######
########################

library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
library(mice) # for imputation if needed
library(RColorBrewer)
library(VIM)
library(DataExplorer)

# If DataExplorer doesn't work run this
if (!require(devtools)) install.packages("devtools")
devtools::install_github("boxuancui/DataExplorer")
# 
# if (!require(devtools)) install.packages("devtools")
# devtools::install_github("boxuancui/DataExplorer", ref = "develop")

setwd("C:/Users/Carlos Farinha/Desktop/IST/4º Ano/2º Semestre/MEDM/dengue-mosquito-spreading")

####################################
###### DATASET WITH NA VALUES ######
####################################

train_na <- read.table("Train_Set.csv", sep = ",", header = TRUE)
train_labels_na <- read.table("Train_Set_Labels.csv", sep = ",", header = TRUE)
test_na <- read.table("Test_Set.csv", sep = ",", header = TRUE)

summary(train_na)

# Selecting only San Juan city
train_na <- train_na %>% filter(city == 'sj')
train_labels_na <- train_labels_na %>% filter(city == 'sj')
test_na <- test_na %>% filter(city == 'sj')

# Remove identification of the city because it isn't required
train_na$city <- NULL
train_labels_na$city <- NULL
test_na$city <- NULL

# Summary of dataset
cat("Dimensions of the training set: ", dim(train_na), "\n")
summary(train_na)
summary(train_labels_na)

# Checking NA values in all dataset using mice package
md.pattern(train_na)
md.pattern(train_labels_na)

train_na_plot <- aggr(train_na, col=c('navyblue','yellow'), numbers=TRUE, sortVars=TRUE, labels=names(train_na),
                     cex.axis=.7, gap=3, ylab=c("Missing data","Pattern"))


###### DATA EXPLORER PACKAGE ######

# Generic view of datasets
data_list <- list(train_na, train_labels_na, test_na)
plot_str(data_list)

# Basis Satistics
introduce(train_na)
introduce(train_labels_na$total_cases)
plot_intro(train_na)
plot_intro(train_labels_na$total_cases)

# Missing values: NA
plot_missing(train_na) # covariable ndvi_ne and ndvi_nw account for most of the NA values
plot_missing(train_labels_na) # no NA values
plot_missing(test_na) # covariable ndvi_ne accounts for most of the NA values

# Distributions
plot_bar(train_na) # No discrete features
plot_bar(train_labels_na$total_cases) # No discrete features

plot_histogram(train_na) # histogram for each variable
plot_histogram(train_labels_na$total_cases) # as expected the distribution is skewed

# Maybe set the variables Year and Week of the year to categorical
#train_na <- update_columns(train_na, c("weekofyear"), as.factor)
#train_na <- update_columns(train_na, c("year"), as.factor)
#summary(train_na)

# QQ Plot
plot_qq(train_na[,c(4:23)])
# check if some of the covariables should be transformed to straighten the tails

# Correlation Analysis
plot_correlation(na.omit(train_na))
