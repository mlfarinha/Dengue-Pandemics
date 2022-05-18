################################
###### EDA 2 (WITHOUT NA) ######
################################

# DataExplorer Package

library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
library(mice) # for imputation if needed
library(RColorBrewer)
library(VIM)
library(DataExplorer)
library(ggplot2)

setwd("C:/Users/Carlos Farinha/Desktop/IST/4º Ano/2º Semestre/MEDM/dengue-mosquito-spreading")

#######################################
###### DATASET WITHOUT NA VALUES ######
#######################################

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

# Checking NA values in all dataset using mice package
md.pattern(train)
md.pattern(train_labels)

###### DATA EXPLORER PACKAGE ######

# Generic view of datasets
data_list <- list(train, train_labels, test)
plot_str(data_list)

# Basis Satistics
introduce(train)
introduce(train_labels$total_cases)
plot_intro(train)
plot_intro(train_labels$total_cases)

# Missing values: NA
plot_missing(train) # no NA values
plot_missing(train_labels) # no NA values
plot_missing(test) # no NA values

# Distributions
plot_bar(train) # No discrete features
plot_bar(train_labels$total_cases) # No discrete features

plot_histogram(train) # histogram for each variable


# Transformations of variables: Check!!!!!
plot_histogram(log(train$station_precip_mm + 1)) # improved
plot_histogram(log(train$reanalysis_sat_precip_amt_mm)) # not very good
plot_histogram(log(train$reanalysis_precip_amt_kg_per_m2 + 1)) # improved

plot_histogram(train_labels)

summary(train_labels$total_cases)
ggplot(data=train_labels, aes(x=train_labels$total_cases)) +
  geom_bar(fill = "blue") +
  geom_text(stat='count', aes(label=..count..), vjust=-1, size = 2.5) +
  xlab("Total Cases of Dengue") +
  ylab("Frequency of total cases of Dengue") + 
  scale_x_continuous(breaks = c(0,50,100,150,200,250,300,350,400,450,500)) +
  scale_y_continuous(breaks = c(0,5,10,15,20,25,30,35)) +
  theme(text = element_text(size = 12, face = "italic"),
        axis.title = element_text(size = 9, face = "bold"))

# QQ Plot
plot_qq(train[,c(4:23)])
# check if some of the covariables should be transformed to straighten the tails
# station_precip_mm / reanalysis_sat_precip_amt_mm / reanalysis_precip_amt_kg_per_m2 / precipitation_amt_mm
# have tail problems (there are others but these are the ones that stand out)

# Correlation Analysis
plot_correlation(train) # temp variables have high correlation see EDA 2.1
