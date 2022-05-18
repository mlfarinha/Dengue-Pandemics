#################################################################
###### QQ PLOTS AND HISTOGRAMS FOR VARIABLE TRANSFORMATION ######
#################################################################

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
library(qqplotr)

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

summary(train)
summary(train_labels)

# Summary of dataset
cat("Dimensions of the training set: ", dim(train), "\n")
cat("Dimensions of the training_label set: ", dim(train_labels), "\n")
summary(train)
summary(train_labels)

# Merging the two datasets
total_train <- cbind(train[,-c(3)], train_labels$total_cases)
summary(total_train)
colnames(total_train)[23] <- "total_cases"
summary(total_train)
View(total_train)

# CLASSES
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

total_train <- total_train %>% mutate(class = map_dbl(total_cases, class_funct))
total_train$class <- as.factor(total_train$class)
summary(total_train$class) # number of observations per class

# QQ-PLOTS
ggplot(total_train, mapping = aes(sample=ndvi_ne)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$ndvi_ne) # 1.011e-15

ggplot(total_train, mapping = aes(sample=ndvi_nw) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$ndvi_nw) # 1.703e-09

ggplot(total_train, mapping = aes(sample=ndvi_se)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$ndvi_se) # 0.002287

ggplot(total_train, mapping = aes(sample=ndvi_sw)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$ndvi_sw) # 0.019

ggplot(total_train, mapping = aes(sample=precipitation_amt_mm)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$precipitation_amt_mm) # < 2.2e-16

ggplot(total_train, mapping = aes(sample=reanalysis_air_temp_k)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$reanalysis_air_temp_k) # 4.019e-11

ggplot(total_train, mapping = aes(sample=reanalysis_avg_temp_k)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$reanalysis_avg_temp_k) # 2.224e-10

ggplot(total_train, mapping = aes(sample=reanalysis_dew_point_temp_k)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$reanalysis_dew_point_temp_k) # < 2.2e-16

ggplot(total_train, mapping = aes(sample=reanalysis_max_air_temp_k)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$reanalysis_max_air_temp_k) # 6.331e-08

ggplot(total_train, mapping = aes(sample=reanalysis_min_air_temp_k)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$reanalysis_min_air_temp_k) # 3.263e-14

ggplot(total_train, mapping = aes(sample=reanalysis_precip_amt_kg_per_m2)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$reanalysis_precip_amt_kg_per_m2) # < 2.2e-16

ggplot(total_train, mapping = aes(sample=reanalysis_relative_humidity_percent)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$reanalysis_relative_humidity_percent) # 0.00267

ggplot(total_train, mapping = aes(sample=reanalysis_sat_precip_amt_mm)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$reanalysis_sat_precip_amt_mm) # < 2.2e-16

ggplot(total_train, mapping = aes(sample=reanalysis_specific_humidity_g_per_kg)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$reanalysis_specific_humidity_g_per_kg) # 4.924e-16

ggplot(total_train, mapping = aes(sample=reanalysis_tdtr_k)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$reanalysis_tdtr_k) # 8.201e-13

ggplot(total_train, mapping = aes(sample=station_avg_temp_c)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$station_avg_temp_c) # 3.792e-13

ggplot(total_train, mapping = aes(sample=station_diur_temp_rng_c)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$station_diur_temp_rng_c) # 0.0005713

ggplot(total_train, mapping = aes(sample=station_max_temp_c)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$station_max_temp_c) # 3.02e-13

ggplot(total_train, mapping = aes(sample=station_min_temp_c)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$station_min_temp_c) # 9.404e-14

ggplot(total_train, mapping = aes(sample=station_precip_mm)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$station_precip_mm) # < 2.2e-16

ggplot(total_train, mapping = aes(sample=station_precip_mm)) +
  stat_qq_band(bandType="ks", alpha=0.2, fill="#2d74b3") +
  stat_qq_line(linetype=2, color="black", alpha=0.5) +
  stat_qq_point(color="#b22222", alpha=0.5) +
  labs(x="Theoretical Quantiles", y = "Experimental Quantiles") +
  theme(panel.grid.minor = element_blank(), panel.border = element_blank())
shapiro.test(total_train$) # < 2.2e-16


