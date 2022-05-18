################################
###### VARIABLE SELECTION ######
################################

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

##########################################################
######## CORRELATION MATRIX OF NORMAL COVARIABLES ########
##########################################################

# some correlations between variables which we think will pose problems
cor(total_train) # correlation matrix
cor(total_train$ndvi_sw,total_train$ndvi_se) # 0.8091525
cor(total_train$precipitation_amt_mm, total_train$reanalysis_sat_precip_amt_mm) # 0.9988104
cor(total_train$precipitation_amt_mm, total_train$reanalysis_precip_amt_kg_per_m2) # 0.4875255 low but they both say the same
cor(total_train$reanalysis_avg_temp_k, total_train$reanalysis_air_temp_k) # 0.9973875
cor(total_train$reanalysis_avg_temp_k, total_train$reanalysis_dew_point_temp_k) # 0.8842569
cor(total_train$reanalysis_avg_temp_k, total_train$reanalysis_max_air_temp_k) # 0.926691
cor(total_train$reanalysis_avg_temp_k, total_train$reanalysis_min_air_temp_k) # 0.9356675
cor(total_train$reanalysis_specific_humidity_g_per_kg, total_train$reanalysis_dew_point_temp_k) # 0.99823
cor(total_train$reanalysis_tdtr_k, total_train$station_diur_temp_rng_c) # 0.3666345
cor(total_train$reanalysis_specific_humidity_g_per_kg, total_train$total_cases)
cor(total_train$reanalysis_specific_humidity_g_per_kg, total_train$weekofyear)
cor(total_train$reanalysis_dew_point_temp_k, total_train$total_cases)


chart.Correlation(total_train, histogram = TRUE, pch = 19)

###### CONSTRUCTION OF THE CORRELATION MATRIX IN A SUITABLE FORMAT ######

cor_mat <- round(cor(total_train),4)
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

###### 1ST ANALYSIS OF CORRELATION ######

# VARIABLES: reanalysis_air_temp_k and reanalysis_avg_temp_k

round(cor(total_train$reanalysis_avg_temp_k, total_train$reanalysis_air_temp_k),3) # cor = 0.9973875

round(var(total_train$reanalysis_avg_temp_k),3) # var = 1.455133
round(var(total_train$reanalysis_air_temp_k),3) # var = 1.501216
# Retain the variable reanalysis_avg_temp_k and delete the reanalysis_air_temp_k

which(colnames(total_train)=="reanalysis_avg_temp_k") # 9
which(colnames(total_train)=="reanalysis_air_temp_k") # 8

# Updated correlation matrix (eliminate column 8)
chart.Correlation(total_train[,-c(8)], histogram = TRUE, pch = 19)


# VARIABLES: reanalysis_specific_humidity_g_per_kg and reanalysis_dew_point_temp_k

round(cor(total_train$reanalysis_dew_point_temp_k, total_train$reanalysis_specific_humidity_g_per_kg),3) # cor = 0.99823
cor(total_train$reanalysis_dew_point_temp_k, total_train$reanalysis_relative_humidity_percent)

round(var(total_train$reanalysis_dew_point_temp_k),3) # var = 2.557566
round(var(total_train$reanalysis_specific_humidity_g_per_kg),3) # var = 2.508669
boxplot(total_train$reanalysis_dew_point_temp_k, total_train$reanalysis_specific_humidity_g_per_kg)
# Retain the covariable reanalysis_dew_point_temp_k and remove reanalysis_specific_humidity_g_per_kg
# They have very similar variances but if we look at their correlation with other covariables
# We conclude that reanalysis_specific_humidity_g_per_kg is higher correlated with more variables than dew_point

which(colnames(total_train)=="reanalysis_specific_humidity_g_per_kg") # 16
which(colnames(total_train)=="reanalysis_dew_point_temp_k") # 10

# Updated correlation matrix (eliminate column 16)
chart.Correlation(total_train[,-c(8,16)], histogram = TRUE, pch = 19)


# VARIABLES: reanalysis_tdtr_k and station_diur_temp_rng_c

round(cor(total_train$reanalysis_tdtr_k, total_train$station_diur_temp_rng_c),3) # cor = 0.3666345

round(var(total_train$reanalysis_tdtr_k),3) # var = 0.2427182
round(var(total_train$station_diur_temp_rng_c),3) # var = 0.7056787
# Retain the variable reanalysis_tdtr_k and remove station_diur_temp_rng_c 

which(colnames(total_train)=="reanalysis_tdtr_k") # 17
which(colnames(total_train)=="station_diur_temp_rng_c") # 19

# Updated correlation matrix (eliminate column 19)
chart.Correlation(total_train[,-c(8,16,19)], histogram = TRUE, pch = 19)


# VARIABLES: reanalysis_max_air_temp_k and station_max_temp_c

round(cor(total_train$reanalysis_max_air_temp_k, total_train$station_max_temp_c),3) # cor = 0.757411

round(var(total_train$reanalysis_max_air_temp_k),3) # var = 1.575882
round(var(total_train$station_max_temp_c),3) # var = 2.85056
# Retain the variable reanalysis_max_air_temp_k and remove station_max_temp_c

which(colnames(total_train)=="reanalysis_max_air_temp_k") # 11
which(colnames(total_train)=="station_max_temp_c") # 20

# Updated correlation matrix (eliminate column 20)
chart.Correlation(total_train[,-c(8,16,19,20)], histogram = TRUE, pch = 19)


# VARIABLES: reanalysis_min_air_temp_k and station_min_temp_c

cor(total_train$reanalysis_min_air_temp_k, total_train$station_min_temp_c) # cor = 0.8280897

round(var(total_train$reanalysis_min_air_temp_k),3) # var = 1.671331
round(var(total_train$station_min_temp_c),3) # var = 2.3339
# Retain the variable reanalysis_min_air_temp_k and remove station_min_temp_c

which(colnames(total_train)=="reanalysis_min_air_temp_k") # 12
which(colnames(total_train)=="station_min_temp_c") # 21

# Updated correlation matrix (eliminate column 21)
chart.Correlation(total_train[,-c(8,16,19,20,21)], histogram = TRUE, pch = 19)


# VARIABLES: reanalysis_avg_temp_k and station_avg_temp_c

round(cor(total_train$reanalysis_avg_temp_k, total_train$station_avg_temp_c),3) # cor = 0.8765021

round(var(total_train$reanalysis_avg_temp_k),3) # var = 1.455133
round(var(total_train$station_avg_temp_c),3) # var = 1.967607
# Retain the variable reanalysis_avg_temp_k and remove station_avg_temp_c

which(colnames(total_train)=="reanalysis_avg_temp_k") # 9
which(colnames(total_train)=="station_avg_temp_c") # 18

# Updated correlation matrix (eliminate column 18)
chart.Correlation(total_train[,-c(8,16,18,19,20,21)], histogram = TRUE, pch = 19)


# VARIABLES: reanalysis_relative_humidity_percent and reanalysis_dew_point_temp_k

round(cor(total_train$reanalysis_dew_point_temp_k, total_train$reanalysis_relative_humidity_percent),3) # cor = 0.6971214

round(var(total_train$reanalysis_dew_point_temp_k),3) # var = 2.557566
round(var(total_train$reanalysis_relative_humidity_percent),3) # var = 11.43347
# Retain the covariate reanalysis_relative_humidity_percent since the covariable reanalysis_dew_point_temp_k
# is highly correlated with other variables

which(colnames(total_train)=="reanalysis_dew_point_temp_k") # 10
which(colnames(total_train)=="reanalysis_relative_humidity_percent") # 14

# Updated correlation matrix (eliminate column 14)
chart.Correlation(total_train[,-c(8,14,16,18,19,20,21)], histogram = TRUE, pch = 19)

###########################################
###### TRANSFORMATION OF COVARIABLES ######
###########################################

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
summary(total_train)
summary(total_train$class) # number of observations per class


which(colnames(total_train)=="precipitation_amt_mm") # 7
which(colnames(total_train)=="reanalysis_precip_amt_kg_per_m2") # 13
which(colnames(total_train)=="reanalysis_sat_precip_amt_mm") # 15
which(colnames(total_train)=="station_precip_mm") # 22

plot_histogram(total_train[,c(7,13,15,22)])
plot_qq(total_train[,c(7,13,15,22)])

total_train$Class <- factor(total_train$class, levels = c(1,2,3,4,5), 
                              labels = c("Low", "Low-Medium", "Medium","High","Very High"))

# histogram variable precipitation_amt_mm
ggplot(total_train, aes(x = precipitation_amt_mm, color = Class))+
  geom_histogram(aes(fill = Class), alpha = 1, colour = "black", binwidth = 9)+
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 11, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Frequency") +
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175,200,225,250)) +
  scale_x_continuous(breaks = c(0,50,100,150,200,250,300,350,400))

# histogram variable reanalysis_precip_amt_kg_per_m2
ggplot(total_train, aes(x = reanalysis_precip_amt_kg_per_m2, color = Class))+
  geom_histogram(aes(fill = Class), alpha = 1, colour = "black", binwidth = 9)+
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 11, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Frequency") +
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175,200)) +
  scale_x_continuous(breaks = c(0,50,100,150,200,250,300,350,400,450,500,550,600,650,700))

# histogram variable reanalysis_sat_precip_amt_mm
ggplot(total_train, aes(x = reanalysis_sat_precip_amt_mm, color = Class))+
  geom_histogram(aes(fill = Class), alpha = 1, colour = "black", binwidth = 9)+
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 11, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Frequency") +
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175,200,225,250)) +
  scale_x_continuous(breaks = c(0,50,100,150,200,250,300,350,400))

# histogram variable station_precip_mm
ggplot(total_train, aes(x = station_precip_mm, color = Class))+
  geom_histogram(aes(fill = Class), alpha = 1, colour = "black", binwidth = 9)+
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 11, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Frequency") +
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175,200,225)) +
  scale_x_continuous(breaks = c(0,50,100,150,200,250,300))


train_log <- update_columns(total_train,
    c("precipitation_amt_mm","reanalysis_sat_precip_amt_mm","reanalysis_precip_amt_kg_per_m2","station_precip_mm"),
    function(x) log(x + 1))

# Confirming the 0 value cases of they remain the same
which(total_train$precipitation_amt_mm == 0)
length(which(total_train$precipitation_amt_mm == 0))
which(total_train_log$precipitation_amt_mm ==0)
length(which(total_train_log$precipitation_amt_mm ==0))

which(total_train$reanalysis_sat_precip_amt_mm == 0)
length(which(total_train$reanalysis_sat_precip_amt_mm == 0))
which(total_train_log$reanalysis_sat_precip_amt_mm ==0)
length(which(total_train_log$reanalysis_sat_precip_amt_mm ==0))

plot_histogram(train_log[,c("precipitation_amt_mm","reanalysis_sat_precip_amt_mm","reanalysis_precip_amt_kg_per_m2",
                            "station_precip_mm")])
plot_qq(train_log[,c("precipitation_amt_mm","reanalysis_sat_precip_amt_mm","reanalysis_precip_amt_kg_per_m2",
                     "station_precip_mm")])

summary(train_log)


# histogram variable log(precipitation_amt_mm + 1)
ggplot(train_log, aes(x = precipitation_amt_mm, color = Class))+
  geom_histogram(aes(fill = Class), alpha = 1, colour = "black", binwidth = 0.25)+
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 11, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Frequency") +
  xlab("log(precipitation_amt_mm + 1)") +
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175,200,225)) +
  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6))

# histogram variable log(reanalysis_precip_amt_kg_per_m2 + 1)
ggplot(train_log, aes(x = reanalysis_precip_amt_kg_per_m2, color = Class))+
  geom_histogram(aes(fill = Class), alpha = 1, colour = "black", binwidth = 0.25)+
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 11, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Frequency") +
  xlab("log(reanalysis_precip_amt_kg_per_m2 + 1)") +
  scale_y_continuous(breaks = c(0,20,40,60,80,100,120)) +
  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6))

# histogram variable log(reanalysis_sat_precip_amt_mm + 1)
ggplot(train_log, aes(x = reanalysis_sat_precip_amt_mm, color = Class))+
  geom_histogram(aes(fill = Class), alpha = 1, colour = "black", binwidth = 0.25)+
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 11, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Frequency") +
  xlab("log(reanalysis_sat_precip_amt_mm + 1)") +
  scale_y_continuous(breaks = c(0,25,50,75,100,125,150,175,200,225)) +
  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6))

# histogram variable log(station_precip_mm + 1)
ggplot(train_log, aes(x = station_precip_mm, color = Class))+
  geom_histogram(aes(fill = Class), alpha = 1, colour = "black", binwidth = 0.25)+
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 11, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Frequency") +
  xlab("log(station_precip_mm + 1)") +
  scale_y_continuous(breaks = c(0,10,20,30,40,50,60,70,80,90)) +
  scale_x_continuous(breaks = c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6))

# Determine the new column for each of the transformed covariables
which(colnames(train_log)=="precipitation_amt_mm") # 7
which(colnames(train_log)=="reanalysis_precip_amt_kg_per_m2") # 13
which(colnames(train_log)=="reanalysis_sat_precip_amt_mm") # 15
which(colnames(train_log)=="station_precip_mm") # 22

colnames(train_log)[7] <- "log(precipitation_amt_mm + 1)"
colnames(train_log)[13] <- "log(reanalysis_precip_amt_kg_per_m2 + 1)"
colnames(train_log)[15] <- "log(reanalysis_sat_precip_amt_mm + 1)"
colnames(train_log)[22] <- "log(station_precip_mm + 1)"
which(colnames(train_log) == "Class")
summary(train_log)

# CORRELATION MATRIX OF THE NEW DATASET WITH TRANSFORMED COVARIABLES WITHOUT MENTIONED COVARIABLES
chart.Correlation(train_log[,-c(8,14,16,18,19,20,21,24,25)], histogram = TRUE, pch = 19)

cor_mat <- round(cor(train_log[,-c(8,14,16,18,19,20,21,24,25)]),4)
print(cor_mat)
melted_cor_mat <- melt(cor_mat)
print(melted_cor_mat)
colnames(melted_cor_mat) <- c("Variable1", "Variable2", "Corr")

ggplot(data = melted_cor_mat, aes(x=Variable1, y=Variable2)) +
  geom_tile(aes(fill = Corr), colour="White") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())


train_elim_pre <- train_log[,-c(7,8,14,16,18,19,20,21,24,25)] # remove only  precipitation_amt_mm
train_elim_re <- train_log[,-c(8,13,14,16,18,19,20,21,24,25)] # remove only reanalysis_precip_amt_kg_per_m2

# I THINK THIS WILL BE THE BEST CAUSE STATION_PRECIP HAS A BIGGER VARIANCE THAN REANALYSIS
# assuming that precpitation_amt_mm is good to explain the total cases cause if not best eliminate precipitation_amt
train_elim_stat <- train_log[,-c(8,14,16,18,19,20,21,22,24,25)] # remove only station_precip_mm

chart.Correlation(train_elim_pre, histogram = TRUE, pch = 19)
chart.Correlation(train_elim_re, histogram = TRUE, pch = 19)
chart.Correlation(train_elim_stat, histogram = TRUE, pch = 19)

#HEATMAP OF THE NEW CORRELATION MATRIX FOR train_sta_pre
cor_mat <- round(cor(train_elim_re),4)
print(cor_mat)
melted_cor_mat <- melt(cor_mat)
print(melted_cor_mat)
colnames(melted_cor_mat) <- c("Variable1", "Variable2", "Corr")

ggplot(data = melted_cor_mat, aes(x=Variable1, y=Variable2)) +
  geom_tile(aes(fill = Corr), colour="White") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#HEATMAP OF THE NEW CORRELATION MATRIX FOR train_sta_re
cor_mat <- round(cor(train_elim_stat),4)
print(cor_mat)
melted_cor_mat <- melt(cor_mat)
print(melted_cor_mat)
colnames(melted_cor_mat) <- c("Variable1", "Variable2", "Corr")

ggplot(data = melted_cor_mat, aes(x=Variable1, y=Variable2)) +
  geom_tile(aes(fill = Corr), colour="White") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

#HEATMAP OF THE NEW CORRELATION MATRIX FOR train_pre_re
cor_mat <- round(cor(train_elim_pre),4)
print(cor_mat)
melted_cor_mat <- melt(cor_mat)
print(melted_cor_mat)
colnames(melted_cor_mat) <- c("Variable1", "Variable2", "Corr")

ggplot(data = melted_cor_mat, aes(x=Variable1, y=Variable2)) +
  geom_tile(aes(fill = Corr), colour="White") +
  scale_fill_gradient(low = "white", high = "blue") +
  theme(text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

# VARIABLES: log(precipitation_amt_mm + 1) and log(reanalysis_sat_precip_amt_mm + 1)

round(cor(train_log$`log(precipitation_amt_mm + 1)`, train_log$`log(reanalysis_sat_precip_amt_mm + 1)`),3) # cor = 0.9985342

round(cor(train_log$`log(precipitation_amt_mm + 1)`, train_log$`log(station_precip_mm + 1)`),3) # cor = 0.459
round(cor(train_log$`log(precipitation_amt_mm + 1)`, train_log$`log(reanalysis_precip_amt_kg_per_m2 + 1)`),3)# cor=0.576
round(cor(train_log$`log(reanalysis_precip_amt_kg_per_m2 + 1)`, train_log$`log(station_precip_mm + 1)`),3)# cor = 0.645 

round(cor(total_train$precipitation_amt_mm, total_train$reanalysis_sat_precip_amt_mm),3) # cor = 0.9985342
round(var(train_log$`log(reanalysis_sat_precip_amt_mm + 1)`),3) # var = 2.938953

round(var(train_log$`log(precipitation_amt_mm + 1)`),3) # var = 2.931
round(var(train_log$`log(reanalysis_precip_amt_kg_per_m2 + 1)`),3) # var = 0.918
round(var(train_log$`log(station_precip_mm + 1)`),3) # var = 1.29

round(var(total_train$precipitation_amt_mm),3)
var(total_train$reanalysis_precip_amt_kg_per_m2)
round(var(total_train$reanalysis_sat_precip_amt_mm),3)
var(total_train$station_precip_mm)
# Retain the variable precipitation_amt_mm and delete reanalysis_sat_precip_amt_mm

which(colnames(train_log)=="log(precipitation_amt_mm + 1)") # 7
which(colnames(train_log)=="log(reanalysis_precip_amt_kg_per_m2 + 1)") # 13
which(colnames(train_log)=="log(station_precip_mm + 1)") # 22

##################################################################################
################### FINAL DATASET ################################################
##################################################################################

chart.Correlation(train_log[,-c(8,14,16,18,19,20,21,24)], histogram = TRUE, pch = 19)

train_elim_pre <- train_log[,-c(7,8,14,16,18,19,20,21,24)] # remove only precipitation_amt_mm
train_elim_re <- train_log[,-c(8,13,14,16,18,19,20,21,24)] # remove only reanalysis_precip_amt_kg_per_m2

# I THINK THIS WILL BE THE BEST CAUSE STATION_PRECIP HAS A BIGGER VARIANCE THAN REANALYSIS
# assuming that precpitation_amt_mm is good to explain the total cases cause if not best eliminate precipitation_amt
train_elim_stat <- train_log[,-cc(8,14,16,18,19,20,21,22,24)] # remove only station_precip_mm









