##########################
###### EDA 1.1 (NA) ######
##########################

# BOXPLOTS

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

###### IMPORT DATA ######
setwd("C:/Users/Carlos Farinha/Desktop/IST/4º Ano/2º Semestre/MEDM/dengue-mosquito-spreading")

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

########################################
###### BOXPLOTS FOR EACH VARIABLE ######
########################################

##############################################
###### ANALYSIS OF THE NDVI_NE VARIABLE ######
##############################################

summary(train_na$ndvi_ne)

# BOXPLOT FOR NVDI_NE VARIABLE
ggplot(train_na, aes(x = "", y = ndvi_ne)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Pixel northeast of city centroid") +
  scale_y_continuous(breaks = c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of Pixel northeast of city centroid ")

#IDENTIFY OUTLIERS FOR NDVI_NE VARIABLE USING TUKEY
is_over_supndvi_ne <- train_na$ndvi_ne > 0.11328 + 1.5 * (0.11328 - 0.00876)
head(is_over_supndvi_ne)
sum(is_over_supndvi_ne)
train_labels[is_over_supndvi_ne,]
which_over_supndvi_ne <- which(is_over_supndvi_ne)
train_labels[which_over_supndvi_ne,]
which_over_supndvi_ne

is_under_infndvi_ne <- train_na$ndvi_ne < 0.00876 - 1.5 * (0.11328 - 0.00876)
head(is_under_infndvi_ne)
sum(is_under_infndvi_ne)
train_na[is_under_infndvi_ne,]
which_under_infndvi_ne <- which(is_under_infndvi_ne)
train_na[which_under_infndvi_ne,]
which_under_infndvi_ne

##############################################
###### ANALYSIS OF THE NVDI_NW VARIABLE ######
##############################################

summary(train_na$ndvi_nw)

# BOXPLOT FOR NVDI_NW VARIABLE
ggplot(train_na, aes(x = "", y = ndvi_nw)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Pixel northwest of city centroid") +
  scale_y_continuous(breaks = c(-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of Pixel northwest of city centroid ")

#IDENTIFY OUTLIERS FOR NVDI_NW VARIABLE USING TUKEY
is_over_supndvi_nw <- train_na$ndvi_nw > 0.12166 + 1.5*(0.12166-0.02523)
head(is_over_supndvi_nw)
sum(is_over_supndvi_nw)
train_na[is_over_supndvi_nw,]
which_over_supndvi_nw <- which(is_over_supndvi_nw)
train_na[which_over_supndvi_nw,]
which_over_supndvi_nw

is_under_infndvi_nw <- train_na$ndvi_nw < 0.02523 - 1.5*(0.12166-0.02523)
head(is_under_infndvi_nw)
sum(is_under_infndvi_nw)
train_na[is_under_infndvi_nw,]
which_under_infndvi_nw <- which(is_under_infndvi_nw)
train_na[which_under_infndvi_nw,]
which_under_infndvi_nw

##############################################
###### ANALYSIS OF THE NVDI_SE VARIABLE ######
##############################################

summary(train_na$ndvi_se)

# BOXPLOT FOR NVDI_SE VARIABLE
ggplot(train_na, aes(x = "", y = ndvi_se)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Pixel southeast of city centroid") +
  scale_y_continuous(breaks = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of Pixel southeast of city centroid ")

#IDENTIFY OUTLIERS FOR NVDI_SE VARIABLE USING TUKEY
is_over_supndvi_se <- train_na$ndvi_se > 0.21256 + 1.5*(0.21256-0.14044)
head(is_over_supndvi_se)
sum(is_over_supndvi_se)
train_na[is_over_supndvi_se,]
which_over_supndvi_se <- which(is_over_supndvi_se)
train_na[which_over_supndvi_se,]
which_over_supndvi_se

is_under_infndvi_se <- train_na$ndvi_se < 0.14044 - 1.5*(0.21256-0.14044)
head(is_under_infndvi_se)
sum(is_under_infndvi_se)
train_na[is_under_infndvi_se,]
which_under_infndvi_se <- which(is_under_infndvi_se)
train_na[which_under_infndvi_se,]
which_under_infndvi_se

##############################################
###### ANALYSIS OF THE NVDI_SW VARIABLE ######
##############################################

summary(train_na$ndvi_sw)

# BOXPLOT FOR NVDI_SW VARIABLE
ggplot(train_na, aes(x = "", y = ndvi_sw)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Pixel southeast of city centroid") +
  scale_y_continuous(breaks = c(0,0.05,0.1,0.15,0.2,0.25,0.3,0.35,0.4,0.45,0.5)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of Pixel southeast of city centroid ")

#IDENTIFY OUTLIERS FOR NVDI_SW VARIABLE USING TUKEY
is_over_supndvi_sw <- train_na$ndvi_sw > 0.20401 + 1.5*(0.20401-0.13052)
head(is_over_supndvi_sw)
sum(is_over_supndvi_sw)
train_na[is_over_supndvi_sw,]
which_over_supndvi_sw <- which(is_over_supndvi_sw)
train_na[which_over_supndvi_sw,]
which_over_supndvi_sw

is_under_infndvi_sw <- train_na$ndvi_sw < 0.13052 - 1.5*(0.20401-0.13052)
head(is_under_infndvi_sw)
sum(is_under_infndvi_sw)
train_na[is_under_infndvi_sw,]
which_under_infndvi_sw <- which(is_under_infndvi_sw)
train_na[which_under_infndvi_sw,]
which_under_infndvi_sw

###########################################################
###### ANALYSIS OF THE PRECIPITATION_AMT_MM VARIABLE ######
###########################################################

summary(train_na$precipitation_amt_mm)

# BOXPLOT FOR PRECIPITATION_AMT_MM VARIABLE
ggplot(train_na, aes(x = "", y = precipitation_amt_mm)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total precipitation") +
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350,400)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of Total precipitation ")

#IDENTIFY OUTLIERS FOR PRECIPITATION_AMT_MM VARIABLE USING TUKEY
is_over_supprecip <- train_na$precipitation_amt_mm > 52.2350 + 1.5*(52.2350 - 0.8525)
head(is_over_supprecip)
sum(is_over_supprecip)
train_na[is_over_supprecip,]
which_over_supprecip <- which(is_over_supprecip)
train_na[which_over_supprecip,]
which_over_supprecip

############################################################
###### ANALYSIS OF THE REANALYSIS_AIR_TEMP_K VARIABLE ######
############################################################

summary(train_na$reanalysis_air_temp_k)

# BOXPLOT FOR REANALYSIS_AIR_TEMP_K VARIABLE
ggplot(train_na, aes(x = "", y = reanalysis_air_temp_k)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Mean air temperature") +
  scale_y_continuous(breaks = c(295,296,297,298,299,300,301,302,303)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of mean air temperature ")

#IDENTIFY OUTLIERS FOR REANALYSIS_AIR_TEMP_K VARIABLE USING TUKEY
is_over_supair_k <- train_na$reanalysis_air_temp_k > 300.1 + 1.5 * (300.1-298.2)
head(is_over_supair_k)
sum(is_over_supair_k)
train_na[is_over_supair_k,]
which_over_supair_k <- which(is_over_supair_k)
train_na[which_over_supair_k,]
which_over_supair_k

is_under_infair_k <- train_na$reanalysis_air_temp_k < 298.2 - 1.5* (300.1-298.2)
head(is_under_infair_k)
sum(is_under_infair_k)
train_na[is_under_infair_k,]
which_under_infair_k <- which(is_under_infair_k)
train_na[which_under_infair_k,]
which_under_infair_k

############################################################
###### ANALYSIS OF THE REANALYSIS_AVG_TEMP_K VARIABLE ######
############################################################

summary(train_na$reanalysis_avg_temp_k)

# BOXPLOT FOR REANALYSIS_AVG_TEMP_K VARIABLE
ggplot(train_na, aes(x = "", y = reanalysis_avg_temp_k)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Average air temperature") +
  scale_y_continuous(breaks = c(295,296,297,298,299,300,301,302,303)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of average air temperature ")

#IDENTIFY OUTLIERS FOR REANALYSIS_AVG_TEMP_K VARIABLE USING TUKEY
is_over_supavg_k <- train_na$reanalysis_avg_temp_k > 300.2 + 1.5*(300.2-298.3)
head(is_over_supavg_k)
sum(is_over_supavg_k)
train_na[is_over_supavg_k,]
which_over_supavg_k <- which(is_over_supavg_k)
train_na[which_over_supavg_k,]
which_over_supavg_k

is_under_infavg_k <- train_na$reanalysis_avg_temp_k < 298.3 - 1.5* (300.2-298.2)
head(is_under_infavg_k)
sum(is_under_infavg_k)
train_na[is_under_infavg_k,]
which_under_infavg_k <- which(is_under_infavg_k)
train_na[which_under_infavg_k,]
which_under_infavg_k

##################################################################
###### ANALYSIS OF THE REANALYSIS_DEW_POINT_TEMP_K VARIABLE ######
##################################################################

summary(train_na$reanalysis_dew_point_temp_k)

# BOXPLOT FOR REANALYSIS_DEW_POINT_TEMP_K VARIABLE
ggplot(train_na, aes(x = "", y = reanalysis_dew_point_temp_k)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Mean dew point temperature") +
  scale_y_continuous(breaks = c(289,290,291,292,293,294,295,296,297,298)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of mean dew point temperature")

#IDENTIFY OUTLIERS FOR REANALYSIS_DEW_POINT_TEMP_K VARIABLE USING TUKEY
is_under_infdew <- train_na$reanalysis_dew_point_temp_k < 296.4 - 1.5*(296.4-293.9)
head(is_under_infdew)
sum(is_under_infdew)
train_na[is_under_infdew,]
which_under_infdew <- which(is_under_infdew)
train_na[which_under_infdew,]
which_under_infdew

################################################################
###### ANALYSIS OF THE REANALYSIS_MAX_AIR_TEMP_K VARIABLE ######
################################################################

summary(train_na$reanalysis_max_air_temp_k)

# BOXPLOT FOR REANALYSIS_MAX_AIR_TEMP_K VARIABLE
ggplot(train_na, aes(x = "", y = reanalysis_max_air_temp_k)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Maximum air temperature") +
  scale_y_continuous(breaks = c(297,298,299,300,301,302,303,304)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of maximum air temperature")

#IDENTIFY OUTLIERS FOR REANALYSIS_MAX_AIR_TEMP_K VARIABLE USING TUKEY
is_over_supmax_k <- train_na$reanalysis_max_air_temp_k > 302.4 + 1.5*(302.4-300.4)
head(is_over_supmax_k)
sum(is_over_supmax_k)
train_na[is_over_supmax_k,]
which_over_supmax_k <- which(is_over_supmax_k)
train_na[which_over_supmax_k,]
which_over_supmax_k

is_under_infmax_k <- train_na$reanalysis_max_air_temp_k < 298.3 - 1.5* (300.2-298.2)
head(is_under_infmax_k)
sum(is_under_infmax_k)
train_na[is_under_infmax_k,]
which_under_infmax_k <- which(is_under_infmax_k)
train_na[which_under_infmax_k,]
which_under_infmax_k

################################################################
###### ANALYSIS OF THE REANALYSIS_MIN_AIR_TEMP_K VARIABLE ######
################################################################

summary(train_na$reanalysis_min_air_temp_k)

# BOXPLOT FOR REANALYSIS_MIN_AIR_TEMP_K VARIABLE
ggplot(train_na, aes(x = "", y = reanalysis_min_air_temp_k)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Minimum air temperature") +
  scale_y_continuous(breaks = c(297,298,299,300,301,302,303,304)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of minimum air temperature")

#IDENTIFY OUTLIERS FOR REANALYSIS_MIN_AIR_TEMP_K VARIABLE USING TUKEY
is_over_supmin_k <- train_na$reanalysis_min_air_temp_k > 298.3 + 1.5*(298.3-296.3)
head(is_over_supmin_k)
sum(is_over_supmin_k)
train_na[is_over_supmin_k,]
which_over_supmin_k <- which(is_over_supmin_k)
train_na[which_over_supmin_k,]
which_over_supmin_k

is_under_infmin_k <- train_na$reanalysis_min_air_temp_k < 296.3 - 1.5* (298.3-296.3)
head(is_under_infmin_k)
sum(is_under_infmin_k)
train_na[is_under_infmin_k,]
which_under_infmin_K <- which(is_under_infmin_k)
train_na[which_under_infmin_K,]
which_under_infmin_K

######################################################################
###### ANALYSIS OF THE REANALYSIS_PRECIP_AMT_KG_PER_M2 VARIABLE ######
######################################################################

summary(train_na$reanalysis_precip_amt_kg_per_m2)

# BOXPLOT FOR REANALYSIS_PRECIP_AMT_KG_PER_M2 VARIABLE
ggplot(train_na, aes(x = "", y = reanalysis_precip_amt_kg_per_m2)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total precipitation") +
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350,400,450,500,550,600)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of total precipitation")

#IDENTIFY OUTLIERS FOR REANALYSIS_PRECIP_AMT_KG_PER_M2 VARIABLE USING TUKEY
is_over_supprecip_kg <- train_na$reanalysis_precip_amt_kg_per_m2 > 38.30 + 1.5*(38.30-11.20)
head(is_over_supprecip_kg)
sum(is_over_supprecip_kg)
train_na[is_over_supprecip_kg,]
which_over_supprecip_kg <- which(is_over_supprecip_kg)
train_na[which_over_supprecip_kg,]
which_over_supprecip_kg

is_under_infprecip_kg <- train_na$reanalysis_precip_amt_kg_per_m2 < 11.20 - 1.5* (38.30 - 11.20)
head(is_under_infprecip_kg)
sum(is_under_infprecip_kg)
train_na[is_under_infprecip_kg,]
which_under_infprecip_kg <- which(is_under_infprecip_kg)
train_na[which_under_infprecip_kg,]
which_under_infprecip_kg

###########################################################################
###### ANALYSIS OF THE REANALYSIS_RELATIVE_HUMIDITY_PERCENT VARIABLE ######
###########################################################################

summary(train_na$reanalysis_relative_humidity_percent)

# BOXPLOT FOR REANALYSIS_RELATIVE_HUMIDITY_PERCENT VARIABLE
ggplot(train_na, aes(x = "", y = reanalysis_relative_humidity_percent)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Mean relative humidity") +
  scale_y_continuous(breaks = c(66,68,70,72,74,76,78,80,82,84,86,88)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of mean relative humidity")

#IDENTIFY OUTLIERS FOR REANALYSIS_RELATIVE_HUMIDITY_PERCENT VARIABLE USING TUKEY
is_over_suprel <- train_na$reanalysis_relative_humidity_percent > 81.13 + 1.5*(81.13 - 76.60)
head(is_over_suprel)
sum(is_over_suprel)
train_na[is_over_suprel,]
which_over_suprel <- which(is_over_suprel)
train_na[which_over_suprel,]
which_over_suprel

is_under_infrel <- train_na$reanalysis_relative_humidity_percent < 76.60 - 1.5* (81.13 - 76.60)
head(is_under_infrel)
sum(is_under_infrel)
train_na[is_under_infrel,]
which_under_infrel <- which(is_under_infrel)
train_na[which_under_infrel,]
which_under_infrel

###################################################################
###### ANALYSIS OF THE REANALYSIS_SAT_PRECIP_AMT_MM VARIABLE ######
###################################################################

summary(train_na$reanalysis_sat_precip_amt_mm)

# BOXPLOT FOR REANALYSIS_SAT_PRECIP_AMT_MM VARIABLE
ggplot(train_na, aes(x = "", y = reanalysis_sat_precip_amt_mm)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total precipitation") +
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350,400)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of total precipitation")

#IDENTIFY OUTLIERS FOR REANALYSIS_SAT_PRECIP_AMT_MM VARIABLE USING TUKEY
is_over_supsat <- train_na$reanalysis_sat_precip_amt_mm > 52.2350 + 1.5*(52.2350 - 0.8525)
head(is_over_supsat)
sum(is_over_supsat)
train_na[is_over_supsat,]
which_over_supsat <- which(is_over_supsat)
train_na[which_over_supsat,]
which_over_supsat

is_under_infsat <- train_na$reanalysis_sat_precip_amt_mm < 0.8525 - 1.5* (52.2350 - 0.8525)
head(is_under_infsat)
sum(is_under_infsat)
train_na[is_under_infsat,]
which_under_infsat <- which(is_under_infsat)
train_na[which_under_infsat,]
which_under_infsat

############################################################################
###### ANALYSIS OF THE REANALYSIS_SPECIFIC_HUMIDITY_G_PER_KG VARIABLE ######
############################################################################

summary(train_na$reanalysis_specific_humidity_g_per_kg)

# BOXPLOT FOR REANALYSIS_SPECIFIC_HUMIDITY_G_PER_KG VARIABLE
ggplot(train_na, aes(x = "", y = reanalysis_specific_humidity_g_per_kg)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Mean specific humidity") +
  scale_y_continuous(breaks = c(11,12,13,14,15,16,17,18,19,20)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of mean specific humidity")

#IDENTIFY OUTLIERS FOR REANALYSIS_SPECIFIC_HUMIDITY_G_PER_KG VARIABLE USING TUKEY
is_over_supspf <- train_na$reanalysis_specific_humidity_g_per_kg > 17.88 + 1.5*(17.88 - 15.27)
head(is_over_supspf)
sum(is_over_supspf)
train_na[is_over_supspf,]
which_over_supspf <- which(is_over_supspf)
train_na[which_over_supspf,]
which_over_supspf

is_under_infspf <- train_na$reanalysis_specific_humidity_g_per_kg < 15.27 - 1.5* (17.88 - 15.27)
head(is_under_infspf)
sum(is_under_infspf)
train_na[is_under_infspf,]
which_under_infspf <- which(is_under_infspf)
train_na[which_under_infspf,]
which_under_infspf

########################################################
###### ANALYSIS OF THE REANALYSIS_TDTR_K VARIABLE ######
########################################################

summary(train_na$reanalysis_tdtr_k)

# BOXPLOT FOR REANALYSIS_TDTR_K VARIABLE
ggplot(train_na, aes(x = "", y = reanalysis_tdtr_k)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Diurnal temperature range") +
  scale_y_continuous(breaks = c(1,1.5,2,2.5,3,3.5,4,4.5)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of diurnal temperature range")

#IDENTIFY OUTLIERS FOR REANALYSIS_TDTR_K VARIABLE USING TUKEY
is_over_suptdtr <- train_na$reanalysis_tdtr_k > 2.771 + 1.5*(2.771 - 2.143)
head(is_over_suptdtr)
sum(is_over_suptdtr)
train_na[is_over_suptdtr,]
which_over_suptdtr <- which(is_over_suptdtr)
train_na[which_over_suptdtr,]
which_over_suptdtr

is_under_inftdtr <- train_na$reanalysis_tdtr_k < 2.143 - 1.5* (2.771 - 2.143)
head(is_under_inftdtr)
sum(is_under_inftdtr)
train_na[is_under_inftdtr,]
which_under_inftdtr <- which(is_under_inftdtr)
train_na[which_under_inftdtr,]
which_under_inftdtr

#########################################################
###### ANALYSIS OF THE STATION_AVG_TEMP_C VARIABLE ######
#########################################################

summary(train_na$station_avg_temp_c)

# BOXPLOT FOR STATION_AVG_TEMP_C VARIABLE
ggplot(train_na, aes(x = "", y = station_avg_temp_c)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Average temperature") +
  scale_y_continuous(breaks = c(22,23,24,25,26,27,28,29,30,31)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of average temperature")

#IDENTIFY OUTLIERS FOR STATION_AVG_TEMP_C VARIABLE USING TUKEY
is_over_supavg_c <- train_na$station_avg_temp_c > 28.17 + 1.5*(28.17 - 25.87)
head(is_over_supavg_c)
sum(is_over_supavg_c)
train_na[is_over_supavg_c,]
which_over_supavg_c <- which(is_over_supavg_c)
train_na[which_over_supavg_c,]
which_over_supavg_c

is_under_infavg_c <- train_na$station_avg_temp_c < 25.87 - 1.5* (28.17 - 25.87)
head(is_under_infavg_c)
sum(is_under_infavg_c)
train_na[is_under_infavg_c,]
which_under_infavg_c <- which(is_under_infavg_c)
train_na[which_under_infavg_c,]
which_under_infavg_c

##############################################################
###### ANALYSIS OF THE STATION_DIUR_TEMP_RNG_C VARIABLE ######
##############################################################

summary(train_na$station_diur_temp_rng_c)

# BOXPLOT FOR STATION_DIUR_TEMP_RNG_C VARIABLE
ggplot(train_na, aes(x = "", y = station_diur_temp_rng_c)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Diurnal temperature range") +
  scale_y_continuous(breaks = c(4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of diurnal temperature range")

#IDENTIFY OUTLIERS FOR STATION_DIUR_TEMP_RNG_C VARIABLE USING TUKEY
is_over_supdiur <- train_na$station_diur_temp_rng_c > 7.300 + 1.5*(7.300 - 6.257)
head(is_over_supdiur)
sum(is_over_supdiur)
train_na[is_over_supdiur,]
which_over_supdiur <- which(is_over_supdiur)
train_na[which_over_supdiur,]
which_over_supdiur

is_under_infdiur <- train_na$station_diur_temp_rng_c < 6.257 - 1.5* (7.300 - 6.257)
head(is_under_infdiur)
sum(is_under_infdiur)
train_na[is_under_infdiur,]
which_under_infdiur <- which(is_under_infdiur)
train_na[which_under_infdiur,]
which_under_infdiur

#########################################################
###### ANALYSIS OF THE STATION_MAX_TEMP_C VARIABLE ######
#########################################################

summary(train_na$station_max_temp_c)

# BOXPLOT FOR STATION_MAX_TEMP_C VARIABLE
ggplot(train_na, aes(x = "", y = station_max_temp_c)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Maximum temperature") +
  scale_y_continuous(breaks = c(26,27,28,29,30,31,32,33,34,35,36)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of maximum temperature")

#IDENTIFY OUTLIERS FOR STATION_MAX_TEMP_C VARIABLE USING TUKEY
is_over_supmax_c <- train_na$station_max_temp_c > 32.80 + 1.5*(32.80 - 30.60)
head(is_over_supmax_c)
sum(is_over_supmax_c)
train_na[is_over_supmax_c,]
which_over_supmax_c <- which(is_over_supmax_c)
train_na[which_over_supmax_c,]
which_over_supmax_c

is_under_infmax_c <- train_na$station_max_temp_c < 30.60 - 1.5* (32.80 - 30.60)
head(is_under_infmax_c)
sum(is_under_infmax_c)
train_na[is_under_infmax_c,]
which_under_infmax_c <- which(is_under_infmax_c)
train_na[which_under_infmax_c,]
which_under_infmax_c


#########################################################
###### ANALYSIS OF THE STATION_MIN_TEMP_C VARIABLE ######
#########################################################

summary(train_na$station_min_temp_c)

# BOXPLOT FOR STATION_MAX_TEMP_C VARIABLE
ggplot(train_na, aes(x = "", y = station_min_temp_c)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Minimum temperature") +
  scale_y_continuous(breaks = c(17,18,19,20,21,22,23,24,25,26)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of minimum temperature")

#IDENTIFY OUTLIERS FOR STATION_MAX_TEMP_C VARIABLE USING TUKEY
is_over_supmin_c <- train_na$station_min_temp_c > 23.90 + 1.5*(23.90 - 21.70)
head(is_over_supmin_c)
sum(is_over_supmin_c)
train_na[is_over_supmin_c,]
which_over_supmin_c <- which(is_over_supmin_c)
train_na[which_over_supmin_c,]
which_over_supmin_c

is_under_infmin_c <- train_na$station_min_temp_c < 21.70 - 1.5* (23.90 - 21.70)
head(is_under_infmin_c)
sum(is_under_infmin_c)
train_na[is_under_infmin_c,]
which_under_infmin_c <- which(is_under_infmin_c)
train_na[which_under_infmin_c,]
which_under_infmin_c

########################################################
###### ANALYSIS OF THE STATION_PRECIP_MM VARIABLE ######
########################################################

summary(train_na$station_precip_mm)

# BOXPLOT FOR STATION_PRECIP_MM VARIABLE
ggplot(train_na, aes(x = "", y = station_precip_mm)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 3, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 6, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total precipitation") +
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  ggtitle("Boxplot of total precipitation")

#IDENTIFY OUTLIERS FOR STATION_PRECIP_MM VARIABLE USING TUKEY
is_over_supstprecip <- train_na$station_precip_mm > 35.90 + 1.5*(35.90 - 7)
head(is_over_supstprecip)
sum(is_over_supstprecip)
train_na[is_over_supstprecip,]
which_over_supstprecip <- which(is_over_supstprecip)
train_na[which_over_supstprecip,]
which_over_supstprecip

is_under_infstprecip <- train_na$station_precip_mm < 7 - 1.5* (35.90 - 7)
head(is_under_infstprecip)
sum(is_under_infstprecip)
train_na[is_under_infstprecip,]
which_under_infstprecip <- which(is_under_infstprecip)
train_na[which_under_infstprecip,]
which_under_infstprecip

####################################
######## CORRELATION MATRIX ########
####################################

cor(na.omit(train_na[,c(4:23)])) # correlation matrix
chart.Correlation(na.omit(train_na[,c(4:23)]), histogram = TRUE, pch = 19)

#CONSTRUCTION OF THE CORRELATION MATRIX IN A SUITABLE FORMAT
cor_mat <- round(cor(na.omit(train_na[,c(4:23)])),4)
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