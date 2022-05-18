##################################
###### EDA 2.2 (WITHOUT NA) ######
##################################

# SCATTERPLOTS FOR EACH VARIABLE

library(ggplot2)
library(viridis)
library(RColorBrewer)
library(scales)
library(PerformanceAnalytics)
library(reshape2)
library(hrbrthemes)
library(DataExplorer)
library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots

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

# Binding the two datasets
total_train <- cbind(train, train_labels$total_cases)
summary(total_train)
View(total_train)

############################## SCATTERPLOTS #############################
plot_scatterplot(total_train, by="total_cases") # takes a really long time

#####################################################
######## SCATTERPLOT OF YEAR vs. TOTAL_CASES ########
#####################################################

ggplot(total_train, aes(x = year, y = total_cases)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total Cases") +
  xlab("Year")

###########################################################
######## SCATTERPLOT OF WEEKOFYEAR vs. TOTAL_CASES ########
###########################################################

ggplot(total_train, aes(x = weekofyear, y = total_cases)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total Cases") +
  xlab("Week of year")

########################################################
######## SCATTERPLOT OF NDVI_NE vs. TOTAL_CASES ########
########################################################

ggplot(total_train, aes(x = total_cases, y = ndvi_ne)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Pixel northeast of city centroid") +
  xlab("Total cases")

########################################################
######## SCATTERPLOT OF NDVI_sE vs. TOTAL_CASES ########
########################################################

ggplot(total_train, aes(x = total_cases, y = ndvi_se)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Pixel southeast of city centroid") +
  xlab("Total cases")

########################################################
######## SCATTERPLOT OF NDVI_NW vs. TOTAL_CASES ########
########################################################

ggplot(total_train, aes(x = total_cases, y = ndvi_nw)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Pixel northwest of city centroid") +
  xlab("Total cases")

########################################################
######## SCATTERPLOT OF NDVI_SW vs. TOTAL_CASES ########
########################################################

ggplot(total_train, aes(x = total_cases, y = ndvi_sw)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Pixel southwest of city centroid") +
  xlab("Total cases")

###################################################################
######## SCATTERPLOT OF STATION_MAX_TEMP_c vs. TOTAL_CASES ########
###################################################################

ggplot(total_train, aes(x = total_cases, y = station_max_temp_c)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Pixel northeast of city centroid") +
  xlab("Total cases")

###################################################################
######## SCATTERPLOT OF STATION_MAX_TEMP_c vs. TOTAL_CASES ########
###################################################################

ggplot(total_train, aes(x = total_cases, y = station_max_temp_c)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Maximum temperature") +
  xlab("Total Cases")

###################################################################
######## SCATTERPLOT OF STATION_MIN_TEMP_c vs. TOTAL_CASES ########
###################################################################

ggplot(total_train, aes(x = total_cases, y = station_min_temp_c)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Minimum temperature") +
  xlab("Total Cases")

###################################################################
######## SCATTERPLOT OF STATION_AVG_TEMP_c vs. TOTAL_CASES ########
###################################################################

ggplot(total_train, aes(x = total_cases, y = station_avg_temp_c)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Average temperature") +
  xlab("Total cases")

##################################################################
######## SCATTERPLOT OF STATION_PRECIP_MM vs. TOTAL_CASES ########
##################################################################

ggplot(total_train, aes(x = total_cases, y = station_precip_mm)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total precipitation") +
  xlab("Total cases")

########################################################################
######## SCATTERPLOT OF STATION_DIUR_TEMP_RNG_C vs. TOTAL_CASES ########
########################################################################

ggplot(total_train, aes(x = total_cases, y = station_max_temp_c)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Diurnal temperature range") +
  xlab("Total cases")

#####################################################################
######## SCATTERPLOT OF PRECIPITATION_AMT_MM vs. TOTAL_CASES ########
#####################################################################

ggplot(total_train, aes(x = total_cases, y = precipitation_amt_mm)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total precipitation") +
  xlab("Total cases")

#############################################################################
######## SCATTERPLOT OF REANALYSIS_SAT_PRECIP_AMT_MM vs. TOTAL_CASES ########
#############################################################################

ggplot(total_train, aes(x = total_cases, y = reanalysis_sat_precip_amt_mm)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total precipitation") +
  xlab("Total cases")

############################################################################
######## SCATTERPLOT OF REANALYSIS_DEW_POINT_TEMP_K vs. TOTAL_CASES ########
############################################################################

ggplot(total_train, aes(x = total_cases, y = reanalysis_dew_point_temp_k)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Mean dew point temperature") +
  xlab("Total cases")

#######################################################################
######## SCATTERPLOT OF REANALYSIS_AIR_TEMP_K vs. TOTAL_CASES #########
#######################################################################

ggplot(total_train, aes(x = total_cases, y = reanalysis_air_temp_k)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Mean air temperature") +
  xlab("Total cases")

#####################################################################################
######## SCATTERPLOT OF REANALYSIS_RELATIVE_HUMUDITY_PERCENT vs. TOTAL_CASES ########
#####################################################################################

ggplot(total_train, aes(x = total_cases, y = reanalysis_relative_humidity_percent)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Mean relative humidity") +
  xlab("Total cases")

#####################################################################################
######## SCATTERPLOT OF REANALYSIS_SPECIFIC_HUMIDITY_G_PER_KG vs. TOTAL_CASES #######
#####################################################################################

ggplot(total_train, aes(x = total_cases, y = reanalysis_specific_humidity_g_per_kg)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Mean specific humidity") +
  xlab("Total cases")

################################################################################
######## SCATTERPLOT OF REANALYSIS_PRECIP_AMT_KG_PER_M2 vs. TOTAL_CASES ########
################################################################################

ggplot(total_train, aes(x = total_cases, y = reanalysis_precip_amt_kg_per_m2)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total precipitation") +
  xlab("Total cases")

##########################################################################
######## SCATTERPLOT OF REANALYSIS_MAX_AIR_TEMP_K vs. TOTAL_CASES ########
##########################################################################

ggplot(total_train, aes(x = total_cases, y = reanalysis_max_air_temp_k)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Maximum air temperature") +
  xlab("Total cases")

##########################################################################
######## SCATTERPLOT OF REANALYSIS_MIN_AIR_TEMP_K vs. TOTAL_CASES ########
##########################################################################

ggplot(total_train, aes(x = total_cases, y = reanalysis_min_air_temp_k)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Minimum air temperature") +
  xlab("Total cases")

######################################################################
######## SCATTERPLOT OF REANALYSIS_AVG_TEMP_K vs. TOTAL_CASES ########
######################################################################

ggplot(total_train, aes(x = total_cases, y = reanalysis_avg_temp_k)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Average air temperature") +
  xlab("Total cases")

##################################################################
######## SCATTERPLOT OF REANALYSIS_TDTR_K vs. TOTAL_CASES ########
##################################################################

ggplot(total_train, aes(x = total_cases, y = reanalysis_tdtr_k)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Diurnal temperature range") +
  xlab("Total cases")

######################################################################################################
######## SCATTERPLOT OF REANALYSIS_DEW_POINT_TEMP_K vs. REANALYSIS_SPECIFIC_HUMIDITY_G_PER_KG ########
######################################################################################################

ggplot(total_train, aes(x = reanalysis_dew_point_temp_k, y = reanalysis_specific_humidity_g_per_kg)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Mean dew point temperature") +
  xlab("Mean specific humidity") +
  scale_x_continuous(breaks = c(289,290,291,292,293,294,295,296,297,298)) +
  scale_y_continuous(breaks = c(11,12,13,14,15,16,17,18,19))

ggplot(total_train, aes(x = reanalysis_dew_point_temp_k, y = reanalysis_relative_humidity_percent)) + 
  geom_point(color = "blue", fill = "blue", alpha = 0.5, shape = 16, size = 1) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 10, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Mean dew point temperature") +
  xlab("Mean specific humidity") +
  scale_x_continuous(breaks = c(289,290,291,292,293,294,295,296,297,298)) +
  scale_y_continuous(breaks = c(11,12,13,14,15,16,17,18,19))
