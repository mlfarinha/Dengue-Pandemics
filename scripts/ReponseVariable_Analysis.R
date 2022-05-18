###############################################
###### ANALYSIS OF THE RESPONSE VARIABLE ######
###############################################

library(kdensity)
library(SkewHyperbolic)
library(EQL)
library(extraDistr)
library(ggpubr)
library(purrr)
library(kedd)
library(dplyr) # for select, filter, summarize, etc.
library(plotly) # for fancy plots
library(corrplot) # for correlation plots
library(RColorBrewer)

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

# ANALYSIS OF THE RESPONSE VARIABLE

# Normal density estimation
hist(train_labels$total_cases, breaks = "FD")
summary(train_labels$total_cases)

# Classes obtained from quantiles
quantile(train_labels$total_cases)
# class 1: 0 - 9
# class 2: 10 - 19
# class 3: 20 - 37
# class 4: 38 - 461

# class 4 is not appropriate which is confirmed by looking at the kernel below

# KERNEL DENSITY ESTIMATION USING KERNEL::GAUSSIAN
h.amise(train_labels$total_cases, kernel = "epanechnikov", deriv.order = 0)
h.amise(train_labels$total_cases, kernel = "gaussian", deriv.order = 0)
par(mfrow = c(1,2))
kde_normal <- kdensity(train_labels$total_cases, kernel = "gaussian", adjust = 1/3, bw = "nrd")
kde_normal.plot <- plot(kde_normal , lwd = 2, col = "blue", main = "Total cases kernel density estimation",
                        cex.lab = 1.1, bty = "n", ylim = c(0,0.035), xaxt = "n", yaxt = "n")
axis(1, c(0,50,100,150,200,250,300,350,400,450,500), cex.axis = 1.1, font = 3)
axis(2, c(0,0.005,0.01,0.015,0.020,0.025,0.030,0.035), cex.axis = 1.1, font = 3)

hist.response <- hist(train_labels$total_cases, breaks = "FD", xlim = c(0,500), bty = "n", cex.lab = 1.1,
                      main = "Histogram of Total Cases variable", xaxt = "n", yaxt = "n", xlab = "Total Cases", ylab = "Frequency")
axis(1, c(0,50,100,150,200,250,300,350,400,450,500), cex.axis = 1.1, font = 3)
axis(2, c(0,10,20,30,40,50,60,70,80,90,100,110,120,130), cex.axis = 1.1, font = 3)


# Density estimation using basic methods
ruff.density <- plot(density(train_labels$total_cases), lwd = 2, col = "red", cex.lab = 1.1,
                     main = "Simple density estimation", bty = "n", xaxt = "n", yaxt = "n")
axis(1, c(0,50,100,150,200,250,300,350,400,450,500), cex.axis = 1.1, font = 3)
axis(2, c(0,0.005,0.01,0.015,0.020,0.025,0.030), cex.axis = 1.1, font = 3)

# Attempt to generate a kernel to check if we could get a more suitable class assigment
# skew_hyperbolic = list(density   = SkewHyperbolic::dskewhyp,
#                        estimator = function(x) {
#                          SkewHyperbolic::skewhypFit(x, printOut = FALSE)$param
#                        },
#                        support   = c(-Inf, Inf)
# )
# 
# kde_skewhyp = kdensity(train_labels$total_cases, start = skew_hyperbolic)
# plot(kde_skewhyp, lwd = 2, col = "blue", main = "Total Cases Density")
# lines(kde_skewhyp, plot_start = TRUE, lty = 2, lwd = 2)
# rug(train_labels$total_cases)
# This did not work due to the reduced number of observations


# Outliers in total_cases variable

summary(train_labels)

# BOXPLOT FOR TOTAL_CASES VARIABLE
ggplot(train_labels, aes(x = "", y = total_cases)) +
  geom_boxplot(color = "blue", fill = "blue", alpha= 0.2, size = 0.75, outlier.colour = "red", 
               outlier.shape = 16, outlier.size = 2, outlier.alpha = 0.9, notch = FALSE, width = 0.8) +
  theme(text = element_text(size = 15, face = "italic"), panel.background = element_rect(fill = "gray80"),
        axis.title = element_text(size = 11, face = "bold"), panel.grid.minor = element_line(color = "gray80"), 
        panel.grid.major.y = element_line(color = "white", size = 1.08)) +
  ylab("Total Cases") +
  scale_y_continuous(breaks = c(0,50,100,150,200,250,300,350,400,450,500)) + 
  scale_fill_viridis(discrete = TRUE, alpha = 0.6) +
  geom_jitter(color = "black", size = 0.7, alpha = 0.9) + 
  #ggtitle("Boxplot of Total Cases ") +
  rotate()

#IDENTIFY OUTLIERS FOR TOTAL_CASES VARIABLE USING TUKEY
is_over_suptotal_cases <- train_labels$total_cases > 37 + 1.5*(37-9)
head(is_over_suptotal_cases)
sum(is_over_suptotal_cases)
train_labels[is_over_suptotal_cases,]
which_over_suptotal_cases <- which(is_over_suptotal_cases)
train_labels[which_over_suptotal_cases,]
which_over_suptotal_cases

# the total_cases variable has 67 outliers

# NEW DENSITY WITHOUT CONSIDERING OUTLIERS
par(mfrow = c(1,2))
kde_normal.nout <- kdensity(train_labels[-which_over_supndvi_ne,4], kernel = "gaussian", start = "gaussian",
                            adjust = 1/3)
kde_normal.nout.plot <- plot(kde_normal.nout , lwd = 2, col = "blue",
                        main = "Total cases Kernel density estimation without outliers", cex.lab = 1.1, bty = "n",
                        ylim = c(0,0.04), xlim = c(0,80), xaxt = "n", yaxt = "n")
axis(1, c(0,10,20,30,40,50,60,70,80), cex.axis = 1.1, font = 3)
axis(2, c(0,0.005,0.01,0.015,0.020,0.025,0.030,0.035,0.04), cex.axis = 1.1, font = 3)

hist.response.nout <- hist(train_labels[-which_over_supndvi_ne,4], breaks = "FD", xlim = c(0,80), bty = "n",
                      cex.lab = 1.1, main = "Histogram of the response variable without outliers", ylim = c(0,140),
                      xaxt = "n", yaxt = "n", xlab = "Total Cases", ylab = "Frequency")
axis(1, c(0,10,20,30,40,50,60,70,80), cex.axis = 1.1, font = 3)
axis(2, c(0,10,20,30,40,50,60,70,80,90,100,110,120,130,140), cex.axis = 1.1, font = 3)

# Creation of classes according to response variable quantiles

quantile(train_labels[-which_over_suptotal_cases,4]) 

# Class 1 : 0 to 8 total cases
# Class 2 : 9 to 17 total cases
# Class 3 : 18 to 31 total cases
# Class 4 : 32 to 78 total cases
# Class 5 : 79 to 461 total cases is given by the outliers (class of extreme cases)

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
summary(train_labels$class) # number of observations per class

train_labels$class1 <- factor(train_labels$class, levels = c(1,2,3,4,5), 
                          labels = c("Low", "Low-Medium", "Medium","High","Very High"))

# Plots to put in report
ggplot(data=train_labels, aes(x = class1)) +
  geom_bar(fill = brewer.pal(length(unique(train_labels$class)), "Set3")) +
  geom_text(stat='count', aes(label=..count..), vjust=-1, size = 3) +
  xlab("Class") +
  ylab("Number of observations per class") + 
  scale_y_continuous(breaks = c(0,50,100,150,200,250)) +
  theme(text = element_text(size = 15, face = "italic"),
        axis.title = element_text(size = 11, face = "bold"))


coul <- brewer.pal(5, "Set1")
barplot(table(train_labels$class), ylab = "Number of observations per class", xlab = "Class",
        names.arg = c("1","2","3","4","5"), ylim = c(0,205), font.axis=3, cex.axis = 1, cex.lab = 0.9,
        font.lab = 2, col= coul, width = 0.2, space = 0.2, border = NA)



