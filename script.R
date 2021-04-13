## Customer Models - Assignment 1 --

# load packages
packages <- c("dplyr", "ggplot2", "tidyr", "lubridate", "mice")
install.packages(packages)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(mice)

# setwd
setwd("D:/GitHub_Repository_Collection/Customer Models Assignments/customer-models-assignment-1")

# load data
df <- read.csv("restaurants_IL.csv", header = T, sep = ",")

# drop first column
df <- df[,-1]

# inspect data
str(df)
summary(df)

### Custom Functions ###

# This function returns the number of columns with missing values in a dataframe
count_columns_with_missing <- function(dataframe) {
  
  number_columns_with_missing = sum(ifelse(sapply(dataframe, FUN = function(x) {sum(is.na(x))}) > 0,1,0))
  return(number_columns_with_missing)
}

# --------------------------------------------- # 

# This function returns a overview of the each column associated to the input dataframe
describe_columns <- function(dataframe) {
  
  out <- data.frame(dataset_name = rep(deparse(substitute(dataframe)), times = ncol(dataframe)))
  
  out$features <- rep(NA, times = ncol(dataframe))
  out$missing_values <- rep(NA, times = ncol(dataframe))
  out$percentage_missing <- rep(NA, times = ncol(dataframe))
  out$unique_levels <- rep(NA, times = ncol(dataframe))
  out$type <- rep(NA, times = ncol(dataframe))
  
  for (i in 1:ncol(dataframe)) {
    out$features[i] <- names(dataframe)[i]
    out$missing_values[i] <- sum(is.na(dataframe[,i]))
    out$percentage_missing[i] <- round((sum(is.na(dataframe[,i]))/nrow(dataframe))*100, digits = 2)
    out$unique_levels[i] <- length(unique(dataframe[,i]))
    out$type[i] <- class(dataframe[,i])
  }
  
  return(out)
}

# --------------------------------------------- # 

# replace "None" with NA
df[df == "None"] <- NA

# Inspect Missings
count_columns_with_missing(df)
describe_columns(df)

# convert to factor
df$categories <- as.factor(df$categories)
df$GoodForKids <- factor(df$GoodForKids, levels = c("False", "True"), labels = c("False", "True"))
df$RestaurantsReservations <- factor(df$RestaurantsReservations, levels = c("False", "True"), labels = c("False", "True"))
df$Caters <- factor(df$Caters, levels = c("False", "True"), labels = c("False", "True"))
df$NoiseLevel <- factor(df$NoiseLevel, levels = c("quiet", "average", "loud", "very_loud"), labels = c("quiet", "average", "loud", "very_loud"), ordered = TRUE) # ordinal for now
df$RestaurantsTakeOut <- factor(df$RestaurantsTakeOut, levels = c("False", "True"), labels = c("False", "True"))
df$RestaurantsPriceRange2 <- as.factor(df$RestaurantsPriceRange2)
df$OutdoorSeating <- factor(df$OutdoorSeating, levels = c("False", "True"), labels = c("False", "True"))
df$WiFi <- factor(df$WiFi, levels = c("no", "free", "paid"), labels = c("no", "free", "paid"))
df$RestaurantsAttire <- as.factor(df$RestaurantsAttire)
df$RestaurantsDelivery <- factor(df$RestaurantsDelivery, levels = c("False", "True"), labels = c("False", "True"))
df$BusinessAcceptsCreditCards <- factor(df$BusinessAcceptsCreditCards, levels = c("False", "True"), labels = c("False", "True"))
df$dessert <- factor(df$dessert, levels = c("False", "True"), labels = c("False", "True")) # dessert - breakfast: Good meals for X
df$latenight <- factor(df$latenight, levels = c("False", "True"), labels = c("False", "True"))
df$lunch <- factor(df$lunch, levels = c("False", "True"), labels = c("False", "True"))
df$dinner <- factor(df$dinner, levels = c("False", "True"), labels = c("False", "True"))
df$brunch <- factor(df$brunch, levels = c("False", "True"), labels = c("False", "True"))
df$breakfast <- factor(df$breakfast, levels = c("False", "True"), labels = c("False", "True"))
df$garage <- factor(df$garage, levels = c("False", "True"), labels = c("False", "True"))
df$street <- factor(df$street, levels = c("False", "True"), labels = c("False", "True"))
df$validated <- factor(df$validated, levels = c("False", "True"), labels = c("False", "True"))
df$lot <- factor(df$lot, levels = c("False", "True"), labels = c("False", "True"))
df$valet <- factor(df$valet, levels = c("False", "True"), labels = c("False", "True"))
df$OpenSundays <- factor(df$OpenSundays, levels = c(0, 1), labels = c("no", "yes"))
df$Open7days <- factor(df$Open7days, levels = c(0, 1), labels = c("no", "yes"))

# Replace NA in pos/neg-review with 0 --> Assumption: No positive/negative reviews were created
df$posreview[is.na(df$posreview)] <- 0
df$negreview[is.na(df$negreview)] <- 0
df$posreview2016[is.na(df$posreview2016)] <- 0
df$negreview2016[is.na(df$negreview2016)] <- 0
df$posreview2017[is.na(df$posreview2017)] <- 0
df$negreview2017[is.na(df$negreview2017)] <- 0
df$posreview2018[is.na(df$posreview2018)] <- 0
df$negreview2018[is.na(df$negreview2018)] <- 0
