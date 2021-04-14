## Customer Models - Assignment 1 --
print("hello")
# load packages
packages <- c("dplyr", "ggplot2", "tidyr", "lubridate", "mice", "VIM")
install.packages(packages)
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(mice)
library(VIM)

# setwd
set.seed(123)
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

# Replace NA in pos/neg-review with 0 --> Assumption: No positive/negative reviews were created
df$posreview[is.na(df$posreview)] <- 0
df$negreview[is.na(df$negreview)] <- 0
df$posreview2016[is.na(df$posreview2016)] <- 0
df$negreview2016[is.na(df$negreview2016)] <- 0
df$posreview2017[is.na(df$posreview2017)] <- 0
df$negreview2017[is.na(df$negreview2017)] <- 0
df$posreview2018[is.na(df$posreview2018)] <- 0
df$negreview2018[is.na(df$negreview2018)] <- 0

# Replace NA in checkinX with 0 --> Assumption: Restaurants were closed >= 2016
df$checkedin100[is.na(df$checkedin100)]=0
df$checkedin2016[is.na(df$checkedin2016)]=0
df$checkedin2017[is.na(df$checkedin2017)]=0
df$checkedin2018[is.na(df$checkedin2018)]=0

# Test for missing values again
describe_columns(df)

# convert to factor
df$categories <- as.factor(df$categories)
df$GoodForKids <- factor(df$GoodForKids, levels = c("False", "True"), labels = c("False", "True"))
df$RestaurantsReservations <- factor(df$RestaurantsReservations, levels = c("False", "True"), labels = c("False", "True"))
df$Caters <- factor(df$Caters, levels = c("False", "True"), labels = c("False", "True"))
df$NoiseLevel <- factor(df$NoiseLevel, levels = c("quiet", "average", "loud", "very_loud"), labels = c("quiet", "average", "loud", "very_loud")) # maybe change to ordinal later
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
df$garage <- factor(df$garage, levels = c("False", "True"), labels = c("False", "True")) # praking option X available for restaurant: False = no; True = yes
df$street <- factor(df$street, levels = c("False", "True"), labels = c("False", "True"))
df$validated <- factor(df$validated, levels = c("False", "True"), labels = c("False", "True"))
df$lot <- factor(df$lot, levels = c("False", "True"), labels = c("False", "True"))
df$valet <- factor(df$valet, levels = c("False", "True"), labels = c("False", "True"))
df$OpenSundays <- factor(df$OpenSundays, levels = c(0, 1), labels = c("no", "yes"))
df$Open7days <- factor(df$Open7days, levels = c(0, 1), labels = c("no", "yes"))
df$checkedin100 <- factor(df$checkedin100, levels = c(0, 1), labels = c("no", "yes")) # More than 100 check-ins each year: 0 = no; 1 = yes
df$checkedin2016 <- factor(df$checkedin2016, levels = c(0, 1), labels = c("no", "yes"))
df$checkedin2017 <- factor(df$checkedin2017, levels = c(0, 1), labels = c("no", "yes"))
df$checkedin2018 <- factor(df$checkedin2018, levels = c(0, 1), labels = c("no", "yes"))

# Dealing with missing values
missing_pattern_full <- md.pattern(df, plot = FALSE)
print(missing_pattern_full[nrow(missing_pattern_full),])

# Columns excluded: Sentiment data and checkin data for each year (Makes sense, as we have too much missing for sentiment and checkin per year does not yield insights)
names(df)[!names(df) %in% c("city","state","postal_code","latitude","longitude",
                            "stars","review_count","categories","OpenSundays",
                            "Open7days","checkedin100","BusinessAcceptsCreditCards",
                            "RestaurantsTakeOut","RestaurantsPriceRange2",
                            "RestaurantsReservations","RestaurantsDelivery",
                            "GoodForKids","OutdoorSeating","RestaurantsAttire",
                            "garage","street","validated","lot","valet","WiFi",
                            "NoiseLevel","Caters","dessert","latenight","lunch",
                            "dinner","brunch","breakfast")]

# subset df to exclude sentiment data and checkin data for each individual year
df <- subset(df, select = c("city","state","postal_code","latitude","longitude",
                                "stars","review_count","categories","OpenSundays",
                                "Open7days","checkedin100","BusinessAcceptsCreditCards",
                                "RestaurantsTakeOut","RestaurantsPriceRange2",
                                "RestaurantsReservations","RestaurantsDelivery",
                                "GoodForKids","OutdoorSeating","RestaurantsAttire",
                                "garage","street","validated","lot","valet","WiFi",
                                "NoiseLevel","Caters","dessert","latenight","lunch",
                                "dinner","brunch","breakfast"))

# Inspect missing pattern of subset df
missing_pattern_sub <- md.pattern(df, plot = FALSE)
print(missing_pattern_sub[nrow(missing_pattern_sub),]) # Q: Does imputatation make sense here, some restaurants may not have specific feature (e.g. breakfast offer) --> Hence, not good for it or just not applicable

# Visual representation of missing values in subset df
missing_plot <- aggr(df,
                     col=c('navyblue','red'), 
                     numbers=TRUE,
                     sortVars=TRUE,
                     labels=names(data),
                     cex.axis=.7,
                     gap=3,
                     ylab=c("Histogram of missing data","Pattern"))

# Create a predictor matrix for each feature combination
# It specifies the target variable or block in the rows, and the predictor variables on the columns.
# An entry of 0 means that the column variable is NOT used to impute the row variableor block.
# A nonzero value indicates that it is used.
predictorMatrix <- matrix(0,nrow = ncol(df), ncol = ncol(df)) # Make a matrix of zeros
colnames(predictorMatrix) <- colnames(df)
rownames(predictorMatrix) <- colnames(df)

# extract feature (columns) where missing values are observed
containingNA <- colnames(missing_pattern_sub)[missing_pattern_sub[nrow(missing_pattern_sub),]>0]
containingNA <- containingNA[-length(containingNA)] # columns containing NA's

# create vector with features used for predicting missing values
usedforprediction <- c("city","state","postal_code","latitude","longitude",
                    "categories","OpenSundays", "Open7days",containingNA)

# indicate a 1 for every row and column combination where features are used for predicting a feature that contains missing values
predictorMatrix[usedforprediction,containingNA] <- 1 
diag(predictorMatrix) <- 0 #diagonal must be zero

# impute data
df_imputed <- mice(df, predictorMatrix = predictorMatrix, m=1, maxit = 50, seed = 666) # Number of multiple imputation is 1 (m=1) with 50 iterations
summary(df_imputed)

#get one of the complete data sets
df_sub_imputed <- complete(df_imputed, 1)

# check if imputation worked
describe_columns(df_sub_imputed)

# save imputed dataset in wd
write.csv(df_sub_imputed, file="data_sub_imputed.csv")
