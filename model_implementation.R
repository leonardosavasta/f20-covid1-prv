setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Visualization preferences
# library(ggthemes)
# par(bg = '#222222', fg = 'white', col='white', col.axis='white', col.lab='white', col.main='white', col.sub='white')
# theme_set(theme_par())

rm(list=ls())

library(randomForest)
library(ggplot2)

# Read data with predictors and response
covid_data <- read.csv("./data/final_data.csv", header = TRUE, sep= ",")

#dimensions of final data
dim(covid_data)

# Creating data frame with model predictors
data <- covid_data[, c("Population_Count_2019", 
    "Median_Age", 
    "Median_Household_Income", 
    "Covid_Infection_Rate_Average")]

data$MaskUsage <- covid_data$RARELY + covid_data$FREQUENTLY + covid_data$ALWAYS
data$Education <- covid_data$associate_degree + covid_data$bachelor_degree

# Sampling training and testing data
set.seed(1)
N <- nrow(data)
train <- sample(1:N, N/1.5)
test <- seq(1:N)[-train]

data.test <- data[test, "Covid_Infection_Rate_Average"]
data.train <- data[train, "Covid_Infection_Rate_Average"]

# Implementing random forest model
rf.data <- randomForest(Covid_Infection_Rate_Average ~ ., data=data, subset=train, ry=6, importance=TRUE)
yhat.rf <- predict(rf.data, data[test,])
mean((yhat.rf - data.test)^2)

# Plotting true response vs predicted values
ggplot(data[test,], 
    aes(x=yhat.rf, y=Covid_Infection_Rate_Average)) + 
    geom_point(color="white") +
    geom_smooth(method='lm', color="red", size=0.5, alpha=0.8)

varImpPlot(rf.data, type=1)

# Compare results to MSE from Regression Line
lm.data <- lm(Covid_Infection_Rate_Average ~ ., data, subset=train)
yhat.lm <- predict(lm.data, data[test,])
mean((yhat.lm-data.test)^2)