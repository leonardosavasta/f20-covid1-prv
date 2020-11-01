setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())
library(mgcv)

# Read data with predictors and response
covid_data <- read.csv("./data/final_data.csv", header = TRUE, sep= ",")

#dimensions of final data
dim(covid_data)

# Creating data frame with model predictors
data <- covid_data[, c("Population_Count_2019", "Median_Age", "Median_Household_Income", "Covid_Infection_Rate_Average")]
data$MaskUsage <- covid_data$RARELY + covid_data$FREQUENTLY + covid_data$ALWAYS
data$Education <- covid_data$associate_degree + covid_data$bachelor_degree

#fitting a multiple linear regression model
library(boot)
lm.fit <- glm(Covid_Infection_Rate_Average ~ ., data=data)
summary(lm.fit)

#performing 10 fold cross validation
cv.error.10 <- cv.glm(data, lm.fit, K=10)
cv.error.10$delta

set.seed(2020)
#sampling data to obtain observations
sampledata=data[sample(nrow(data), 3),]
sampledata

#predicting the response for the sampled data
pred.data <- data.frame(sampledata)
predict(lm.fit, pred.data)
