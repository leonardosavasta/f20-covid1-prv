setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list=ls())

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


# ******* OUT OF SAMPLE PREDICTIONS *******

set.seed(2020)
#sampling data to obtain 6 observations
sampledata=data[sample(nrow(data), 6),]
sampledata

#predicting the response for the sampled data
pred.data <- data.frame(sampledata)
predict(lm.fit, pred.data)


# Constructing interesting predictions

View(pred.data)

# The following is similar to what we expect for a county such as Greenville
pred.data <- data.frame(
    Population_Count_2019= 500000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 50000,
    MaskUsage= 0.5
)

predict(lm.fit, pred.data)

# We change the median household income increasing it by 30,000
pred.data <- data.frame(
    Population_Count_2019= 500000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 80000,
    MaskUsage= 0.5
)

predict(lm.fit, pred.data)

# Our response variable (rate of infection) dramatically decreases
# by modifying the median household income parameter

# We change the Mask Usage from 0.5 to 0.9
pred.data <- data.frame(
    Population_Count_2019= 500000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 50000,
    MaskUsage= 0.9
)

predict(lm.fit, pred.data)

# Our response variable (rate of infection) slightly decreases by modifying
# the mask usage parameter
