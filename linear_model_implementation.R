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

attach(data)

#fitting a multiple linear regression model
library(boot)
lm.fit <- glm(Covid_Infection_Rate_Average ~ ., data=data)
summary(lm.fit)

#performing 10 fold cross validation
cv.error.10 <- cv.glm(data, lm.fit, K=10)
cv.error.10$delta


# ******* OUT OF SAMPLE PREDICTIONS *******

# Constructing interesting predictions

# We change the median household income increasing it by 20,000 and 50,000
pred.data <- data.frame(
    Population_Count_2019= 20000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= c(30000,50000,80000),
    MaskUsage= 0.3
)

predict(lm.fit, pred.data)

# Our response variable (rate of infection) dramatically decreases almost linearly by modifying the median household income parameter

# We change the Mask Usage to 0.5 and 0.9
pred.data <- data.frame(
    Population_Count_2019= 20000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 30000,
    MaskUsage= c(0.3,0.5,0.9)
)

predict(lm.fit, pred.data)

# Our response variable (rate of infection) decreases by modifying the mask usage parameter

# We change the Population Count to 100000 and 1000000
pred.data <- data.frame(
    Population_Count_2019= c(20000,100000,1000000),
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 30000,
    MaskUsage= 0.3
)

predict(lm.fit, pred.data)

# Our response variable (rate of infection) decreases as the population count increases
