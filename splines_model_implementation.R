setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Visualization preferences
# library(ggthemes)
# par(bg = '#222222', fg = 'white', col='white', col.axis='white', col.lab='white', col.main='white', col.sub='white')
# theme_set(theme_par())

rm(list=ls())

library(mgcv)
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

# Training smoothing splines model
gam.fit <- gam(Covid_Infection_Rate_Average ~ s(Median_Household_Income, m=4) + s(Population_Count_2019, m=5) + s(Median_Age, m=4) + s(MaskUsage, m=5), data=data[train,], select=TRUE)
yhat.gam <- predict(gam.fit, data[test,])
mean((yhat.gam - data.test)^2)

# Plotting true response vs predicted values
ggplot(data[test,], 
    aes(x=yhat.gam, y=Covid_Infection_Rate_Average)) + 
    geom_point(color="white") +
    geom_smooth(method='lm', color="red", size=0.5, alpha=0.8)


# ******* OUT OF SAMPLE PREDICTIONS *******

# Constructing interesting predictions

# The following is similar to what we expect for a county such as Greenville
pred.data <- data.frame(
    Population_Count_2019= 500000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 50000,
    MaskUsage= 0.5
)

predict(gam.fit, pred.data)

# We change the median household income increasing it by 30,000
pred.data <- data.frame(
    Population_Count_2019= 500000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 80000,
    MaskUsage= 0.5
)

predict(gam.fit, pred.data)

# Our response variable (rate of infection) dramatically decreases
# by modifying the median household income parameter

# We change the median household income increasing it by 60,000
pred.data <- data.frame(
    Population_Count_2019= 500000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 110000,
    MaskUsage= 0.5
)

predict(gam.fit, pred.data)

# The response variable (rate of infection) dramatically decreases again
# almost linearly by modifying the median household income parameter

# We change the Mask Usage from 0.5 to 0.9
pred.data <- data.frame(
    Population_Count_2019= 500000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 50000,
    MaskUsage= 0.9
)

predict(gam.fit, pred.data)

# Our response variable (rate of infection) slightly decreases by modifying
# the mask usage parameter. But the decrease is not as dramatic as the previous case.