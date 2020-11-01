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

#fitting an alternative model
fit2 <- smooth.spline(data$Covid_Infection_Rate_Average, data$Median_Household_Income, cv=TRUE)

gam2 <- gam(Covid_Infection_Rate_Average ~
    s(log1p(Median_Age), m=2) +
    s(log1p(Median_Household_Income), m=1) +
    s(log1p(Education), m=3) +
    s(log1p(MaskUsage), m=2) +
    s(log1p(Population_Count_2019), m=2),
    data=data)

gam2 <- gam(Covid_Infection_Rate_Average ~
    s(log1p(Median_Age), m=2),
    data=data)

par(mfrow=c(3,2))
par(mar=c(3,3,3,3))
plot(gam2, se=TRUE, col="blue")

gam2
head(data)
summary(gam2)
