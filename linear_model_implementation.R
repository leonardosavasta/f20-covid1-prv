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
#sampling data to obtain 6 observations
sampledata=data[sample(nrow(data), 6),]
sampledata

#predicting the response for the sampled data
pred.data <- data.frame(sampledata)
predict(lm.fit, pred.data)

# fitting alternative models(i.e. non-linear models)
# fit Covid_Infection_Rate_Average with each individual predictors and produce plot of the data and smoothing line
fit <- smooth.spline(data$Covid_Infection_Rate_Average, data$Median_Household_Income, df=16)
fit2 <- smooth.spline(data$Covid_Infection_Rate_Average, data$Median_Household_Income, cv=TRUE)
plot(data$Covid_Infection_Rate_Average, data$Median_Household_Income, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","6.8 DF (LOOCV)"), col=c("red", "blue"),lty=1, lwd=2, cex=.8)

fit <- smooth.spline(data$Covid_Infection_Rate_Average, data$Population_Count_2019, df=16)
fit2 <- smooth.spline(data$Covid_Infection_Rate_Average, data$Population_Count_2019, cv=TRUE)
plot(data$Covid_Infection_Rate_Average, data$Population_Count_2019, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","6.8 DF (LOOCV)"), col=c("red", "blue"),lty=1, lwd=2, cex=.8)

fit <- smooth.spline(data$Covid_Infection_Rate_Average, data$Median_Age, df=16)
fit2 <- smooth.spline(data$Covid_Infection_Rate_Average, data$Median_Age, cv=TRUE)
plot(data$Covid_Infection_Rate_Average, data$Median_Age, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","6.8 DF (LOOCV)"), col=c("red", "blue"),lty=1, lwd=2, cex=.8)


gam2 <- gam(Covid_Infection_Rate_Average ~ s(log1p(Median_Age), m=2), data=data)
par(mfrow=c(3,2))
par(mar=c(3,3,3,3))
dev.new(width=5, height=4)
plot(gam2, se=TRUE, col="blue")

gam2 <- gam(Covid_Infection_Rate_Average ~ s(log1p(Median_Household_Income), m=2), data=data)
par(mfrow=c(3,2))
par(mar=c(3,3,3,3))
dev.new(width=5, height=4)
plot(gam2, se=TRUE, col="blue")

gam2 <- gam(Covid_Infection_Rate_Average ~ s(log1p(Median_Household_Income), m=2), data=data)
par(mfrow=c(3,2))
par(mar=c(3,3,3,3))
dev.new(width=5, height=4)
plot(gam2, se=TRUE, col="blue")
