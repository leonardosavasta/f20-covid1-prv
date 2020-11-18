setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

rm(list=ls())

library(gam)
library(mgcv)
library(ggplot2)

# Read data with predictors and response
covid_data <- read.csv("./data/final_data.csv", header = TRUE, sep= ",")

#dimensions of final data
dim(covid_data)

# Creating data frame with model predictors
data <- covid_data[, c("Population_Count_2019","Median_Age","Median_Household_Income","Covid_Infection_Rate_Average")]
data$MaskUsage <- covid_data$RARELY + covid_data$FREQUENTLY + covid_data$ALWAYS
data$Education <- covid_data$associate_degree + covid_data$bachelor_degree

attach(data)

# fitting non-linear models

# fitting Covid_Infection_Rate_Average with Median_Household_Income and producing a plot of the data and smoothing line
fit <- smooth.spline(Covid_Infection_Rate_Average,Median_Household_Income, df=16)
fit2 <- smooth.spline(Covid_Infection_Rate_Average,Median_Household_Income, cv=TRUE)

plot(Covid_Infection_Rate_Average,Median_Household_Income, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","11.4 DF (LOOCV)"), col=c("red", "blue"),lty=1, lwd=2, cex=.8)

# fitting Covid_Infection_Rate_Average with Population_Count_2019 and producing a plot of the data and smoothing line
fit <- smooth.spline(Covid_Infection_Rate_Average,Population_Count_2019, df=16)
fit2 <- smooth.spline(Covid_Infection_Rate_Average,Population_Count_2019, cv=TRUE)

plot(Covid_Infection_Rate_Average,Population_Count_2019, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","6.6 DF (LOOCV)"), col=c("red", "blue"),lty=1, lwd=2, cex=.8)

# fitting Covid_Infection_Rate_Average with Median_Age and producing a plot of the data and smoothing line
fit <- smooth.spline(Covid_Infection_Rate_Average,Median_Age, df=16)
fit2 <- smooth.spline(Covid_Infection_Rate_Average,Median_Age, cv=TRUE)

plot(Covid_Infection_Rate_Average,Median_Age, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","3 DF (LOOCV)"), col=c("red", "blue"),lty=1, lwd=2, cex=.8)

# fitting Covid_Infection_Rate_Average with MaskUsage and producing a plot of the data and smoothing line
fit <- smooth.spline(Covid_Infection_Rate_Average,MaskUsage, df=16)
fit2 <- smooth.spline(Covid_Infection_Rate_Average,MaskUsage, cv=TRUE)

plot(Covid_Infection_Rate_Average,MaskUsage, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","11.6 DF (LOOCV)"), col=c("red", "blue"),lty=1, lwd=2, cex=.8)

# fitting Covid_Infection_Rate_Average with Education and producing a plot of the data and smoothing line
fit <- smooth.spline(Covid_Infection_Rate_Average,Education, df=16)
fit2 <- smooth.spline(Covid_Infection_Rate_Average,Education, cv=TRUE)

plot(Covid_Infection_Rate_Average,Education, cex=.5, col="darkgray")
lines(fit,  col="red", lwd=2)
lines(fit2, col="blue", lwd=2)
legend("topright", legend=c("16 DF","2.5 DF (LOOCV)"), col=c("red", "blue"),lty=1, lwd=2, cex=.8)

gam.fit <- gam(Covid_Infection_Rate_Average ~ s(Median_Household_Income,4)+s(Population_Count_2019,5)+s(Median_Age,4)+s(MaskUsage,5)+s(Education,4))
summary(gam.fit)

library(boot)
#performing 10 fold cross validation
cv.error.10 <- cv.glm(data, gam.fit, K=10)
cv.error.10$delta

par(mfrow= c(2,3))
plot(gam.fit, se=TRUE, col="red")

# Sampling training and testing data
set.seed(1)
N <- nrow(data)
train <- sample(1:N, N/1.5)
test <- seq(1:N)[-train]
data.test <- data[test, "Covid_Infection_Rate_Average"]
data.train <- data[train, "Covid_Infection_Rate_Average"]

# Training smoothing splines model
gam.fit <- gam(Covid_Infection_Rate_Average ~ s(Median_Household_Income,m=4) + s(Population_Count_2019,m=5) + s(Median_Age,m=4) + s(MaskUsage,m=5) + s(Education,m=4), data=data[train,], select=TRUE)
yhat.gam <- predict(gam.fit, data[test,])
mean((yhat.gam - data.test)^2)

# Plotting true response vs predicted values
ggplot(data[test,],aes(x=yhat.gam, y=Covid_Infection_Rate_Average)) +
    geom_point(color="black")+geom_smooth(method='lm', color="red", size=0.5, alpha=0.8)

# Performing Principal Component Analysis
par(mfrow=c(1,1))
prc.data <- prcomp(data, scale=TRUE)
biplot(prc.data, scale=0, xlim=c(-5,5), ylim=c(-5,5), xlabs=rep(".", nrow(data)), col=c("red", "white"))


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
# the mask usage parameter

# ******* FITTING ALTERNATIVE MODELS *******
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
