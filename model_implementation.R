setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(gam)

# Read data with predictors and response
covid_data <- read.csv("./data/final_data.csv", header = TRUE, sep= ",")

#dimensions of final data
dim(covid_data)

# Creating data frame with model predictors
data <- covid_data[, c("Population_Count_2019", "Median_Age", "Median_Household_Income", "Covid_Infection_Rate_Average")]
data$MaskUsage <- covid_data$RARELY + covid_data$FREQUENTLY + covid_data$ALWAYS
data$Education <- covid_data$bachelor_degree

lm.fit <- lm(Covid_Infection_Rate_Average ~ ., data=data)
summary(lm.fit)

fit2 <- smooth.spline(data$Covid_Infection_Rate_Average, data$Median_Household_Income, cv=TRUE)

gam2 <- gam(Covid_Infection_Rate_Average ~ s(log1p(Median_Age), 3) + s(log1p(Median_Household_Income), 3) + s(log1p(Education), 3) + s(log1p(MaskUsage), 3) + s(log1p(Population_Count_2019),3), data=data)

par(mfrow=c(3,2))
par(mar=c(3,3,3,3))
plot(gam2, se=TRUE, col="blue")

gam2

summary(gam2)