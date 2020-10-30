setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(gam)

# Read data with predictors and response
covid_data <- read.csv("./data/final_data.csv", header = TRUE, sep= ",")

#dimensions of final data
dim(covid_data)

# Creating data frame with model predictors
data <- covid_data[, c("Population_Count_2019", "Median_Age", "Median_Household_Income", "Covid_Infection_Rate")]
data$MaskUsage <- covid_data$RARELY + covid_data$FREQUENTLY + covid_data$ALWAYS
data$Education <- covid_data$bachelor_degree + covid_data$associate_degree

lm.fit <- lm(Covid_Infection_Rate ~ ., data=data)
summary(lm.fit)


gam2 <- gam(Covid_Infection_Rate ~ s(Median_Age, 4) + s(Median_Household_Income, 5) + s(Education, 3), data=data)

par(mfrow=c(1,3))
par(mar=c(3,3,3,3))
plot(gam2, se=TRUE, col="blue")

plot.gam(gam2)
gam2

summary(gam2)