setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Read data with predictors and response
covid_data <- read.csv("./data/final_data.csv", header = TRUE, sep= ",")

#dimensions of final data
dim(covid_data)

# Creating data frame with model predictors
data <- covid_data[, c("Population_Count_2019", "Median_Age", "bachelor_degree", "Median_Household_Income", "Covid_Infection_Rate")]
data$MaskUsage <- covid_data$FREQUENTLY + covid_data$ALWAYS

lm.fit <- lm(Covid_Infection_Rate ~ ., data=data)
summary(lm.fit)
