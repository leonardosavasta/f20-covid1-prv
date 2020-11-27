setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Visualization preferences
# library(ggthemes)
# par(bg = '#222222', fg = 'white', col='white', col.axis='white', col.lab='white', col.main='white', col.sub='white')
# theme_set(theme_par())

par(mfrow=c(1,1))

rm(list=ls())

library(randomForest)
library(ggplot2)
library(tree)
library(gbm)

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

# Implementing random forest model
rf.data <- randomForest(Covid_Infection_Rate_Average ~ ., data=data, subset=train, ry=6, importance=TRUE)
yhat.rf <- predict(rf.data, data[test,])
mean((yhat.rf - data.test)^2)

# Plotting true response vs predicted values
ggplot(data[test,], 
    aes(x=yhat.rf, y=Covid_Infection_Rate_Average)) + 
    geom_point(color="white") +
    geom_smooth(method='lm', color="red", size=0.5, alpha=0.8)

varImpPlot(rf.data, type=1)

# Compare results to MSE from Regression Line
lm.data <- lm(Covid_Infection_Rate_Average ~ ., data, subset=train)
yhat.lm <- predict(lm.data, data[test,])
mean((yhat.lm-data.test)^2)


# fitting regression tree model
tree.data <- tree(Covid_Infection_Rate_Average ~ ., data, subset=train)
summary(tree.data)
plot(tree.data)
text(tree.data)

# pruning the tree
prune.data <- prune.tree(tree.data, best=2)
plot(prune.data)
text(prune.data)

# using the unpruned tree to make predictions
yhat.tree <- predict(tree.data, data[test,])
data.test <- data[test, "Covid_Infection_Rate_Average"]
plot(yhat.tree, data.test)
abline(0,1)
mean((yhat.tree-data.test)^2)

# comparing the results to a linear model
lm.data <- lm(Covid_Infection_Rate_Average~ ., data, subset=train)
yhat.lm <- predict(lm.data, data[test,])
data.test <- data[test, "Covid_Infection_Rate_Average"]
mean((yhat.lm-data.test)^2)

#Boosting
boost.data <- gbm(Covid_Infection_Rate_Average~., data=data[train,], distribution="gaussian", n.trees=5000, interaction.depth=4, shrinkage=0.27)
summary(boost.data)
# we can see that Population_Count_2019 and Median_Household_Income are the most important variables
par(mfrow=c(1,2))
plot(boost.data, i="Population_Count_2019")
plot(boost.data, i="Median_Household_Income")

# Prediction using the boosted model
yhat.boost <- predict(boost.data, newdata=data[test,], n.trees=5000)
mean((yhat.boost-data.test)^2)


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

predict(rf.data, pred.data)

# We change the median household income increasing it by 30,000
pred.data <- data.frame(
    Population_Count_2019= 500000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 80000,
    MaskUsage= 0.5
)

predict(rf.data, pred.data)

# Our response variable (rate of infection) slightly decreases
# by modifying the median household income parameter

# We change the median household income increasing it by 60,000
pred.data <- data.frame(
    Population_Count_2019= 500000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 110000,
    MaskUsage= 0.5
)

predict(rf.data, pred.data)

# The response variable (rate of infection) remains almost the same as the previous case
# by modifying the median household income parameter by a higher number

# We change the Mask Usage from 0.5 to 0.9
pred.data <- data.frame(
    Population_Count_2019= 500000,
    Education= 0.30,
    Median_Age= 40,
    Median_Household_Income= 50000,
    MaskUsage= 0.9
)

predict(rf.data, pred.data)

# Our response variable (rate of infection) considerably decreases by modifying
# the mask usage parameter
