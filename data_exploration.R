# Plotting predictors vs response value
library(ggplot2)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

clean_outliers <- function(data) {
  
  "
  Function cleans outliers from data
  "
  
  outliers <- boxplot.stats(data)$out

  return(!(data %in% outliers))
  
}

# Read data with predictors and response

covid_data <- read.csv("./data/final_data.csv", header = TRUE, sep= ",")

# Correlation between the predictors and the response variable

cor(covid_data$Covid_Infection_Rate_count, covid_data$Population_Count_2019)
cor(covid_data$Covid_Infection_Rate_count, covid_data$ALWAYS)
cor(covid_data$Covid_Infection_Rate_count, covid_data$NEVER)
cor(covid_data$Covid_Infection_Rate_count, covid_data$SOMETIMES)
cor(covid_data$Covid_Infection_Rate_count, covid_data$FREQUENTLY)
cor(as.numeric(as.character(covid_data$Median_Household_Income)), covid_data$Covid_Infection_Rate_count)
cor(as.numeric(as.character(covid_data$Median_Age)), covid_data$Covid_Infection_Rate_count)

# Plot response variable distribution

ggplot(
  covid_data,
  aes(
    x=Covid_Infection_Rate
  )
) +
  geom_histogram()

# Plot response variable distribution removing outliers

ggplot(
  covid_data[clean_outliers(covid_data$Covid_Infection_Rate),],
  aes(
    x=Covid_Infection_Rate
  )
) +
  geom_histogram(aes(fill=..count..)) + 
  ggtitle("Distribution of Covid Infection Rate per County") +
  xlab("Covid Invection Rate") + ylab("Count")

# Plot Median Household Income vs response

ggplot(
  covid_data[
    clean_outliers(covid_data$Covid_Infection_Rate) 
    & clean_outliers(covid_data$Median_Household_Income),],
  aes(x=Median_Household_Income, y=Covid_Infection_Rate)) + 
geom_point() +
  ggtitle("Socioeconomic Status vs Covid Infection Rate") +
  xlab("Median Household Income") + ylab("Covid infection rate")

# Plot Education vs response

ggplot(
  covid_data[
    clean_outliers(covid_data$Covid_Infection_Rate),], 
  aes(
    x=associate_degree+bachelor_degree, 
    y=Covid_Infection_Rate)) + 
  geom_point() +
  ggtitle("Education vs Covid Infection Rate") +
  xlab("Ratio of Population With Associate Degree or Higher") + ylab("Covid infection rate")

# Plot Age vs response

ggplot(
  covid_data[
    clean_outliers(covid_data$Covid_Infection_Rate),], 
  aes(
    x=Median_Age,
    y=Covid_Infection_Rate)) + 
  geom_point() +
  ggtitle("Age vs Covid Infection Rate") +
  xlab("County median age") + ylab("Covid infection rate")

# Plot County Population vs response

ggplot(
  covid_data[
    clean_outliers(covid_data$Covid_Infection_Rate) &
      clean_outliers(covid_data$Population_Count_2019),],
  aes(x=Population_Count_2019, y=Covid_Infection_Rate)) + 
  geom_point() +
  ggtitle("Population vs Covid Infection Rate") +
  xlab("County population") + ylab("Covid infection rate")

# Plot Usage of masks vs response

ggplot(
  covid_data[
    clean_outliers(covid_data$Covid_Infection_Rate) &
      clean_outliers(covid_data$Population_Count_2019),],
  aes(x=FREQUENTLY + ALWAYS, y=Covid_Infection_Rate)) + 
  geom_point() +
  ggtitle("Mask Usage vs Covid Infection Rate") +
  xlab("Ratio of population that always/freq. wears masks") + ylab("Covid infection rate")

