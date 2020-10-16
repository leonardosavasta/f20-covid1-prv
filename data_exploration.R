# Plotting predictors vs response value

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)

clean_outliers <- function(data) {
  
  "
  Function cleans outliers from data
  "
  
  outliers <- boxplot.stats(data)$out

  return(!(data %in% outliers))
  
}

# Read data with predictors and response

covid_data <- read.csv("./data/final_data.csv", header = TRUE, sep= ",")



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
  geom_histogram(color="blue")

# Plot Median Household Income vs response

ggplot(
  covid_data[
    clean_outliers(covid_data$Covid_Infection_Rate) 
    & clean_outliers(covid_data$Median_Household_Income),],
  aes(x=Median_Household_Income, y=Covid_Infection_Rate)) + 
geom_point()

# Plot Education vs response

ggplot(
  covid_data[
    clean_outliers(covid_data$Covid_Infection_Rate),], 
  aes(
    x=bachelor_degree, 
    y=Covid_Infection_Rate)) + 
geom_point()

# Plot Age vs response

ggplot(
  covid_data[
    clean_outliers(covid_data$Covid_Infection_Rate),], 
  aes(
    x=Median_Age,
    y=Covid_Infection_Rate)) + 
  geom_point()

# Plot County Population vs response

ggplot(
  covid_data[
    clean_outliers(covid_data$Covid_Infection_Rate) &
      clean_outliers(covid_data$Population_Count_2019),],
  aes(x=Population_Count_2019, y=Covid_Infection_Rate)) + 
  geom_point()

