# Plotting predictors vs response value

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(ggplot2)

max_quantile <- function(data, max_quantile=0.75) {
  
  "
  Function return indexes of data below max_quantile
  "
  
  rh <- quantile(data, probs=max_quantile, na.rm = TRUE)
  return(data < rh)
}

# Read data with predictors and response

covid_data <- read.csv("./data/final_data.csv", header = TRUE, sep= ",")


# Plot Median Household Income vs Percentage of cases

ggplot(
  covid_data[
    max_quantile((covid_data$Confirmed_06.17.2020 / covid_data$Population_Count_2019)) 
    & max_quantile(covid_data$Median_Household_Income),],
  aes(x=Median_Household_Income, y=(Confirmed_06.17.2020 / Population_Count_2019))) + 
geom_point()

# Plot Education vs Percentage of cases

ggplot(
  covid_data[
    max_quantile((covid_data$Confirmed_06.17.2020 / covid_data$Population_Count_2019)),], 
  aes(
    x=Percent.of.adults.with.a.bachelor.s.degree.or.higher..2014.18, 
    y=(Confirmed_06.17.2020 / Population_Count_2019))) + 
geom_point()

# Plot Age vs Percentage of cases

ggplot(
  covid_data[
    max_quantile((covid_data$Confirmed_06.17.2020 / covid_data$Population_Count_2019)),], 
  aes(
    x=Median_Age,
    y=(Confirmed_06.17.2020 / Population_Count_2019))) + 
  geom_point()
