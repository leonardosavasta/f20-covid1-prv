setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# loading ggplot library
library(ggplot2)

# Read data with predictors and response
covid_data <- read.csv("./data/final_data.csv", header = TRUE, sep= ",")

#dimensions of final data
dim(covid_data)

#unique observations
unique(covid_data)

#first 5 rows of the data set
head(covid_data,n=5)

#first 18 variables
names(covid_data[1:18])

#response variable
names(covid_data[82])

#function to remove outliers
clean_outliers <- function(data) {
  
  "
  Function cleans outliers from data
  "
  
  outliers <- boxplot.stats(data)$out

  return(!(data %in% outliers))
  
}

# plotting response variable distribution
ggplot(covid_data, aes(x=Covid_Infection_Rate)) + geom_histogram()

# plotting response variable distribution after removing outliers
ggplot(covid_data[clean_outliers(covid_data$Covid_Infection_Rate),]
       ,aes(x=Covid_Infection_Rate)) +geom_histogram(color="blue")

#plotting population variable distribution
ggplot(covid_data[clean_outliers(covid_data$Population_Count_2019),]
       ,aes(x=Population_Count_2019)) +geom_histogram(color="red")

# plotting County Population vs response
ggplot(covid_data[clean_outliers(covid_data$Covid_Infection_Rate)
                  & clean_outliers(covid_data$Population_Count_2019),]
       ,aes(x=Population_Count_2019, y=Covid_Infection_Rate)) + geom_point()

# Correlation between the predictors and the response variable
cor(covid_data$Covid_Infection_Rate_count, covid_data$Population_Count_2019)
cor(covid_data$Covid_Infection_Rate_count, covid_data$ALWAYS)
cor(covid_data$Covid_Infection_Rate_count, covid_data$NEVER)
cor(covid_data$Covid_Infection_Rate_count, covid_data$SOMETIMES)
cor(covid_data$Covid_Infection_Rate_count, covid_data$FREQUENTLY)
cor(as.numeric(as.character(covid_data$Median_Household_Income)), covid_data$Covid_Infection_Rate_count)
cor(as.numeric(as.character(covid_data$Median_Age)), covid_data$Covid_Infection_Rate_count)

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
    x=associate_degree+bachelor_degree, 
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

# Plot Usage of masks vs response

ggplot(
  covid_data[
    clean_outliers(covid_data$Covid_Infection_Rate) &
      clean_outliers(covid_data$Population_Count_2019),],
  aes(x=FREQUENTLY + ALWAYS, y=Covid_Infection_Rate)) + 
  geom_point()

