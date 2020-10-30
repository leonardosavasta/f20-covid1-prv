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

#summary of the data set
summary(covid_data)

# plotting response variable distribution
ggplot(covid_data, aes(x=Covid_Infection_Rate_Average)) + geom_histogram()

# plotting response variable distribution after removing outliers
ggplot(covid_data
       ,aes(x=Covid_Infection_Rate_Average)) +geom_histogram(aes(fill=..count..))

#plotting population variable distribution
ggplot(covid_data
       ,aes(x=Population_Count_2019)) +geom_histogram(color="red")

# plotting County Population vs response
ggplot(covid_data
       ,aes(x=Population_Count_2019, y=Covid_Infection_Rate_Average)) + geom_point()

#converting Median_Age column to numerical format
covid_data$Median_Age <- as.numeric(covid_data$Median_Age)

#finding min and max values of Median_Age column
min(covid_data$Median_Age)
max(covid_data$Median_Age)

# setting up cut-off values 
breaks <- c(23.4,30,40,50,67)

# specifying interval/bin labels
tags <- c("[23.4-30)","[30-40)", "[40-50)" , "[50-67)")

# bucketing values into bins
group_tags <- cut(covid_data$Median_Age, breaks=breaks,include.lowest=TRUE
                  ,right=FALSE,labels=tags)

# inspecting bins
summary(group_tags)

#plotting frquecy plot for bins
ggplot(data = as_tibble(group_tags), mapping = aes(x=value)) + geom_bar() + labs(x='Age',y='Frequency')

# plotting age vs response
ggplot(covid_data
       ,aes(x=Median_Age,y=Covid_Infection_Rate_Average)) + geom_point()

# plotting Education vs response

ggplot(covid_data
       ,aes(x=Less_than_high_school_diploma,y=Covid_Infection_Rate_Average)) + geom_point()

ggplot(covid_data
       ,aes(x=bachelor_degree,y=Covid_Infection_Rate_Average)) + geom_point()


# plotting Median Household Income vs response
ggplot(covid_data
       ,aes(x=Median_Household_Income, y=Covid_Infection_Rate_Average)) + geom_point()

#plotting mask usage data vs response

ggplot(covid_data
       ,aes(x=ALWAYS, y=Covid_Infection_Rate_Average)) + geom_point()

ggplot(covid_data
       ,aes(x=FREQUENTLY, y=Covid_Infection_Rate_Average)) + geom_point()

ggplot(covid_data
       ,aes(x=NEVER, y=Covid_Infection_Rate_Average)) + geom_point()

ggplot(covid_data
       ,aes(x=FREQUENTLY + ALWAYS, y=Covid_Infection_Rate_Average)) + geom_point()

#calculating the correlation between variables

cor(covid_data[4:15],covid_data[4:15])

cor(covid_data[4:15],covid_data$Covid_Infection_Rate_Average)



