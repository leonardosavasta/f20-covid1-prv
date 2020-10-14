rm(list=ls())

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyr)
library(stringr)

#function to clean the covid data files before merging
renaming <- function(date) {
  
  data <- read.csv(paste("./data", date, sep="/"),header = TRUE, sep= ",")
  names(data)[names(data) == "Admin2"] <- "County"
  drop <- c("Last_Update" , "Combined_Key")
  data <- data[,!(names(data) %in% drop)]
  s<-gsub("\\..*","",date)
  names(data)[names(data) == "Confirmed"] <- paste("Confirmed",s,sep="_")
  names(data)[names(data) == "Deaths"] <- paste("Deaths",s,sep="_")
  names(data)[names(data) == "Recovered"] <- paste("Recovered",s,sep="_")
  names(data)[names(data) == "Active"] <- paste("Active",s,sep="_")
  names(data)[names(data) == "Incidence_Rate"] <- paste("Incidence_Rate",s,sep="_")
  names(data)[names(data) == "Case.Fatality_Ratio"] <- paste("Case.Fatality_Ratio",s,sep="_")
  
  return(data)
}

#list of data files
dates <- c("06-17-2020.csv","06-18-2020.csv","06-19-2020.csv","06-20-2020.csv"
          ,"06-21-2020.csv","06-22-2020.csv","06-23-2020.csv","06-24-2020.csv"
          ,"06-25-2020.csv","06-26-2020.csv","06-27-2020.csv","06-28-2020.csv"
          ,"06-29-2020.csv","06-30-2020.csv","07-01-2020.csv","07-02-2020.csv"
          ,"07-03-2020.csv","07-04-2020.csv","07-05-2020.csv","07-06-2020.csv"
          ,"07-07-2020.csv","07-08-2020.csv","07-09-2020.csv","07-10-2020.csv"
          ,"07-11-2020.csv","07-12-2020.csv","07-13-2020.csv","07-14-2020.csv"
          ,"07-15-2020.csv","07-16-2020.csv","07-17-2020.csv","07-18-2020.csv"
          ,"07-19-2020.csv","07-20-2020.csv","07-21-2020.csv","07-22-2020.csv"
          ,"07-23-2020.csv","07-24-2020.csv","07-25-2020.csv","07-26-2020.csv"
          ,"07-27-2020.csv","07-28-2020.csv","07-29-2020.csv")

datalist <- list()
l <- length(dates)

#cleaning all the datafiles using the above defined function
for (i in 1:l) {
  datalist[[i]] <- renaming(dates[i])
}

final_data <- datalist[[1]]

#merging all covid datasets into 'final_data' dataframe
for (j in 2:l){
  final_data <- merge(final_data, datalist[[j]], by= c("FIPS","County","Province_State"
                                                  ,"Country_Region","Lat","Long_"))
}

mask <- read.csv("./data/Mask_Usage.csv",header = TRUE, sep= ",")
names(mask)[names(mask) == "COUNTYFP"] <- "FIPS"

#merging mask usage data into 'final_data'
final_data <- merge(mask,final_data, by = "FIPS")

# Get list of all states
states <- read.csv("./data/States.csv", header = TRUE, sep= ",")[,1]

# Read data on median household income per county
median_income <- read.csv(
  "./data/ACSST5Y2018.S1901_data_with_overlays_2020-10-14T145217.csv", 
  header=TRUE, 
  sep=",")

# Data transformation
names(median_income) <- as.matrix(median_income[1,])
median_income <- median_income[-1,]
median_income <- median_income[,c("id", "Estimate!!Households!!Median income (dollars)")]
median_income$id <- str_remove(median_income$id, "0500000US")
colnames(median_income) <- c("FIPS", "Median_Household_Income")

# Merging county median household income with final data
final_data <- merge(median_income, final_data, by= c("FIPS"))

# Reading data on Population_count by county
pop_data1 <- read.csv("./data/population_data.csv",header = TRUE, sep= ",", encoding="UTF-8")

# Data transformation
pop_data1 <- separate(pop_data1, County_State, c("County", "State"), sep=",")
pop_data1 <- pop_data1[, c(1,2,13)]
colnames(pop_data1)[2] <- "Province_State"
colnames(pop_data1)[3] <- "Population_Count_2019"

# Merging county Population_Count data with final data
final_data <- merge(pop_data1, final_data, by= c("County","Province_State"))

final_data$Population_Count_2019 <- as.numeric(gsub(",","", final_data$Population_Count_2019))

# Read data on education level by county

education_by_county <- read.csv("./data/Education.csv", header = TRUE, sep= ",")
education_by_county <- education_by_county[, c(1,12,13,14,15)]
colnames(education_by_county)[1] <- "FIPS"

final_data <- merge(education_by_county, final_data, by= "FIPS")

# Read data on median age per county

age_data <- read.csv(
  "./data/ACSST5Y2018.S0101_data_with_overlays_2020-10-13T210419.csv", 
  header=TRUE, 
  sep=",")
names(age_data) <- as.matrix(age_data[1,])
age_data <- age_data[-1,]
age_data <- age_data[,c("id", "Estimate!!Total!!Total population", "Estimate!!Total!!Total population!!SUMMARY INDICATORS!!Median age (years)")]
age_data$id <- str_remove(age_data$id, "0500000US")
colnames(age_data) <- c("FIPS", "Total_Population", "Median_Age")

final_data <- merge(age_data[, c(1,3)], final_data, by=c("FIPS"))

write.csv(final_data, "./data/final_data.csv")


final_data <- read.csv('./data/final_data.csv')

