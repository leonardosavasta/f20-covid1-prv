setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

library(tidyr)

# Reading COVID-19 cases data by county for dates

data1 <- read.csv("./data/6-15-2020.csv",header = TRUE, sep= ",")
data2 <- read.csv("./data/7-19-2020.csv",header = TRUE, sep= ",")

data <- merge(data1, data2, by= c("FIPS","Admin2","Province_State","Country_Region","Lat","Long_","Combined_Key"))

# Reading mask usage data by county

mask <- read.csv("./data/Mask Usage.csv",header = TRUE, sep= ",")

# Merging cases info data with mask usage data by county

final_data <- merge(data,mask,by=c("FIPS"))


colnames(final_data)[2] = "County"

# Reading data on GDP by county

county_gdp <- read.csv("./data/lagdp1219.csv", header = TRUE, sep= ",")

# Data transformation

# Get list of all states
states <- read.csv("./data/States.csv", header = TRUE, sep= ",")[,1]

# Create column of states for each county

county_gdp$Province_State <- NA
county_gdp[county_gdp[,1] %in% states,"Province_State"] <- county_gdp[county_gdp[,1] %in% states,1]
county_gdp <- fill(county_gdp, Province_State, .direction = "down")
county_gdp <- county_gdp[!(county_gdp[,1] %in% states),]
county_gdp <- county_gdp[, c(1,5,6)]
colnames(county_gdp)[1] <- "County"
colnames(county_gdp)[2] <- "GDP 2018"

# Merging county GDP data with final data

final_data <- merge(final_data, county_gdp, by= c("County","Province_State"))

# Reading data on Population_count by county
pop_data1 <- read.csv("./data/population_data.csv",header = TRUE, sep= ",")

# Data transformation
pop_data1 <- separate(pop_data1, County_State, c("County", "State"), sep=",")
pop_data1 <- pop_data1[, c(1,2,13)]
colnames(pop_data1)[2] <- "Province_State"
colnames(pop_data1)[3] <- "Population_Count_2019"

# Merging county Population_Count data with final data
final_data <- merge(final_data, pop_data1, by= c("County","Province_State"))
write.csv(final_data, "./data/final_data.csv")

