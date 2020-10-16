# f20-covid1
It is an inevitable reality that we are living in a world of COVID. But, to get back
to a sense of normalcy, it is prudent that we take certain necessary measures to
combat this virus.
So, our project involves understanding the various factors that influence the spread (and maybe fatality)
rate of COVID-19.

# Data:
The data folder consists of several datasets in .csv format collected from various sources that we intend to use in our project.
- The MaskUsage.csv file includes data on mask usage in each county.
- Education.csv file contains information about the educational qualifications of people in each county.
- MedianAge.csv contains information about the median age of people in each county.
- lagdp.csv file contains the GDP values for each county from 2015 to 2018.
- population_data.csv file contains the population figures for each county from 2010 to 2019.
- The other datasets include data on COVID19 cases between June 17 and July 29.

We merge all these datasets into a single dataset (i.e. final_data.csv), clean and transform the data according to our needs using the DataCleanAndMerge.R file. The final dataset consists of 2065 rows and 83 columns in total.
Some of the major fields/predictors in the final dataset are:
- FIPS: County codes
- County: County names
- Province_State: Name of the state to which the county belongs
- Population_Count_2019: The population count of each county
- Median_Age: Median age of the population in each county
- NEVER: The estimated share of people in this county who would say never in response to the question “How often do you wear a mask in public when you expect to be within six feet of another person?”
- RARELY: The estimated share of people in this county who would say rarely
- SOMETIMES: The estimated share of people in this county who would say sometimes
- FREQUENTLY: The estimated share of people in this county who would say frequently
- ALWAYS: The estimated share of people in this county who would say always
- confirmed_mm-dd-yyyy: The number of laboratory confirmed Covid-19 cases on that particular day
- deaths_mm-dd-yyyy: The total number of deaths from Covid-19, including both confirmed and probable on that particular day
- Covid_Infection_Rate_Count: The total number of cases in each county between June 17 and July 29
- Covid_Infection_Rate: The percentage increase in COVID19 cases in each county between June 17 and July 29
