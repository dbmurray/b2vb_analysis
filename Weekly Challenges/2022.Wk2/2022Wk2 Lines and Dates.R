#2022.Week 2 Lines and Dates
# https://data.world/back2vizbasics/2022week-2-dates-line-charts
# This week we're going to use the challenge brief to build an index chart. This will take some processing
# in R first to make sure we get the data correct. 

# LIBRARIES
library(tidyverse)
library(readxl)

# DATA IMPORT
OECD_unemploment <- read_csv("Weekly Challenges/2022.Wk2/OECD Unemployment Rate_Full Data.csv")

# TIDY DATA

# the date field is of the wrong data type and we need to sort on date so this needs to be fixed by converting the type.
# Keep in mind the date is in American format.
OECD_unemploment <- mutate(OECD_unemploment, Date = as.Date(Date, "%m/%d/%Y"))

# DATA TRANSFORMATIONS

# To create the index data, we're basically tracking change in the unemployment rate from one year 
# to the next by a set of grouped variables - Country and Gender.


OECD_unemploment <- OECD_unemploment%>%
  group_by(`Country Code`, `Gender`) %>% # create groups in the data
  arrange(`Country Code`, `Gender`, Date) %>% #put in correct sort order - must sort by the group vars as well
  mutate(percent_change = (`Unemployement Rate` - lag(`Unemployement Rate`))/lag(`Unemployement Rate`)) # calculate change by year

# CHARTS & TABLES

# EXPORTS
write.csv(OECD_unemploment, "Weekly Challenges/2022.Wk2/OECD_unemployment_data.csv", row.names=FALSE)

