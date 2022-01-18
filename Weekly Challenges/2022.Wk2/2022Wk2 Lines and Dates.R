#2022.Week 2 Lines and Dates
# https://data.world/back2vizbasics/2022week-2-dates-line-charts
# This week we're going to use the challenge brief to build an index chart. This will take some processing
# in R first to make sure we get the data correct. 

# LIBRARIES
library(tidyverse)
library(readxl)
library(ggplot2)
library(hrbrthemes)
library(stringr)

# DATA IMPORT
OECD_unemployment <- read_csv("Weekly Challenges/2022.Wk2/OECD Unemployment Rate_Full Data.csv")

# TIDY DATA

# the date field is of the wrong data type and we need to sort on date so this needs to be fixed by converting the type.
# Keep in mind the date is in American format.
# We also want to give a nice percentage number for unemployment rate rather than the whole number used here. 
# Also, I hate all caps in values so converting Gender to title case.
OECD_unemployment <- mutate(OECD_unemployment, Date = as.Date(Date, "%m/%d/%Y"), `Unemployement Rate` = `Unemployement Rate`/100, Gender = str_to_title(Gender))

# DATA TRANSFORMATIONS

# To create the index data, we're basically tracking change in the unemployment rate from one year 
# to the next by a set of grouped variables - Country and Gender.


OECD_unemployment <- OECD_unemployment%>%
  group_by(`Country Code`, `Gender`) %>% # create groups in the data
  arrange(`Country Code`, `Gender`, Date) %>% #put in correct sort order - must sort by the group vars as well
  mutate(percent_change = (`Unemployement Rate` - lag(`Unemployement Rate`))/lag(`Unemployement Rate`)) # calculate change by year

#unforunately this process creates a lot of NAs for the seed value of the %change index
OECD_unemployment <- OECD_unemployment %>% replace(is.na(.), 0)

# CHARTS & TABLES

#lets look at some data just for the hell of it!

#Heres the unemployment rate for all genders by country over time!
ggplot(subset(OECD_unemployment, Gender %in% c("ALL")), aes(x = Date, y = `Unemployement Rate`, group = `Country Name`, colour = `Country Name`))+
  geom_line() +
  theme_ipsum() +
  ggtitle("OECD Unemployment Rates | 2000 to 2021")

#large long period of high unemployment in what seems to be Greece - that's quite interesting. 

# EXPORTS

# Enough stuff around, let's export our tidied dataset.
write.csv(OECD_unemployment, "Weekly Challenges/2022.Wk2/OECD_unemployment_data.csv", row.names=FALSE)

