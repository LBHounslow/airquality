# This R Script is to view all pollutants data for Hounslow's Feltham (hfe)

# Data Labels 
# --------------------------------------------------------------------------------------------------------
# hcr = Hounslow Cranford
# hfe = Hounslow Chiswick
# hbr = Hounslow Brentford
# hhe = Hounslow Heston
# hha = Hounslow Hatton Cross
# hgu = Hounslow Gunnersbury
# hfe = Hounslow Feltham
# hbo = Hounslow Boston Manor Park
# --------------------------------------------------------------------------------------------------------

# Col naming conventions - xxx_pm10, xxx_no, xxx_no2, xxx_o3, xxx_so2, xxx_pm25, xxx_v10, xxx_n10

# --------------------------------------------------------------------------------------------------------

# Required Libraries

require(tidyverse)
require(dplyr)
require(readr)
require(janitor)
require(ggplot2)

# --------------------------------------------------------------------------------------------------------

# Data Source Selection (Working Directory)

df_yr_2012 <- readr::read_csv("/dsq_data_projects/aq_project/EH_project_work/datasets/df_yr_2012.csv")
df_yr_2012
View(df_yr_2012)

df_yr_2013 <- readr::read_csv("/dsq_data_projects/aq_project/EH_project_work/datasets/df_yr_2013.csv")
df_yr_2013
View(df_yr_2013)

df_yr_2014 <- readr::read_csv("/dsq_data_projects/aq_project/EH_project_work/datasets/df_yr_2014.csv")
df_yr_2014
View(df_yr_2014)

df_yr_2015 <- readr::read_csv("/dsq_data_projects/aq_project/EH_project_work/datasets/df_yr_2015.csv")
df_yr_2015
View(df_yr_2015)

df_yr_2016 <- readr::read_csv("/dsq_data_projects/aq_project/EH_project_work/datasets/df_yr_2016.csv")
df_yr_2016
View(df_yr_2016)

df_yr_2017 <- readr::read_csv("/dsq_data_projects/aq_project/EH_project_work/datasets/df_yr_2017.csv")
df_yr_2017
View(df_yr_2017)

df_yr_2018 <- readr::read_csv("/dsq_data_projects/aq_project/EH_project_work/datasets/df_yr_2018.csv")
df_yr_2018
View(df_yr_2018)

df_yr_2019 <- readr::read_csv("/dsq_data_projects/aq_project/EH_project_work/datasets/df_yr_2019.csv")
df_yr_2019
View(df_yr_2019)

df_yr_2020 <- readr::read_csv("/dsq_data_projects/aq_project/EH_project_work/datasets/df_yr_2020.csv")
df_yr_2020
View(df_yr_2020)

df_yr_2021 <- readr::read_csv("/dsq_data_projects/aq_project/EH_project_work/datasets/df_yr_2021.csv")
df_yr_2021
View(df_yr_2021)

# ------------------------------------------------------------------------------------------------------

# This is to view entire composition of data set
str(df_yr_2012)

# This is to view streamlined information in compare to str above
sapply(df_yr_2012, class)

# This is to view cols composition of data set
sapply(df_yr_2012, typeof)

# This is to view quick composition of AQ data sets using a PLOT Visualization
df_yr_2012_types <- function(df_yr_2012) {
  res <- lapply(df_yr_2012, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="AQ 2012 Data Types", col="purple", ylab="Number of Features")
}
df_yr_2012_types(iris)

df_yr_2017_types <- function(df_yr_2017) {
  res <- lapply(df_yr_2017, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="AQ 2017 Data Types", col="purple", ylab="Number of Features")
}
df_yr_2017_types(iris)

df_yr_2021_types <- function(df_yr_2021) {
  res <- lapply(df_yr_2021, class)
  res_frame <- data.frame(unlist(res))
  barplot(table(res_frame), main="AQ 2021 Data Types", col="purple", ylab="Number of Features")
}
df_yr_2021_types(iris)

# ------------------------------------------------------------------------------------------------------

# Checking position of the datasets and its values (This would provide full dimension of data incl. mean, median and number of NA's)

#2012 Data Analysis - SWAP 20xx with relevant year to project
summary(df_yr_2012)
summary(df_yr_2012$hfe_pm10)
summary(df_yr_2012$hfe_no)
summary(df_yr_2012$hfe_no2)
summary(df_yr_2012$hfe_noxasno2)
summary(df_yr_2012$hfe_nv10)

# ------------------------------------------------------------------------------------------------------

# Data sets cleaning steps 

# Step 1: Remove unwanted (duplicated )cols for i.e. pollutants status / unit is always the same or vice versa select only relevant cols by hotspots
#=========================

#hfe data for 2012
dfhfe2012 <- df_yr_2012 %>% 
  
  select(starts_with('end'),starts_with('hfe')) %>% 
  select(!ends_with('unit')) 

dfhfe2012

View(dfhfe2012)

#hfe data for 2013
dfhfe2013 <- df_yr_2013 %>% 
  
  select(starts_with('end'),starts_with('hfe')) %>% 
  select(!ends_with('unit')) 

dfhfe2013
View(dfhfe2013)

#hfe data for 2014
dfhfe2014 <- df_yr_2014 %>% 
  
  select(starts_with('end'),starts_with('hfe')) %>% 
  select(!ends_with('unit')) 

dfhfe2014

#hfe data for 2015
dfhfe2015 <- df_yr_2015 %>% 
  
  select(starts_with('end'),starts_with('hfe')) %>% 
  select(!ends_with('unit')) 

dfhfe2015

#hfe data for 2016
dfhfe2016 <- df_yr_2016 %>% 
  
  select(starts_with('end'),starts_with('hfe')) %>% 
  select(!ends_with('unit')) 

dfhfe2016

#hfe data for 2017
dfhfe2017 <- df_yr_2017 %>% 
  
  select(starts_with('end'),starts_with('hfe')) %>% 
  select(!ends_with('unit')) 

dfhfe2017

#hfe data for 2018
dfhfe2018 <- df_yr_2018 %>% 
  
  select(starts_with('end'),starts_with('hfe')) %>% 
  select(!ends_with('unit')) 

dfhfe2018

#hfe data for 2019
dfhfe2019 <- df_yr_2019 %>% 
  
  select(starts_with('end'),starts_with('hfe')) %>% 
  select(!ends_with('unit')) 

dfhfe2019

#hfe data for 2020
dfhfe2020 <- df_yr_2020 %>% 
  
  select(starts_with('end'),starts_with('hfe')) %>% 
  select(!ends_with('unit')) 

dfhfe2020

#hfe data for 2021
dfhfe2021 <- df_yr_2021 %>% 
  
  select(starts_with('end'),starts_with('hfe')) %>% 
  select(!ends_with('unit')) 

dfhfe2021


# Step 2: Data Cleaning Checks - to locate "No Data" text saved within the pollutants data (This would reduce bias in data sets)
#===========================

# Checking Missing Values in the data set
anyNA(dfhfe2012)
anyNA(dfhfe2013)
anyNA(dfhfe2014)
anyNA(dfhfe2015)
anyNA(dfhfe2016)
anyNA(dfhfe2017)
anyNA(dfhfe2018)
anyNA(dfhfe2019)
anyNA(dfhfe2020)
anyNA(dfhfe2021)

# Checking col values composition (2012-2021)
colSums(is.na(dfhfe2012))
is.numeric(dfhfe2012$hfe_no)
is.numeric(dfhfe2012$hfe_pm10)
is.numeric(dfhfe2012$hfe_no2)
is.numeric(dfhfe2012$hfe_noxasno2)


colSums(is.na(dfhfe2013))
is.numeric(dfhfe2013$hfe_no)
is.numeric(dfhfe2013$hfe_pm10)
is.numeric(dfhfe2013$hfe_no2)
is.numeric(dfhfe2013$hfe_noxasno2)

colSums(is.na(dfhfe2014))
is.numeric(dfhfe2014$hfe_no)
is.numeric(dfhfe2014$hfe_pm10)
is.numeric(dfhfe2014$hfe_no2)
is.numeric(dfhfe2014$hfe_noxasno2)

colSums(is.na(dfhfe2015))
is.numeric(dfhfe2015$hfe_no)
is.numeric(dfhfe2015$hfe_pm10)
is.numeric(dfhfe2015$hfe_no2)
is.numeric(dfhfe2015$hfe_noxasno2)

colSums(is.na(dfhfe2016))
is.numeric(dfhfe2016$hfe_no)
is.numeric(dfhfe2016$hfe_pm10)
is.numeric(dfhfe2016$hfe_no2)
is.numeric(dfhfe2016$hfe_noxasno2)

colSums(is.na(dfhfe2017))
is.numeric(dfhfe2017$hfe_no)
is.numeric(dfhfe2017$hfe_pm10)
is.numeric(dfhfe2017$hfe_no2)
is.numeric(dfhfe2017$hfe_noxasno2)

colSums(is.na(dfhfe2018))
is.numeric(dfhfe2018$hfe_no)
is.numeric(dfhfe2018$hfe_pm10)
is.numeric(dfhfe2018$hfe_no2)
is.numeric(dfhfe2018$hfe_noxasno2)

colSums(is.na(dfhfe2019))
is.numeric(dfhfe2019$hfe_no)
is.numeric(dfhfe2019$hfe_pm10)
is.numeric(dfhfe2019$hfe_no2)
is.numeric(dfhfe2019$hfe_noxasno2)

colSums(is.na(dfhfe2020))
is.numeric(dfhfe2020$hfe_no)
is.numeric(dfhfe2020$hfe_pm10)
is.numeric(dfhfe2020$hfe_no2)
is.numeric(dfhfe2020$hfe_noxasno2)

colSums(is.na(dfhfe2021))
is.numeric(dfhfe2021$hfe_no)
is.numeric(dfhfe2021$hfe_pm10)
is.numeric(dfhfe2021$hfe_no2)
is.numeric(dfhfe2021$hfe_noxasno2)

# 2021 Discovered nonnumerical data saved within all pollutants cols - Need correction (Answer: All hfe data is NA)


summary(dfhfe2021)
unique(dfhfe2021$hfe_pm10)
unique(dfhfe2021$hfe_no)
unique(dfhfe2021$hfe_no2)
unique(dfhfe2021$hfe_noxasno2)

View(dfhfe2021)

# Step 3: Combining hfe Datasets into 1 large repository 
#============================

comb_df_hfe <- bind_rows(dfhfe2012, dfhfe2013, dfhfe2014, dfhfe2015, dfhfe2016, dfhfe2017, dfhfe2018, dfhfe2019, dfhfe2020, dfhfe2021)
comb_df_hfe

tail(comb_df_hfe)
View(comb_df_hfe)


# Step 4: Arranging data class into the right format/class
# ============================

# Checking current class of date col
class(comb_df_hfe$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_hfe$end_date <- as.Date(comb_df_hfe$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hfe$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hfe$end_time)

# Declaring date and time objects
date_obj <- comb_df_hfe$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hfe$end_time
time_obj <- as.difftime(time_obj)

# New col to combine date & time = date_time_stamp
comb_df_hfe$date_time_stamp <- combine.date.and.time(date = date_obj, time = time_obj)

# Formatting decimal values to max of 3 small digits at the end - this would include all less decimal values and reduces biasness in data
comb_df_hfe$hfe_pm10 <- format(round(comb_df_hfe$hfe_pm10, 2), nsmall = 3)
comb_df_hfe$hfe_no <- format(round(comb_df_hfe$hfe_no, 2), nsmall = 3)
comb_df_hfe$hfe_no2 <- format(round(comb_df_hfe$hfe_no2, 2), nsmall = 3)
comb_df_hfe$hfe_noxasno2 <- format(round(comb_df_hfe$hfe_noxasno2, 2), nsmall = 3)

#Calling all col titles
colnames(comb_df_hfe)

# Re-arrangement of data including removal of end_date & end_time from the data set
comb2_df_hfe <- comb_df_hfe %>% 
  select(7,3:6)

head(comb2_df_hfe)
View(comb2_df_hfe)


# Step 5: hfe Data Export (VCM)
# ===============================

df_spot_hfe <- readr::write_csv(comb2_df_hfe, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/filter_by_hotspots/comb_df_hfe_with_decimals.csv")

#--------------------------------------------------------------------------------------------------------------------------------------
# hfe Combined Dataset Task Accomplished
#--------------------------------------------------------------------------------------------------------------------------------------
