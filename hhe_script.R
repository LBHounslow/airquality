# This R Script is to view all pollutants data for Hounslow's Cranford (hhe)

# Data Labels 
# --------------------------------------------------------------------------------------------------------
# hcr = Hounslow Cranford
# hhe = Hounslow Chiswick
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
require(M3)

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
summary(df_yr_2012$hhe_pm10)
summary(df_yr_2012$hhe_no)
summary(df_yr_2012$hhe_no2)
summary(df_yr_2012$hhe_noxasno2)
summary(df_yr_2012$hhe_nv10)

# ------------------------------------------------------------------------------------------------------

# Data sets cleaning steps 

# Step 1: Remove unwanted (duplicated )cols for i.e. pollutants status / unit is always the same or vice versa select only relevant cols by hotspots
#=========================

#hhe data for 2012
dfhhe2012 <- df_yr_2012 %>% 
  
  select(starts_with('end'),starts_with('hhe')) %>% 
  select(!ends_with('unit')) 

dfhhe2012

View(dfhhe2012)

#hhe data for 2013
dfhhe2013 <- df_yr_2013 %>% 
  
  select(starts_with('end'),starts_with('hhe')) %>% 
  select(!ends_with('unit')) 

dfhhe2013
View(dfhhe2013)

#hhe data for 2014
dfhhe2014 <- df_yr_2014 %>% 
  
  select(starts_with('end'),starts_with('hhe')) %>% 
  select(!ends_with('unit')) 

dfhhe2014

#hhe data for 2015
dfhhe2015 <- df_yr_2015 %>% 
  
  select(starts_with('end'),starts_with('hhe')) %>% 
  select(!ends_with('unit')) 

dfhhe2015

#hhe data for 2016
dfhhe2016 <- df_yr_2016 %>% 
  
  select(starts_with('end'),starts_with('hhe')) %>% 
  select(!ends_with('unit')) 

dfhhe2016

#hhe data for 2017
dfhhe2017 <- df_yr_2017 %>% 
  
  select(starts_with('end'),starts_with('hhe')) %>% 
  select(!ends_with('unit')) 

dfhhe2017

#hhe data for 2018
dfhhe2018 <- df_yr_2018 %>% 
  
  select(starts_with('end'),starts_with('hhe')) %>% 
  select(!ends_with('unit')) 

dfhhe2018

#hhe data for 2019
dfhhe2019 <- df_yr_2019 %>% 
  
  select(starts_with('end'),starts_with('hhe')) %>% 
  select(!ends_with('unit')) 

dfhhe2019

#hhe data for 2020
dfhhe2020 <- df_yr_2020 %>% 
  
  select(starts_with('end'),starts_with('hhe')) %>% 
  select(!ends_with('unit')) 

dfhhe2020

#hhe data for 2021
dfhhe2021 <- df_yr_2021 %>% 
  
  select(starts_with('end'),starts_with('hhe')) %>% 
  select(!ends_with('unit')) 

dfhhe2021


# Step 2: Data Cleaning Checks - to locate "No Data" text saved within the pollutants data (This would reduce bias in data sets)
#===========================

# Checking Missing Values in the data set
anyNA(dfhhe2012)
anyNA(dfhhe2013)
anyNA(dfhhe2014)
anyNA(dfhhe2015)
anyNA(dfhhe2016)
anyNA(dfhhe2017)
anyNA(dfhhe2018)
anyNA(dfhhe2019)
anyNA(dfhhe2020)
anyNA(dfhhe2021)

# Checking col values composition (2012-2021)
colSums(is.na(dfhhe2012))
is.numeric(dfhhe2012$hhe_no)
is.numeric(dfhhe2012$hhe_pm10)
is.numeric(dfhhe2012$hhe_no2)
is.numeric(dfhhe2012$hhe_noxasno2)


colSums(is.na(dfhhe2013))
is.numeric(dfhhe2013$hhe_no)
is.numeric(dfhhe2013$hhe_pm10)
is.numeric(dfhhe2013$hhe_no2)
is.numeric(dfhhe2013$hhe_noxasno2)

colSums(is.na(dfhhe2014))
is.numeric(dfhhe2014$hhe_no)
is.numeric(dfhhe2014$hhe_pm10)
is.numeric(dfhhe2014$hhe_no2)
is.numeric(dfhhe2014$hhe_noxasno2)

colSums(is.na(dfhhe2015))
is.numeric(dfhhe2015$hhe_no)
is.numeric(dfhhe2015$hhe_pm10)
is.numeric(dfhhe2015$hhe_no2)
is.numeric(dfhhe2015$hhe_noxasno2)

colSums(is.na(dfhhe2016))
is.numeric(dfhhe2016$hhe_no)
is.numeric(dfhhe2016$hhe_pm10)
is.numeric(dfhhe2016$hhe_no2)
is.numeric(dfhhe2016$hhe_noxasno2)

colSums(is.na(dfhhe2017))
is.numeric(dfhhe2017$hhe_no)
is.numeric(dfhhe2017$hhe_pm10)
is.numeric(dfhhe2017$hhe_no2)
is.numeric(dfhhe2017$hhe_noxasno2)

colSums(is.na(dfhhe2018))
is.numeric(dfhhe2018$hhe_no)
is.numeric(dfhhe2018$hhe_pm10)
is.numeric(dfhhe2018$hhe_no2)
is.numeric(dfhhe2018$hhe_noxasno2)

colSums(is.na(dfhhe2019))
is.numeric(dfhhe2019$hhe_no)
is.numeric(dfhhe2019$hhe_pm10)
is.numeric(dfhhe2019$hhe_no2)
is.numeric(dfhhe2019$hhe_noxasno2)

colSums(is.na(dfhhe2020))
is.numeric(dfhhe2020$hhe_no)
is.numeric(dfhhe2020$hhe_pm10)
is.numeric(dfhhe2020$hhe_no2)
is.numeric(dfhhe2020$hhe_noxasno2)

colSums(is.na(dfhhe2021))
is.numeric(dfhhe2021$hhe_no)
is.numeric(dfhhe2021$hhe_pm10)
is.numeric(dfhhe2021$hhe_no2)
is.numeric(dfhhe2021$hhe_noxasno2)

# 2021 Discovered nonnumerical data saved within all pollutants cols - Need correction (Answer: All hhe data is NA)


summary(dfhhe2021)
unique(dfhhe2021$hhe_pm10)
unique(dfhhe2021$hhe_no)
unique(dfhhe2021$hhe_no2)
unique(dfhhe2021$hhe_noxasno2)

View(dfhhe2021)

# Step 3: Combining hhe Datasets into 1 large repository 
#============================

comb_df_hhe <- bind_rows(dfhhe2012, dfhhe2013, dfhhe2014, dfhhe2015, dfhhe2016, dfhhe2017, dfhhe2018, dfhhe2019, dfhhe2020, dfhhe2021)
comb_df_hhe

tail(comb_df_hhe)
View(comb_df_hhe)


# Step 4: Arranging data class into the right format/class
# ============================

# Checking current class of date col
class(comb_df_hhe$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_hhe$end_date <- as.Date(comb_df_hhe$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hhe$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hhe$end_time)

# Declaring date and time objects
date_obj <- comb_df_hhe$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hhe$end_time
time_obj <- as.difftime(time_obj)

# New col to combine date & time = date_time_stamp
comb_df_hhe$date_time_stamp <- combine.date.and.time(date = date_obj, time = time_obj)

# Formatting decimal values to max of 3 small digits at the end - this would include all less decimal values and reduces biasness in data
comb_df_hhe$hhe_pm10 <- format(round(comb_df_hhe$hhe_pm10, 2), nsmall = 3)
comb_df_hhe$hhe_no <- format(round(comb_df_hhe$hhe_no, 2), nsmall = 3)
comb_df_hhe$hhe_no2 <- format(round(comb_df_hhe$hhe_no2, 2), nsmall = 3)
comb_df_hhe$hhe_noxasno2 <- format(round(comb_df_hhe$hhe_noxasno2, 2), nsmall = 3)

#Calling all col titles
colnames(comb_df_hhe)

# Re-arrangement of data including removal of end_date & end_time from the data set
comb2_df_hhe <- comb_df_hhe %>% 
  select(7,3:6)

head(comb2_df_hhe)
View(comb2_df_hhe)


# Step 5: hhe Data Export (VCM)
# ===============================

df_spot_hhe <- readr::write_csv(comb2_df_hhe, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/filter_by_hotspots/comb_df_hhe_with_decimals.csv")

#--------------------------------------------------------------------------------------------------------------------------------------
# hhe Combined Dataset Task Accomplished
#--------------------------------------------------------------------------------------------------------------------------------------
