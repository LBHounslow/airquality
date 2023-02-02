# This R Script is to view all pollutants data for Hounslow's Brentford (hbr)

# Data Labels 
# --------------------------------------------------------------------------------------------------------
# hcr = Hounslow Cranford
# hch = Hounslow Chiswick
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
summary(df_yr_2012$hbr_pm10)
summary(df_yr_2012$hbr_no)
summary(df_yr_2012$hbr_no2)
summary(df_yr_2012$hbr_noxasno2)
summary(df_yr_2012$hbr_nv10)

# ------------------------------------------------------------------------------------------------------

# Data sets cleaning steps 

# Step 1: Remove unwanted (duplicated )cols for i.e. pollutants status / unit is always the same or vice versa select only relevant cols by hotspots
#=========================

#hbr data for 2012
dfhbr2012 <- df_yr_2012 %>% 
  
  select(starts_with('end'),starts_with('hbr')) %>% 
  select(!ends_with('unit')) 

dfhbr2012

View(dfhbr2012)

#hbr data for 2013
dfhbr2013 <- df_yr_2013 %>% 
  
  select(starts_with('end'),starts_with('hbr')) %>% 
  select(!ends_with('unit')) 

dfhbr2013
View(dfhbr2013)

#hbr data for 2014
dfhbr2014 <- df_yr_2014 %>% 
  
  select(starts_with('end'),starts_with('hbr')) %>% 
  select(!ends_with('unit')) 

dfhbr2014

#hbr data for 2015
dfhbr2015 <- df_yr_2015 %>% 
  
  select(starts_with('end'),starts_with('hbr')) %>% 
  select(!ends_with('unit')) 

dfhbr2015

#hbr data for 2016
dfhbr2016 <- df_yr_2016 %>% 
  
  select(starts_with('end'),starts_with('hbr')) %>% 
  select(!ends_with('unit')) 

dfhbr2016

#hbr data for 2017
dfhbr2017 <- df_yr_2017 %>% 
  
  select(starts_with('end'),starts_with('hbr')) %>% 
  select(!ends_with('unit')) 

dfhbr2017

#hbr data for 2018
dfhbr2018 <- df_yr_2018 %>% 
  
  select(starts_with('end'),starts_with('hbr')) %>% 
  select(!ends_with('unit')) 

dfhbr2018

#hbr data for 2019
dfhbr2019 <- df_yr_2019 %>% 
  
  select(starts_with('end'),starts_with('hbr')) %>% 
  select(!ends_with('unit')) 

dfhbr2019

#hbr data for 2020
dfhbr2020 <- df_yr_2020 %>% 
  
  select(starts_with('end'),starts_with('hbr')) %>% 
  select(!ends_with('unit')) 

dfhbr2020

#hbr data for 2021
dfhbr2021 <- df_yr_2021 %>% 
  
  select(starts_with('end'),starts_with('hbr')) %>% 
  select(!ends_with('unit')) 

dfhbr2021


# Step 2: Data Cleaning Checks - to locate "No Data" text saved within the pollutants data (This would reduce bias in data sets)
#===========================

# Checking Missing Values in the data set
anyNA(dfhbr2012)
anyNA(dfhbr2013)
anyNA(dfhbr2014)
anyNA(dfhbr2015)
anyNA(dfhbr2016)
anyNA(dfhbr2017)
anyNA(dfhbr2018)
anyNA(dfhbr2019)
anyNA(dfhbr2020)
anyNA(dfhbr2021)

# Checking col values composition (2012-2021)
colSums(is.na(dfhbr2012))
is.numeric(dfhbr2012$hbr_no)
is.numeric(dfhbr2012$hbr_pm10)
is.numeric(dfhbr2012$hbr_no2)
is.numeric(dfhbr2012$hbr_noxasno2)
is.numeric(dfhbr2012$hbr_nv10)
is.numeric(dfhbr2012$hbr_v10)

colSums(is.na(dfhbr2013))
is.numeric(dfhbr2013$hbr_no)
is.numeric(dfhbr2013$hbr_pm10)
is.numeric(dfhbr2013$hbr_no2)
is.numeric(dfhbr2013$hbr_noxasno2)

colSums(is.na(dfhbr2014))
is.numeric(dfhbr2014$hbr_no)
is.numeric(dfhbr2014$hbr_pm10)
is.numeric(dfhbr2014$hbr_no2)
is.numeric(dfhbr2014$hbr_noxasno2)

colSums(is.na(dfhbr2015))
is.numeric(dfhbr2015$hbr_no)
is.numeric(dfhbr2015$hbr_pm10)
is.numeric(dfhbr2015$hbr_no2)
is.numeric(dfhbr2015$hbr_noxasno2)

colSums(is.na(dfhbr2016))
is.numeric(dfhbr2016$hbr_no)
is.numeric(dfhbr2016$hbr_pm10)
is.numeric(dfhbr2016$hbr_no2)
is.numeric(dfhbr2016$hbr_noxasno2)

colSums(is.na(dfhbr2017))
is.numeric(dfhbr2017$hbr_no)
is.numeric(dfhbr2017$hbr_pm10)
is.numeric(dfhbr2017$hbr_no2)
is.numeric(dfhbr2017$hbr_noxasno2)
is.numeric(dfhbr2017$hbr_pm25)

colSums(is.na(dfhbr2018))
is.numeric(dfhbr2018$hbr_no)
is.numeric(dfhbr2018$hbr_pm10)
is.numeric(dfhbr2018$hbr_no2)
is.numeric(dfhbr2018$hbr_noxasno2)
is.numeric(dfhbr2018$hbr_pm25)

colSums(is.na(dfhbr2019))
is.numeric(dfhbr2019$hbr_no)
is.numeric(dfhbr2019$hbr_pm10)
is.numeric(dfhbr2019$hbr_no2)
is.numeric(dfhbr2019$hbr_noxasno2)
is.numeric(dfhbr2019$hbr_pm25)

colSums(is.na(dfhbr2020))
is.numeric(dfhbr2020$hbr_no)
is.numeric(dfhbr2020$hbr_pm10)
is.numeric(dfhbr2020$hbr_no2)
is.numeric(dfhbr2020$hbr_noxasno2)
is.numeric(dfhbr2020$hbr_pm25)

colSums(is.na(dfhbr2021))
is.numeric(dfhbr2021$hbr_no)
is.numeric(dfhbr2021$hbr_pm10)
is.numeric(dfhbr2021$hbr_no2)
is.numeric(dfhbr2021$hbr_noxasno2)
is.numeric(dfhbr2021$hbr_pm25)

# 2021 Discovered nonnumerical data saved within all pollutants cols - Need correction (Answer: All hbr data is NA)


summary(dfhbr2021)
unique(dfhbr2021$hbr_pm10)
unique(dfhbr2021$hbr_no)
unique(dfhbr2021$hbr_no2)
unique(dfhbr2021$hbr_noxasno2)
unique(dfhbr2021$hbr_pm25)

View(dfhbr2021)

# Step 3: Combining hbr Datasets into 1 large repository 
#============================

comb_df_hbr <- bind_rows(dfhbr2012, dfhbr2013, dfhbr2014, dfhbr2015, dfhbr2016, dfhbr2017, dfhbr2018, dfhbr2019, dfhbr2020, dfhbr2021)
comb_df_hbr

tail(comb_df_hbr)
View(comb_df_hbr)


# Step 4: Arranging data class into the right format/class
# ============================

# Checking current class of date col
class(comb_df_hbr$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_hbr$end_date <- as.Date(comb_df_hbr$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hbr$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hbr$end_time)

# Declaring date and time objects
date_obj <- comb_df_hbr$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hbr$end_time
time_obj <- as.difftime(time_obj)

# New col to combine date & time = date_time_stamp
comb_df_hbr$date_time_stamp <- combine.date.and.time(date = date_obj, time = time_obj)

# Formatting decimal values to max of 3 small digits at the end - this would include all less decimal values and reduces biasness in data
comb_df_hbr$hbr_pm10 <- format(round(comb_df_hbr$hbr_pm10, 2), nsmall = 3)
comb_df_hbr$hbr_no <- format(round(comb_df_hbr$hbr_no, 2), nsmall = 3)
comb_df_hbr$hbr_no2 <- format(round(comb_df_hbr$hbr_no2, 2), nsmall = 3)
comb_df_hbr$hbr_noxasno2 <- format(round(comb_df_hbr$hbr_noxasno2, 2), nsmall = 3)
comb_df_hbr$hbr_nv10 <- format(round(comb_df_hbr$hbr_nv10, 2), nsmall = 3)
comb_df_hbr$hbr_pm25 <- format(round(comb_df_hbr$hbr_pm25, 2), nsmall = 3)
comb_df_hbr$hbr_v10 <- format(round(comb_df_hbr$hbr_v10, 2), nsmall = 3)

#Calling all col titles
colnames(comb_df_hbr)

# Re-arrangement of data including removal of end_date & end_time from the data set
comb2_df_hbr <- comb_df_hbr %>% 
  select(10,3:9)

head(comb2_df_hbr)
View(comb2_df_hbr)


# Step 5: hbr Data Export (VCM)
# ===============================

df_spot_hbr <- readr::write_csv(comb2_df_hbr, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/filter_by_hotspots/comb_df_hbr_with_decimals.csv")

#--------------------------------------------------------------------------------------------------------------------------------------
# hbr Combined Dataset Task Accomplished
#--------------------------------------------------------------------------------------------------------------------------------------
