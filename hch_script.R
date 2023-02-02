# This R Script is to view all pollutants data for Hounslow's Chiswick(HCH)

# Data Labels 
# --------------------------------------------------------------------------------------------------------
# hcr = Hounslow Cranford
# hch = Hounslow Chiswick
# bhr = Hounslow Brentford
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
summary(df_yr_2012$hch_pm10)
summary(df_yr_2012$hch_no)
summary(df_yr_2012$hch_no2)
summary(df_yr_2012$hch_noxasno2)
summary(df_yr_2012$hch_nv10)

# ------------------------------------------------------------------------------------------------------

# Data sets cleaning steps 

# Step 1: Remove unwanted (duplicated )cols for i.e. pollutants status / unit is always the same or vice versa select only relevant cols by hotspots
#=========================

#HCH data for 2012
dfhch2012 <- df_yr_2012 %>% 
  
  select(starts_with('end'),starts_with('hch')) %>% 
  select(!ends_with('unit')) 

dfhch2012

View(dfhch2012)

#hch data for 2013
dfhch2013 <- df_yr_2013 %>% 
  
  select(starts_with('end'),starts_with('hch')) %>% 
  select(!ends_with('unit')) 

dfhch2013
View(dfhch2013)

#hch data for 2014
dfhch2014 <- df_yr_2014 %>% 
  
  select(starts_with('end'),starts_with('hch')) %>% 
  select(!ends_with('unit')) 

dfhch2014

#hch data for 2015
dfhch2015 <- df_yr_2015 %>% 
  
  select(starts_with('end'),starts_with('hch')) %>% 
  select(!ends_with('unit')) 

dfhch2015

#hch data for 2016
dfhch2016 <- df_yr_2016 %>% 
  
  select(starts_with('end'),starts_with('hch')) %>% 
  select(!ends_with('unit')) 

dfhch2016

#hch data for 2017
dfhch2017 <- df_yr_2017 %>% 
  
  select(starts_with('end'),starts_with('hch')) %>% 
  select(!ends_with('unit')) 

dfhch2017

#hch data for 2018
dfhch2018 <- df_yr_2018 %>% 
  
  select(starts_with('end'),starts_with('hch')) %>% 
  select(!ends_with('unit')) 

dfhch2018

#hch data for 2019
dfhch2019 <- df_yr_2019 %>% 
  
  select(starts_with('end'),starts_with('hch')) %>% 
  select(!ends_with('unit')) 

dfhch2019

#hch data for 2020
dfhch2020 <- df_yr_2020 %>% 
  
  select(starts_with('end'),starts_with('hch')) %>% 
  select(!ends_with('unit')) 

dfhch2020

#hch data for 2021
dfhch2021 <- df_yr_2021 %>% 
  
  select(starts_with('end'),starts_with('hch')) %>% 
  select(!ends_with('unit')) 

dfhch2021


# Step 2: Data Cleaning Checks - to locate "No Data" text saved within the pollutants data (This would reduce bias in data sets)
#===========================

# Checking Missing Values in the data set
anyNA(dfhch2012)
anyNA(dfhch2013)
anyNA(dfhch2014)
anyNA(dfhch2015)
anyNA(dfhch2016)
anyNA(dfhch2017)
anyNA(dfhch2018)
anyNA(dfhch2019)
anyNA(dfhch2020)
anyNA(dfhch2021)

# Checking col values composition (2012-2021)
colSums(is.na(dfhch2012))
is.numeric(dfhch2012$hch_no)
is.numeric(dfhch2012$hch_pm10)
is.numeric(dfhch2012$hch_no2)
is.numeric(dfhch2012$hch_noxasno2)
is.numeric(dfhch2012$hch_nv10)
is.numeric(dfhch2012$hch_v10)

colSums(is.na(dfhch2013))
is.numeric(dfhch2013$hch_no)
is.numeric(dfhch2013$hch_pm10)
is.numeric(dfhch2013$hch_no2)
is.numeric(dfhch2013$hch_noxasno2)


colSums(is.na(dfhch2014))
is.numeric(dfhch2014$hch_no)
is.numeric(dfhch2014$hch_pm10)
is.numeric(dfhch2014$hch_no2)
is.numeric(dfhch2014$hch_noxasno2)



colSums(is.na(dfhch2015))
is.numeric(dfhch2015$hch_no)
is.numeric(dfhch2015$hch_pm10)
is.numeric(dfhch2015$hch_no2)
is.numeric(dfhch2015$hch_noxasno2)


colSums(is.na(dfhch2016))
is.numeric(dfhch2016$hch_no)
is.numeric(dfhch2016$hch_pm10)
is.numeric(dfhch2016$hch_no2)
is.numeric(dfhch2016$hch_noxasno2)


colSums(is.na(dfhch2017))
is.numeric(dfhch2017$hch_no)
is.numeric(dfhch2017$hch_pm10)
is.numeric(dfhch2017$hch_no2)
is.numeric(dfhch2017$hch_noxasno2)


colSums(is.na(dfhch2018))
is.numeric(dfhch2018$hch_no)
is.numeric(dfhch2018$hch_pm10)
is.numeric(dfhch2018$hch_no2)
is.numeric(dfhch2018$hch_noxasno2)


colSums(is.na(dfhch2019))
is.numeric(dfhch2019$hch_no)
is.numeric(dfhch2019$hch_pm10)
is.numeric(dfhch2019$hch_no2)
is.numeric(dfhch2019$hch_noxasno2)


colSums(is.na(dfhch2020))
is.numeric(dfhch2020$hch_no)
is.numeric(dfhch2020$hch_pm10)
is.numeric(dfhch2020$hch_no2)
is.numeric(dfhch2020$hch_noxasno2)



colSums(is.na(dfhch2021))
is.numeric(dfhch2021$hch_no)
is.numeric(dfhch2021$hch_pm10)
is.numeric(dfhch2021$hch_no2)
is.numeric(dfhch2021$hch_noxasno2)



# 2021 Discovered nonnumerical data saved within all pollutants cols - Need correction (Answer: All hch data is NA)


summary(dfhch2021)
unique(dfhch2021$hch_pm10)
unique(dfhch2021$hch_no)
unique(dfhch2021$hch_no2)
unique(dfhch2021$hch_noxasno2)


View(dfhch2021)

# Step 3: Combining hch Datasets into 1 large repository 
#============================

comb_df_hch <- bind_rows(dfhch2012, dfhch2013, dfhch2014, dfhch2015, dfhch2016, dfhch2017, dfhch2018, dfhch2019, dfhch2020, dfhch2021)
comb_df_hch

tail(comb_df_hch)
View(comb_df_hch)


# Step 4: Arranging data class into the right format/class
# ============================

# Checking current class of date col
class(comb_df_hch$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_hch$end_date <- as.Date(comb_df_hch$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hch$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hch$end_time)

# Declaring date and time objects
date_obj <- comb_df_hch$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hch$end_time
time_obj <- as.difftime(time_obj)

# New col to combine date & time = date_time_stamp
comb_df_hch$date_time_stamp <- combine.date.and.time(date = date_obj, time = time_obj)

#Calling all col titles
colnames(comb_df_hch)

# Formatting decimal values to max of 3 small digits at the end - this would include all less decimal values and reduces biasness in data
comb_df_hch$hch_pm10 <- format(round(comb_df_hch$hch_pm10, 2), nsmall = 3)
comb_df_hch$hch_no <- format(round(comb_df_hch$hch_no, 2), nsmall = 3)
comb_df_hch$hch_no2 <- format(round(comb_df_hch$hch_no2, 2), nsmall = 3)
comb_df_hch$hch_noxasno2 <- format(round(comb_df_hch$hch_noxasno2, 2), nsmall = 3)
comb_df_hch$hch_nv10 <- format(round(comb_df_hch$hch_nv10, 2), nsmall = 3)
comb_df_hch$hch_pm25 <- format(round(comb_df_hch$hch_pm25, 2), nsmall = 3)
comb_df_hch$hch_v10 <- format(round(comb_df_hch$hch_v10, 2), nsmall = 3)

# Re-arrangement of data including removal of end_date & end_time from the data set
comb2_df_hch <- comb_df_hch %>% 
  select(10,3:9)

head(comb2_df_hch)
View(comb2_df_hch)


# Step 5: hch Data Export (VCM)
# ===============================

df_spot_hch <- readr::write_csv(comb2_df_hch, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/filter_by_hotspots/comb_df_hch_with_decimals.csv")

#--------------------------------------------------------------------------------------------------------------------------------------
# HCH Combined Dataset Task Accomplished
#--------------------------------------------------------------------------------------------------------------------------------------
