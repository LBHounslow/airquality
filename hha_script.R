# This R Script is to view all pollutants data for Hounslow's Hatton Cross (hha)

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
summary(df_yr_2012$hha_pm10)
summary(df_yr_2012$hha_no)
summary(df_yr_2012$hha_no2)
summary(df_yr_2012$hha_noxasno2)
summary(df_yr_2012$hha_nv10)

# ------------------------------------------------------------------------------------------------------

# Data sets cleaning steps 

# Step 1: Remove unwanted (duplicated )cols for i.e. pollutants status / unit is always the same or vice versa select only relevant cols by hotspots
#=========================

#hha data for 2012
dfhha2012 <- df_yr_2012 %>% 
  
  select(starts_with('end'),starts_with('hha')) %>% 
  select(!ends_with('unit')) 

dfhha2012

View(dfhha2012)

#hha data for 2013
dfhha2013 <- df_yr_2013 %>% 
  
  select(starts_with('end'),starts_with('hha')) %>% 
  select(!ends_with('unit')) 

dfhha2013
View(dfhha2013)

#hha data for 2014
dfhha2014 <- df_yr_2014 %>% 
  
  select(starts_with('end'),starts_with('hha')) %>% 
  select(!ends_with('unit')) 

dfhha2014

#hha data for 2015
dfhha2015 <- df_yr_2015 %>% 
  
  select(starts_with('end'),starts_with('hha')) %>% 
  select(!ends_with('unit')) 

dfhha2015

#hha data for 2016
dfhha2016 <- df_yr_2016 %>% 
  
  select(starts_with('end'),starts_with('hha')) %>% 
  select(!ends_with('unit')) 

dfhha2016

#hha data for 2017
dfhha2017 <- df_yr_2017 %>% 
  
  select(starts_with('end'),starts_with('hha')) %>% 
  select(!ends_with('unit')) 

dfhha2017

#hha data for 2018
dfhha2018 <- df_yr_2018 %>% 
  
  select(starts_with('end'),starts_with('hha')) %>% 
  select(!ends_with('unit')) 

dfhha2018

#hha data for 2019
dfhha2019 <- df_yr_2019 %>% 
  
  select(starts_with('end'),starts_with('hha')) %>% 
  select(!ends_with('unit')) 

dfhha2019

#hha data for 2020
dfhha2020 <- df_yr_2020 %>% 
  
  select(starts_with('end'),starts_with('hha')) %>% 
  select(!ends_with('unit')) 

dfhha2020

#hha data for 2021
dfhha2021 <- df_yr_2021 %>% 
  
  select(starts_with('end'),starts_with('hha')) %>% 
  select(!ends_with('unit')) 

dfhha2021


# Step 2: Data Cleaning Checks - to locate "No Data" text saved within the pollutants data (This would reduce bias in data sets)
#===========================

# Checking Missing Values in the data set
anyNA(dfhha2012)
anyNA(dfhha2013)
anyNA(dfhha2014)
anyNA(dfhha2015)
anyNA(dfhha2016)
anyNA(dfhha2017)
anyNA(dfhha2018)
anyNA(dfhha2019)
anyNA(dfhha2020)
anyNA(dfhha2021)

# Checking col values composition (2012-2021)
colSums(is.na(dfhha2012))
is.numeric(dfhha2012$hha_no)
is.numeric(dfhha2012$hha_pm10)
is.numeric(dfhha2012$hha_no2)
is.numeric(dfhha2012$hha_noxasno2)


colSums(is.na(dfhha2013))
is.numeric(dfhha2013$hha_no)
is.numeric(dfhha2013$hha_pm10)
is.numeric(dfhha2013$hha_no2)
is.numeric(dfhha2013$hha_noxasno2)

colSums(is.na(dfhha2014))
is.numeric(dfhha2014$hha_no)
is.numeric(dfhha2014$hha_pm10)
is.numeric(dfhha2014$hha_no2)
is.numeric(dfhha2014$hha_noxasno2)

colSums(is.na(dfhha2015))
is.numeric(dfhha2015$hha_no)
is.numeric(dfhha2015$hha_pm10)
is.numeric(dfhha2015$hha_no2)
is.numeric(dfhha2015$hha_noxasno2)

colSums(is.na(dfhha2016))
is.numeric(dfhha2016$hha_no)
is.numeric(dfhha2016$hha_pm10)
is.numeric(dfhha2016$hha_no2)
is.numeric(dfhha2016$hha_noxasno2)

colSums(is.na(dfhha2017))
is.numeric(dfhha2017$hha_no)
is.numeric(dfhha2017$hha_pm10)
is.numeric(dfhha2017$hha_no2)
is.numeric(dfhha2017$hha_noxasno2)

colSums(is.na(dfhha2018))
is.numeric(dfhha2018$hha_no)
is.numeric(dfhha2018$hha_pm10)
is.numeric(dfhha2018$hha_no2)
is.numeric(dfhha2018$hha_noxasno2)

colSums(is.na(dfhha2019))
is.numeric(dfhha2019$hha_no)
is.numeric(dfhha2019$hha_pm10)
is.numeric(dfhha2019$hha_no2)
is.numeric(dfhha2019$hha_noxasno2)

colSums(is.na(dfhha2020))
is.numeric(dfhha2020$hha_no)
is.numeric(dfhha2020$hha_pm10)
is.numeric(dfhha2020$hha_no2)
is.numeric(dfhha2020$hha_noxasno2)

colSums(is.na(dfhha2021))
is.numeric(dfhha2021$hha_no)
is.numeric(dfhha2021$hha_pm10)
is.numeric(dfhha2021$hha_no2)
is.numeric(dfhha2021$hha_noxasno2)

# 2021 Discovered nonnumerical data saved within all pollutants cols - Need correction (Answer: All hha data is NA)


summary(dfhha2021)
unique(dfhha2021$hha_pm10)
unique(dfhha2021$hha_no)
unique(dfhha2021$hha_no2)
unique(dfhha2021$hha_noxasno2)

View(dfhha2021)

# Step 3: Combining hha Datasets into 1 large repository 
#============================

comb_df_hha <- bind_rows(dfhha2012, dfhha2013, dfhha2014, dfhha2015, dfhha2016, dfhha2017, dfhha2018, dfhha2019, dfhha2020, dfhha2021)
comb_df_hha

tail(comb_df_hha)
View(comb_df_hha)


# Step 4: Arranging data class into the right format/class
# ============================

# Checking current class of date col
class(comb_df_hha$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_hha$end_date <- as.Date(comb_df_hha$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hha$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hha$end_time)

# Declaring date and time objects
date_obj <- comb_df_hha$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hha$end_time
time_obj <- as.difftime(time_obj)

# New col to combine date & time = date_time_stamp
comb_df_hha$date_time_stamp <- combine.date.and.time(date = date_obj, time = time_obj)

# Formatting decimal values to max of 3 small digits at the end - this would include all less decimal values and reduces biasness in data
comb_df_hha$hha_pm10 <- format(round(comb_df_hha$hha_pm10, 2), nsmall = 3)
comb_df_hha$hha_no <- format(round(comb_df_hha$hha_no, 2), nsmall = 3)
comb_df_hha$hha_no2 <- format(round(comb_df_hha$hha_no2, 2), nsmall = 3)
comb_df_hha$hha_noxasno2 <- format(round(comb_df_hha$hha_noxasno2, 2), nsmall = 3)

#Calling all col titles
colnames(comb_df_hha)

# Re-arrangement of data including removal of end_date & end_time from the data set
comb2_df_hha <- comb_df_hha %>% 
  select(7,3:6)

head(comb2_df_hha)
View(comb2_df_hha)


# Step 5: hha Data Export (VCM)
# ===============================

df_spot_hha <- readr::write_csv(comb2_df_hha, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/filter_by_hotspots/comb_df_hha_with_decimals.csv")

#--------------------------------------------------------------------------------------------------------------------------------------
# hha Combined Dataset Task Accomplished
#--------------------------------------------------------------------------------------------------------------------------------------
