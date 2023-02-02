# This R Script is to view all pollutants data for Hounslow's Gunnersbury (hgu)

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
summary(df_yr_2012$hgu_pm10)
summary(df_yr_2012$hgu_no)
summary(df_yr_2012$hgu_no2)
summary(df_yr_2012$hgu_noxasno2)
summary(df_yr_2012$hgu_nv10)

# ------------------------------------------------------------------------------------------------------

# Data sets cleaning steps 

# Step 1: Remove unwanted (duplicated )cols for i.e. pollutants status / unit is always the same or vice versa select only relevant cols by hotspots
#=========================

#hgu data for 2012
dfhgu2012 <- df_yr_2012 %>% 
  
  select(starts_with('end'),starts_with('hgu')) %>% 
  select(!ends_with('unit')) 

dfhgu2012

View(dfhgu2012)

#hgu data for 2013
dfhgu2013 <- df_yr_2013 %>% 
  
  select(starts_with('end'),starts_with('hgu')) %>% 
  select(!ends_with('unit')) 

dfhgu2013
View(dfhgu2013)

#hgu data for 2014
dfhgu2014 <- df_yr_2014 %>% 
  
  select(starts_with('end'),starts_with('hgu')) %>% 
  select(!ends_with('unit')) 

dfhgu2014

#hgu data for 2015
dfhgu2015 <- df_yr_2015 %>% 
  
  select(starts_with('end'),starts_with('hgu')) %>% 
  select(!ends_with('unit')) 

dfhgu2015

#hgu data for 2016
dfhgu2016 <- df_yr_2016 %>% 
  
  select(starts_with('end'),starts_with('hgu')) %>% 
  select(!ends_with('unit')) 

dfhgu2016

#hgu data for 2017
dfhgu2017 <- df_yr_2017 %>% 
  
  select(starts_with('end'),starts_with('hgu')) %>% 
  select(!ends_with('unit')) 

dfhgu2017

#hgu data for 2018
dfhgu2018 <- df_yr_2018 %>% 
  
  select(starts_with('end'),starts_with('hgu')) %>% 
  select(!ends_with('unit')) 

dfhgu2018

#hgu data for 2019
dfhgu2019 <- df_yr_2019 %>% 
  
  select(starts_with('end'),starts_with('hgu')) %>% 
  select(!ends_with('unit')) 

dfhgu2019

#hgu data for 2020
dfhgu2020 <- df_yr_2020 %>% 
  
  select(starts_with('end'),starts_with('hgu')) %>% 
  select(!ends_with('unit')) 

dfhgu2020

#hgu data for 2021
dfhgu2021 <- df_yr_2021 %>% 
  
  select(starts_with('end'),starts_with('hgu')) %>% 
  select(!ends_with('unit')) 

dfhgu2021


# Step 2: Data Cleaning Checks - to locate "No Data" text saved within the pollutants data (This would reduce bias in data sets)
#===========================

# Checking Missing Values in the data set
anyNA(dfhgu2012)
anyNA(dfhgu2013)
anyNA(dfhgu2014)
anyNA(dfhgu2015)
anyNA(dfhgu2016)
anyNA(dfhgu2017)
anyNA(dfhgu2018)
anyNA(dfhgu2019)
anyNA(dfhgu2020)
anyNA(dfhgu2021)

# Checking col values composition (2012-2021)
colSums(is.na(dfhgu2012))
is.numeric(dfhgu2012$hgu_no)
is.numeric(dfhgu2012$hgu_pm10)
is.numeric(dfhgu2012$hgu_no2)
is.numeric(dfhgu2012$hgu_noxasno2)


colSums(is.na(dfhgu2013))
is.numeric(dfhgu2013$hgu_no)
is.numeric(dfhgu2013$hgu_pm10)
is.numeric(dfhgu2013$hgu_no2)
is.numeric(dfhgu2013$hgu_noxasno2)

colSums(is.na(dfhgu2014))
is.numeric(dfhgu2014$hgu_no)
is.numeric(dfhgu2014$hgu_pm10)
is.numeric(dfhgu2014$hgu_no2)
is.numeric(dfhgu2014$hgu_noxasno2)

colSums(is.na(dfhgu2015))
is.numeric(dfhgu2015$hgu_no)
is.numeric(dfhgu2015$hgu_pm10)
is.numeric(dfhgu2015$hgu_no2)
is.numeric(dfhgu2015$hgu_noxasno2)

colSums(is.na(dfhgu2016))
is.numeric(dfhgu2016$hgu_no)
is.numeric(dfhgu2016$hgu_pm10)
is.numeric(dfhgu2016$hgu_no2)
is.numeric(dfhgu2016$hgu_noxasno2)

colSums(is.na(dfhgu2017))
is.numeric(dfhgu2017$hgu_no)
is.numeric(dfhgu2017$hgu_pm10)
is.numeric(dfhgu2017$hgu_no2)
is.numeric(dfhgu2017$hgu_noxasno2)

colSums(is.na(dfhgu2018))
is.numeric(dfhgu2018$hgu_no)
is.numeric(dfhgu2018$hgu_pm10)
is.numeric(dfhgu2018$hgu_no2)
is.numeric(dfhgu2018$hgu_noxasno2)

colSums(is.na(dfhgu2019))
is.numeric(dfhgu2019$hgu_no)
is.numeric(dfhgu2019$hgu_pm10)
is.numeric(dfhgu2019$hgu_no2)
is.numeric(dfhgu2019$hgu_noxasno2)

colSums(is.na(dfhgu2020))
is.numeric(dfhgu2020$hgu_no)
is.numeric(dfhgu2020$hgu_pm10)
is.numeric(dfhgu2020$hgu_no2)
is.numeric(dfhgu2020$hgu_noxasno2)

colSums(is.na(dfhgu2021))
is.numeric(dfhgu2021$hgu_no)
is.numeric(dfhgu2021$hgu_pm10)
is.numeric(dfhgu2021$hgu_no2)
is.numeric(dfhgu2021$hgu_noxasno2)

# 2021 Discovered nonnumerical data saved within all pollutants cols - Need correction (Answer: All hgu data is NA)


summary(dfhgu2021)
unique(dfhgu2021$hgu_pm10)
unique(dfhgu2021$hgu_no)
unique(dfhgu2021$hgu_no2)
unique(dfhgu2021$hgu_noxasno2)

View(dfhgu2021)

# Step 3: Combining hgu Datasets into 1 large repository 
#============================

comb_df_hgu <- bind_rows(dfhgu2012, dfhgu2013, dfhgu2014, dfhgu2015, dfhgu2016, dfhgu2017, dfhgu2018, dfhgu2019, dfhgu2020, dfhgu2021)
comb_df_hgu

tail(comb_df_hgu)
View(comb_df_hgu)


# Step 4: Arranging data class into the right format/class
# ============================

# Checking current class of date col
class(comb_df_hgu$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_hgu$end_date <- as.Date(comb_df_hgu$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hgu$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hgu$end_time)

# Declaring date and time objects
date_obj <- comb_df_hgu$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hgu$end_time
time_obj <- as.difftime(time_obj)

# New col to combine date & time = date_time_stamp
comb_df_hgu$date_time_stamp <- combine.date.and.time(date = date_obj, time = time_obj)

# Formatting decimal values to max of 3 small digits at the end - this would include all less decimal values and reduces biasness in data
comb_df_hgu$hgu_pm10 <- format(round(comb_df_hgu$hgu_pm10, 2), nsmall = 3)
comb_df_hgu$hgu_no <- format(round(comb_df_hgu$hgu_no, 2), nsmall = 3)
comb_df_hgu$hgu_no2 <- format(round(comb_df_hgu$hgu_no2, 2), nsmall = 3)
comb_df_hgu$hgu_noxasno2 <- format(round(comb_df_hgu$hgu_noxasno2, 2), nsmall = 3)

#Calling all col titles
colnames(comb_df_hgu)

# Re-arrangement of data including removal of end_date & end_time from the data set
comb2_df_hgu <- comb_df_hgu %>% 
  select(7,3:6)

head(comb2_df_hgu)
View(comb2_df_hgu)


# Step 5: hgu Data Export (VCM)
# ===============================

df_spot_hgu <- readr::write_csv(comb2_df_hgu, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/filter_by_hotspots/comb_df_hgu_with_decimals.csv")

#--------------------------------------------------------------------------------------------------------------------------------------
# hgu Combined Dataset Task Accomplished
#--------------------------------------------------------------------------------------------------------------------------------------
