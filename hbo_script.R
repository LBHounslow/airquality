# This R Script is to view all pollutants data for Hounslow's Boston Manor Park (hbo) - Data is only available b/w 2018-2020

# Data Labels 
# --------------------------------------------------------------------------------------------------------
# hcr = Hounslow Cranford
# hch = Hounslow Chiswick
# hbr = Hounslow Brentford
# hhe = Hounslow Heston
# hha = Hounslow Hatton Cross
# hgu = Hounslow Gunnersbury
# hbo = Hounslow Feltham
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
summary(df_yr_2012$hbo_pm10)
summary(df_yr_2012$hbo_no)
summary(df_yr_2012$hbo_no2)
summary(df_yr_2012$hbo_noxasno2)
summary(df_yr_2012$hbo_nv10)

# ------------------------------------------------------------------------------------------------------

# Data sets cleaning steps 

# Step 1: Remove unwanted (duplicated )cols for i.e. pollutants status / unit is always the same or vice versa select only relevant cols by hotspots
#=========================

#hbo data for 2012
dfhbo2012 <- df_yr_2012 %>% 
  
  select(starts_with('end'),starts_with('hbo')) %>% 
  select(!ends_with('unit')) 

dfhbo2012

View(dfhbo2012)

#hbo data for 2013
dfhbo2013 <- df_yr_2013 %>% 
  
  select(starts_with('end'),starts_with('hbo')) %>% 
  select(!ends_with('unit')) 

dfhbo2013
View(dfhbo2013)

#hbo data for 2014
dfhbo2014 <- df_yr_2014 %>% 
  
  select(starts_with('end'),starts_with('hbo')) %>% 
  select(!ends_with('unit')) 

dfhbo2014

#hbo data for 2015
dfhbo2015 <- df_yr_2015 %>% 
  
  select(starts_with('end'),starts_with('hbo')) %>% 
  select(!ends_with('unit')) 

dfhbo2015

#hbo data for 2016
dfhbo2016 <- df_yr_2016 %>% 
  
  select(starts_with('end'),starts_with('hbo')) %>% 
  select(!ends_with('unit')) 

dfhbo2016

#hbo data for 2017
dfhbo2017 <- df_yr_2017 %>% 
  
  select(starts_with('end'),starts_with('hbo')) %>% 
  select(!ends_with('unit')) 

dfhbo2017

#hbo data for 2018
dfhbo2018 <- df_yr_2018 %>% 
  
  select(starts_with('end'),starts_with('hbo')) %>% 
  select(!ends_with('unit')) 

dfhbo2018

#hbo data for 2019
dfhbo2019 <- df_yr_2019 %>% 
  
  select(starts_with('end'),starts_with('hbo')) %>% 
  select(!ends_with('unit')) 

dfhbo2019

#hbo data for 2020
dfhbo2020 <- df_yr_2020 %>% 
  
  select(starts_with('end'),starts_with('hbo')) %>% 
  select(!ends_with('unit')) 

dfhbo2020

#hbo data for 2021
dfhbo2021 <- df_yr_2021 %>% 
  
  select(starts_with('end'),starts_with('hbo')) %>% 
  select(!ends_with('unit')) 

dfhbo2021


# Step 2: Data Cleaning Checks - to locate "No Data" text saved within the pollutants data (This would reduce bias in data sets)
#===========================

# Checking Missing Values in the data set
anyNA(dfhbo2012)
anyNA(dfhbo2013)
anyNA(dfhbo2014)
anyNA(dfhbo2015)
anyNA(dfhbo2016)
anyNA(dfhbo2017)
anyNA(dfhbo2018)
anyNA(dfhbo2019)
anyNA(dfhbo2020)
anyNA(dfhbo2021)

# Checking col values composition (2012-2021)
colSums(is.na(dfhbo2012))

colSums(is.na(dfhbo2013))

colSums(is.na(dfhbo2014))

colSums(is.na(dfhbo2015))

colSums(is.na(dfhbo2016))

colSums(is.na(dfhbo2017))

colSums(is.na(dfhbo2018))
is.numeric(dfhbo2018$hbo_no)
is.numeric(dfhbo2018$hbo_pm10)
is.numeric(dfhbo2018$hbo_no2)
is.numeric(dfhbo2018$hbo_noxasno2)

colSums(is.na(dfhbo2019))
is.numeric(dfhbo2019$hbo_no)
is.numeric(dfhbo2019$hbo_pm10)
is.numeric(dfhbo2019$hbo_no2)
is.numeric(dfhbo2019$hbo_noxasno2)

colSums(is.na(dfhbo2020))
is.numeric(dfhbo2020$hbo_no)
is.numeric(dfhbo2020$hbo_pm10)
is.numeric(dfhbo2020$hbo_no2)
is.numeric(dfhbo2020$hbo_noxasno2)

colSums(is.na(dfhbo2021))

# 2021 Discovered nonnumerical data saved within all pollutants cols - Need correction (Answer: All hbo data is NA)


summary(dfhbo2020)
unique(dfhbo2020$hbo_pm10)
unique(dfhbo2020$hbo_no)
unique(dfhbo2020$hbo_no2)
unique(dfhbo2020$hbo_noxasno2)

View(dfhbo2020)

# Step 3: Combining hbo Datasets into 1 large repository 
#============================

comb_df_hbo <- bind_rows(dfhbo2012, dfhbo2013, dfhbo2014, dfhbo2015, dfhbo2016, dfhbo2017, dfhbo2018, dfhbo2019, dfhbo2020, dfhbo2021)
comb_df_hbo

tail(comb_df_hbo)
View(comb_df_hbo)


# Step 4: Arranging data class into the right format/class
# ============================

# Checking current class of date col
class(comb_df_hbo$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_hbo$end_date <- as.Date(comb_df_hbo$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hbo$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hbo$end_time)

# Declaring date and time objects
date_obj <- comb_df_hbo$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hbo$end_time
time_obj <- as.difftime(time_obj)

# New col to combine date & time = date_time_stamp
comb_df_hbo$date_time_stamp <- combine.date.and.time(date = date_obj, time = time_obj)

# Formatting decimal values to max of 3 small digits at the end - this would include all less decimal values and reduces biasness in data
comb_df_hbo$hbo_pm10 <- format(round(comb_df_hbo$hbo_pm10, 2), nsmall = 3)
comb_df_hbo$hbo_no <- format(round(comb_df_hbo$hbo_no, 2), nsmall = 3)
comb_df_hbo$hbo_no2 <- format(round(comb_df_hbo$hbo_no2, 2), nsmall = 3)
comb_df_hbo$hbo_noxasno2 <- format(round(comb_df_hbo$hbo_noxasno2, 2), nsmall = 3)

#Calling all col titles
colnames(comb_df_hbo)

# Re-arrangement of data including removal of end_date & end_time from the data set
comb2_df_hbo <- comb_df_hbo %>% 
  select(7,3:6)

head(comb2_df_hbo)
View(comb2_df_hbo)


# Step 5: hbo Data Export (VCM)
# ===============================

df_spot_hbo <- readr::write_csv(comb2_df_hbo, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/filter_by_hotspots/comb_df_hbo_with_decimals.csv")

#--------------------------------------------------------------------------------------------------------------------------------------
# hbo Combined Dataset Task Accomplished
#--------------------------------------------------------------------------------------------------------------------------------------
