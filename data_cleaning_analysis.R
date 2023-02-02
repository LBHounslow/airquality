# This R Script is to view dataset prior to deep analysis process

# Data Labels 
# --------------------------------------------------------------------------------------------------------
# hcr = Hounslow Cranford
# hch = Hounslow Chiswick
# bhr = Hounslow Brentford
# hhe = Hounslow Heston
# hha = Hounslow Heston
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
summary(df_yr_2012$hcr_pm10)
summary(df_yr_2012$hcr_no)
summary(df_yr_2012$hcr_no2)
summary(df_yr_2012$hcr_noxasno2)
summary(df_yr_2012$hcr_nv10)

# ------------------------------------------------------------------------------------------------------

# Data sets cleaning steps 

# Step 1: Remove unwanted (duplicated )cols for i.e. pollutants status / unit is always the same or vice versa select only relevant cols by hotspots
#=========================

#HCR data for 2012
dfhcr2012 <- df_yr_2012 %>% 
  
  select(starts_with('end'),starts_with('hcr')) %>% 
  select(!ends_with('unit')) 

dfhcr2012

View(dfhcr2012)

#HCR data for 2013
dfhcr2013 <- df_yr_2013 %>% 
  
  select(starts_with('end'),starts_with('hcr')) %>% 
  select(!ends_with('unit')) 

dfhcr2013
View(dfhcr2013)

#HCR data for 2014
dfhcr2014 <- df_yr_2014 %>% 
  
  select(starts_with('end'),starts_with('hcr')) %>% 
  select(!ends_with('unit')) 

dfhcr2014

#HCR data for 2015
dfhcr2015 <- df_yr_2015 %>% 
  
  select(starts_with('end'),starts_with('hcr')) %>% 
  select(!ends_with('unit')) 

dfhcr2015

#HCR data for 2016
dfhcr2016 <- df_yr_2016 %>% 
  
  select(starts_with('end'),starts_with('hcr')) %>% 
  select(!ends_with('unit')) 

dfhcr2016

#HCR data for 2017
dfhcr2017 <- df_yr_2017 %>% 
  
  select(starts_with('end'),starts_with('hcr')) %>% 
  select(!ends_with('unit')) 

dfhcr2017

#HCR data for 2018
dfhcr2018 <- df_yr_2018 %>% 
  
  select(starts_with('end'),starts_with('hcr')) %>% 
  select(!ends_with('unit')) 

dfhcr2018

#HCR data for 2019
dfhcr2019 <- df_yr_2019 %>% 
  
  select(starts_with('end'),starts_with('hcr')) %>% 
  select(!ends_with('unit')) 

dfhcr2019

#HCR data for 2020
dfhcr2020 <- df_yr_2020 %>% 
  
  select(starts_with('end'),starts_with('hcr')) %>% 
  select(!ends_with('unit')) 

dfhcr2020

#HCR data for 2021
dfhcr2021 <- df_yr_2021 %>% 
  
  select(starts_with('end'),starts_with('hcr')) %>% 
  select(!ends_with('unit')) 

dfhcr2021

# Step 2: Data Cleaning Checks - to locate "No Data" text saved within the pollutants data (This would reduce bias in data sets)
#===========================

# Checking Missing Values in the data set
anyNA(dfhcr2012)
anyNA(dfhcr2013)
anyNA(dfhcr2014)
anyNA(dfhcr2015)
anyNA(dfhcr2016)
anyNA(dfhcr2017)
anyNA(dfhcr2018)
anyNA(dfhcr2019)
anyNA(dfhcr2020)
anyNA(dfhcr2021)

# Checking col values composition (2012-2021)
colSums(is.na(dfhcr2012))
is.numeric(dfhcr2012$hcr_no)
is.numeric(dfhcr2012$hcr_pm10)
is.numeric(dfhcr2012$hcr_no2)
is.numeric(dfhcr2012$hcr_noxasno2)
is.numeric(dfhcr2012$hcr_nv10)
is.numeric(dfhcr2012$hcr_o3)
is.numeric(dfhcr2012$hcr_so2)
is.numeric(dfhcr2012$hcr_v10)

colSums(is.na(dfhcr2013))
is.numeric(dfhcr2013$hcr_no)
is.numeric(dfhcr2013$hcr_pm10)
is.numeric(dfhcr2013$hcr_no2)
is.numeric(dfhcr2013$hcr_noxasno2)
is.numeric(dfhcr2013$hcr_o3)
is.numeric(dfhcr2013$hcr_so2)

colSums(is.na(dfhcr2014))
is.numeric(dfhcr2014$hcr_no)
is.numeric(dfhcr2014$hcr_pm10)
is.numeric(dfhcr2014$hcr_no2)
is.numeric(dfhcr2014$hcr_noxasno2)
is.numeric(dfhcr2014$hcr_o3)
is.numeric(dfhcr2014$hcr_so2)

colSums(is.na(dfhcr2015))
is.numeric(dfhcr2015$hcr_no)
is.numeric(dfhcr2015$hcr_pm10)
is.numeric(dfhcr2015$hcr_no2)
is.numeric(dfhcr2015$hcr_noxasno2)
is.numeric(dfhcr2015$hcr_o3)
is.numeric(dfhcr2015$hcr_so2)

colSums(is.na(dfhcr2016))
is.numeric(dfhcr2016$hcr_no)
is.numeric(dfhcr2016$hcr_pm10)
is.numeric(dfhcr2016$hcr_no2)
is.numeric(dfhcr2016$hcr_noxasno2)
is.numeric(dfhcr2016$hcr_o3)
is.numeric(dfhcr2016$hcr_so2)

colSums(is.na(dfhcr2017))
is.numeric(dfhcr2017$hcr_no)
is.numeric(dfhcr2017$hcr_pm10)
is.numeric(dfhcr2017$hcr_no2)
is.numeric(dfhcr2017$hcr_noxasno2)
is.numeric(dfhcr2017$hcr_o3)
is.numeric(dfhcr2017$hcr_so2)

colSums(is.na(dfhcr2018))
is.numeric(dfhcr2018$hcr_no)
is.numeric(dfhcr2018$hcr_pm10)
is.numeric(dfhcr2018$hcr_no2)
is.numeric(dfhcr2018$hcr_noxasno2)
is.numeric(dfhcr2018$hcr_o3)
is.numeric(dfhcr2018$hcr_so2)

colSums(is.na(dfhcr2019))
is.numeric(dfhcr2019$hcr_no)
is.numeric(dfhcr2019$hcr_pm10)
is.numeric(dfhcr2019$hcr_no2)
is.numeric(dfhcr2019$hcr_noxasno2)
is.numeric(dfhcr2019$hcr_o3)
is.numeric(dfhcr2019$hcr_so2)

colSums(is.na(dfhcr2020))
is.numeric(dfhcr2020$hcr_no)
is.numeric(dfhcr2020$hcr_pm10)
is.numeric(dfhcr2020$hcr_no2)
is.numeric(dfhcr2020$hcr_noxasno2)
is.numeric(dfhcr2020$hcr_o3)
is.numeric(dfhcr2020$hcr_so2)

colSums(is.na(dfhcr2021))
is.numeric(dfhcr2021$hcr_no)
is.numeric(dfhcr2021$hcr_pm10)
is.numeric(dfhcr2021$hcr_no2)
is.numeric(dfhcr2021$hcr_noxasno2)
is.numeric(dfhcr2021$hcr_o3)
is.numeric(dfhcr2021$hcr_so2)

# 2021 Discovered nonnumerical data saved within all pollutants cols - Need correction (Answer: All HCR data is NA)


summary(dfhcr2021)
unique(dfhcr2021$hcr_pm10)
unique(dfhcr2021$hcr_no)
unique(dfhcr2021$hcr_no2)
unique(dfhcr2021$hcr_noxasno2)
unique(dfhcr2021$hcr_o3)
unique(dfhcr2021$hcr_so2)

View(dfhcr2021)

# Step 3: Combining hcr Datasets into 1 large repository 
#============================

comb_df_hcr <- bind_rows(dfhcr2012, dfhcr2013, dfhcr2014, dfhcr2015, dfhcr2016, dfhcr2017, dfhcr2018, dfhcr2019, dfhcr2020, dfhcr2021)
comb_df_hcr

tail(comb_df_hcr)
View(comb_df_hcr)


# Step 4: HCR Data Export (VCM)

readr::write_csv(comb_df_hcr, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/comb_df_hcr.csv")

#--------------------------------------------------------------------------------------------------------------------------------------
# HCR Combined Dataset Task Accomplished
#--------------------------------------------------------------------------------------------------------------------------------------
