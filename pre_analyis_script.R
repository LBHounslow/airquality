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

# Understanding Col titles, its naming conventions and to see common problems like NA's

dplyr::glimpse(df_yr_2012)
dplyr::glimpse(df_yr_2013)
dplyr::glimpse(df_yr_2014)
dplyr::glimpse(df_yr_2015)
dplyr::glimpse(df_yr_2016)
dplyr::glimpse(df_yr_2017)
dplyr::glimpse(df_yr_2018)
dplyr::glimpse(df_yr_2019)
dplyr::glimpse(df_yr_2020)
dplyr::glimpse(df_yr_2021)

# Viewing auto pick n select samples of data

dplyr::sample_n(df_yr_2013, size = 5)

dplyr::sample_n(df_yr_2016, size = 10)

dplyr::sample_n(df_yr_2019, size = 10)

# ------------------------------------------------------------------------------------------------------

# To view/check top and bottom shape of the datasets 

head(df_yr_2012, 5)
tail(df_yr_2012, 5)

dim(df_yr_2012)
nrow(df_yr_2012)
ncol(df_yr_2012)
df_col_titles <- names(df_yr_2012)
View(df_col_titles)

# ------------------------------------------------------------------------------------------------------

# Understanding of datasets overall statistics 

dim(df_yr_2012)
dim(df_yr_2013)
dim(df_yr_2014)
dim(df_yr_2015)
dim(df_yr_2016)
dim(df_yr_2017)
dim(df_yr_2018)
dim(df_yr_2019)
dim(df_yr_2020)
dim(df_yr_2021)

# 2012 (8784 cols & 74 rows)
# 2013 (8760 cols & 62 rows)
# 2014 (8760 cols & 62 rows)
# 2015 (8760 cols & 62 rows)
# 2016 (8784 cols & 62 rows)
# 2017 (8760 cols & 66 rows)
# 2018 (8760 cols & 74 rows)
# 2019 (8760 cols & 74 rows)
# 2020 (8783 cols & 74 rows)
# 2021 (8760 cols & 66 rows)

# ----------------------------------------------------------------------------------------------------

# Overall Datasets Orientation / Calculation

x1 <- nrow(df_yr_2012)
x2 <- nrow(df_yr_2013)
x3 <- nrow(df_yr_2014)
x4 <- nrow(df_yr_2015)
x5 <- nrow(df_yr_2016)
x6 <- nrow(df_yr_2017)
x7 <- nrow(df_yr_2018)
x8 <- nrow(df_yr_2019)
x9 <- nrow(df_yr_2020)
x10 <- nrow(df_yr_2021)

total_x <- c(x1+x2+x3+x4+x5+x6+x7+x8+x9+x10)
total_x

# Total rows are 87,671 which covers 10 yrs period from Jan 2012 till Dec 2021)

# ----------------------------------------------------------------------------------------------------

# To filter out unique values in dataset columns (to determine consistency of pollutants readings)

unique(df_yr_2012$hcr_pm10)
unique(df_yr_2012$hcr_pm10, df_yr_2013$hcr_pm10)
unique(df_yr_2012$hcr_pm10, df_yr_2013$hcr_pm10, df_yr_2014$hcr_pm10)

unique(df_yr_2014$hch_pm10)
unique(df_yr_2014$hch_pm10, df_yr_2015$hch_pm10)
unique(df_yr_2014$hch_pm10, df_yr_2015$hch_pm10, df_yr_2017$hch_pm10)

# ----------------------------------------------------------------------------------------------------

# To sense check Missing Values / NA's count in all datasets (2012-2021)

is.null(df_yr_2012)
sum(is.na(df_yr_2012))

is.null(df_yr_2013)
sum(is.na(df_yr_2013))

is.null(df_yr_2014)
sum(is.na(df_yr_2014))

is.null(df_yr_2015)
sum(is.na(df_yr_2015))

is.null(df_yr_2016)
sum(is.na(df_yr_2016))

is.null(df_yr_2017)
sum(is.na(df_yr_2017))

is.null(df_yr_2018)
sum(is.na(df_yr_2018))

is.null(df_yr_2019)
sum(is.na(df_yr_2019))

is.null(df_yr_2020)
sum(is.na(df_yr_2020))

is.null(df_yr_2021)
sum(is.na(df_yr_2021))
summary(is.na(df_yr_2021$hcr_pm10))

# Hot spot hcr down all year 2021

# -----------------------------------------------------------------------------------------------------


