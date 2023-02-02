# This R Script is to view dataset prior to deep comparative analysis process

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
require(data.table)
require(lubridate)

# --------------------------------------------------------------------------------------------------------

# Data Source - PULLED from individual HotSpot R Scripts

# ============================
# Hotspot Hounslow Cranford

comb_df_hcr

# Arranging data class into the right format/class

# Checking current class of date col
class(comb_df_hcr$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_hcr$end_date <- as.Date(comb_df_hcr$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hcr$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hcr$end_time)

# Declaring date and time objects
date_obj <- comb_df_hcr$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hcr$end_time
time_obj <- as.difftime(time_obj)

comp_df_hcr <- comb_df_hcr %>% 
  select(1:10)

# Changing Characters to Numeric Variables
comp_df_hcr$hcr_pm10 <- as.numeric(comp_df_hcr$hcr_pm10)
comp_df_hcr$hcr_no <- as.numeric(comp_df_hcr$hcr_no)
comp_df_hcr$hcr_no2 <- as.numeric(comp_df_hcr$hcr_no2)
comp_df_hcr$hcr_noxasno2 <- as.numeric(comp_df_hcr$hcr_noxasno2)
comp_df_hcr$hcr_nv10 <- as.numeric(comp_df_hcr$hcr_nv10)
comp_df_hcr$hcr_o3 <- as.numeric(comp_df_hcr$hcr_o3)
comp_df_hcr$hcr_so2 <- as.numeric(comp_df_hcr$hcr_so2)
comp_df_hcr$hcr_v10 <- as.numeric(comp_df_hcr$hcr_v10)

comp_df_hcr

# ============================
# Hotspot Hounslow Chiswick
comb_df_hch

# Arranging data class into the right format/class

# Checking current class of date col
class(comb_df_hch$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_$end_date <- as.Date(comb_df_hch$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hch$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hch$end_time)

# Declaring date and time objects
date_obj <- comb_df_hch$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hch$end_time
time_obj <- as.difftime(time_obj)

comp_df_hch <- comb_df_hch %>% 
  select(1:9)
comp_df_hch

# Changing Characters to Numeric Variables
comp_df_hch$hch_pm10 <- as.numeric(comp_df_hch$hch_pm10)
comp_df_hch$hch_no <- as.numeric(comp_df_hch$hch_no)
comp_df_hch$hch_no2 <- as.numeric(comp_df_hch$hch_no2)
comp_df_hch$hch_noxasno2 <- as.numeric(comp_df_hch$hch_noxasno2)
comp_df_hch$hch_nv10 <- as.numeric(comp_df_hch$hch_nv10)
comp_df_hch$hch_v10 <- as.numeric(comp_df_hch$hch_v10)
comp_df_hch$hch_pm25 <- as.numeric(comp_df_hch$hch_pm25)

comp_df_hch

# ============================
# Hotspot Hounslow Brentford

comb_df_hbr

# Arranging data class into the right format/class

# Checking current class of date col
class(comb_df_hbr$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_$end_date <- as.Date(comb_df_hbr$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hbr$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hbr$end_time)

# Declaring date and time objects
date_obj <- comb_df_hbr$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hbr$end_time
time_obj <- as.difftime(time_obj)

comp_df_hbr <- comb_df_hbr %>% 
  select(1:9)
comp_df_hbr

# Changing Characters to Numeric Variables
comp_df_hbr$hbr_pm10 <- as.numeric(comp_df_hbr$hbr_pm10)
comp_df_hbr$hbr_no <- as.numeric(comp_df_hbr$hbr_no)
comp_df_hbr$hbr_no2 <- as.numeric(comp_df_hbr$hbr_no2)
comp_df_hbr$hbr_noxasno2 <- as.numeric(comp_df_hbr$hbr_noxasno2)
comp_df_hbr$hbr_nv10 <- as.numeric(comp_df_hbr$hbr_nv10)
comp_df_hbr$hbr_v10 <- as.numeric(comp_df_hbr$hbr_v10)
comp_df_hbr$hbr_pm25 <- as.numeric(comp_df_hbr$hbr_pm25)

comp_df_hbr

# ============================
# Hotspot Hounslow Heston

comb_df_hhe

# Arranging data class into the right format/class

# Checking current class of date col
class(comb_df_hhe$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_$end_date <- as.Date(comb_df_hhe$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hhe$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hhe$end_time)

# Declaring date and time objects
date_obj <- comb_df_hhe$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hhe$end_time
time_obj <- as.difftime(time_obj)

comp_df_hhe <- comb_df_hhe %>% 
  select(1:6)
comp_df_hhe

# Changing Characters to Numeric Variables
comp_df_hhe$hhe_pm10 <- as.numeric(comp_df_hhe$hhe_pm10)
comp_df_hhe$hhe_no <- as.numeric(comp_df_hhe$hhe_no)
comp_df_hhe$hhe_no2 <- as.numeric(comp_df_hhe$hhe_no2)
comp_df_hhe$hhe_noxasno2 <- as.numeric(comp_df_hhe$hhe_noxasno2)

comp_df_hhe

# ============================
# Hotspot Hounslow Hatton Cross

comb_df_hha

# Arranging data class into the right format/class

# Checking current class of date col
class(comb_df_hha$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_$end_date <- as.Date(comb_df_hha$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hha$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hha$end_time)

# Declaring date and time objects
date_obj <- comb_df_hha$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hha$end_time
time_obj <- as.difftime(time_obj)

comp_df_hha <- comb_df_hha %>% 
  select(1:6)
comp_df_hha

# Changing Characters to Numeric Variables
comp_df_hha$hha_pm10 <- as.numeric(comp_df_hha$hha_pm10)
comp_df_hha$hha_no <- as.numeric(comp_df_hha$hha_no)
comp_df_hha$hha_no2 <- as.numeric(comp_df_hha$hha_no2)
comp_df_hha$hha_noxasno2 <- as.numeric(comp_df_hha$hha_noxasno2)

comp_df_hha

# ============================
# Hotspot Hounslow Gunnersbury
comb_df_hgu

# Arranging data class into the right format/class

# Checking current class of date col
class(comb_df_hgu$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_$end_date <- as.Date(comb_df_hgu$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hgu$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hgu$end_time)

# Declaring date and time objects
date_obj <- comb_df_hgu$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hgu$end_time
time_obj <- as.difftime(time_obj)

comp_df_hgu <- comb_df_hgu %>% 
  select(1:6)
comp_df_hgu

# Changing Characters to Numeric Variables
comp_df_hgu$hgu_pm10 <- as.numeric(comp_df_hgu$hgu_pm10)
comp_df_hgu$hgu_no <- as.numeric(comp_df_hgu$hgu_no)
comp_df_hgu$hgu_no2 <- as.numeric(comp_df_hgu$hgu_no2)
comp_df_hgu$hgu_noxasno2 <- as.numeric(comp_df_hgu$hgu_noxasno2)

comp_df_hgu

# ============================
# Hotspot Hounslow Feltham
comb_df_hfe

# Arranging data class into the right format/class

# Checking current class of date col
class(comb_df_hfe$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_$end_date <- as.Date(comb_df_hfe$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hfe$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hfe$end_time)

# Declaring date and time objects
date_obj <- comb_df_hfe$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hfe$end_time
time_obj <- as.difftime(time_obj)

comp_df_hfe <- comb_df_hfe %>% 
  select(1:6)
comp_df_hfe

# Changing Characters to Numeric Variables
comp_df_hfe$hfe_pm10 <- as.numeric(comp_df_hfe$hfe_pm10)
comp_df_hfe$hfe_no <- as.numeric(comp_df_hfe$hfe_no)
comp_df_hfe$hfe_no2 <- as.numeric(comp_df_hfe$hfe_no2)
comp_df_hfe$hfe_noxasno2 <- as.numeric(comp_df_hfe$hfe_noxasno2)

comp_df_hfe

# ============================
# Hotspot Hounslow Boston Manor Park [Closed]
comb_df_hbo

# Arranging data class into the right format/class

# Checking current class of date col
class(comb_df_hbo$end_date)

# Mutating to data class with the right yearly format YYYY-MM-DD
comb_df_$end_date <- as.Date(comb_df_hbo$end_date, format = "%d/%m/%Y")

# Confirming col class after adjustments
class(comb_df_hbo$end_date)

# Confirming col time class which is correct HH:MM:SS
class(comb_df_hbo$end_time)

# Declaring date and time objects
date_obj <- comb_df_hbo$end_date
date_obj <- as.Date(date_obj)

time_obj <- comb_df_hbo$end_time
time_obj <- as.difftime(time_obj)

comp_df_hbo <- comb_df_hbo %>% 
  select(1:6)
comp_df_hbo

# Changing Characters to Numeric Variables
comp_df_hbo$hbo_pm10 <- as.numeric(comp_df_hbo$hbo_pm10)
comp_df_hbo$hbo_no <- as.numeric(comp_df_hbo$hbo_no)
comp_df_hbo$hbo_no2 <- as.numeric(comp_df_hbo$hbo_no2)
comp_df_hbo$hbo_noxasno2 <- as.numeric(comp_df_hbo$hbo_noxasno2)

comp_df_hbo

# ------------------------------------------------------------------------------------------------------

# Quick Data Export for ML Processing

readr::write_csv(comp_df_hcr, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/comp_df_hcr.csv")
readr::write_csv(comp_df_hch, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/comp_df_hch.csv")
readr::write_csv(comp_df_hbr, path = "/dsq_data_projects/aq_project/EH_project_work/datasets/comp_df_hbr.csv")

# ------------------------------------------------------------------------------------------------------

# Deep Analysis of Missing Values (No Data) per Hotspots ()

comp_df_hcr # Data set for HCR
summary(comp_df_hcr) # Summary analysis for HCR

missing_values_hcr <- comp_df_hcr %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_hcr

missing_values_hcr %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "#800080") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow CRANFORD AQ HOTSPOT 2012-2021') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "#800080", face = "bold"))

# ------------------------------------------------------------------------------------------------------

# Deep Analysis of Missing Values (No Data) per Hotspots ()

comp_df_hch # Data set for hch
summary(comp_df_hch) # Summary analysis for hch

missing_values_hch <- comp_df_hch %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_hch

missing_values_hch %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "blue") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow CHISWICK AQ HOTSPOT 2012-2021') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "blue", face = "bold"))
  
missing_values_hch

# ------------------------------------------------------------------------------------------------------

# Deep Analysis of Missing Values (No Data) per Hotspots ()

comp_df_hbr # Data set for hbr
summary(comp_df_hbr) # Summary analysis for hbr

missing_values_hbr <- comp_df_hbr %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_hbr

missing_values_hbr %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "dark green") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow BRENTFORD AQ HOTSPOT 2012-2021') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "dark green", face = "bold"))

missing_values_hbr

# ------------------------------------------------------------------------------------------------------

# Deep Analysis of Missing Values (No Data) per Hotspots ()

comp_df_hhe # Data set for hhe
summary(comp_df_hhe) # Summary analysis for hhe

missing_values_hhe <- comp_df_hhe %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_hhe

missing_values_hhe %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "black") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow HESTON AQ HOTSPOT 2012-2021') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "black", face = "bold"))

missing_values_hhe 

# ------------------------------------------------------------------------------------------------------

# Deep Analysis of Missing Values (No Data) per Hotspots ()

comp_df_hha # Data set for hha
summary(comp_df_hha) # Summary analysis for hha

missing_values_hha <- comp_df_hha %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_hha

missing_values_hha %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "dark grey") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow HATTON CROSS AQ HOTSPOT 2012-2021') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "black", face = "bold"))

missing_values_hha

# ------------------------------------------------------------------------------------------------------

# Deep Analysis of Missing Values (No Data) per Hotspots ()

comp_df_hfe # Data set for hgu
summary(comp_df_hgu) # Summary analysis for hgu

missing_values_hgu <- comp_df_hgu %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_hgu

missing_values_hgu %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "orange") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow GUNNERSBURY AQ HOTSPOT 2012-2021') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "orange", face = "bold"))

missing_values_hgu

# ------------------------------------------------------------------------------------------------------

# Deep Analysis of Missing Values (No Data) per Hotspots ()

comp_df_hfe # Data set for hfe
summary(comp_df_hfe) # Summary analysis for hfe

missing_values_hfe <- comp_df_hfe %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_hfe

missing_values_hfe %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "light blue") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow FELTHAM AQ HOTSPOT 2012-2021') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "black", face = "bold"))

missing_values_hfe

# ------------------------------------------------------------------------------------------------------

# Deep Analysis of Missing Values (No Data) per Hotspots ()

comp_df_hbo # Data set for hbo
summary(comp_df_hbo) # Summary analysis for hbo

missing_values_hbo <- comp_df_hbo %>% # Missing Values as Numbers
  gather(key = "key", value = "val") %>% 
  mutate(is.missing = is.na(val)) %>%
  group_by(key, is.missing) %>%
  summarise(num.missing = n()) %>%
  filter(is.missing==T) %>%
  select(-is.missing) %>%
  arrange(desc(num.missing))

missing_values_hbo

missing_values_hbo %>% 
  ggplot() +
  geom_bar(aes(x=key, y=num.missing), stat = 'identity', fill = "brown") +
  labs(x='variable', y="number of missing values", 
       title='Number of Missing values - Hounslow BOSTON MANOR PARK AQ HOTSPOT 2012-2021') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, color = "#000000", face = "bold")) +
  theme(axis.text.y = element_text(hjust = 1, color = "#000000", face = "bold")) +
  theme(title = element_text(colour = "brown", face = "bold"))

missing_values_hbo

# ------------------------------------------------------------------------------------------------------

# Deep Analysis of HotSpots (IoT Devices) Performance based on 'No Data'

#HBO ---------------------------
col_hbo <- data.frame(comp_df_hbo$hbo_pm10,comp_df_hbo$hbo_no,comp_df_hbo$hbo_no2,comp_df_hbo$hbo_noxasno2)
hbo_col_mean <- (colMeans(is.na(col_hbo)))*100
hbo_col_mean
mean(hbo_col_mean) # 87.68% data is missing

#HCH ---------------------------
col_hch <- data.frame(comp_df_hch$hch_pm10,comp_df_hch$hch_no,comp_df_hch$hch_no2,comp_df_hch$hch_noxasno2,comp_df_hch$hch_pm25)
hch_col_mean <- (colMeans(is.na(col_hch)))*100
hch_col_mean
mean(hch_col_mean) # 14.49% data is missing

#HCR ---------------------------
col_hcr <- data.frame(comp_df_hcr$hcr_pm10,comp_df_hcr$hcr_no,comp_df_hcr$hcr_no2,comp_df_hcr$hcr_noxasno2,comp_df_hcr$hcr_o3,comp_df_hcr$hcr_so2)
hcr_col_mean <- (colMeans(is.na(col_hcr)))*100
hcr_col_mean
mean(hcr_col_mean) # 27.85% data is missing

#HBR ---------------------------
col_hbr <- data.frame(comp_df_hbr$hbr_pm10,comp_df_hbr$hbr_no,comp_df_hbr$hbr_no2,comp_df_hbr$hbr_noxasno2,comp_df_hbr$hbr_pm25)
hbr_col_mean <- (colMeans(is.na(col_hbr)))*100
hbr_col_mean
mean(hbr_col_mean) # 15.37% data is missing

#HHE ---------------------------
col_hhe <- data.frame(comp_df_hhe$hhe_pm10,comp_df_hhe$hhe_no,comp_df_hhe$hhe_no2,comp_df_hhe$hhe_noxasno2)
hhe_col_mean <- (colMeans(is.na(col_hhe)))*100
hhe_col_mean
mean(hhe_col_mean) # 9.16% data is missing

#HHA ---------------------------
col_hha <- data.frame(comp_df_hha$hha_pm10,comp_df_hha$hha_no,comp_df_hha$hha_no2,comp_df_hha$hha_noxasno2)
hha_col_mean <- (colMeans(is.na(col_hha)))*100
hha_col_mean
mean(hha_col_mean) # 11.59% data is missing

#HFE ---------------------------
col_hfe <- data.frame(comp_df_hfe$hfe_pm10,comp_df_hfe$hfe_no,comp_df_hfe$hfe_no2,comp_df_hfe$hfe_noxasno2)
hfe_col_mean <- (colMeans(is.na(col_hfe)))*100
hfe_col_mean
mean(hfe_col_mean) # 6.45% data is missing

#HGU ---------------------------
col_hgu <- data.frame(comp_df_hgu$hgu_pm10,comp_df_hgu$hgu_no,comp_df_hgu$hgu_no2,comp_df_hgu$hgu_noxasno2)
hgu_col_mean <- (colMeans(is.na(col_hgu)))*100
hgu_col_mean
mean(hgu_col_mean) # 8.05% data is missing

#HBO TEST ---------------------------
col_hhe <- data.frame(comp_df_hhe$hhe_pm10,comp_df_hhe$hhe_no,comp_df_hhe$hhe_no2,comp_df_hhe$hhe_noxasno2)
totalcells = prod(comp_df_hhe$hhe_pm10,comp_df_hhe$hhe_no,comp_df_hhe$hhe_no2,comp_df_hhe$hhe_noxasno2)
missingcells = sum(is.na(totalcells))
percentage = (missingcells*100)/(totalcells)
print(percentage)
hhe_col_mean <- sum(is.na(col_hhe))
hhe_col_mean
mean(hhe_col_mean) 

dim(comp_df_hhe)
summary(comp_df_hhe)

# ------------------------------------------------------------------------------------------------------

# Summary of National Air Quality Standards and Objectives
# Pollutant	Standard / Objective (UK)	Averaging Period	Date(1)
# Nitrogen dioxide (NO2)	200 ??g m-3 not to be exceeded more than 18 times a year	1-hour mean	31 Dec 2005
# Nitrogen dioxide (NO2)	40 ??g m-3	Annual mean	31 Dec 2005
# Particles (PM10)	50 ??g m-3 not to be exceeded more than 35 times a year	24-hour mean	31 Dec 2004
# Particles (PM10)	40 ??g m-3	Annual mean	31 Dec 2004
# Particles (PM2.5)	25 ??g m-3	Annual mean	2020
# Particles (PM2.5)	Target of 15% reduction in concentration at urban background locations	3-year mean	Between 2010 and 2020
# Sulphur dioxide (SO2)	266 ??g m-3 not to be exceeded more than 35 times a year	15-minute mean	31 Dec 2005
# Sulphur dioxide (SO2)	350 ??g m-3 not to be exceeded more than 24 times a year	1-hour mean	31 Dec 2004
# Sulphur dioxide (SO2)	125 ??g m-3 mot to be exceeded more than 3 times a year	24-hour mean	31 Dec 2004

# 2021 WHO Guideline
# Pollutant	WHO Guideline
# Averaging Period	Date
# Nitrogen dioxide (NO2)	10 ??g/m3	annual mean	2021
# Nitrogen dioxide (NO2)	25 ??g/m3	24-hour mean	2021
# Particles (PM10)	15 ??g/m3	annual mean	2021
# Particles (PM10)	45 ??g/m3 	24-hour mean	2021
# Particles (PM2.5)	5 ??g/m3  	annual mean	2021
# Particles (PM2.5)	15 ??g/m3 	24-hour mean	2021
  # Analysis 1: (PM10) in comparative with UK Standards and WHO Standards

# Annual Mean Values

# Hourly Mean Values




# ------------------------------------------------------------------------------------------------------




#--------------------------------------------------------------------------------------------------------------------------------------
# All Comparative Analysis based on HotSpots - Task Accomplished
#--------------------------------------------------------------------------------------------------------------------------------------
