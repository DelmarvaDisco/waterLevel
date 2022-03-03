#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 2/25/2021
#Purpose: Analysis of 20200508 Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#James started processing from here

#Issues with this download
# - Missing export for QB-UW1, no serial number on the field sheet


#Table of Contents~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Organize workspace
# Step 2: Field Worksheet
# Step 3: Determine offset for each well
# Step 4: Baro Data
# Step 5: Water Depth
# Step 6: QAQC
# Step 7: Print

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup workspace-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Clear workspace 
remove(list=ls())

#Gather libraries of interest
library(dygraphs)
library(xts)
library(lubridate)
library(zoo)
library(tidyverse)
library(stringr)
library(readxl)

#Read custom R functions
source("functions//file_fun.R")
source("functions//dygraph_ts_fun.R")
source("functions//download_fun.R")
source("functions//db_get_ts.R")

#Define data directory
data_dir<-"data\\"

#list pt, baro, and log file locations
files<-list.files(paste0(data_dir, "20200508_Downloads\\export"), full.names =  TRUE) 
  pt_files<-files[!str_detect(files, "log")]
  pt_files<-pt_files[!str_detect(pt_files, "Baro")]
field_logs<-paste0(data_dir, 'well_log.csv')

#gather pt data
df<-pt_files %>% map_dfr(download_fun)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Field Worksheet--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download Field Worksheet
field_logs<-read_csv(paste0(data_dir, "20200508_Downloads\\well_log.csv"))

#create df of site name, sonde_id, and measured offset
field_logs<-field_logs %>% 
  select(Site_Name, Sonde_ID, Relative_Water_Level_m)

#Check to make sure pt files match field logs
check_fun(pt_files, field_logs)
rm(check_fun_errors)

#join to master df
df <- df %>% left_join(., field_logs) 
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Read the offset file for wells ----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Some quick notes aobut our definition of offset
#   waterLevel = waterHeight + offset
#   offset = waterLevel - waterHeight

#Read offset file
offset<-read_csv(paste0(data_dir,"Database Information\\offset.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Barometric Pressure Data----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download baro assignment [this may need to be changed manually]
baro_index<-read_csv(paste0(data_dir, "Database Information\\baro_assignment.csv"))

#Join baro index to df
df<-df %>% left_join(.,baro_index)

#download baro information
baro_files<- files %>% as_tibble() %>%  filter(str_detect(value,"Baro")) %>% as_vector()
baro<-lapply(baro_files, download_fun) %>% bind_rows()

#Assign Baro logger to each row
baro<-baro %>% mutate(baro_logger = ifelse(Sonde_ID==10589038, "QB Baro", "GR Baro"))

#Create interpolation functions 
qb_baro<-baro %>% filter(baro_logger == "QB Baro")
qb_baro_fun<-approxfun(qb_baro$Timestamp, qb_baro$pressureAbsolute)
gr_baro<-baro %>% filter(baro_logger == "GR Baro")
gr_baro_fun<-approxfun(gr_baro$Timestamp, gr_baro$pressureAbsolute)

#aplly baro function to df
df<-df %>% 
  #Estimate baro pressure
  mutate(temp_qb = qb_baro_fun(Timestamp), 
         temp_gr = gr_baro_fun(Timestamp)) %>% 
  #Now select based on sonde
  mutate(pressureBaro = if_else(baro_logger == "GR Baro", temp_gr, temp_qb)) %>% 
  #remove unwanted rows
  select(-temp_gr, -temp_qb)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: WaterHeight Data-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate waterHeight
df<-df %>% 
  mutate(
    pressureGauge = pressureAbsolute-pressureBaro, 
    waterHeight   = pressureGauge/9.81)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Calculate waterLevel -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 6.0 Compare offsets to measured values?  ----------------------------------------------

checks <- tibble(Site_Name = c("na"), sensor_wtrlvl = c("na"))


# 6.1 DB-UW1 --------------------------------------------------

#List the site in question
site <- "DB-UW1"

#Find the offset
offset_temp <- offset %>% 
  filter(Site_Name == site) %>% 
  pull(offset)

#Estimate water level
temp<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, level)


#plot in dygraphs
temp_dy <- temp %>% 
  mutate(waterLevel = waterLevel + 100)

dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

#Extract the last measured water level to check against field sheet
check <- temp %>% 
  arrange(desc(Timestamp)) %>% 
  slice(1:10) %>% 
  pull(waterLevel) %>% 
  mean() %>% 
  as.character()

#Add to the check table
checks <- checks %>% 
  add_row(Site_Name = site, sensor_wtrlvl = check)
  
rm(site, temp, temp_dy, check, offset_temp)

# 6.2 QB-UW1 --------------------------------------------------

#Define site 
site <- "QB-UW1"

#Find the offset
offset_temp <- offset %>% 
  filter(Site_Name == site) %>% 
  pull(offset)

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  filter(!is.na(waterLevel)) %>%
  select(Timestamp, waterLevel, level)

#plot in dygraphs
temp_dy <- temp %>% 
  mutate(waterLevel = waterLevel + 100)

dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))


#Extract the last measured water level to check against field sheet
check <- temp %>% 
  arrange(desc(Timestamp)) %>% 
  slice(1:10) %>% 
  pull(waterLevel) %>% 
  mean() %>% 
  as.character()

#Add to the check table
checks <- checks %>% 
  add_row(Site_Name = site, sensor_wtrlvl = check)

rm(site, temp, temp_dy, check, offset_temp)

# 6.3 DB-SW ---------------------------------------------------------
#List the site in question
site <- "DB-SW"

#Find the offset
offset_temp <- offset %>% 
  filter(Site_Name == site) %>% 
  pull(offset)

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, level)

#plot in dygraphs
temp_dy <- temp %>% 
  mutate(waterLevel = waterLevel + 100)

dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

#Extract the last measured water level to check against field sheet
check <- temp %>% 
  arrange(desc(Timestamp)) %>% 
  slice(1:10) %>% 
  pull(waterLevel) %>% 
  mean() %>% 
  as.character()

#Add to the check table
checks <- checks %>% 
  add_row(Site_Name = site, sensor_wtrlvl = check)

rm(site, temp, temp_dy, check, offset_temp)

# 6.4 ND-UW1  ---------------------------------------------------------

# 6.5 ND-UW2  ---------------------------------------------------------

# 6.6 ND-SW ---------------------------------------------------------

# 6.7 QB-UW2 ------------------------------------------------------------

# 6.9 QB-SW ------------------------------------------------------------

# 6.10 TB-UW1 ------------------------------------------------------------

# 6.11 TB-UW2 ------------------------------------------------------------

# 6.12 TB-UW3 ------------------------------------------------------------

# 6.13 TB-SW ------------------------------------------------------------

# 6.14 TA-SW ------------------------------------------------------------

# 6.15 TI-SW ------------------------------------------------------------

# 6.16 DK-SW ------------------------------------------------------------

# 6.17 DK-UW1 ------------------------------------------------------------

# 6.18 DK-UW2 ------------------------------------------------------------

# 6.19  DF-SW -------------------------------------------------------------------

# 6.20 FN-SW --------------------------------------------------------------

# 6.21 JA-SW --------------------------------------------------------------

# 6.22 JB-SW --------------------------------------------------------------

# 6.23 JB-UW1 -------------------------------------------------------------

# 6.24 JB-UW2 -------------------------------------------------------------

# 6.25 JC-SW --------------------------------------------------------------

# 6.26 JC-UW1 -------------------------------------------------------------

# 6.27 NB-SW --------------------------------------------------------------



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 7: Apply offset and export ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#export 
write_csv(df,paste0(data_dir,"output.csv"))