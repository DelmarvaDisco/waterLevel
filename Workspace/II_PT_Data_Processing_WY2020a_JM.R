#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu) & James Maze (jtmaze@umd.edu)
#Date: 2/25/2021
#Purpose: Analysis of 20200508 Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#James started processing from here

#Issues with this download

# NB-SW well casing broke. Needed to change the offset halfway through.
# Extra filtering at a few sites (QB-UW2, JA-SW, DB-UW1)


#Table of Contents~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Organize workspace
# Step 2: Field Worksheet
# Step 3: Determine offset for each well
# Step 4: Baro Data
# Step 5: Water Depth
# Step 6: QAQC
# Step 7: Print

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup workspace -------------------------------------------------------
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
source("functions//fun_anomalous.R")

#Define data directory
data_dir<-"data\\"

#list pt, baro, and log file locations
files<-list.files(paste0(data_dir, "20200508_Downloads\\export"), full.names =  TRUE) 
  pt_files<-files[!str_detect(files, "log")]
  pt_files<-pt_files[!str_detect(pt_files, "Baro")]

#gather pt data
df<-pt_files %>% map_dfr(download_fun)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Field Worksheet --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download Field Worksheet
field_logs<-read_csv(paste0(data_dir, "20200508_Downloads\\well_log.csv"))

#create df of site name, sonde_id, and measured offset
field_logs<-field_logs %>% 
  select(Site_Name, Sonde_ID, Relative_water_level_m, Notes)

#Check to make sure pt files match field logs
#Commented out this step to speed up run time
#check_fun(pt_files, field_logs)
#rm(check_fun_errors)

#join to master df
df <- df %>% left_join(., field_logs) 
  
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Read the offset file for wells ----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Some quick notes about our definition of offset
#   waterLevel = waterHeight + offset
#   offset = waterLevel - waterHeight

#Read offset file
offset<-read_csv(paste0(data_dir,"Database Information\\offset.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Barometric Pressure Data ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download baro assignment [this may need to be changed manually]
baro_index<-read_csv(paste0(data_dir, "Database Information\\baro_assignment.csv"))

#Join baro index to df
df<-df %>% left_join(.,baro_index)

#download baro information
baro_files <- paste0(data_dir, "all_baros\\", list.files(path = paste0(data_dir, "all_baros")))
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
  select(-temp_gr, -temp_qb, -download_date)

rm(gr_baro, qb_baro, gr_baro_fun, qb_baro_fun, baro_files, baro, baro_index)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: WaterHeight Data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate waterHeight
df<-df %>% 
  mutate(
    pressureGauge = pressureAbsolute - pressureBaro, 
    waterHeight   = pressureGauge / 9.81) %>% 
  select(-c(pressureAbsolute, pressureBaro, pressureGauge, Relative_water_level_m))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Calculate waterLevel -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 6.0 Table comparing sensor's values to field measured values  ----------------------------------------------

checks <- tibble(Site_Name = c("na"), sensor_wtrlvl = c("na"))

# 6.1 DB-UW1 --------------------------------------------------

#List the site in question
site <- "DB-UW1"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>%
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.05, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

#Additional filtering 
temp <- temp %>% 
  filter(Timestamp <= "2020-01-16 16:30:00" | Timestamp >= "2020-01-16 21:00:00") %>% 
  filter(Timestamp <= "2020-03-10 14:30:00" | Timestamp >= "2020-03-10 20:00:00")

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

#Append processed data to the output table
output <- temp 

#Clean up environment  
rm(site, temp, temp_dy, check, offset_temp)

# 6.2 QB-UW1 --------------------------------------------------

#Define site 
site <- "QB-UW1"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "Two") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>%
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)


#remove anomalous values
temp <- fun_anomalous(temp, min = -0.05, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))


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

#Append to the output file
output <- output %>% 
  add_row(temp)

#Clean up environment 
rm(site, temp, temp_dy, check, offset_temp)

# 6.3 DB-SW ---------------------------------------------------------
#List the site in question
site <- "DB-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.05, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.4 ND-UW1  ---------------------------------------------------------

site <- "ND-UW1"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.05, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.5 ND-UW2  ---------------------------------------------------------
site <- "ND-UW2"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>%
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.05, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.6 ND-SW ---------------------------------------------------------

site <- "ND-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.05, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.7 QB-UW2 ------------------------------------------------------------

site <- "QB-UW2"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.04, max = 0.2)

#Need to do a little extra filtering on this site
temp <- temp %>% 
  filter(Timestamp <= "2020-03-09 18:30:00" | Timestamp >= "2020-03-09 22:30:00")


#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.9 QB-SW ------------------------------------------------------------
site <- "QB-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.10 TB-UW1 ------------------------------------------------------------

site <- "TB-UW1"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.11 TB-UW2 ------------------------------------------------------------

site <- "TB-UW2"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.12 TB-UW3 ------------------------------------------------------------

site <- "TB-UW3"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.13 TB-SW ------------------------------------------------------------

site <- "TB-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.14 TA-SW ------------------------------------------------------------

site <- "TA-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.15 TI-SW ------------------------------------------------------------

site <- "TI-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.16 DK-SW ------------------------------------------------------------

site <- "DK-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.17 DK-UW1 ------------------------------------------------------------

site <- "DK-UW1"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.18 DK-UW2 ------------------------------------------------------------

site <- "DK-UW2"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.19 DF-SW -------------------------------------------------------------------

site <- "DF-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.20 FN-SW --------------------------------------------------------------

site <- "FN-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.21 JA-SW --------------------------------------------------------------

site <- "JA-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#Needed to extra filtering
temp <- temp %>% 
  filter(Timestamp <= "2020-03-01 12:15:00" | Timestamp >= "2020-03-03 3:15:00") %>% 
  filter(Timestamp <= "2020-04-17 12:15:00" | Timestamp >= "2020-04-17 19:00:00") %>% 
  filter(Timestamp <= "2020-04-19 8:15:00" | Timestamp >= "2020-04-19 20:15:00") %>% 
  filter(Timestamp <= "2020-05-10 3:15:00")

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.22 JB-SW --------------------------------------------------------------

site <- "JB-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.23 JB-UW1 -------------------------------------------------------------

site <- "JB-UW1"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.24 JB-UW2 -------------------------------------------------------------

site <- "JB-UW2"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.25 JC-SW --------------------------------------------------------------

site <- "JC-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.26 JC-UW1 -------------------------------------------------------------

site <- "JC-UW1"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.27 NB-SW --------------------------------------------------------------

site <- "NB-SW"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "Two") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
#Flags and ntoes for this download
  add_column(Flag = 1, Notes = "Well ripped up. PT just laying on wetland bottom.")

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#A little extra filtering
temp <- temp %>% 
  filter(Timestamp >= "2019-12-8 16:15:00" | Timestamp <= "2019-12-8 4:15:00") %>% 
  filter(Timestamp <= "2019-12-28 6:15:00" | Timestamp >= "2019-12-29 1:15:00") %>% 
  filter(Timestamp <= "2020-1-16 19:45:00" | Timestamp >= "2020-1-16 23:30:00")
  
#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)


# 6.28 TP-CH -------------------------------------------------------------------
site <- "TP-CH"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.29 Jones Road South Catchment Outlet -----------------------------------------

site <- "Jones Road South Catchment Outlet"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.30 Jones Road North Catchment Outlet -----------------------------------------

site <- "Jones Road North Catchment Outlet"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 6.31 Tiger Paw Catchment Outlet ----------------------------------------------

site <- "Tiger Paw Catchment Outlet"

#Find the offsets for a given site
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "One") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 0, Notes = NA)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.03, max = 0.2)

#plot in dygraphs
# temp_dy <- temp %>% 
#   mutate(waterLevel = waterLevel + 100)
# 
# dygraph_ts_fun(temp_dy %>% select(Timestamp, waterLevel))

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

#Append to output file
output <- output %>% 
  add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp)

# 7. Tidy things up before export -----------------------------------------

checks <- unique(checks)

#Plot all the sites together. Maybe something looks weird

# all_sites <- ggplot(data = output, 
#                     mapping = aes(x = Timestamp,
#                                   y = waterLevel,
#                                   color = Site_Name)) +
#   geom_line() +
#   theme_bw()
# 
# (all_sites)
# 
# rm(all_sites)

#Compare the PT measurements to the field log
checks <- checks %>% 
  left_join(., field_logs) 

#Calculate the difference between sensor and field measurements
checks <- checks %>% 
  filter(!Site_Name == "na") %>% 
  mutate(measured_diff = as.numeric(sensor_wtrlvl) - as.numeric(Relative_water_level_m)) 

#Dot plot showing distributions of the measurement discrepencies
checks_plot <- ggplot(data = checks,
                       mapping = aes(x = measured_diff)) +
  theme_bw() +
  geom_dotplot()

(checks_plot)

#Filter the sites with problematic offsets
problems <- checks %>% 
  select(c(measured_diff, Notes, Site_Name)) %>% 
  filter(measured_diff >= 0.05 | measured_diff <= -0.05)

#Write the checks file (for now)
write_csv(checks, paste0(data_dir,"checks_20200508_JM.csv"))

#Remove duplicates from the output file
output <- unique(output) %>% 
  #convert Timestamp to character, so write_csv doesn't screw up formating. 
  mutate(Timestamp = as.character(Timestamp))

#export 
write_csv(output, paste0(data_dir,"output_20200508_JM.csv"))


