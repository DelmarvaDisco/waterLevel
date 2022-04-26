#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu) & James Maze (jtmaze@umd.edu)
#Date: 3/10/2022
#Purpose: Analysis of 20201015 Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Issues with this download
# What is the offset measurement for TS-SW (temporary well). Check JM field notes?
# Discrepancies between sensors and field measurements at TB-UW2, QB-UW1, ND-SW, NB-SW & DB-SW
# DB-SW, ND-SW (well pushed deeper), NB-SW (new casing) is not matching up between May 2020 and Oct 2020 downloads
# Extra filtering for TB-UW2
# DB-SW required a new offset, bc it was hung loosely in the well. Also, flagged data 8/16/2020-9/11/2020


#Table of Contents~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Organize workspace
# Step 2: Field Worksheet
# Step 3: Determine offset for each well
# Step 4: Baro Data
# Step 5: Water Depth
# Step 6: Apply the offset and QAQC
# Step 7: Export the data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup Workspace-------------------------------------------------------
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
files <- list.files(paste0(data_dir, "20201015_Downloads\\export"), full.names =  TRUE) 
pt_files <- files[!str_detect(files, "log")]
pt_files <- pt_files[!str_detect(pt_files, "Baro")]

#gather pt data
df <- pt_files %>% map_dfr(download_fun)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Field Worksheet--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Download Field Worksheet
field_logs <- read_csv(paste0(data_dir, "20201015_Downloads\\well_log.csv")) %>% 
#Get rid of scientific notation from field log
  mutate(Relative_water_level_m = format(Relative_water_level_m, scientific = FALSE)) %>% 
  mutate(Depth_to_water_m = format(Depth_to_water_m, scientific = FALSE))

#Check to make sure pt files match field logs
# check_fun(pt_files, field_logs)
# 
# rm(check_fun_errors)

#create df of site name, sonde_id, and measured water level
field_logs<-field_logs %>% 
  select(Site_Name, Sonde_ID, Relative_water_level_m, Well_head_m, Depth_to_water_m, Notes)

#join to master df
df<-df %>% left_join(., field_logs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Determine offset for each well ----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Some quick notes aobut our definition of offset
#   waterLevel = waterHeight + offset
#   offset = waterLevel - waterHeight

#Read offset file
offset<-read_csv("data/Database Information/offset.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Barometric Pressure Data ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download baro assignment [this may need to be changed manually]
baro_index<-read_csv("data//Database Information//baro_assignment.csv") 

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

#aply baro function to df
df<-df %>% 
  #Estimate baro pressure
  mutate(temp_qb = qb_baro_fun(Timestamp), 
         temp_gr = gr_baro_fun(Timestamp)) %>% 
  #Now select based on sonde
  mutate(pressureBaro = if_else(baro_logger == "GR Baro", temp_gr, temp_qb)) %>% 
  #remove unwanted rows
  select(-temp_gr, -temp_qb, -download_date)

rm(files, pt_files, baro_files, gr_baro, qb_baro, gr_baro_fun, qb_baro_fun, baro, baro_index)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: WaterHeight Calculation -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate waterHeight
df<-df %>% 
  mutate(
    pressureGauge = pressureAbsolute-pressureBaro, 
    waterHeight   = pressureGauge/9.81) %>% 
  select(-c(pressureAbsolute, pressureBaro, pressureGauge, Relative_water_level_m))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Calculate waterLevel using the offset & QAQC -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Make table to compare sensors to physically measured values 

checks <- tibble(Site_Name = c("na"), sensor_wtrlvl = c("na"))

#Read in the previous output table
dt <- read_csv("data/output_20200508_JM.csv") 
#Just incase the maker sure timestamps are in datetime format
dt <- dt %>% 
  mutate(Timestamp = ymd_hms(Timestamp, tz = "GMT"))

# 6.0 DB-UW1 ---------------------------------------------------------------------

#List the site in question
site <- "DB-UW1"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append processed data to the output table
output <- temp 

#Clean up environment  
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.1 DB-SW -------------------------------------------------------------------

#List the site in question
site <- "DB-SW"

#Find the site's offsets 
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
#!!! Needed to use offset version #2, because PT was hung loosely in well for this deployment.
  filter(Version_num == "Two") %>% 
  pull(offset) 

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  add_column(Flag = 1, 
             Notes = "PT not hanging properly in well. Used a different offset to correct. Especially problematic (8/5/2020 - 9/7/2020)")

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.05, max = 0.2)

#plot in dygraphs
temp2 <- dt %>% 
  filter(Site_Name == site)

temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.2 ND-UW1 ------------------------------------------------------------------

#List the site in question
site <- "ND-UW1"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.3 ND-UW2 ------------------------------------------------------------------
#List the site in question
site <- "ND-UW2"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.4 ND-SW -------------------------------------------------------------------

#List the site in question
site <- "ND-SW"

#Find the site's offsets 
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
temp2 <- dt %>% 
  filter(Site_Name == site)

temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.5 QB-UW1 ---------------------------------------------------------------------

#List the site in question
site <- "QB-UW1"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.6 QB-UW2 ------------------------------------------------------------------
#List the site in question
site <- "QB-UW2"

#Find the site's offsets 
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

#Extra filtering 
temp <- temp %>% 
  filter(Timestamp >= "2020-09-21 20:00:00" | Timestamp <= "2020-09-21 16:00:00")

#plot in dygraphs
# temp2 <- dt %>%
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>%
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.7 QB-SW ---------------------------------------------------------------
#List the site in question
site <- "QB-SW"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.8 TB-UW1 ---------------------------------------------------------------------
site <- "TB-UW1"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.9 TB-UW2 ------------------------------------------------------------------
site <- "TB-UW2"

#Find the site's offsets 
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
  add_column(Flag = 0, Notes = NA) %>% 
  #!!!Bad battery flag data late in deployment!!! 
  mutate(Flag = ifelse(Timestamp >= "2020-09-21 12:00:00", 
                       2, 
                       Flag),
         Notes = ifelse(Timestamp >= "2020-09-21 12:00:00", 
                        "Low battery noisy/inaccurate data", 
                        Notes))
  

#remove anomalous values. Needs a little extra filtering
temp <- temp %>% 
  filter(Timestamp <= "2020-08-24 14:15:00" | Timestamp >= "2020-08-24 20:15:00") %>% 
  filter(Timestamp <= "2020-10-19 17:30:00")

#anomalous fun
temp <- fun_anomalous(temp, min = -0.05, max = 0.2)

#plot in dygraphs
temp2 <- dt %>%
  filter(Site_Name == site)

temp_dy <- rbind(temp, temp2) %>%
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.10 TB-UW3 -------------------------------------------------------------
site <- "TB-UW3"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.11 TB-SW -------------------------------------------------------------------
site <- "TB-SW"

#Find the site's offsets 
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
temp2 <- dt %>% 
  filter(Site_Name == site)

temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.12 TA-SW --------------------------------------------------------------
site <- "TA-SW"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.13 TI-SW --------------------------------------------------------------------
site <- "TI-SW"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.14 DK-SW --------------------------------------------------------------------
site <- "DK-SW"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)


# 6.15 DK-UW1 -------------------------------------------------------------
site <- "DK-UW1"

#Find the site's offsets 
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
# temp2 <- dt %>%
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>%
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.16 DK-UW2 -------------------------------------------------------------
site <- "DK-UW2"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.17 DF-SW --------------------------------------------------------------
site <- "DF-SW"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.18 FN-SW -------------------------------------------------------------------
site <- "FN-SW"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.19 JA-SW --------------------------------------------------------------------
site <- "JA-SW"

#Find the site's offsets 
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
# temp2 <- dt %>%
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>%
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.20 JB-SW --------------------------------------------------------------------
site <- "JB-SW"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.21 JB-UW1 -------------------------------------------------------------
site <- "JB-UW1"

#Find the site's offsets 
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
temp2 <- dt %>% 
  filter(Site_Name == site)

temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.22 JB-UW2 ------------------------------------------------------------------
site <- "JB-UW2"

#Find the site's offsets 
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
temp2 <- dt %>% 
  filter(Site_Name == site)

temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.23 JC-SW --------------------------------------------------------------------
site <- "JC-SW"

#Find the site's offsets 
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
temp2 <- dt %>% 
  filter(Site_Name == site)

temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.24 JC-UW1 -------------------------------------------------------------
site <- "JC-UW1"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.25 NB-SW --------------------------------------------------------------------
site <- "NB-SW"

#Find the site's offsets 
offset_temp <- offset %>% 
  filter(Site_Name == site) 

#Inspect the notes and version number
(offset_temp)

#Filter based on the correct version number
offset_temp <- offset_temp %>% 
  filter(Version_num == "Three") %>% 
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
temp2 <- dt %>% 
  filter(Site_Name == site)

temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)


# 6.26 TP-CH --------------------------------------------------------------------

site <- "TP-CH"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)


# 6.27 Jones Road South Catchment Outlet -------------------------------------------------------------------

site <- "Jones Road South Catchment Outlet"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.28 Jones Road North Catchment Outlet --------------------------------------------------------------------

site <- "Jones Road North Catchment Outlet"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.29 Tiger Paw Catchment Outlet --------------------------------------------------------------------

site <- "Tiger Paw Catchment Outlet"

#Find the site's offsets 
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
# temp2 <- dt %>% 
#   filter(Site_Name == site)
# 
# temp_dy <- rbind(temp, temp2) %>% 
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
checks <- checks %>% add_row(Site_Name = site, sensor_wtrlvl = check)

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)


# 7.0 Aggregate and export ------------------------------------------------

#Plot some sites together. Maybe something looks weird
# select_sites <- output %>% filter(Site_Name %in% c("TB-UW1", "TB-UW2", "TB-UW3"))
# 
# select_sites_gg <- ggplot(data = select_sites, 
#                           mapping = aes(x = Timestamp,
#                                         y = waterLevel,
#                                         color = Site_Name)) +
#                    geom_line() +
#                    theme_bw()
# 
# (select_sites_gg)
# 
# rm(select_sites, select_sites_gg)

#Compare the PT measurements to the field log

checks <- unique(checks)

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

#Filter the sites with problematic 
problems <- checks %>% 
  select(c(measured_diff, Notes, Site_Name)) %>% 
  filter(measured_diff >= 0.05 | measured_diff <= -0.05)

#export 
output <- unique(output) %>% 
  #convert Timestamp to character, so write_csv doesn't screw up formating. 
  mutate(Timestamp = as.character(Timestamp))


write_csv(checks, paste0(data_dir, "checks_20201015_JM.csv"))

write_csv(output, paste0(data_dir, "output_20201015_JM.csv")) 

