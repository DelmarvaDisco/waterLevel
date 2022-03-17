#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: James Maze (jtmaze@umd.edu) & Nate Jones (cnjones7@ua.edu)
#Date: 12/1/2021
#Purpose: Analysis of 20201015 Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Notes on this download

#  - DB-SW serial number did not read in when using the download function. 
#    The column name was weird ("Abs Pres, kPa (LGR S/N: 10258771, SEN S/N: 10258771, LBL: PSI)")
#    Since the SN had "LBL : PSI "after it, download_fun broke. Manually assigned SN to the site
#    in Step #2
#  - 

#Steps
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

#data directory
data_dir <- "data//20210525_Downloads//"

#list pt, baro, and log file locations
files<-list.files(paste0(data_dir, "export"), full.names =  TRUE) 
pt_files<-files[!str_detect(files, "log")]
pt_files<-files[!str_detect(files, "Baro")]

#gather pt data
df<-files %>% map_dfr(download_fun) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Field worksheets -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download Field Worksheet
field_logs <- read_csv(paste0(data_dir, "well_log.csv")) 


#Check to make sure pt files match field logs
check_fun(pt_files, field_logs)

rm(check_fun_errors)

#create df of site name, sonde_id, and measured water level & Notes
field_logs<-field_logs %>% 
  select(Site_Name, Sonde_ID, Relative_water_level_m, Notes) %>% 
  mutate(Relative_water_level_m = as.numeric(Relative_water_level_m))

#Join field log to master df. First, make Sonde_ID a character
df$Sonde_ID <- as.character(df$Sonde_ID)
field_logs$Sonde_ID <- as.character(field_logs$Sonde_ID)

df<-df %>% left_join(., field_logs)
 
#!!! download_fun didn't recognize DB-SW's SN. Wonky column name. Manual fix here. 
df <- df %>% 
  mutate(Sonde_ID = if_else(is.na(Sonde_ID), "10258771", Sonde_ID)) %>% 
  mutate(Site_Name = if_else(is.na(Site_Name), "DB-SW", Site_Name))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Well offsets -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Some quick notes aobut our definition of offset
#   waterLevel = waterHeight + offset
#   offset = waterLevel - waterHeight

#Read offset file
offset<-read_csv("data/Database Information/offset.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Baro data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

baro_index<-read_csv("data//Database Information//baro_assignment.csv") 

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
# Step 5: Calculate water depth -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate waterHeight
df<-df %>% 
  mutate(
    pressureGauge = pressureAbsolute-pressureBaro, 
    waterHeight   = pressureGauge/9.81) %>% 
  select(-c(pressureAbsolute, pressureBaro, pressureGauge, Relative_water_level_m))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Apply offset and QAQC -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create a checks table comparing sensors to field measurements
checks <- tibble(Site_Name = c("na"), sensor_wtrlvl = c("na"))

#Read in the previous output tables

dx <- read_csv("data/output_20200508_JM.csv")

dy <- read_csv("data/output_20201015_JM.csv")

dt <- bind_rows(dx, dy)

rm(dx, dy)

# 6.0 DB-UW1 ---------------------------------------------------------------
site <- "DB-UW1"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>% pull(offset)

#Estimate water level
temp<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name)

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

#Append processed data to the output table
output <- temp 

#Clean up environment  
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.1 DB-SW ---------------------------------------------------------------
site <- "DB-SW"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>% pull(offset)

#Estimate water level
temp<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name)

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

#Append processed data to the output table
output <- output %>% add_row(temp)

#Clean up environment  
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.2  ND-UW1 --------------------------------------------------------------------
site <- "ND-UW1"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>% pull(offset)

#Estimate water level
temp<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name)

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

#Append processed data to the output table
output <- output %>% add_row(temp)

#Clean up environment  
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.3 ND-UW2 --------------------------------------------------------------
site <- "ND-UW2"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>% pull(offset)

#Estimate water level
temp<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name)

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

#Append processed data to the output table
output <- output %>% add_row(temp)

#Clean up environment  
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.4 ND-SW -------------------------------------------------------------------
site <- "ND-SW"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>% pull(offset)

#Estimate water level
temp<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name)

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

#Append processed data to the output table
output <- output %>% add_row(temp)

#Clean up environment  
rm(site, temp, temp_dy, check, offset_temp, temp2)









#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 7: Export the data -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

