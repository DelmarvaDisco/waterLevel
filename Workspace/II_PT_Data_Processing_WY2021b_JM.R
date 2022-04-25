#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu) & James Maze (jtmaze@umd.edu)
#Date: 3/10/2022
#Purpose: Analysis of 2021 Fall Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Issues with this download
#  - DB-SW serial number did not read in when using the download function. 
#    The column name was weird ("Abs Pres, kPa (LGR S/N: 10258771, SEN S/N: 10258771, LBL: PSI)")
#    Since the SN had "LBL : PSI "after it, download_fun broke. Manually assigned SN to the site
#    in Step #2
#  - TB-SW battery died. Bad data at the end of the deployment
#  - A little extra filtering for QB-UW2
#  - Need to redownload baro loggers for checks
#

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
data_dir <- "data//"

#list pt, baro, and log file locations
files<-list.files(paste0(data_dir, "20211112_Downloads//export"), full.names =  TRUE) 
pt_files<-files[!str_detect(files, "log")]
pt_files<-files[!str_detect(files, "baro")]

#gather pt data
df<-files %>% map_dfr(download_fun) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Field sheet -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download Field Worksheet
field_logs <- read_csv(paste0(data_dir, "20211112_Downloads//well_log.csv"))

#Get rid of scientific notation from field log
field_logs <- field_logs %>% 
  select(c(Relative_water_level_m, Date, Sonde_ID, Site_Name, 
           Notes, Depth_to_water_m, Well_head_m)) %>% 
  mutate(Relative_water_level_m = as.numeric(Relative_water_level_m))

#Check to make sure pt files match field logs
check_fun(pt_files, field_logs)

rm(check_fun_errors)

#Join field log to master df. First, make Sonde_ID a character
df$Sonde_ID <- as.character(df$Sonde_ID)
field_logs$Sonde_ID <- as.character(field_logs$Sonde_ID)

#join to master df
df<-df %>% left_join(., field_logs)

#!!! download_fun didn't recognize DB-SW's SN. Wonky column name. Manual fix here. 
df <- df %>% 
  mutate(Sonde_ID = if_else(is.na(Sonde_ID), "10258771", Sonde_ID)) %>% 
  mutate(Site_Name = if_else(is.na(Site_Name), "DB-SW", Site_Name))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Read in the offset file -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Some quick notes aobut our definition of offset
#   waterLevel = waterHeight + offset
#   offset = waterLevel - waterHeight

#Read offset file
offset<-read_csv("data/Database Information/offset.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Barometric Pressure Data -------------------------------------------------------
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

# Make table to compare sensors to physically measured values 

checks <- tibble(Site_Name = c("na"), sensor_wtrlvl = c("na"))

# Read previous downloads
dx <- read_csv("data/output_20200508_JM.csv")
dy <- read_csv("data/output_20201015_JM.csv")
dz <- read_csv("data/output_20210525_JM.csv")

dt <- bind_rows(dx, dy, dz) %>% 
  mutate(Timestamp = ymd_hms(Timestamp, tz = "GMT"))

rm(dx, dy, dz)


# 6.0 DB-UW1 ------------------------------------------------------------------
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

# 6.1 DB-SW -------------------------------------------------------------------
site <- "DB-SW"

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

# 6.2 ND-UW1 ---------------------------------------------------------------------
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

# 6.4 ND-SW ---------------------------------------------------------------
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

# 6.5 QB-UW1 -----------------------------------------------------------------
site <- "QB-UW1"

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

# 6.6 QB-UW2  -------------------------------------------------------------

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
temp<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.05, max = 0.2)

#Extra filtering
temp <- temp %>% 
  filter(Timestamp < "2021-06-18 16:30:00" | Timestamp > "2021-06-18 20:00:00")

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

# 6.7 QB-SW ---------------------------------------------------------------
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

# 6.8 TB-UW1 --------------------------------------------------------------------
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

# 6.9 TB-UW2 --------------------------------------------------------------

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

# 6.10 TB-UW3 ------------------------------------------------------------------

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

# 6.11 TB-SW --------------------------------------------------------------

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

# 6.13 TI-SW --------------------------------------------------------------

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

# 6.15 DK-UW1 --------------------------------------------------------------------

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
temp<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name)

#remove anomalous values
temp <- fun_anomalous(temp, min = -0.05, max = 0.2)

temp <- temp %>% 
  filter(Timestamp < "2021-10-19 19:30:00" | Timestamp > "2021-10-19 23:30:00")

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

# 6.18 FN-SW --------------------------------------------------------------------
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

# 6.21 JB-UW1 --------------------------------------------------------------------

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

# 6.22 JB-UW2 --------------------------------------------------------------------

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

# 6.23 JC-SW  --------------------------------------------------------------------

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

# 6.24 JC-UW1  --------------------------------------------------------------------

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

# 6.25 NB-SW -------------------------------------------------------------------

site <- "NB-SW"

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

#Append to output file
output <- output %>% add_row(temp)

rm(site, temp, temp_dy, check, offset_temp, temp2)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 7: Aggregate and export -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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


write_csv(checks, paste0(data_dir, "checks_20211112_JM.csv"))

write_csv(output, paste0(data_dir, "output_20211112_JM.csv"))


