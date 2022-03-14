#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu) & James Maze (jtmaze@umd.edu)
#Date: 3/10/2022
#Purpose: Analysis of 20201015 Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Issues with this download
# What is the offset measurement for TS-SW. Check JM field notes?
# DB-SW, ND-SW is not matching up between May 2020 and Oct 2020 downloads
# Extra filtering on TB-UW2


#Table of Contents~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Organize workspace
# Step 2: Field Worksheet
# Step 3: Determine offset for each well
# Step 4: Baro Data
# Step 5: Water Depth
# Step 6: Apply the offset 
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
data_dir<-"data\\20201015_Downloads\\"

#list pt, baro, and log file locations
files<-list.files(paste0(data_dir, "export"), full.names =  TRUE) 
pt_files<-files[!str_detect(files, "log")]
pt_files<-pt_files[!str_detect(pt_files, "Baro")]
field_logs<-paste0(data_dir, 'well_log.csv')

#gather pt data
df<-pt_files %>% map_dfr(download_fun)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Field Worksheet--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download Field Worksheet
field_logs <- read_csv(paste0(data_dir, "well_log.csv"))

#Get rid of scientific notation from field log
field_logs <- field_logs %>% 
  mutate(Relative_Water_Level_m = format(Relative_Water_Level_m, scientific = FALSE))

#Check to make sure pt files match field logs
check_fun(pt_files, field_logs)

rm(check_fun_errors)

#create df of site name, sonde_id, and measured water level
field_logs<-field_logs %>% 
  select(Site_Name, Sonde_ID, Relative_Water_Level_m, Notes)

#join to master df
df<-df %>% left_join(., field_logs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Determine offset for each piezometer----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Some quick notes aobut our definition of offset
#   waterLevel = waterHeight + offset
#   offset = waterLevel - waterHeight

#Read offset file
offset<-read_csv("data/Database Information/offset.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Barometric Pressure Data----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download baro assignment [this may need to be changed manually]
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
# Step 5: WaterHeight Calculation-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate waterHeight
df<-df %>% 
  mutate(
    pressureGauge = pressureAbsolute-pressureBaro, 
    waterHeight   = pressureGauge/9.81) %>% 
  select(-c(pressureAbsolute, pressureBaro, pressureGauge, Relative_Water_Level_m))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Calculate waterLevel using the offset & QAQC -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Make table to compare sensors to physically measured values 

checks <- tibble(Site_Name = c("na"), sensor_wtrlvl = c("na"))

#Read in the previous output table
dt <- read_csv("data/output_20200508_JM.csv")


# 6.0 DB-UW1 ---------------------------------------------------------------------

#List the site in question
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

# 6.1 DB-SW -------------------------------------------------------------------

#List the site in question
site <- "DB-SW"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.2 ND-UW1 ------------------------------------------------------------------

#List the site in question
site <- "ND-UW1"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.3 ND-UW2 ------------------------------------------------------------------
#List the site in question
site <- "ND-UW2"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.4 ND-SW -------------------------------------------------------------------

#List the site in question
site <- "ND-SW"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.5 QB-UW1 ---------------------------------------------------------------------

#List the site in question
site <- "QB-UW1"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.6 QB-UW2 ------------------------------------------------------------------
#List the site in question
site <- "QB-UW2"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.7 QB-SW ---------------------------------------------------------------
#List the site in question
site <- "QB-SW"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.8 TB-UW1 ---------------------------------------------------------------------
site <- "TB-UW1"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.9 TB-UW2 ------------------------------------------------------------------
site <- "TB-UW2"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

#Estimate water level
temp <- df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  filter(!is.na(waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name)

#remove anomalous values. Needs a little extra filtering
temp <- temp %>% 
  filter(Timestamp <= "2020-08-24 14:15:00" | Timestamp >= "2020-08-24 20:15:00")

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

# 6.11 TB-UW3 -------------------------------------------------------------
site <- "TB-UW3"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.12 TB-SW -------------------------------------------------------------------
site <- "TB-SW"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.13 TA-SW --------------------------------------------------------------
site <- "TA-SW"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.14 TI-SW --------------------------------------------------------------------
site <- "TI-SW"

#Find the offset
offset_temp <- offset %>% filter(Site_Name == site) %>%  pull(offset)

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

#Clean up environment
rm(site, temp, temp_dy, check, offset_temp, temp2)

# 6.15 --------------------------------------------------------------------



# 7.0 Aggregate and export ------------------------------------------------


#export 

output <- unique(output)
  
write_csv(output, paste0(data_dir, "output_20201015_JM.csv")) 

