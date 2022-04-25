#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu) & James Maze (jtmaze@umd.edu)
#Date: 4/24/2022
#Purpose: Analysis of 2022 Spring Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Issues with this download
#  - DB-SW serial number did not read in when using the download function. 
#    The column name was weird ("Abs Pres, kPa (LGR S/N: 10258771, SEN S/N: 10258771, LBL: PSI)")
#    Since the SN had "LBL : PSI "after it, download_fun broke. Manually assigned SN to the site
#    in Step #2

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
files<-list.files(paste0(data_dir, "20220410_Downloads//export"), full.names =  TRUE) 
pt_files<-files[!str_detect(files, "log")]
pt_files<-files[!str_detect(files, "Baro")]

#gather pt data
df<-files %>% map_dfr(download_fun) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Field sheet -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

field_logs <- read_csv(paste0(data_dir, "20220410_Downloads//well_log.csv"))

#Get rid of scientific notation from field log
field_logs <- field_logs %>% 
  select(c(Relative_water_level_m, Date, Sonde_ID, Site_Name, 
           Notes, Depth_to_water_m, Well_head_m)) %>% 
  filter(!is.na(Site_Name)) %>% 
  mutate(Relative_water_level_m = as.numeric(Relative_water_level_m))

#Check to make sure pt files match field logs
check_fun(pt_files, field_logs)

rm(check_fun_errors)

#Join field log to master df. First, make Sonde_ID a character in both objects. 
df$Sonde_ID <- as.character(df$Sonde_ID)
field_logs$Sonde_ID <- as.character(field_logs$Sonde_ID)

#join field log to master df
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


