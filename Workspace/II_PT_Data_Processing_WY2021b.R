#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu) & James Maze (jtmaze@umd.edu)
#Date: 3/10/2022
#Purpose: Analysis of 2021 Fall Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Issues with this download

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
data_dir <- "data//20211112_Downloads//"

#list pt, baro, and log file locations
files<-list.files(paste0(data_dir, "export"), full.names =  TRUE) 
pt_files<-files[!str_detect(files, "log")]
pt_files<-files[!str_detect(files, "Baro")]

#gather pt data
df<-files %>% map_dfr(download_fun) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Setup Workspace-------------------------------------------------------
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
# Step 3: Determine offset for each well -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Some quick notes aobut our definition of offset
#   waterLevel = waterHeight + offset
#   offset = waterLevel - waterHeight

#Read offset file
offset<-read_csv("data/Database Information/offset.csv")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Step 4: Barometric Pressure Data -------------------------------------------------------
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
# Step 5: WaterHeight Calculation -------------------------------------------------------
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

# Make table to compare sensors to physically measured values 

checks <- tibble(Site_Name = c("na"), sensor_wtrlvl = c("na"))

# Read previous downloads


# 6.0 DB-UW1 ------------------------------------------------------------------


# 6.1 DB-SW -------------------------------------------------------------------


# 6.2 ND-UW1 ---------------------------------------------------------------------


# 6.3 ND-UW2 --------------------------------------------------------------


# 6.4 ND-SW ---------------------------------------------------------------


# 6.5 QB-UW1 -----------------------------------------------------------------


# 6.6 QB-UW2  -------------------------------------------------------------


# 6.7 QB-SW ---------------------------------------------------------------


# 6.8 TB-UW1 --------------------------------------------------------------------


# 6.9 TB-UW2 --------------------------------------------------------------


# 6.10 TB-UW3 ------------------------------------------------------------------


# 6.11 TB-SW --------------------------------------------------------------


# 6.12 TA-SW --------------------------------------------------------------


# 6.13 TI-SW --------------------------------------------------------------


# 6.14 DK-SW --------------------------------------------------------------------


# 6.15 DK-UW1 --------------------------------------------------------------------


# 6.16 DK-UW2 -------------------------------------------------------------


# 6.17 DF-SW --------------------------------------------------------------


# 6.18 FN-SW --------------------------------------------------------------------


# 6.19 JA-SW --------------------------------------------------------------------


# 6.20 JB-SW --------------------------------------------------------------------


# 6.21 JB-UW1 --------------------------------------------------------------------


# 6.22 JB-UW2 --------------------------------------------------------------------


# 6.23 JC-SW  --------------------------------------------------------------------


# 6.24 JC-UW1  --------------------------------------------------------------------


# 6.25 NB-SW -------------------------------------------------------------------

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
  mutate(measured_diff = as.numeric(sensor_wtrlvl) - as.numeric(Relative_Water_Level_m)) 

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







