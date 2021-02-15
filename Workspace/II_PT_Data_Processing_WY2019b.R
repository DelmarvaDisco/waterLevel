#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 5/28/2020
#Purpose: Analysis of 20190729 Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Issues with this download
#   1) Lost JB wetland well. Somethign FUNKY happened on download?
#   2) QB Upland Well 1 offset looks off...not sure whats up
#   3) Deal with seperate baro loggers
#   4) Add QAQC steps from James
#   5) Finish sondes


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
data_dir<-"data\\20190729_Downloads\\"

#list pt, baro, and log file locations
pt_files<-list.files(paste0(data_dir, "export"), full.names =  TRUE) 
  pt_files<-pt_files[!str_detect(pt_files, "log")]
baro_files<-pt_files %>% as_tibble() %>% filter(str_detect(value,"Baro"))
field_logs<-paste0(data_dir, 'well_log.csv')

#Remove JB wetland well center
pt_files<-pt_files[!str_detect(pt_files, "JB_Center")]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Field Worksheet--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download Field Worksheet
field_logs<-read_csv(field_logs)

#Check to make sure pt files match field logs
check_fun(pt_files,field_logs)

#create df of site name, sonde_id, and measured offset
field_logs<-field_logs %>% 
  select(Site_Name, Sonde_ID, Relative_Water_Level_m)

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
#Gather baro data
baro<-download_fun(baro_files)

#Plot to check for any issues
baro %>% select(Timestamp, pressureAbsolute) %>%  dygraph_ts_fun()
#There are some weird spikes on near Sept 30?

#Create interpolation function 
baro_fun<-approxfun(baro$Timestamp, baro$pressureAbsolute)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: WaterHeight Data-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gather PT data
df<-lapply(pt_files, download_fun) %>% bind_rows()

#Joint to df
df<-df %>% left_join(., field_logs) 

#Estimate waterHeight
df<-df %>% 
  mutate(pressureBaro  = baro_fun(Timestamp), 
         pressureGauge = pressureAbsolute-pressureBaro, 
         waterHeight   = pressureGauge/9.81)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Calculate waterLevel -------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #Some Notes. I will update this going forward. 
  #First, I"m only going to process KT's wetland sites: ND, QB, TB, and DB

  #We're goign to estimate the offset for each gage, then apply all at once


# 6.0 Create offset summary table ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create list of sites
index<-df %>% 
  #Select unique site names
  select(Site_Name) %>% 
  unique() %>% 
  #Remove baro and deep wetland wells
  filter(!str_detect(Site_Name,"Baro")) %>% 
  filter(!str_detect(Site_Name, "Deep")) %>% 
  #Filter to sites of interest [for now!]
  filter(str_detect(Site_Name,"QB")) %>% 
  #Add final offset col
  mutate(offset = 0)

#limit df to those sites
offset<-offset %>% 
  #Select offset of relevant sites
  filter(Site_Name %in% sites) %>% 
  drop_na() %>% 
  #Add collumn 
  mutate(actual = 0)

#Connect to database 
library(RSQLite)
library(DBI)
db<-dbConnect(RSQLite::SQLite(),"data//choptank.sqlite")

  
# 6.1 QB Wetland Well Shallow --------------------------------------------------
#List the site in question
site<-index$Site_Name[1]

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp<- 0.68-1.16

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1900"), mdy('1/1/2100'))
temp_2<-temp_2 %>% as_tibble() %>% mutate(level = 1)
temp<-bind_rows(temp_1, temp_2) %>% 
  # mutate(Timestamp = ceiling_date(Timestamp), 
  #        Timestamp = as_date(Timestamp)) %>% 
  # group_by(Timestamp, level) %>% 
  # summarise(waterLevel = mean(waterLevel, na.rm=T)+100)
  mutate(waterLevel = waterLevel + 100)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)

# 6.2 QB Upland Well 2----------------------------------------------------------
#List the site in question
site<-index$Site_Name[2]

#Examine the measured offset 
offset %>% filter(Site_Name==site) 
offset %>% filter(Site_Name==site) %>% ggplot(aes(x=offset)) +  geom_dotplot()

#Define offset
offset_temp<- -1.22
  # offset %>% 
  # filter(Site_Name==site) %>% 
  # filter(offset>-3) %>% 
  # summarise(offset = mean(offset, na.rm=T)) %>% 
  # pull()

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1900"), mdy('1/1/2100'))
temp_2<-temp_2 %>% as_tibble() %>% mutate(level = 1)
temp<-bind_rows(temp_1, temp_2) %>% 
  # mutate(Timestamp = ceiling_date(Timestamp), 
  #        Timestamp = as_date(Timestamp)) %>% 
  # group_by(Timestamp, level) %>% 
  # summarise(waterLevel = mean(waterLevel, na.rm=T)+100)
  mutate(waterLevel = waterLevel + 100)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")])

# 6.3 QB Upland Well 1 ---------------------------------------------------------
#List the site in question
site<-index$Site_Name[3]

#Examine the measured offset 
offset %>% filter(Site_Name==site) 
offset %>% filter(Site_Name==site) %>% ggplot(aes(x=offset)) +  geom_dotplot()

#Define offset
offset_temp<- 0# offset %>% filter(Site_Name==site) %>% filter(offset>-2, offset<0) %>% summarise(offset=mean(offset)) %>% pull()

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1900"), mdy('1/1/2100'))
temp_2<-temp_2 %>% as_tibble() %>% mutate(level = 1)
temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 7: Apply offset and export ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#epxort df
df<-df %>% 
  left_join(.,index) %>% 
  mutate(waterLevel = waterHeight + offset)

#export 
write_csv(df,paste0(data_dir,"output.csv"))
