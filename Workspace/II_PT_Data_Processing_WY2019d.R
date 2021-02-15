#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 5/28/2020
#Purpose: Analysis of 20190729 Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Issues with this download

#Something is screwy with my offsets. For example ND Upland well is >1 meter in the record
# but only -0.3 when the offseet is applied?

#Need to create a check to show wells were consistent between downloads

#Deal with two baro loggers

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
data_dir<-"data\\20191124_Downloads\\"

#list pt, baro, and log file locations
pt_files<-list.files(paste0(data_dir, "export"), full.names =  TRUE) 
pt_files<-pt_files[!str_detect(pt_files, "log")]
field_logs<-paste0(data_dir, 'well_log.csv')

#gather pt data
df<-lapply(pt_files, download_fun) %>% bind_rows()

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
baro_index<-read_csv("data//Database Information//baro_assignment.csv") %>% 
  rename(Site_Name = site_code)

#Join baro index to df
df<-df %>% left_join(.,baro_index)

#download baro information
baro_files<- pt_files %>% as_tibble() %>%  filter(str_detect(value,"Baro")) %>% as_vector()
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
  select(-temp_gr, -temp_qb, -baro_logger)
  
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

#Some Notes. I will update this going forward. 
#First, I"m only going to process KT's wetland sites: ND, QB, TB, and DB

#We're goign to estimate the offset for each gage, then apply all at once


# 6.0 Create offset summary table ----------------------------------------------
#Create list of sites
index<-df %>% 
  #Select unique site names
  select(Site_Name) %>% 
  unique() %>% 
  #Remove baro and deep wetland wells
  filter(!str_detect(Site_Name,"Baro")) %>% 
  filter(!str_detect(Site_Name, "Deep")) %>% 
  #Filter to sites of interest [for now!]
  filter(str_detect(Site_Name,"ND|QB|TB|DB")) %>% 
  #Add final offset col
  mutate(offset = 0)

#limit df to those sites
offset<-offset %>% 
  #Select offset of relevant sites
  #filter(Site_Name %in% sites) %>% 
  drop_na() %>% 
  #Add collumn 
  mutate(actual = 0)

#Connect to database 
library(RSQLite)
library(DBI)
db<-dbConnect(RSQLite::SQLite(),"data//choptank.sqlite")


# 6.1 DB Wetland Well Shallow --------------------------------------------------
#List the site in question
site<-index$Site_Name[1]

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

offset %>% 
  filter(Site_Name==site) %>% 
  filter(offset < -0.3) %>% 
  filter(offset > -0.4) %>% 
  select(offset) %>% 
  summary(offset = mean(offset))

#Define offset
offset_temp<- -0.28

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

# 6.2 ND Upland Well 1  --------------------------------------------------------
#List the site in question
site<-index$Site_Name[2]

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

offset %>% 
  filter(Site_Name==site) %>% 
  filter(offset < -1.7) %>% 
  filter(offset > -1.9) %>% 
  select(offset) %>% 
  summary(offset = mean(offset))

#Define offset
offset_temp<- -0.27

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

# 6.3 ND Upland Well 2  --------------------------------------------------------
#List the site in question
site<-index$Site_Name[3]

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

offset %>% 
  filter(Site_Name==site) %>% 
  filter(offset < -1.7) %>% 
  filter(offset > -1.9) %>% 
  select(offset) %>% 
  summary(offset = mean(offset))

#Define offset
offset_temp<- 100.85-101.19

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

# 6.4 ND Wetland Well Shallow  -------------------------------------------------
#List the site in question
site<-index$Site_Name[4]

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

offset %>% 
  filter(Site_Name==site) %>% 
  filter(offset < -0.5) %>% 
  filter(offset > -0.7) %>% 
  select(offset) %>% 
  summary(offset = mean(offset))
# -0.59

#Define offset
offset_temp<- 101.03-101.58

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

# 6.5 TB Upland Well 1  -------------------------------------------------
#List the site in question
site<-index$Site_Name[5]

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

offset %>% 
  filter(Site_Name==site) %>% 
  filter(offset < -1.45) %>% 
  filter(offset > -1.6) %>% 
  select(offset) %>% 
  summary(offset = mean(offset))
# -1.47

#Define offset
offset_temp<- 0

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

# 6.6 TB Upland Well 3  -------------------------------------------------
#List the site in question
site<-index$Site_Name[6]

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

offset %>% 
  filter(Site_Name==site) %>% 
  filter(offset < -1.55) %>% 
  filter(offset > -1.65) %>% 
  select(offset) %>% 
  summary(offset = mean(offset))
# -1.58

#Define offset
offset_temp<- 100.9-101.22#-1.58

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

# 6.7 TB Upland Well 3  -------------------------------------------------
#List the site in question
site<-index$Site_Name[7]

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

offset %>% 
  filter(Site_Name==site) %>% 
  filter(offset < -1.4) %>% 
  filter(offset > -1.5) %>% 
  select(offset) %>% 
  summary(offset = mean(offset))
# -1.45

#Define offset
offset_temp<- 100.82-100.87

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

# 6.8 TB Wetland Well ---------------------------------------------------------
#List the site in question
site<-index$Site_Name[8]

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

offset %>% 
  filter(Site_Name==site) %>% 
  filter(offset < -.45) %>% 
  filter(offset > -.55) %>% 
  select(offset) %>% 
  summary(offset = mean(offset))
# -49

#Define offset
offset_temp<- 100.8-101.3

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


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 7: Apply offset and export ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#epxort df
df<-df %>% 
  left_join(.,index) %>% 
  mutate(waterLevel = waterHeight + offset)

#export 
write_csv(df,paste0(data_dir,"output.csv"))
