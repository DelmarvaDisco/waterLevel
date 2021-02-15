#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 5/28/2020
#Purpose: Analysis of 20190729 Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Issues with this download

# Missing DB Upland Well (S/N 10258802). Notes say it was dead.
#Missing Greg Upland Well 1 (10210741).  Not sure...
# Extra sonde: 10778289. Weird...

#Something is screwy with my offsets. For example ND Upland well is >1 meter in the record
# but only -0.3 when the offseet is applied?


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
data_dir<-"data\\20190730_Downloads\\"

#list pt, baro, and log file locations
pt_files<-list.files(paste0(data_dir, "export"), full.names =  TRUE) 
  pt_files<-pt_files[!str_detect(pt_files, "log")]
baro_files<-pt_files %>% as_tibble() %>% filter(str_detect(value,"10808360"))
field_logs<-paste0(data_dir, 'well_log.csv')

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

    #----------------------------------------
    #Edit logged date for TB Wetland Well (issue with header...fml)
    #end date in df is 2/22/1954 16:25
    #Download date is 7/30/2019 13:15
    
    #Estimate time difference
    diff<-mdy_hm("2/22/2054 16:25") - mdy_hm("7/30/2019 13:15")
    
    #Substract difference (in new tibble) 
    temp <- df %>% 
      filter(Site_Name=="TB Wetland Well Shallow") %>% 
      mutate(Timestamp = Timestamp - diff)
    
    #Join to master df
    df<-df %>% 
      filter(Site_Name!="TB Wetland Well Shallow") %>% 
      bind_rows(., temp)
    
    #Clean up 
    remove(temp)
    #---------------------------------------


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
