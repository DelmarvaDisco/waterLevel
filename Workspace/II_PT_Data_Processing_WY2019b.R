#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 5/28/2020
#Purpose: Analysis of 20190729 Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Issues with this download 
#   1) Lost JB wetland well. Something FUNKY happened on download?
#   2) SN:10246253 is on the field_sheet but not in the download batch
#   2) QB Upland Well 1 offset looks off...not sure whats up
#   3) QB Upland Well 2 similar offset problems (Apr 22 2019 ~ 1.17 and Oct 11 2018 ~ 0.67)
#   4) Denver Catchment Outlet flagged for possible sedimentation accumulating under the PT
#   5) DF Wetland Well has huge offset (0.68) at Apr 22 2019
#   6) !!! QB Wetland Well Deep strange behavior Jul 23-29 2019. Weird 0.2m jump on Jul 11th. No data prior to May 2018??
#   7) Jones Rd South Outlet has weird behavior on Oct 11th 2018 Download... Bad offset??
#   8) TI Wetland Well Shallow has the same Oct 11th 2018 offset issue (~0.26)
#   9) Solute Catchment Outlet has same Oct11th 2018 offset (~.3)
#   10) !!!NB Wetland Well has a data gap. How do I handle this offset?
#   11) Only one upland well download for JB-UW?? not sure which
#   12) JC-SW PT filled, how to do offset with data gap? Has similar offset problem (~0.26) on Oct 11th download
#   13) JC-UW1 filled, data gap... Offset problem (~0.6)
#   14) JA-SW with data gap.. Offset problem (0.2)
#   15) JR Outlet with data gap ... Offset problem (~0.39)
#Deal with seperate baro loggers


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

# 6.0 Create offset summary table ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Read in previous output files for comparison
dt <- read_csv("data_long")
  

# 6.1 QB Wetland Well Shallow --------------------------------------------------

#List the site in question
site<-"QB Wetland Well Shallow"

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
temp_2 <- dt %>% 
  filter(site == Site_Name)
  
temp<-bind_rows(temp_1, temp_2) %>% 
  # mutate(Timestamp = ceiling_date(Timestamp), 
  #        Timestamp = as_date(Timestamp)) %>% 
  # group_by(Timestamp, level) %>% 
  # summarise(waterLevel = mean(waterLevel, na.rm=T)+100)
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)


#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)

# 6.2 QB Upland Well 2----------------------------------------------------------
#List the site in question
site<-"QB Upland Well 2"

#Examine the measured offset 
offset %>% filter(Site_Name==site) 
offset %>% filter(Site_Name==site) %>% ggplot(aes(x=offset)) +  geom_dotplot()

#Define offset
offset_temp<- -1.22

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(Site_Name == site)

temp <- bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

  
#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")])

# 6.3 QB Upland Well 1 ---------------------------------------------------------
#List the site in question
site<- "QB Upland Well 1"

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

temp_2 <- dt %>% 
  filter(Site_Name == site)
temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100)



#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 6.4 Denver Catchment Outflow --------------------------------------------
#List the site in question
site <-"Denver Catchment Outlet"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(site == Site_Name)

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)


#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.5 DF Wetland Well Shallow -------------------------------------------------------------------
site <-"DF Wetland Well Shallow"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- -0.68

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(site == Site_Name)

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.6 QB Wetland Well Deep -------------------------------------------------------------------

site <-"QB Wetland Well Deep"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(site == Site_Name)

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.7 Jones Road South Catchment Outlet ---------------------------------------------------------------------
site <- "Jones Road South Catchment Outlet"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 100.29-100.51

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(site == Site_Name)

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.8 TI Wetland Well Shallow ------------------------------------------------------------------
site <- "TI Wetland Well Shallow"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 100.53-101.08

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(site == Site_Name)

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.9 Solute Catchment Outlet ---------------------------------------------------------------------
site <- "Solute Catchment Outlet"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 100.05 - 100.36

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(site == Site_Name)

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.11 NB Wetland Well --------------------------------------------------------------------
site <- "NB Wetland Well"


#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(Site_Name == "NB Wetland Well Shallow")

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.12 ????JB Upland Well???? -----------------------------------------------------------------------
site <- "JB Upland Well"
#Which upland well? not specified

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(site == Site_Name)

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.13 JB Wetland Well --------------------------------------------------------------------
### Site is missing


# 6.14 JC Wetland Well Shallow --------------------------------------------------------------------
site <- "JC Wetland Well Shallow"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(site == Site_Name)

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.14 JC Upland Well 1 ------------------------------------------------------------------
site <- "JC Upland Well 1"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(site == Site_Name)

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.15 JA Wetland Well Shallow --------------------------------------------------------------------
site <- "JA Wetland Well Shallow"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(site == Site_Name)

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)


# 6.16 JR Outlet ------------------------------------------------

site <- "JR Outlet"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(Site_Name == "Jones Road North Catchment Outlet")

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)

# 6.17 	DV Wetland Well Shallow --------------------------------------------------------------------
site <- "DV Wetland Well Shallow"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(Site_Name == "Jones Road North Catchment Outlet")

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)

# 6.18 GB Wetland Well Shallow --------------------------------------------------------------------
site <- "GB Wetland Well Shallow"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(Site_Name == "Jones Road North Catchment Outlet")

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)

# 6.19 JU Wetland Well Shallow --------------------------------------------------------------------
site <- "JU Wetland Well Shallow"

#Examine the measured offset 
offset %>% 
  filter(Site_Name==site) %>% 
  ggplot(aes(x=offset)) +
  geom_dotplot()

#Define offset
offset_temp <- 0

#Estimate water level
temp_1<-df %>% 
  filter(Site_Name == site) %>%
  mutate(waterLevel = waterHeight + offset_temp) %>% 
  mutate(level = 0) %>% 
  select(Timestamp, waterLevel, level)

#Create temp df
temp_2 <- dt %>% 
  filter(Site_Name == "Jones Road North Catchment Outlet")

temp<-bind_rows(temp_1, temp_2) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  drop_na(waterLevel)

#plot in dygraphs
dygraph_ts_fun(temp %>% select(Timestamp, waterLevel))

#From here, iteratively change offset until it matches dygraph

#export offet and clean up space
index$offset[index$Site_Name == site] <-offset_temp
remove(list=ls()[str_detect(ls(), "temp")]) ; remove(site)




# Step 7: Apply offset and export ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#epxort df
df<-df %>% 
  left_join(.,index) %>% 
  mutate(waterLevel = waterHeight + offset)

#export 
write_csv(df,paste0(data_dir,"output.csv"))
