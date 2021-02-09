#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 5/28/2020
#Purpose: Analysis of 20190729 Download
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Issues with this download
#   1) Lost JB wetland well. Somethign FUNKY happened on download?


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

#Define data directory
data_dir<-"data\\20190729_Downloads\\"

#list pt, baro, and log file locations
pt_files<-list.files(paste0(data_dir, "export"), full.names =  TRUE) 
  pt_files<-pt_files[!str_detect(pt_files, "log")]
baro_files<-pt_files %>% as_tibble() %>% filter(str_detect(value,"Baro"))
log_files<-paste0(data_dir, 'well_log.csv')

#Remove JB wetland well center
pt_files<-pt_files[!str_detect(pt_files, "JB_Center")]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Field Worksheet--------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download Field Worksheet
log_files<-read_csv(log_files)

#Check to make sure pt files match field logs
check_fun(pt_files,log_files)

#create df of site name, sonde_id, and measured offset
log_files<-log_files %>% 
  select(Site_Name, Sonde_ID, Relative_Water_Level_m)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Determine offset for each piezometer----------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Some notes before I quite
#Create baro file for entire record
#Create lookup table [pull from 2019a] to find file
#pull pt file and look up pressure based on table
#convert to depth
#examine offset

#random though: make this its own script



#4.1 Create lookup table for PT data -------------------------------------------
#Identify files with HOBO export files
files<-list.files("data/", recursive = T, full.names = T)
files<-files[substr(files,nchar(files)-2,nchar(files))=="csv"] 
files<-files[grep(files,pattern = "export")]

#Select files to process
files<-files %>% 
  enframe(name = NULL) %>% 
  filter(!str_detect(value, "archive")) %>%
  as.matrix(.)

#Create function to retrieve info from each file
pt_lookup<-function(n){
  
  #Download data
  temp<-read_csv(files[n], skip=1)
  
  #Determine serial number
  serial_number<-colnames(temp)[grep("LGR",colnames(temp))][1]  #Find collumn name with serial number
  serial_number<-substr(serial_number,   #isolate serial number
                        gregexpr("SEN.S.N",serial_number)[[1]][1]+9, #Start
                        nchar(serial_number)-1) #stop
  serial_number<-as.numeric(serial_number) 
  
  #Determine TZ
  time_zone<-colnames(temp)[grep("GMT",colnames(temp))]  #Grab collumn name w/ time offset
  time_zone<-substr(time_zone,
                    regexpr('GMT', time_zone)[1],
                    nchar(time_zone))
  time_zone<-if_else(time_zone=="GMT-04:00",
                     "EST",
                     if_else(time_zone=="GMT-05:00",
                             "EDT",
                             "-9999"))
  #Determin units
  units<-colnames(temp)[grep("Abs Pres,",colnames(temp))]
  units<-substr(units,
                regexpr("Abs Pres,", units)+10,
                regexpr("Abs Pres,", units)+12)
  
  #Organize
  colnames(temp)<-c("ID","Timestamp","pressureAbsolute", "temp")
  temp<-temp[,c("Timestamp","pressureAbsolute", "temp")]
  temp<-temp %>%
    #Select collumns of interest
    dplyr::select(Timestamp, pressureAbsolute, temp) %>%
    #Convert to POSIX
    dplyr::mutate(Timestamp = as.POSIXct(strptime(Timestamp, "%m/%d/%y %I:%M:%S %p"), tz = time_zone))  %>%
    #Convert to GMT
    dplyr::mutate(Timestamp = with_tz(Timestamp, "GMT")) %>%
    #Order the intput
    dplyr::arrange(Timestamp)
  
  #create output
  tibble(path       = files[n], 
         Sonde_ID   = serial_number,
         units      = units, 
         start_date = min(temp$Timestamp), 
         end_date   = max(temp$Timestamp))
}

#run function
files<-lapply(X = seq(1,length(files)), FUN = pt_lookup)
files<-bind_rows(files)

#4.2 Compile field log tables --------------------------------------------------
logs<-list.files("data//", recursive = T, full.names = T)
logs<-logs[str_detect(logs, "well_log")]
logs<-lapply(FUN = read_csv, X=logs) %>% bind_rows()

#4.3 Create function to look up gage pressure for each log entry ---------------
pressure_fun<-function(n){
  #Create mode function
  mode <- function(x) {
    ux <- unique(x)
    ux[which.max(tabulate(match(x, ux)))]
  }
  
  #Use try catch to estimate pressure
  pressure<-tryCatch({
    #Grab row of interest
    log<-logs[n,]
    
    #identify file of interest
    file<-files %>% 
      #Filter to sonde ID
      filter(Sonde_ID == log$Sonde_ID) %>% 
      #Filter to date of interest
      filter(start_date <= mdy(log$Date)) %>% 
      filter(end_date   >= mdy(log$Date)) 
    
    #Select top row
    file<-file[1,]
    
    #download data
    pressure<-download_fun(file$path) %>% 
      mutate(Timestamp = floor_date(Timestamp)) %>% 
      mutate(Timestamp = as_date(Timestamp)) %>% 
      group_by(Timestamp) %>% 
      summarise(pressureAbsolute = mean(pressureAbsolute, na.rm=T), 
                Sonde_ID = mode(Sonde_ID)) %>% 
      filter(Timestamp == mdy(log$Date))
  }, 
  error = function(e){tibble(Timestamp = NA, pressureAbsolute=NA, Sonde_ID=NA)})
  
  #Export pressure
  pressure
}

#Execute function (for the love of all things good and holy, parralelize this...)
pressure<-lapply(seq(1, nrow(logs)), pressure_fun) %>% 
  bind_rows() %>% 
  drop_na()

#4.4 Curate baro logger data for period of record-------------------------------
#Define Baro Loggers
baro_id<-c("10589038", #QB Baro
           "10808360") #GR Baro

#Define Baro Logger Files
baro_files<-files %>% filter(Sonde_ID %in% baro_id)
  
#run function
baro<-lapply(X = baro_files$path, FUN = download_fun) %>% bind_rows()

#Organize barometric pressure
baro<-baro %>%
  #Take daily average
  
  
  #rename baro collumn
  rename(barometricPressure=pressureAbsolute) %>%
  #remove duplicate records from same sonde
  distinct(.) %>%
  #Remove na's
  na.omit()










  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Four this download, I'm going to pull the WY2018 spreadsheet. From here on out, 
  # I will only pull data from central db file. (Whatever that becomes!!!)
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  #setup db function to pull from SQLite databse
  library(RSQLite)
  library(DBI)
  source("functions//db_get_ts.R")

  #Create list of sites
  sites<-logs %>% 
    select(Site_Name) %>% 
    distinct()
  
  #Connect to database
  #Define database connection
  db<-dbConnect(RSQLite::SQLite(),"data//choptank.sqlite")
  
  
  


  #Connect to 
  test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1900"), mdy('10/30/2100'))
  # #Read previous waterLevel data
  # SWL<-read_xlsx(
  #     path = "data//20190423_Downloads//Choptank_Wetlands_WY2018.xlsx", 
  #     sheet = "SWL", 
  #     na="NA") %>% 
  #   pivot_longer(-Timestamp)
  # GWL<-read_xlsx(
  #   path = "data//20190423_Downloads//Choptank_Wetlands_WY2018.xlsx", 
  #   sheet = "GWL", 
  #   na="NA") %>% 
  #   pivot_longer(-Timestamp)
  # OWL<-read_xlsx(
  #   path = "data//20190423_Downloads//Choptank_Wetlands_WY2018.xlsx", 
  #   sheet = "OWL", 
  #   na="NA") %>% 
  #   pivot_longer(-Timestamp)
  # 
  # #Combine to create waterLevel data
  # waterLevel<-bind_rows(SWL, GWL, OWL)
  # 
  # #Export waterLevel and clean up
  # write_csv(waterLevel, "data//20190423_Downloads//output.csv")
  # write_csv(waterLevel, "data//choptank.csv")
  # remove(SWL, GWL, OWL)
  # waterLevel<-read_csv("data//choptank.csv")

  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Return to normal coiding
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  

#4.2 Download relative water level data from download logs ---------------------
logs<-list.files("data//", recursive = T, full.names = T)
logs<-logs[str_detect(logs, "well_log")]
logs<-lapply(FUN = read_csv, X=logs) %>% bind_rows()

#4.3 Combine water level based on daily mean
#offset
waterLevel %>% 
  #Remove NA's
  drop_na() %>% 
  #group by day
  mutate(Timestamp = ceiling_date(Timestamp, "day"),
         Timestamp = as_date(Timestamp)) %>% 
  group_by(Timestamp, name) %>% 
  summarise(waterLevel = mean(value, na.rm=T))
  

#FUDGE -- I need waterDepth [or whatever the equivolent i in ODM2 luangage]





















#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Barometric Pressure Data----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gather baro data
baro<-download_fun(baro_files)

#Plot to check for any issues
baro %>% select(Timestamp, pressureAbsolute) %>%  dygraph_ts_fun()
#There are some weird spikes on near Sept 30?

#Create interpolation function 
baro_fun<-approxfun(baro$Timestamp, baro$pressureAbsolute)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: WaterDepth Data-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gather PT data
df<-lapply(pt_files, download_fun) %>% bind_rows()

#Joint to df
df<-df %>% left_join(., field_log) 

#Estimate waterHeight
df<-df %>% 
  mutate(pressureBaro  = baro_fun(Timestamp), 
         pressureGauge = pressureAbsolute-pressureBaro, 
         waterHeight   = pressureGauge/9.81)

#Joint to df
df<-df %>% left_join(., offset) 

#Estimate waterdepth
df<-df %>% mutate(waterDepth = waterHeight + offset)

#Subset to waterDepth (after inspection!)
df<-df %>% select(Timestamp, Site_Name, waterDepth) 

#Add prcessing level
df$processing_level<-"raw"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 5: QAQC------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#list site names
field_log %>% select(Site_Name) %>% arrange(Site_Name) %>% pull()

#5.1 BN-A-----------------------------------------------------------------------
#Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site<-"BN-A"
waterDepth <- df %>% filter(Site_Name==site) %>% select(Timestamp, waterDepth)
dygraph_QAQC_fun(waterDepth)

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add rolling average
updated<-waterDepth %>% 
  mutate(waterDepth = rollmean(waterDepth, k=5, fill=NA))

#remove weird periods
updated<-updated %>% 
  filter(Timestamp < mdy_hms("9/7/2018, 23:00:00") |
           Timestamp > mdy_hms("9/8/2018, 17:45:00")) %>% 
  filter(Timestamp < mdy_hms("9/11/2018, 5:45:00") |
           Timestamp > mdy_hms("9/11/2018, 23:45:00")) %>%
  filter(Timestamp < mdy_hms("9/12/2018, 5:00:00") |
           Timestamp > mdy_hms("9/12/2018, 23:00:00")) 

#interpolate between weird periods
interpfun<-approxfun(updated$Timestamp, updated$waterDepth)  
updated<-tibble(
  Timestamp = waterDepth$Timestamp,
  waterDepth = interpfun(waterDepth$Timestamp))

#Inspect output
dygraph_QAQC_fun(waterDepth, historic=NULL, updated)

#print~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add info to 'updated' dataframe
updated$Site_Name = site
updated$processing_level = 'processed'

#Append master dataframe
df<-bind_rows(df, updated)

#5.2 BN-B-----------------------------------------------------------------------
#Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site<-"BN-B"
waterDepth <- df %>% filter(Site_Name==site) %>% select(Timestamp, waterDepth)
dygraph_QAQC_fun(waterDepth)

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove bogus point
updated<-waterDepth %>% 
  filter(Timestamp!=mdy_hm("9/6/2018 19:15"))

#Add rolling average
updated<-updated %>% 
  mutate(waterDepth = rollmean(waterDepth, k=5, fill=NA))

#remove weird periods
updated<-updated %>% 
  filter(Timestamp < mdy_hms("9/7/2018, 23:00:00") |
           Timestamp > mdy_hms("9/8/2018, 17:45:00")) %>% 
  filter(Timestamp < mdy_hms("9/11/2018, 5:45:00") |
           Timestamp > mdy_hms("9/11/2018, 23:45:00")) %>%
  filter(Timestamp < mdy_hms("9/12/2018, 5:00:00") |
           Timestamp > mdy_hms("9/12/2018, 23:00:00")) 

#interpolate between weird periods
interpfun<-approxfun(updated$Timestamp, updated$waterDepth)  
updated<-tibble(
  Timestamp = waterDepth$Timestamp,
  waterDepth = interpfun(waterDepth$Timestamp))

#Inspect output
dygraph_QAQC_fun(waterDepth, historic=NULL, updated)

#print~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add info to 'updated' dataframe
updated$Site_Name = site
updated$processing_level = 'processed'

#Append master dataframe
df<-bind_rows(df, updated)

#5.3 BN-C-----------------------------------------------------------------------
#Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site<-"BN-C"
waterDepth <- df %>% filter(Site_Name==site) %>% select(Timestamp, waterDepth)
dygraph_QAQC_fun(waterDepth)

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add rolling average
updated<-waterDepth %>% 
  mutate(waterDepth = rollmean(waterDepth, k=5, fill=NA))

#remove weird periods
updated<-updated %>% 
  filter(Timestamp < mdy_hms("9/7/2018, 23:00:00") |
           Timestamp > mdy_hms("9/8/2018, 17:45:00")) %>% 
  filter(Timestamp < mdy_hms("9/11/2018, 5:45:00") |
           Timestamp > mdy_hms("9/11/2018, 23:45:00")) %>%
  filter(Timestamp < mdy_hms("9/12/2018, 5:00:00") |
           Timestamp > mdy_hms("9/12/2018, 23:00:00")) 

#interpolate between weird periods
interpfun<-approxfun(updated$Timestamp, updated$waterDepth)  
updated<-tibble(
  Timestamp = waterDepth$Timestamp,
  waterDepth = interpfun(waterDepth$Timestamp))

#Inspect output
dygraph_QAQC_fun(waterDepth, historic=NULL, updated)

#print~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add info to 'updated' dataframe
updated$Site_Name = site
updated$processing_level = 'processed'

#Append master dataframe
df<-bind_rows(df, updated)

#5.4 EP-A-----------------------------------------------------------------------
#Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site<-"EP-A"
waterDepth <- df %>% filter(Site_Name==site) %>% select(Timestamp, waterDepth)
dygraph_QAQC_fun(waterDepth)

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add rolling average
updated<-waterDepth %>% 
  mutate(waterDepth = rollmean(waterDepth, k=5, fill=NA))

#remove weird periods
updated<-updated %>% 
  filter(Timestamp < mdy_hms("9/8/2018, 2:00:00") |
           Timestamp > mdy_hms("9/8/2018, 17:45:00")) %>% 
  filter(Timestamp < mdy_hms("9/11/2018, 5:45:00") |
           Timestamp > mdy_hms("9/11/2018, 23:45:00")) %>%
  filter(Timestamp < mdy_hms("9/12/2018, 5:00:00") |
           Timestamp > mdy_hms("9/12/2018, 23:00:00")) 

#interpolate between weird periods
interpfun<-approxfun(updated$Timestamp, updated$waterDepth)  
updated<-tibble(
  Timestamp = waterDepth$Timestamp,
  waterDepth = interpfun(waterDepth$Timestamp))

#Inspect output
dygraph_QAQC_fun(waterDepth, historic=NULL, updated)

#print~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add info to 'updated' dataframe
updated$Site_Name = site
updated$processing_level = 'processed'

#Append master dataframe
df<-bind_rows(df, updated)

#5.5 EP-B-----------------------------------------------------------------------
#Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site<-"EP-B"
waterDepth <- df %>% filter(Site_Name==site) %>% select(Timestamp, waterDepth)
dygraph_QAQC_fun(waterDepth)

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add rolling average
updated<-waterDepth %>% 
  mutate(waterDepth = rollmean(waterDepth, k=5, fill=NA))

#remove weird periods
updated<-updated %>% 
  filter(Timestamp < mdy_hms("9/8/2018, 2:00:00") |
           Timestamp > mdy_hms("9/8/2018, 17:45:00")) %>% 
  filter(Timestamp < mdy_hms("9/11/2018, 5:45:00") |
           Timestamp > mdy_hms("9/11/2018, 23:45:00")) %>%
  filter(Timestamp < mdy_hms("9/12/2018, 5:00:00") |
           Timestamp > mdy_hms("9/12/2018, 23:00:00")) 

#interpolate between weird periods
interpfun<-approxfun(updated$Timestamp, updated$waterDepth)  
updated<-tibble(
  Timestamp = waterDepth$Timestamp,
  waterDepth = interpfun(waterDepth$Timestamp))

#Inspect output
dygraph_QAQC_fun(waterDepth, historic=NULL, updated)

#print~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add info to 'updated' dataframe
updated$Site_Name = site
updated$processing_level = 'processed'

#Append master dataframe
df<-bind_rows(df, updated)

#5.6 EP-C-----------------------------------------------------------------------
#Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site<-"EP-C"
waterDepth <- df %>% filter(Site_Name==site) %>% select(Timestamp, waterDepth)
dygraph_QAQC_fun(waterDepth)

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove bogus point
updated<-waterDepth %>% 
  filter(Timestamp!=mdy_hm("9/6/2018 14:45"))

#Add rolling average
updated<-updated %>% 
  mutate(waterDepth = rollmean(waterDepth, k=5, fill=NA))

#remove weird periods
updated<-updated %>% 
  filter(Timestamp < mdy_hms("9/8/2018, 2:00:00") |
           Timestamp > mdy_hms("9/8/2018, 17:45:00")) %>% 
  filter(Timestamp < mdy_hms("9/11/2018, 5:45:00") |
           Timestamp > mdy_hms("9/11/2018, 23:45:00")) %>%
  filter(Timestamp < mdy_hms("9/12/2018, 5:00:00") |
           Timestamp > mdy_hms("9/12/2018, 23:00:00")) 

#interpolate between weird periods
interpfun<-approxfun(updated$Timestamp, updated$waterDepth)  
updated<-tibble(
  Timestamp = waterDepth$Timestamp,
  waterDepth = interpfun(waterDepth$Timestamp))

#Inspect output
dygraph_QAQC_fun(waterDepth, historic=NULL, updated)

#print~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add info to 'updated' dataframe
updated$Site_Name = site
updated$processing_level = 'processed'

#Append master dataframe
df<-bind_rows(df, updated)

#6.7 GR-A-----------------------------------------------------------------------
#Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site<-"GR-A"
waterDepth <- df %>% filter(Site_Name==site) %>% select(Timestamp, waterDepth)
dygraph_QAQC_fun(waterDepth)

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add rolling average
updated<-waterDepth %>% 
  mutate(waterDepth = rollmean(waterDepth, k=5, fill=NA))

#remove weird periods
updated<-updated %>% 
  filter(Timestamp < mdy_hms("9/7/2018, 23:00:00") |
           Timestamp > mdy_hms("9/8/2018, 17:45:00")) %>% 
  filter(Timestamp < mdy_hms("9/11/2018, 5:45:00") |
           Timestamp > mdy_hms("9/11/2018, 23:45:00")) %>%
  filter(Timestamp < mdy_hms("9/12/2018, 5:00:00") |
           Timestamp > mdy_hms("9/12/2018, 23:00:00")) 

#interpolate between weird periods
interpfun<-approxfun(updated$Timestamp, updated$waterDepth)  
updated<-tibble(
  Timestamp = waterDepth$Timestamp,
  waterDepth = interpfun(waterDepth$Timestamp))

#Inspect output
dygraph_QAQC_fun(waterDepth, historic=NULL, updated)

#print~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add info to 'updated' dataframe
updated$Site_Name = site
updated$processing_level = 'processed'

#Append master dataframe
df<-bind_rows(df, updated)

#6.8 GR-B-----------------------------------------------------------------------
#Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site<-"GR-B"
waterDepth <- df %>% filter(Site_Name==site) %>% select(Timestamp, waterDepth)
dygraph_QAQC_fun(waterDepth)

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add rolling average
updated<-waterDepth %>% 
  mutate(waterDepth = rollmean(waterDepth, k=5, fill=NA))

#remove weird periods
updated<-updated %>% 
  filter(Timestamp < mdy_hms("9/7/2018, 23:00:00") |
           Timestamp > mdy_hms("9/8/2018, 17:45:00")) %>% 
  filter(Timestamp < mdy_hms("9/11/2018, 5:45:00") |
           Timestamp > mdy_hms("9/11/2018, 23:45:00")) %>%
  filter(Timestamp < mdy_hms("9/12/2018, 5:00:00") |
           Timestamp > mdy_hms("9/12/2018, 23:00:00")) 

#interpolate between weird periods
interpfun<-approxfun(updated$Timestamp, updated$waterDepth)  
updated<-tibble(
  Timestamp = waterDepth$Timestamp,
  waterDepth = interpfun(waterDepth$Timestamp))

#Inspect output
dygraph_QAQC_fun(waterDepth, historic=NULL, updated)

#print~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add info to 'updated' dataframe
updated$Site_Name = site
updated$processing_level = 'processed'

#Append master dataframe
df<-bind_rows(df, updated)

#6.9 GR-C-----------------------------------------------------------------------
#Organize data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
site<-"GR-C"
waterDepth <- df %>% filter(Site_Name==site) %>% select(Timestamp, waterDepth)
dygraph_QAQC_fun(waterDepth)

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add rolling average
updated<-waterDepth %>% 
  mutate(waterDepth = rollmean(waterDepth, k=5, fill=NA))

#remove weird periods
updated<-updated %>% 
  filter(Timestamp < mdy_hms("9/7/2018, 23:00:00") |
           Timestamp > mdy_hms("9/8/2018, 17:45:00")) %>% 
  filter(Timestamp < mdy_hms("9/11/2018, 5:45:00") |
           Timestamp > mdy_hms("9/11/2018, 23:45:00")) %>%
  filter(Timestamp < mdy_hms("9/12/2018, 5:00:00") |
           Timestamp > mdy_hms("9/12/2018, 23:00:00")) 

#interpolate between weird periods
interpfun<-approxfun(updated$Timestamp, updated$waterDepth)  
updated<-tibble(
  Timestamp = waterDepth$Timestamp,
  waterDepth = interpfun(waterDepth$Timestamp))

#Inspect output
dygraph_QAQC_fun(waterDepth, historic=NULL, updated)

#print~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add info to 'updated' dataframe
updated$Site_Name = site
updated$processing_level = 'processed'

#Append master dataframe
df<-bind_rows(df, updated)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 6: Print-----------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
df<-df %>% select(Timestamp, Site_Name, waterDepth, processing_level) %>% filter(processing_level == 'processed') %>% select(-processing_level)
write_csv(df, paste0(data_dir,"20180919_waterLevel.csv"))
