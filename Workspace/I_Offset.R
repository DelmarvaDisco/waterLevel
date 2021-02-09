#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: offset 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date:  2/9/2021
#Purpose: Estimate offset to estimate waterLevel from waterDepth
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2 Compile table of field log ---------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logs<-list.files("data//", recursive = T, full.names = T)
logs<-logs[str_detect(logs, "well_log")]
logs<-lapply(FUN = read_csv, X=logs) %>% bind_rows()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 3: Create lookup table for PT data --------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 4: Create function to look up gage pressure for each log entry ----------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 5: Curate baro logger data for period of record---------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  mutate(Timestamp = ceiling_date(Timestamp), 
         Timestamp = as_date(Timestamp)) %>% 
  group_by(Timestamp, Sonde_ID) %>% 
  summarise(pressureAbsolute = mean(pressureAbsolute, na.rm=T)) %>% 
  #rename baro collumn
  rename(barometricPressure=pressureAbsolute) %>%
  #remove duplicate records from same sonde
  distinct(.) %>%
  #Remove na's
  na.omit()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 6: Estimate offset ------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Some quick notes aobut our definition of offset
#   waterLevel = waterHeight + offset
#   offset = waterLevel - waterHeight

#Estiamte output
output<-logs %>% 
  #Join relevant tables
  mutate(Timestamp = mdy(Date)) %>% 
  left_join(., baro) %>% 
  left_join(., pressure) %>% 
  #Estimate waterHeight above sonde
  mutate(waterHeight = (pressureAbsolute  - barometricPressure)*0.101972) %>% 
  #estimate offset
  rename(waterLevel = Relative_Water_Level_m) %>% 
  mutate(offset = waterLevel - waterHeight) %>% 
  #select cols of interest
  select(Site_Name, Date, Sonde_ID, Timestamp, offset)

#export output
write_csv(output, "data/Database Information/offset.csv")
  
  
















































