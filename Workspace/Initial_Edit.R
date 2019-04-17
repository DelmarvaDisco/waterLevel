####################################################################################
# Name: Initial PT Data Processing
# Coder: C. Nathan Jones
# Date: 16 April 2019
# Purpose: Process PT Data collected across the Palmer Lab Delmarva wetland sites
####################################################################################

####################################################################################
# Step 1: Setup Worskspace ---------------------------------------------------------
####################################################################################
#clear environment
remove(list=ls())

#load relevant packages
library(parallel)
library(devtools)
devtools::install_github("khondula/rodm2")
source("functions/db_get_ts.R")
library(RSQLite)
library(DBI)
library(rodm2)
library(lubridate)
library(tidyverse)

#Define working dir
working_dir<-"//nfs/palmer-group-data/Choptank/Nate/PT_Data/"

####################################################################################
# Step 2: Setup Database -----------------------------------------------------------
####################################################################################
#2.1 Create dabase~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create SQLight databse (only do this once)
db <- create_sqlite(dir = working_dir, filename = "test", connect = T)

#2.2 Insert equipment iunformation~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Load existing equipment table
equipment<-read.csv(paste0(working_dir,"/Database Information/equipment.csv"))

#Create wrapper function for the db_describe_equipment
fun<-function(n){db_describe_equipment(db, 
                                       equip_name    =   as.character(equipment$serial_no[n]), 
                                       serial_no     =   equipment$serial_no[n],
                                       model_name    =   "U20 Pressure Transducer",
                                       vendor        =   "Onset",
                                       manufacturer  =   "HOBO",
                                       equipment_type=   "pressureTransducer",
                                       owner_first   =   "Margaret",
                                       owner_last    =   "Palmer",
                                       owner_email   =   "mpalmer@sesync.org")}

#intiate function
lapply(seq(1, length(equipment[,1])), fun)

#2.3 Insert information about sites~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Load existing information about sites
sites<-read.csv(paste0(working_dir,"/Database Information/sites.csv"))

#Create wrapper function for the db_describe_site function
fun<-function(n){db_describe_site(db, site_code = sites$site_code[n])}

#Inititate function
lapply(seq(1, length(sites[,1])), fun)

#2.4 Insert information about methods~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
db_describe_method(db, 
                   methodname = "pt", 
                   methodcode = "pt",
                   methodtypecv = "Instrument deployment",
                   methoddescription = "pressure transducer")

db_describe_method(db, 
                   methodname = "baro", 
                   methodcode = "baro",
                   methodtypecv = "Instrument deployment",
                   methoddescription = "barometric logger")

db_describe_method(db, methodname = "waterdepth", methodcode = "waterdepth",
                   methodtypecv = "Derivation",
                   methoddescription = "Calculate water depth from baro and pt")

#2.5 Describe variables~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Barometric Pressure (from baro logger)
db_describe_variable(db, 
                     variabletypecv = "Hydrology",
                     variablecode   = "barometricPressure",
                     variablenamecv = "barometricPressure")

#Absolute Pressure
db_describe_variable(db, 
                     variabletypecv = "Hydrology",
                     variablecode   = "pressureAbsolute",
                     variablenamecv = "pressureAbsolute")

#Gage Pressure
db_describe_variable(db, 
                     variabletypecv = "Hydrology",
                     variablecode   = "pressureGauge",
                     variablenamecv = "pressureGauge")

#Water Collumn height [no datum] 
db_describe_variable(db, 
                     variabletypecv = "Hydrology",
                     variablecode   = "waterColumnEquivalentHeightAbsolute",
                     variablenamecv = "waterColumnEquivalentHeightAbsolute")

#Water Collumn Depth 
db_describe_variable(db, 
                     variabletypecv = "Hydrology",
                     variablecode   = "gageHeight",
                     variablenamecv = "gageHeight")

#Offset (to correct water level to water depth)
db_describe_variable(db, 
                     variabletypecv = "Hydrology",
                     variablecode   = "offset",
                     variablenamecv = "offset")

#Water Depth (ground surface = 0, positive values indicate inundation)
db_describe_variable(db, 
                     variabletypecv = "Hydrology",
                     variablecode   = "waterLevel",
                     variablenamecv = "waterLevel")


####################################################################################
# Step 3: Create file lookup table--------------------------------------------------
####################################################################################
#Identify files with downloads
files<-list.files(working_dir, recursive = T)
files<-files[substr(files,nchar(files)-2,nchar(files))=="csv"] 

#Remove non-relevant files 
files<-files[-grep(files,pattern = 'archive')]
files<-files[-grep(files,pattern = 'Database Information')]
files<-files[-grep(files,pattern = 'intermediate')]
files<-files[-grep(files,pattern = 'spring')]
files<-files[-grep(files,pattern = 'precip')]

#Create function to retrieve info from each file
file_fun<-function(n){
  
  #Download data
  temp<-read_csv(paste0(working_dir,files[n]), skip=1)

  #Determine serial number
  serial_number<-colnames(temp)[grep("LGR",colnames(temp))][1]  #Find collumn name with serial number
  serial_number<-substr(serial_number,   #isolate serial number
                        gregexpr("SEN.S.N",serial_number)[[1]][1]+9, #Start
                        nchar(serial_number)-1) #stop
  serial_number<-as.numeric(serial_number) 
  
  #Organize
  colnames(temp)<-c("ID","Timestamp","pressureAbsolute", "temp")
  temp<-temp[,c("Timestamp","pressureAbsolute", "temp")]
  temp<-temp %>% 
    dplyr::select(Timestamp, pressureAbsolute, temp) %>%
    dplyr::mutate(Timestamp = as.POSIXct(strptime(Timestamp, "%m/%d/%y %I:%M:%S %p"))) %>%
    dplyr::arrange(Timestamp) 
  
  #create output
  tibble(path       = files[n], 
         pt_id      = serial_number,
         start_date = min(temp$Timestamp), 
         end_date   = max(temp$Timestamp))
}

#run function
files<-mclapply(X = seq(1,length(files)), FUN = file_fun)
files<-bind_rows(files)

#Clean up workspace
remove(list=ls()[ls()!='working_dir' &
                   ls()!='db' &
                   ls()!='files'])

####################################################################################
# Step 3: Estimate barometric pressure----------------------------------------------
####################################################################################
#Define Baro Loggers
baro_id<-c("10589038", #JR Baro
           "10808360") #JL Baro

#Define Baro Logger Files
baro_files<-files[files$pt_id %in% baro_id,]

#Download all files, then only keep files needed
baro_fun<-function(n){
  
  #Download data
  temp<-read_csv(paste0(working_dir,baro_files$path[n]), skip=1)
  
  #Determine timezone offset in seconds
  time_offset<-colnames(temp)[grep("GMT",colnames(temp))]  #Grab collumn name w/ time offset
  time_offset<-substr(time_offset, 
                      regexpr('GMT', time_offset)[1]+4,
                      nchar(time_offset)-3)
  time_offset<-as.numeric(paste(time_offset))*3600
  
  #Organize
  colnames(temp)<-c("ID","Timestamp","barometricPressure", "temp")
  temp<-temp[,c("Timestamp","barometricPressure", "temp")]
  temp<-temp %>% 
    dplyr::select(Timestamp, barometricPressure, temp) %>%
    dplyr::mutate(Timestamp = as.POSIXct(strptime(Timestamp, "%m/%d/%y %I:%M:%S %p"))) %>%
    dplyr::mutate(Timestamp = Timestamp - time_offset) %>%
    dplyr::arrange(Timestamp) 
  
  #Add serial number
  temp$pt_id<-baro_files$pt_id[n]
  
  #Export temp
  temp
}
  
#run function
baro<-mclapply(X = seq(1,nrow(baro_files)), FUN = baro_fun)
baro<-bind_rows(baro)

#Organize barometric pressure
baro<-baro %>%
  #remove duplicate records
  group_by(Timestamp, pt_id) %>%
  summarise(barometricPressure = mean(barometricPressure), 
            temp = mean(temp)) %>%
  #average baro records 
  group_by(Timestamp) %>%
  summarise(barometricPressure = mean(barometricPressure), 
            temp = mean(temp)) %>%
  na.omit()

#Insert barometric pressure data into db
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = baro,
                            method = "baro",
                            site_code = "BARO",
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            #equipment_name = "10808360",
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "barometricPressure" = list(column = "barometricPressure", units = "Kilopascal"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#Clean up workspace
remove(list=ls()[ls()!='working_dir' &
                 ls()!='db' &
                 ls()!='files'])

####################################################################################
# Step 4: Insert time series--------------------------------------------------------
####################################################################################
#Create well lookup table
wells<-list.files(working_dir, recursive = T)
wells<-wells[grep(wells,pattern = 'well_log')]
wells<-wells[-grep(wells,pattern = 'archive')]

#Create function to download well log files
log_fun<-function(n){
  
  #Download well log
  temp<-read_csv(paste0(working_dir,wells[n]))
  
  #export temp
  temp
}

#run function
wells<-mclapply(X = seq(1,length(wells)), FUN = log_fun)
wells<-bind_rows(wells)


#Thnigs to do next --------------
#Fix Well log collum names
#Create Date + Time Collumn
#Convert to GMT
#Download atmospheric pressure data for each well
#upload into database for each well
#Calculate water height
#Put water height into database
#Esitmate offset for each time period
#Estimate offset to survey & with measured data [together???]

