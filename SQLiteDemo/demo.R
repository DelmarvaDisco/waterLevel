####################################################################################
# Name: SQLite Database Demo
# Coder: C. Nathan Jones
# Date: 15 April 2019
# Purpose: Demo water level editing workflow with SQLite database and rodm2 schema
####################################################################################

####################################################################################
# Step 1: Setup Worskspace ---------------------------------------------------------
####################################################################################
#clear environment
remove(list=ls())

#load relevant packages
library(devtools)
devtools::install_github("khondula/rodm2")
source("functions/db_get_ts.R")
library(RSQLite)
library(DBI)
library(rodm2)
library(lubridate)
library(tidyverse)

#Define working dir
working_dir<-"//nfs/palmer-group-data/Choptank/Nate/PT_Data"

#Create SQLight databse (only do this once)
db <- create_sqlite(dir = working_dir, filename = "test", connect = T)

####################################################################################
# Step 2: Setup Database -----------------------------------------------------------
####################################################################################
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

#2.6 Disconnect from db~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#RSQLite::dbDisconnect(db)

####################################################################################
# Step 3: Demo water level upload and editing!!!  ----------------------------------
####################################################################################
#3.1 Download data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define data directory
data_dir<-paste0(working_dir, "/20171020_Downloads/export/")

#baro data
baro<-read_csv(paste0(data_dir, "baro.csv"), skip=1) 
colnames(baro)<-c("row","Timestamp","barometricPressure", "temp", "notes", "notes2")
baro<-baro %>% 
  dplyr::select(Timestamp, barometricPressure, temp) %>%
  dplyr::mutate(Timestamp = as.POSIXct(strptime(Timestamp, "%m/%d/%y %I:%M:%S %p"))) %>%
  dplyr::arrange(Timestamp) 

#water level data
level<-read_csv(paste0(data_dir,"Bubbly_Bay_10-20-17.csv"), skip=1)
colnames(level)<-c("row","Timestamp","pressureAbsolute", "temp", "notes", "notes2", "notes3", "notes4", "notes5")
level<-level %>% 
  dplyr::select(Timestamp, pressureAbsolute, temp) %>%
  dplyr::mutate(Timestamp = as.POSIXct(strptime(Timestamp, "%m/%d/%y %I:%M:%S %p"))) %>%
  na.omit()

#3.2 Insert raw data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Insert atmospheric pressure data
rodm2::db_insert_results_ts(db = db,
                            datavalues = baro,
                            method = "baro",
                            site_code = "BB Wetland Well Shallow",
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            #equipment_name = "10808360",
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "barometricPressure" = list(column = "barometricPressure", units = "Kilopascal"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))

#insert absolute pressure data
rodm2::db_insert_results_ts(db = db,
                            datavalues = level,
                            method = "pt",
                            site_code = "BB Wetland Well Shallow",
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            #equipment_name = "10808360",
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "pressureAbsolute" = list(column = "pressureAbsolute", units = "Kilopascal"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))

#Estimate water level~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#query database for water pressure and baro data
baro<-db_get_ts(db = db, 
                site_code = "BB Wetland Well Shallow",
                variable_code_CV = "barometricPressure",
                start_datetime = min(level$Timestamp), 
                end_datetime = max(level$Timestamp)) 

level<-db_get_ts(db = db, 
                 site_code = "BB Wetland Well Shallow",
                 variable_code_CV = "pressureAbsolute",
                 start_datetime = min(level$Timestamp), 
                 end_datetime = max(level$Timestamp)) 

#Calculate gage height
baro_fun<-approxfun(baro)  
level_fun<-approxfun(level)  
df<-data.frame(Timestamp = seq(ymd_hms(min(level$Timestamp)),ymd_hms(max(level$Timestamp)), by = '15 mins')) 
df$barometricPressure <- baro_fun(df$Timestamp)
df$pressureAbsolute   <- level_fun(df$Timestamp)
df$pressureGauge      <- df$pressureAbsolute - df$barometricPressure
df$gageHeight<-df$pressureGauge*0.101974 
df<-na.omit(df)

#Insert into the database ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#insert absolute pressure data
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = "BB Wetland Well Shallow",
                            processinglevel = "Derived Data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            #equipment_name = "10808360",
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "pressureGauge" = list(column = "pressureGauge", units = "Kilopascal"),
                              "gageHeight"    = list(column = "gageHeight",    units = "Meter")))

