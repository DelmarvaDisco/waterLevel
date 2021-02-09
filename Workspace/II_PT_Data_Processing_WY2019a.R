#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Initial PT Data Processing
# Coder: C. Nathan Jones
# Date: 7 July 2019
# Purpose: Process PT Data collected across the Palmer Lab Delmarva wetland sites
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Known Issues
#Missing data at BB [dead sonde :(]
#Looks like JB Wetland well died after Dec 10
#Missing data after September for Solute Catchment outlet


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#load relevant packages
library(xts)
library(dygraphs)
library(parallel)
library(devtools)
devtools::install_github("khondula/rodm2")
library(RSQLite)
library(DBI)
library(rodm2)
library(zoo)
library(lubridate)
library(readxl)
library(tidyverse)

#Read custom R functions
funs<-list.files("functions/", full.names = T)
for(i in 1:length(funs)){source(funs[i]);print(paste(i,"-", funs[i]))}

#Define working dir
working_dir<-"data//"

#Set system time zone 
Sys.setenv(TZ="America/New_York")

#Define database connection
db<-dbConnect(RSQLite::SQLite(),"data//choptank.sqlite")

#Download survey data 
survey<-read_csv(paste0(working_dir,"survey_data/survey_V1.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Create lookup table for PT data files--------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Compile a list of file paths-----------------------------------------------
#Identify files with HOBO export files
files<-list.files(working_dir, recursive = T)
files<-files[substr(files,nchar(files)-2,nchar(files))=="csv"] 
files<-files[grep(files,pattern = "export")]

#Select files to process
files<-files %>% 
  enframe(name = NULL) %>% 
  filter(str_detect(value,"20181011") |
         str_detect(value,"20181101") |
         str_detect(value,"20181113") |
         str_detect(value,"20190315") |
         str_detect(value,"20190406") |  
         str_detect(value,"20190423") ) %>%
  filter(!str_detect(value, "archive")) %>%
  as.matrix(.)

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
files<-lapply(X = seq(1,length(files)), FUN = file_fun)
files<-bind_rows(files)

#2.2 Compile well log information-----------------------------------------------
#2.2.a Gather Information-------------------------------------------------------
#Create list of file paths
wells<-list.files(working_dir, recursive = T)
wells<-wells[grep(wells,pattern = 'well_log')]
#wells<-wells[-grep(wells,pattern = 'archive')]

#Select files to process
wells<-wells %>% 
  enframe(name = NULL) %>% 
  filter(str_detect(value,"20181113") |
           str_detect(value,"20190315") |
           str_detect(value,"20190406") |  
           str_detect(value,"20190423") ) %>%
  filter(!str_detect(value, "archive")) %>%
  as.matrix(.)

#Create function to download well log files
log_fun<-function(n){
  
  #Download well log
  temp<-read_csv(paste0(working_dir,wells[n]))
  
  #export temp
  temp
}

#run function
wells<-lapply(X = seq(1,length(wells)), FUN = log_fun)
wells<-bind_rows(wells)

#2.2.b Check well logs for potential errors ------------------------------------
#Create f(x) to scan for input Sonde_IDs that don't match values in DB~~~~~~~~~~
check_fun<-function(n){
  #identify Site_Name
  site_name<-wells$Site_Name[n]
  
  #pull values from database
  df<-db_get_equip_by_site(db, site_name, "waterDepth") %>%
    rename(Site_Name = site_code, 
           Sonde_ID = EquipmentName)
  
  #prep wells id vals
  temp<-wells %>% 
    select(Site_Name, Sonde_ID, Date) %>% 
    mutate(Sonde_ID=paste(Sonde_ID)) %>%
    filter(Site_Name==site_name)
  
  #Query values not in db
  output<-temp[!(temp$Sonde_ID %in% df$Sonde_ID), ]
}

#apply function
problems<-lapply(seq(1, nrow(wells)), check_fun) %>% bind_rows %>%
  #Remove baro loggers
  filter(!str_detect(Site_Name,'Baro')) %>%
  #Remove Wetlnd with sonde replacements
  filter(Site_Name!='JB Wetland Well Shallow') %>%
  filter(Site_Name!='FN Wetland Well Shallow') %>%
  filter(Site_Name!='BB Wetland Well Shallow') %>%
  filter(Site_Name!='DK Wetland Well Shallow') %>%
  filter(Site_Name!='JU Wetland Well Shallow') 
  
#Search line by line to mannualy correct problems in well log files
# problems[1,]
# db_get_equip_by_site(db, problems$Site_Name[1], "waterDepth")

#Make sure sites in well log exist in db~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
wells$Site_Name[!(wells$Site_Name %in% db_get_sites(db))]

#2.2.c Merge well log with file log---------------------------------------------
#Convert times to GMT
wells<-wells %>%
  #Define Timestamp
  mutate(Timestamp = mdy(Date)) %>%
  #Convert to POSIXct and GMT
  mutate(Timestamp = as.POSIXct(Date, format = "%m/%d/%Y", tz = "America/New_York")) %>%
  mutate(Timestamp = with_tz(Timestamp, "GMT")) %>%
  #Add time
  mutate(Timestamp = Timestamp + Time) %>%
  #Idenitfy download date
  mutate(download_date = lubridate::date(Timestamp), 
         download_datetime = Timestamp) %>%
  #Select relevant collumsn
  select(Site_Name, Sonde_ID, download_date, download_datetime, Relative_Water_Level_m)

#Prep files df to join
files <- files %>%
  #estimate download date
  mutate(download_date = date(end_date))

#join to master lookup table!
wells<-left_join(wells, files) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Compile Barometric Pressure Logger Info------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Gather data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Define Baro Loggers
baro_id<-c("10589038", #QB Baro
           "10808360") #GR Baro

#Define Baro Logger Files
baro_files<-wells[wells$Sonde_ID %in% baro_id,] %>% filter(!is.na(path))

#run function
baro<-mclapply(X = paste0(working_dir,baro_files$path), FUN = download_fun, mc.cores = detectCores()) %>% bind_rows()

#Organize barometric pressure
baro<-baro %>%
  #rename baro collumn
  rename(barometricPressure=pressureAbsolute) %>%
  #remove duplicate records from same sonde
  distinct(.) %>%
  #Remove na's
  na.omit()

#Manual edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#Combine datasets and upload!~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create seperate collumns for each logger
baro<-baro %>%
  select(Timestamp, barometricPressure, Sonde_ID) %>% 
  group_by(Timestamp, Sonde_ID) %>%
  summarise(barometricPressure = mean(barometricPressure)) %>%
  spread(Sonde_ID, barometricPressure)  %>%
  arrange(Timestamp) %>%  
  rename(QB_Baro=`10589038`, GR_Baro=`10808360`) 

#plot with dygraphs
dygraph_ts_fun(baro)

#Combine data from both baro loggers
baro<-baro %>% 
  mutate(Timestamp_15min = ceiling_date(Timestamp, "15 min")) %>% 
  group_by(Timestamp_15min) %>% 
  summarise(QB_Baro = mean(QB_Baro, na.rm=T), 
            GR_Baro = mean(GR_Baro, na.rm=T)) %>%
  mutate(barometricPressure = rowMeans(select(.,QB_Baro, GR_Baro), na.rm=T)) %>%
  rename(Timestamp=Timestamp_15min)

#DB Upload~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
                              "barometricPressure" = list(column = "barometricPressure", units = "Kilopascal")))
tf<-Sys.time()
tf-t0

#Clean up workspace
remove(list=ls()[ls()!='working_dir' &
                   ls()!='db' &
                   ls()!='files' & 
                   ls()!='survey' & 
                   ls()!='wells'])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Estimate Shallow Ground Water Level ---------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read custom R functions
funs<-list.files("functions/", full.names = T)
for(i in 1:length(funs)){source(funs[i]);print(paste(i,"-", funs[i]))}

#Create list of sites
site_names<-unique(wells$Site_Name[grep("Upland",wells$Site_Name)])
site_names<-site_names[order(site_names)]
site_names[grep("Upland", site_names)]

#4.1 BB Wetland Well Shallow--------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"BB Upland Well 1"
survey_temp<-survey %>% filter(Wetland == "BB")

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Make Depth Calculations~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Combine with previous ts
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/2018"), mdy('10/30/2018'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, 
                             waterLevel = df$waterLevel))
#Plot
dygraph_ts_fun(test %>% 
                 mutate(waterLevel=waterLevel*100+1000) %>%
                 select(Timestamp, waterLevel))
#Remove NA 
df<-na.omit(df)

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.2 DB Upland Well 2-----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"DB Upland Well 1"
survey_temp<-survey %>% filter(Wetland == "DB")

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Combine with previous ts
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/2018"), mdy('10/30/2018'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, 
                             waterLevel = df$waterLevel))
#Plot
dygraph_ts_fun(test %>% 
                 mutate(waterLevel=waterLevel*100+1000) %>%
                 select(Timestamp, waterLevel))

#Remove NA 
df<-na.omit(df)

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.3 DK Upland Well 1-----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"DK Upland Well 1"
survey_temp<-survey %>% filter(Wetland == "DK")

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Combine with previous ts
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/2018"), mdy('10/30/2018'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, 
                             waterLevel = df$waterLevel))
#Plot
dygraph_ts_fun(test %>% 
                 mutate(waterLevel=waterLevel*100+1000) %>%
                 select(Timestamp, waterLevel))

#Remove NA 
df<-na.omit(df)

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.4 DK Upland Well 2-----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-'DK Upland Well 2'
survey_temp<-survey %>% filter(Wetland == 'DK')

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path[2]), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - 2`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Remove points when well was pulled 
df<-df %>% filter(Timestamp<mdy("4/6/2019"))

#Combine with previous ts
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, 
                             waterLevel = df$waterLevel))
#Plot
dygraph_ts_fun(test %>% 
                 mutate(waterLevel=waterLevel*100+1000) %>%
                 select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID[1]),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.5 GN Upland Well 1 ----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"GN Upland Well 1"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) 

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Remove NA 
df<-na.omit(df)

#Combine with previous ts
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, 
                             waterDepth = df$waterDepth))
#Plot
dygraph_ts_fun(test %>% 
                 mutate(waterDepth=waterDepth*100+1000) %>%
                 select(Timestamp, waterDepth))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.6 GR Upland Well 1-----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"GR Upland Well 1"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Combine with previous ts
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, 
                             waterDepth = df$waterDepth))
#Plot
dygraph_ts_fun(test %>% 
                 mutate(waterDepth=waterDepth*100+1000) %>%
                 select(Timestamp, waterDepth))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.7 JB Upland Well 1-----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"JB Upland Well 1"
survey_temp<-survey %>% filter(Wetland == "JB")

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Combine with previous ts
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, 
                             waterLevel = df$waterLevel))
#Plot
dygraph_ts_fun(test %>% 
                 mutate(waterLevel=waterLevel*100+1000) %>%
                 select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.8 JB Upland Well 2 ----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<- 'JB Upland Well 2'
survey_temp<-survey %>% filter(Wetland == "JB")

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - 2`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Combine with previous ts
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, 
                             waterLevel = df$waterLevel))
#Plot
dygraph_ts_fun(test %>% 
                 mutate(waterLevel=waterLevel*100+1000) %>%
                 select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.9 JC Upland Well 1-----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-'JC Upland Well 1'
survey_temp<-survey %>% filter(Wetland == 'JC')

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.10 ND Upland Well 1----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"ND Upland Well 1"
survey_temp<-survey %>% filter(Wetland == 'ND')

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='download'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.11 ND Upland Well 2----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"ND Upland Well 2"
survey_temp<-survey %>% filter(Wetland == 'ND')

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - 2`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.12 QB Upland Well 1----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-'QB Upland Well 1'
survey_temp<-survey %>% filter(Wetland == 'QB')

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.13 QB Upland Well 2----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"QB Upland Well 2"
survey_temp<-survey %>% filter(Wetland == "QB")

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - 2`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.14 TB Upland Well 1----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-'TB Upland Well 1'
survey_temp<-survey %>% filter(Wetland == "TB")

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.15 TB Upland Well 2----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-'TB Upland Well 2'
survey_temp<-survey %>% filter(Wetland == 'TB')

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - 2`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#4.16 TB Upland Well 3----------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-'TB Upland Well 3'
survey_temp<-survey %>% filter(Wetland == 'TB')

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - 3`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Estimate Wetland Water Level ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read custom R functions
funs<-list.files("functions/", full.names = T)
for(i in 1:length(funs)){source(funs[i]);print(paste(i,"-", funs[i]))}

#Create list of sites
site_names<-unique(wells$Site_Name[grep("Wetland",wells$Site_Name)])
site_names<-site_names[order(site_names)]
site_names

# 5.1  BB Wetland Well Shallow--------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"BB Wetland Well Shallow"
survey_temp<-survey %>% filter(Wetland == "BB")

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth #+ depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add equipment 
db_describe_equipment(db, 
                      equip_name    =   paste(well_log$Sonde_ID),
                      serial_no     =   well_log$Sonde_ID,
                      model_name    =   "U20 Pressure Transducer",
                      vendor        =   "Onset",
                      manufacturer  =   "HOBO",
                      equipment_type=   "pressureTransducer",
                      owner_first   =   "Margaret",
                      owner_last    =   "Palmer",
                      owner_email   =   "mpalmer@sesync.org")

#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#5.2 DB Wetland Well Shallow-------------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-'DB Wetland Well Shallow'
survey_temp<-survey %>% filter(Wetland == 'DB')

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth #+ depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.3 DF Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<- "DF Wetland Well Shallow"
survey_temp<-survey %>% filter(Wetland == 'DF')

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth #+ depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.4 DK Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"DK Wetland Well Shallow"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA,# survey_temp$Date, 
  waterDepth = NA,#survey_temp$`Water Depth (m)`, 
  wellHeight = NA)#survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth # + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID[1]),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.5 DV Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<- "DV Wetland Well Shallow"
  
#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth# + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.6 FN Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"FN Wetland Well Shallow"
  
#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth # + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.7 GB Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"GB Wetland Well Shallow"
  
#Identify well info
well_log<-wells %>% filter(Site_Name==site) #%>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth  #depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID[1]),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.8 JA Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"JA Wetland Well Shallow"
  
#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth # + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0


# 5.9 JC Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"JC Wetland Well Shallow"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))


#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth # + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.10 JB Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"JB Wetland Well Shallow"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure, temp))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth # + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)
df<-df %>% filter(Timestamp<mdy("12/5/2018"))

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.11 JU Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"JU Wetland Well Shallow"
  
#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset[df$Timestamp<mdy_hm("11/13/2018 9:25")]<-mean(depths$offset[depths$event=='offset'])
df$offset[df$Timestamp>mdy_hm("11/13/2018 9:25")]<-mean(depths$offset[depths$event=='offset'])+.069

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth # + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Add equipment 
db_describe_equipment(db, 
                      equip_name    =   paste(well_log$Sonde_ID[2]),
                      serial_no     =   well_log$Sonde_ID[2],
                      model_name    =   "U20 Pressure Transducer",
                      vendor        =   "Onset",
                      manufacturer  =   "HOBO",
                      equipment_type=   "pressureTransducer",
                      owner_first   =   "Margaret",
                      owner_last    =   "Palmer",
                      owner_email   =   "mpalmer@sesync.org")

#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID[2]),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.12 NB Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"NB Wetland Well Shallow"
  
#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth #+ depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.13 ND Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"ND Wetland Well Shallow"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth #+ depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.14 QB Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"QB Wetland Well Shallow"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, ##survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth #+ depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.15 QB Wetland Well Deep---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"QB Wetland Well Deep"
  
#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth #+ depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.16 TA Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"TA Wetland Well Shallow"
  
#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth #+ depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.17 TB Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"TB Wetland Well Shallow"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth #+ depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 5.18 TC Wetland Well Shallow---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"TI Wetland Well Shallow"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth #+ depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#6.0 Estimate Catchment Outlet Water Level -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Read custom R functions
funs<-list.files("functions/", full.names = T)
for(i in 1:length(funs)){source(funs[i]);print(paste(i,"-", funs[i]))}

#Create list of sites
site_names<-unique(wells$Site_Name[grep("Catchment",wells$Site_Name)])
site_names<-site_names[order(site_names)]
site_names

# 6.1 Denver Catchment Outlet---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"Denver Catchment Outlet"
  
#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)
df$waterDepth <- df$waterDepth 

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              #"waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 6.2 Greg Catchment Outlet---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"Greg Catchment Outlet"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)
df$waterDepth <- df$waterDepth 

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              #"waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0


# 6.3 Jones Road North Catchment Outlet-----------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"Jones Road North Catchment Outlet"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='download'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)
df$waterDepth <- df$waterDepth 

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              #"waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 6.4 Jones Rd South Catchment Outlet-------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"Jones Road South Catchment Outlet"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)
df$waterDepth <- df$waterDepth 

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              #"waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 6.5 Solute Catchment Outlet---------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"Solute Catchment Outlet"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)
df$waterDepth <- df$waterDepth 

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              #"waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

# 6.6 Tiger Paw Catchment Outlet------------------------------------------------
#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Identify site and survey data
site<-"Tiger Paw Catchment Outlet"

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = NA, #survey_temp$Date, 
  waterDepth = NA, #survey_temp$`Water Depth (m)`, 
  wellHeight = NA) #survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Remove NA 
df<-na.omit(df)
df$waterDepth <- df$waterDepth 

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              #"waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#7.0 Fin---------------------------------- -------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dbDisconnect(db)
