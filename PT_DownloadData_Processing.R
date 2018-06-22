####################################################################################
#Name: Initial PT Data Processing
#Coder: C. Nathan Jones
#Date: 6/20/2018
#Purpose: Organize raw pressure data
####################################################################################

#Notes
#Step 1: Create folder with downloaded files and "well_log.csv"
#Step 2: Define folder as wd
#Step 3: Run Sections 1 & 2. Look for errors.


#To do: 
#1) Add units check to fun in section 2 adn section 3 (baro)
#2) Define barro logger, calculate water level
#2) Check offset between downloads (maybe create some sort of output or checksheet?)
#4) Create VBA script to export csv for specific date from well log file
#5) Automate Baro logger download in section 3 (time offset)

####################################################################################
# 1. Setup Worskspace ---------------------------------------------------------
####################################################################################
#Clear Memory
rm(list=ls(all=TRUE))

#Define Working Directory
setwd("Z:\\Research Projects/Delmarva_Carbon/Pressure_Transducers/Download_Data/20171020_Downloads")

#Load Required Packages
require("dplyr")         #data processing

#Download well log file
master<-read.csv("well_log.csv")
master<-na.omit(master)

####################################################################################
# 2. Initial Cleaning  --------------------------------------------------------
####################################################################################
#create function download logs, clean, and rewrite in temp folder
fun<-function(n){
  #Read data
  temp<-read.csv(logs[n], skip=1)
  
  #Determine serial number
  serial_number<-colnames(temp)[grep("LGR",colnames(temp))][1]  #Find collumn name with serial number
  serial_number<-substr(serial_number,   #isolate serial number
                        gregexpr("SEN.S.N",serial_number)[[1]][1]+9, #Start
                        nchar(serial_number)-1) #stop
  serial_number<-as.numeric(serial_number) #return numeric data type
  
  #Determine timezone offset in seconds
  time_offset<-colnames(temp)[grep("GMT",colnames(temp))]  #Grab collumn name w/ time offset
  time_offset<-as.numeric(substr(time_offset, 16,18))*3600+as.numeric(substr(time_offset, 19,20))*60
  
  #determine pressure and temp units. If wrong, throw error [for now...]
  
  #create temp collumn [for now this is static, we may need to use collumn search similar to above..]
  temp<-temp[,c(2,3,4)]
  colnames(temp)<-c("DateTime_GMT", "abs_pres_kPa", "temp_c")
  
  #format date_time
  temp$DateTime_GMT<-strptime(temp$DateTime_GMT, "%m/%d/%y %I:%M:%S %p")-time_offset
  temp$DateTime_GMT<-format.Date(temp$DateTime_GMT, "%m/%d/%Y %H:%M:%S")
  
  #Check for existing files
  if(file.exists(paste0("intermediate/",serial_number,".csv"))==TRUE){
    warning(paste0("Multiple files exists for ",serial_number," : ", logs[n]), immediate.=T)
    stop()
  }
  
  #Export csv file
  write.csv(temp, paste0("intermediate/",serial_number,".csv"))
  
  #exprort serial number for completion check
  c(serial_number, 1)
  
}

#Create list of files
logs<-list.files()[list.files()!="well_log.csv" & list.files()!="intermediate" & list.files()!="Thumbs.db"  ]

#Create temp file
unlink("intermediate", recursive = T)
Sys.sleep(0.5)
dir.create("intermediate")

#Run function 
output<-lapply(seq(1, length(logs)), fun)

#Check to see if missing any wells
output<-do.call(rbind, output)
output<-data.frame(output)
colnames(output)<-c("Sonde_ID", "Initial_Check")
master<-left_join(master, output, by="Sonde_ID")
master[is.na(master$Initial_Check),]

####################################################################################
# 3. Calculate water depth----------------------------------------------------------
####################################################################################
#Create function to estimate water depth
fun<-function(n){
  #define PT serioal number
  serial_number<-as.numeric(paste(substr(logs[n],1, nchar(logs[n])-4)))
  site<-master$Site.Name[master$Sonde_ID==serial_number]
  
  #Read PT file
  df<-read.csv(paste0("intermediate/",logs[n]))
  df$DateTime_GMT<-strptime(df$DateTime_GMT, "%m/%d/%Y %H:%M:%S")
  df$DateTime_GMT<-as.POSIXct(df$DateTime_GMT)
  
  #Create baro file (need to automate this a little more...see automation steps in section 2)
  baro<-master$baro_file[master$Sonde_ID==serial_number]
  baro<-read.csv(paste(baro), skip=1)[,c(2,3)]
  colnames(baro)<-c("DateTime_GMT", "atm_pressure_kPa")
  baro$DateTime_GMT<-strptime(baro$DateTime_GMT, "%m/%d/%y %I:%M:%S %p")-(4*3600)
  baro$DateTime_GMT<-as.POSIXct(baro$DateTime_GMT)
  
  #Create interpolation function for baro file
  baro_fun<-approxfun(baro$DateTime_GMT, baro$atm_pressure_kPa)
  
  #Add barometric pressure to df
  df$atm_pres_kPa<-baro_fun(df$DateTime_GMT)
  
  #Estimate gage pressure
  df$gage_pres_kPa<-df$abs_pres_kPa-df$atm_pres_kPa
  
  #Estimate pressure head [m]
  df$level_m<-df$gage_pres_kPa/9.81
  
  #Export df
  df<-df[,c("DateTime_GMT","abs_pres_kPa","level_m","temp_c")]
  write.csv(df,paste0("initial_processing/",serial_number,".csv"))
  
  #exprort serial number for completion check
  c(serial_number, 1)
}

#Create list of PT files 
logs<-list.files("intermediate/")

#Create temp file
unlink("initial_processing", recursive = T)
Sys.sleep(0.5)
dir.create("initial_processing")

#Run function 
output<-lapply(seq(1, length(logs)), fun)
