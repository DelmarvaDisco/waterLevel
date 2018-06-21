####################################################################################
#Name: Paralells Demo
#Coder: C. Nathan Jones
#Date: 6/20/2018
#Purpose: Code to help Jake get aquainted with parallels
####################################################################################

#Notes
#Step 1: Create folder with downloaded files and "well_log.csv"
#Step 2: Define folder as wd
#Step 3: Run Sections 1 & 2. Look for errors.


#To do: 
#1) Add units check to fun in section 2

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
# Step 2: Initial Cleaning  --------------------------------------------------------
####################################################################################
#Create list of files
logs<-list.files()[list.files()!="well_log.csv" & list.files()!="temp" ]

#Create temp file
dir.create("temp")

#create function download logs, clean, and rewrite in temp folder
   #Errors to add -  1. Look for units
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
  colnames(temp)<-c("DateTime_GMT", "abs_pres_kpa", "temp_c")
  
  #format date_time
  temp$DateTime_GMT<-strptime(temp$DateTime_GMT, "%m/%d/%y %I:%M:%S %p")-time_offset
  temp$DateTime_GMT<-format.Date(temp$DateTime_GMT, "%m/%d/%Y %H:%M:%S")
  
  #Check for existing files
  if(file.exists(paste0("temp/",serial_number,".csv"))==TRUE){
    warning(paste0("Multiple files exists for ",serial_number," : ", logs[n]), immediate.=T)
    stop()
  }
  
  #Export csv file
  write.csv(temp, paste0("temp/",serial_number,".csv"))
  
  #exprort serial number for completion check
  c(serial_number, 1)
  
}

#Run function 
output<-lapply(seq(1, length(logs)), fun)

#Check to see if missing any wells
output<-do.call(rbind, output)
output<-data.frame(output)
colnames(output)<-c("Sonde_ID", "Initial_Check")
master<-left_join(master, output, by="Sonde_ID")
master[is.na(master$Initial_Check),]

