#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Download Fun 
# Coder: C. Nathan Jones
# Date: 29 April 2019
# Purpose: Download data from HOBO export files
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#download absolute pressure data
download_fun<-function(file_path){
  #Download data
  temp<-read_csv(paste0(file_path), skip=1) %>% as_tibble()
  
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
                     "America/New_York",
                     if_else(time_zone=="GMT-05:00",
                             "America/Chicago",
                             "-9999"))

  #Determin units
  units<-colnames(temp)[grep("Abs Pres,",colnames(temp))]
  units<-substr(units,
                regexpr("Abs Pres,", units)+10,
                regexpr("Abs Pres,", units)+12)
  
  #Organize Collumns
  colnames(temp)<-c("ID","Timestamp","pressureAbsolute", "temp")
  temp<-temp[,c("Timestamp","pressureAbsolute", "temp")]
  
  #Format timezone
  if(year(strptime(temp$Timestamp[1], format = "%m/%d/%Y %H:%M"))>2000){
    temp<-temp %>%
      #Select collumns of interest
      dplyr::select(Timestamp, pressureAbsolute, temp) %>%
      #Convert to POSIX
      dplyr::mutate(Timestamp = as.POSIXct(strptime(Timestamp, "%m/%d/%Y %H:%M", tz = time_zone)))  %>%
      #Convert to GMT
      dplyr::mutate(Timestamp = with_tz(Timestamp, "GMT")) %>%
      #Order the intput
      dplyr::arrange(Timestamp)
  }
  
  if(!is.na(strptime(temp$Timestamp[1], format = "%m/%d/%y %I:%M:%S %p"))){
    temp<-temp %>%
      #Select collumns of interest
      dplyr::select(Timestamp, pressureAbsolute, temp) %>%
      #Convert to POSIX
      dplyr::mutate(Timestamp = as.POSIXct(strptime(Timestamp, format = "%m/%d/%y %I:%M:%S %p", tz = time_zone)))  %>%
      #Convert to GMT
      dplyr::mutate(Timestamp = with_tz(Timestamp, "GMT")) %>%
      #Order the intput
      dplyr::arrange(Timestamp)
  }
  
  #Format Timestamp for dygraphs
  temp$Timestamp = format(temp$Timestamp, format = "%m/%d/%y %I:%M:%S %p")
  temp$Timestamp = as.POSIXct(temp$Timestamp, format = "%m/%d/%y %I:%M:%S %p")
  
  #Convert from psi to kpa if necessary
  if(units=="psi"){temp$pressureAbsolute<-6.89476*temp$pressureAbsolute}
  
  #Add serial number
  temp$Sonde_ID<-serial_number
  
  #Add a time_zone column
  temp$time_zone <- time_zone
  
  #Add download data
  temp$download_date<-as_date(max(temp$Timestamp))
  
  #Export temp
  temp
}
