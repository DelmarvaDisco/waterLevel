waterHeight_fun<-function(Site, wells, db, working_dir){
  #Define site
  well_index<-wells %>% dplyr::filter(Site_Name==Site) %>% na.omit()
  
  #download absolute pressure data
  download_fun<-function(m){
    #Download data
    temp<-read_csv(paste0(working_dir,well_index$path[m]), skip=1)

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

    #Add serial number
    temp$Sonde_ID<-well_index$Sonde_ID[m]

    #Export temp
    temp
  }
  df<-mclapply(X = seq(1,nrow(well_index)), 
               FUN = download_fun, 
               mc.cores = detectCores()) %>% 
    bind_rows(.) %>% 
    arrange(Timestamp)

  #subract barometric pressure
  baro<-db_get_ts(db = db,
                  site_code='BARO',
                  variable_code_CV = 'barometricPressure',
                  start_datetime = date(min(well_index$start_date)),
                  end_datetime = date(max(well_index$end_date)))
  baro<-baro %>% mutate(Timestamp=force_tz(Timestamp,"GMT"))
  baro_fun<-approxfun(baro$Timestamp, baro$barometricPressure)
  df$barometricPressure<-baro_fun(df$Timestamp)
  df$pressureGauge<-df$pressureAbsolute-df$barometricPressure

  #Estimate water collumn height
  df<-df %>% mutate(waterColumnEquivalentHeightAbsolute = pressureGauge*0.101972)

  #Lets remove extraneous points
  df<-df %>%
    #Estimate difference
    mutate(diff = lead(waterColumnEquivalentHeightAbsolute) - waterColumnEquivalentHeightAbsolute) %>%
    #Identify failure points where signal "drops
    mutate(diff = if_else(diff> -0.1, 0, 1)) %>%
    mutate(diff = rollapply(diff, 10, sum, align='right', fill=NA)) %>%
    filter(diff == 0)

  #Export df
  df
}
