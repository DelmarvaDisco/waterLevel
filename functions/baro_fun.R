baro_fun<-function(Timestamp, db, baro_site_code){

  #Create tibble
  df<-tibble(Timestamp)
  
  #Get Barometric Pressure from DB
  baro<-db_get_ts(db = db,
                  site_code='BARO',
                  variable_code_CV = 'barometricPressure',
                  start_datetime = date(min(df$Timestamp)),
                  end_datetime = date(max(df$Timestamp)))
  
  #Estimate barometric pressure at each timestep
  baro<-baro %>% mutate(Timestamp=force_tz(Timestamp,"GMT"))
  baro_fun<-approxfun(baro$Timestamp, baro$barometricPressure)
  df$barometricPressure<-baro_fun(df$Timestamp)
  
  #Export barometric pressure
  df$barometricPressure
}
