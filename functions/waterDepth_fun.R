waterDepth_fun<-function(#from working df
                         Timestamp, 
                         waterHeight,
                         #from well log
                         download_date, 
                         Relative_Water_Level_m, 
                         #from survey file
                         surveyDate, 
                         waterDepth,
                         wellHeight){
  
  #Organize Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Water height time series data
  ts<-tibble(Timestamp, waterHeight) %>% 
    mutate(Timestamp = floor_date(Timestamp, "day")) %>%
    group_by(Timestamp) %>%
    summarise(waterHeight = mean(waterHeight, na.rm = T))
  
  #Water depth at download (well log file)
  depths<-tibble(Timestamp = download_date, waterDepth=Relative_Water_Level_m) %>%
    mutate(Timestamp = as.POSIXct(Timestamp))
  
  #Survey
  survey<-tibble(Timestamp = surveyDate, waterDepth, wellHeight)
  
  #Estimate water depth during each download~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  depths<-left_join(depths, ts) %>%
    mutate(offset = waterDepth - waterHeight, 
           event = "download") %>%
    select(offset, event) %>% na.omit()
  
  
  #Make estimates from survey
  survey<- survey %>%
    mutate(Timestamp = mdy(Timestamp, tz='GMT')) %>% 
    left_join(., ts, by='Timestamp') %>%
    mutate(survey_wetland_well = waterDepth -waterHeight,
           survey_upland_well  = wellHeight) %>%
    select(survey_wetland_well,survey_upland_well) %>%
    gather(.) %>%
    rename(offset = value, event = key)
  
  #bind rows
  output<-bind_rows(depths, survey)
    
  #Export output
  output
}