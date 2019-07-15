waterDepth_fun<-function(#db information
                         db = db,
                         site = site, 
                         #from working df
                         Timestamp, 
                         waterHeight,
                         #from well log
                         download_date, 
                         Relative_Water_Level_m, 
                         #from survey file
                         surveyDate, 
                         waterDepth,
                         wellHeight){
  
  
  #Organize Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Water height time series data
  ts<-tibble(Timestamp, waterHeight) %>% 
    mutate(Timestamp = date(floor_date(Timestamp, "day"))) %>%
    group_by(Timestamp) %>%
    summarise(waterHeight = mean(waterHeight, na.rm = T)) %>% na.omit(.)
  
  #Water depth at download (well log file)
  depths<-tibble(Timestamp = download_date, waterDepth=Relative_Water_Level_m) %>%
    mutate(Timestamp = ymd(Timestamp)) 
  
  #Survey
  survey<-tibble(Timestamp = surveyDate, waterDepth, wellHeight)
  
  #Estimate water depth during each download~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if(!(depths$Timestamp %in% ts$Timestamp)){
    ts$Timestamp[ts$Timestamp==max(ts$Timestamp)]<-depths$Timestamp[1]
  }
  depths<-left_join(depths, ts) %>%
    mutate(offset = waterDepth - waterHeight, 
           event = "download") %>%
    select(offset, event) %>% na.omit()

  #Make estimates from survey
  survey<- survey %>%
    mutate(Timestamp = mdy(Timestamp)) %>% 
    left_join(., ts, by='Timestamp') %>%
    mutate(survey_wetland_well = waterDepth -waterHeight,
           survey_upland_well  = wellHeight) %>%
    select(survey_wetland_well,survey_upland_well) %>%
    gather(.) %>%
    rename(offset = value, event = key)
  
  #bind rows
  output<-bind_rows(depths, survey)
    
  #Query offset from db [if there is an offset in the db]~~~~~~~~~~~~~~~~~~~~~~~
  diff<-tryCatch(
    db_get_ts(db, 
              site, 
              variable_code_CV = 'offset', 
              mdy("1/1/1900"), mdy('1/1/3000')), 
    error = function(e) 0)
  if(diff!=0){
    diff<-unique(diff$offset)
    diff<-tibble(event = "offset", 
                 offset = diff)
    output<-bind_rows(diff, output)
  }
  
  #Export to global environment~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  output

}
