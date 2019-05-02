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
  depth<-tibble(Timestamp = download_date, waterDepth=Relative_Water_Level_m) %>%
    mutate(Timestamp = as.POSIXct(Timestamp))
  
  #Survey
  survey<-tibble(surveyDate, waterDepth, wellHeight)
  
  #Estimate water depth during each download~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  depth<-left_join(depth, ts) %>%
    mutate(offset = waterDepth - waterHeight)
  
  
  #Make estimates from survey
  
  
  
  
  
  
  
  #Identify well depths at each download
  depths<-wells %>% 
    dplyr::filter(Site_Name==Site) %>%
    dplyr::mutate(Timestamp = as.POSIXct(download_date)) %>%
    dplyr::mutate(Timestamp = lubridate::force_tz(Timestamp, "GMT")) %>%
    dplyr::mutate(Timestamp = floor_date(Timestamp, unit="day")) %>%
    dplyr::select(Timestamp, Relative_Water_Level_m) 
  
  #Estimate water height on downlaod dates
  temp<-df %>%
    #Select timestamp and waterHeight
    select(Timestamp, waterColumnEquivalentHeightAbsolute) %>%
    #Convert Timestamp to day
    mutate(Timestamp =floor_date(Timestamp,unit = "day")) %>%
    #Group by day and estimate daily mean
    group_by(Timestamp) %>%
    summarise(waterHeight = mean(waterColumnEquivalentHeightAbsolute))
  
  #Merge depths and df
  output<- left_join(depths, temp) %>%
    #Calculate difference
    mutate(diff = Relative_Water_Level_m - waterHeight, 
           measurement = "downlaod")  %>%
    #Select collumns
    select(diff, measurement)
  
  #Process survey data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Identify Site Name
  site<-substr(Site, 1,2)
  
  #If survey data exists
  if(site %in% survey$Wetland){
    
    #Identify survey data and create long format
    survey<-survey %>% 
      filter(Wetland==site) %>%
      gather(.) %>%
      filter(value != -9999)
    
    #Seperate date
    date<-mdy(survey$value[survey$key=="Date"])
    survey<-survey %>% 
      filter(key!="Date") %>% filter(key!="Wetland") %>%
      mutate(value = as.numeric(paste(value)))
    
    #Estiamte wl on date
    stage<-temp$waterHeight[temp$Timestamp==date]
    
    #Estiamte Values
    survey <- survey %>%
      mutate(value = value - stage) %>%
      rename(diff = value,
             measurement = key) %>%
      select(diff, measurement)
    
    #Merge with output
    output<-output %>% bind_rows(.,survey)
  }
  
  #Export output
  output
}