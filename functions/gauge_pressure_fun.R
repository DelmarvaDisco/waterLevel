#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: Pressure Gage Function   
# Coder: C. Nathan Jones
# Date: 29 April 2019
# Purpose: Combine PT and Barometric data to estimate Gage Pressure 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

waterDepth_fun<-function(Timestamp, pressureAbsolute, barometricPressure, start_date, end_date, download_datetime){
  
  #Organize workspace~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #Create tibble with ts
  df<-tibble(Timestamp, pressureAbsolute, barometricPressure)
  
  #Create tibble with periods
  well_log<-tibble(start_date, end_date, download_datetime) %>%
    mutate(diff = difftime(download_datetime, end_date, units="hours"), 
           diff = round(diff,0))
  
  #Clip ts to time period
  df<- df %>% 
    filter(Timestamp>min(well_log$start_date, na.rm=T), 
           Timestamp<max(well_log$end_date  , na.rm=T)) 
  
  #Filter potential outliers~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  df<-df %>% 
    #Estimate water height equivolent
    mutate(waterHeight = pressureAbsolute*0.101972) %>%
    #Estimate difference
    mutate(diff = abs(lead(waterHeight) - waterHeight)) %>%
    #Identify failure points where signal "drops
    mutate(diff = if_else(diff< 0.05, 0, 1)) %>%
    mutate(diff = rollapply(diff, 5, sum, align='right', fill=NA)) %>%
    filter(diff == 0)
  
  #Zipper function [minimize variability]~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  zipper_fun<-function(n){
    
    #Select time period of interest
    ts<-df %>%
      #Select relevant collumns
      select(Timestamp, barometricPressure, pressureAbsolute) %>%
      #Subset to time period
      filter(Timestamp>well_log$start_date[n],
             Timestamp<well_log$end_date[n])
  
    #Develop function to miinimize variability over ts
    inside_fun<-function(window){
      #Etimate water height
      if(window>=0){
        x<-window
        waterHeight<-(lag(ts$pressureAbsolute, x) - ts$barometricPressure)*0.101972
      }else{
        x<-window*-1
        waterHeight<-(lead(ts$pressureAbsolute, x) - ts$barometricPressure)*0.101972
      }
      
      #form daily tibble
      temp<-tibble(Timestamp = ts$Timestamp, waterHeight) %>%
        mutate(Timestamp = floor_date(Timestamp, "day")) %>%
        group_by(Timestamp) %>%
        summarise(var = var(waterHeight,na.rm=T))
      
      #estimate standard deviation
      var<-median(temp$var,na.rm=T)
      
      #Export 
      tibble(window, var)
    }
    
    #apply inside fun
    diff<-lapply(seq(-100, 100), inside_fun) %>% 
      bind_rows() %>% 
      filter(var==min(var, na.rm=T)) %>%
      select(window)
    
    #Adjust timing on pressure absolute
    x<-abs(diff$window)
    if(diff$window>=0){
      ts$pressureAbsolute<-lag(ts$pressureAbsolute, x) 
    }else{
      ts$pressureAbsolute<-lead(ts$pressureAbsolute, x) 
    }
    
    #Calculate water depth
    ts$pressureGauge <- ts$pressureAbsolute - ts$barometricPressure
    ts$waterHeight<-ts$pressureGauge*0.101972
    
    #Estimate average timestep
    timestep<-mean(ts$Timestamp-lag(ts$Timestamp), na.rm=T)
    
    #Add diff to df
    ts$log_diff<-well_log$diff[n]
    ts$zipper_diff<-diff$window*timestep
    
    #Export ts
    ts
  }
  
  #Apply zipper fun
  df<-mclapply(seq(1,nrow(well_log)), zipper_fun) %>% bind_rows() %>% arrange(Timestamp)
}




















# #Clip ts to period of interest
# ts<-df %>%
#   #Select relevant collumns
#   select(Timestamp, barometricPressure, pressureAbsolute) %>%
#   #Subset to time period
#   filter(Timestamp>well_log$start_date[n], 
#          Timestamp<well_log$end_date[n]) 
# 
# #Apply time offset
# new_ts<-ts %>% select(Timestamp, pressureAbsolute) %>%
#   mutate(Timestamp = Timestamp + hours(well_log$diff[n]))
# ts<-ts %>% select(Timestamp, barometricPressure)
# 
# #Estimate gage pressure at each timestep
# ts<-left_join(new_ts, ts) %>%
#   mutate(pressureGauge = pressureAbsolute - barometricPressure,
#          waterColumnEquivalentHeightAbsolute = pressureGauge*0.101972)
# 
# #Export ts
# ts