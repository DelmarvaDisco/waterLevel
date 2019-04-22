waterDepth_fun<-function(Site,df, wells){

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
depths<- left_join(depths, temp) %>%
  #Calculate difference
  mutate(diff = Relative_Water_Level_m - waterHeight) %>%
  #Remove Outliers
  mutate(mean_diff = abs(diff-median(diff)), 
         sd_diff   = sd(diff), 
         outlier  = 2*sd_diff/mean_diff) %>%
  filter(outlier>1) 

#Estimate offset
offset<-median(depths$diff)

#Estimate water depth [where 0 = wetland invert]
output<-df %>%
  mutate(waterDepth = waterColumnEquivalentHeightAbsolute + offset)

#print output
output
}