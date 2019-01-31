#Connect to DB
db <- dbConnect(PostgreSQL(), 
                host     = "sesync-postgis01.research.sesync.org",
                dbname   = "choptank",
                user     = "palmergroup", 
                password = 'hor9rnds')

#Download Data
TB<-db_get_water_level_ts(db, "TB Wetland Well Shallow")#Connect to database
ND<-db_get_water_level_ts(db, "ND Wetland Well Shallow")#Connect to database
DK<-db_get_water_level_ts(db, "DK Wetland Well Shallow")#Connect to database
DB<-db_get_water_level_ts(db, "DB Wetland Well Shallow")#Connect to database
QB<-db_get_water_level_ts(db, "QB Wetland Well Shallow")#Connect to database
DF<-db_get_water_level_ts(db, "DF Wetland Well Shallow")#Connect to database
BB<-db_get_water_level_ts(db, "BB Wetland Well Shallow")#Connect to database

#Create plotting function
fun<-function(df){
  #Read df
  df$Timestamp<-as.POSIXct(df$Timestamp)
  
  #create artifical datum and convert to cm
  df$waterLevel<-df$waterLevel*100+1000 
  
  #Selec
  df_xts<-xts(df, order.by=df$Timestamp)
  df_xts<-df_xts[,-1]
  
  #Plot
  ts_plot<-dygraph(df_xts) %>%
    dyRangeSelector() %>%
    dyLegend() %>%
    dyOptions(strokeWidth = 1.5) %>%
    dyOptions(labelsUTC = TRUE) %>%
    dyHighlight(highlightCircleSize = 5,
                highlightSeriesBackgroundAlpha = 0.2,
                hideOnMouseOut = FALSE) %>%
    dyAxis("y", label = "Water Level [cm]")
  
  #Export Plot
  return(ts_plot)
}

#plot
# fun(TB)
# fun(ND)
# fun(DK)
# fun(DB)
# fun(QB)
# fun(DF)

#Create aggregate function
fun<-function(df,name){
  df$date<-as.Date(df$Timestamp)
  df<-df %>% group_by(date) %>% summarise(depth_m=mean(waterLevel))
  write.csv(df,paste0("/nfs/njones-data/",name,".csv"))
}

TB<-fun(TB, "TB")
ND<-fun(ND, "NB")
DK<-fun(DK, "DK")
DB<-fun(DB, "DB")
QB<-fun(QB, "QB")
DF<-fun(DF, "DF")
fun(BB, "BB")
