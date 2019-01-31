

#clear memory
remove(list=ls())

#libraries
library('rodm2')
library('tidyverse')
library('lubridate')
source("db_get_water_level_ts.R")

#find all wells 
wells<-c("TI", "JA", "JB", "JC", "NB", "FN", "TA", "DB", "DK", "ND", "ND", "QB", "TB", "BB")
wells<-paste0(wells, " Wetland Well Shallow")

#create function download data and combine with current data
fun<-function(i){
  
  #Read temp output
  df<-read_csv("/nfs/palmer-group-data/Choptank/Nate/PT_data/initial_water_level.csv") %>%
    mutate(date=mdy(date))
  
  #Connect to DB
  db <- dbConnect(PostgreSQL(), 
                  host     = "sesync-postgis01.research.sesync.org",
                  dbname   = "choptank",
                  user     = "palmergroup", 
                  password = 'hor9rnds')
  
  #define variable name
  var_name<-substr(wells[i],1,2)
  
  #Get old water level data
  df<-df %>% 
    select(date, paste(var_name)) 
    colnames(df)<-c("date","waterLevel")
  df<-na.omit(df)
  
  #Get new water level data
  temp<-db_get_water_level_ts(db, wells[i]) %>%
    as_tibble(.) %>%
    mutate(date=ceiling_date(Timestamp, unit="day")) %>%
    group_by(date) %>%
    summarise(waterLevel=mean(waterLevel, na.rm=T)) %>%
    filter(date>mdy("9/11/2018"))
  
  #Estimate offset
  offset <-temp$waterLevel[1]-df$waterLevel[nrow(df)]
  temp$waterLevel<-temp$waterLevel-offset
  
  #bind rows
  df<-rbind(df, temp) %>%
    distinct(.)
  
  #prep for output
  df$site<-var_name
  
  #disconnect from db
  dbDisconnect(db)
  
  #export df
  df
}

#run function
output<-lapply(seq(1, length(wells)), fun) %>%
  bind_rows(.) %>% 
  distinct(.) %>%
  spread(., site, waterLevel)

#Write output
write.csv(output, paste0("/nfs/palmer-group-data/Choptank/Nate/PT_data/waterLevel_",year(Sys.Date()), month(Sys.Date()), day(Sys.Date()),".csv"))


