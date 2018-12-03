#Connect to DB
db <- dbConnect(PostgreSQL(), 
                host     = "sesync-postgis01.research.sesync.org",
                dbname   = "choptank",
                user     = "palmergroup", 
                password = 'hor9rnds')

#Download Data
TI<-db_get_water_level_ts(db, "TI Wetland Well Shallow")#Connect to database
JA<-db_get_water_level_ts(db, "JA Wetland Well Shallow")#Connect to database
JB<-db_get_water_level_ts(db, "JB Wetland Well Shallow")#Connect to database
JC<-db_get_water_level_ts(db, "JC Wetland Well Shallow")#Connect to database
NB<-db_get_water_level_ts(db, "NB Wetland Well Shallow")#Connect to database
GB<-db_get_water_level_ts(db, "GB Wetland Well Shallow")#Connect to database
JU<-db_get_water_level_ts(db, "JU Wetland Well Shallow")#Connect to database
FN<-db_get_water_level_ts(db, "FN Wetland Well Shallow")#Connect to database
TA<-db_get_water_level_ts(db, "TA Wetland Well Shallow")#Connect to database

#Create aggregate function
fun<-function(df){
  df$date<-as.Date(df$Timestamp)
  df<-df %>% group_by(date) %>% summarise(depth_m=mean(waterLevel))
}
TI<-fun(TI)
JA<-fun(JA)
JB<-fun(JB)
JC<-fun(JC)
NB<-fun(NB)
JU<-fun(JU)
FN<-fun(FN)
TA<-fun(TA)

#join everybody
df<-left_join(TI, JA, by="date")
df<-left_join(df, JB, by='date')
df<-left_join(df, JC, by='date')
df<-left_join(df, NB, by='date')
df<-left_join(df, JU, by='date')
df<-left_join(df, FN, by='date')
df<-left_join(df, TA, by='date')
colnames(df)<-c("date","TI","JA","JB","JC", "NB","JU","FN","TA")

#write csv
write.csv(df, "/nfs/njones-data/water_level.csv")
