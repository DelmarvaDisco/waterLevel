#Create db_get_ts_function
db_get_water_level_ts<-function(site_code){
  
  #Retreive Sampling Feature ID for site code
  SamplingFeatureID<-RSQLite::dbGetQuery(db,
                                         "SELECT SamplingFeatureID FROM SamplingFeatures WHERE SamplingFeatureCode = :x", 
                                         params=list(x=site_code))
  
  #Retreive Feature Action ID[s] for site code
  FeatureActionID<-RSQLite::dbGetQuery(db,
                                       "SELECT FeatureActionID FROM FeatureActions WHERE SamplingFeatureID = :x", 
                                       params=list(x=SamplingFeatureID[,1]))
  
  #Retreive Result ID[s] for each feature action
  ResultID<-RSQLite::dbGetQuery(db,
                                "SELECT ResultID FROM Results WHERE FeatureActionID = :x", 
                                params=list(x=FeatureActionID[,1]))
  
  #Retreive Result values
  Values<-RSQLite::dbGetQuery(db,
                              "SELECT ValueDateTime, DataValue
                                    FROM TimeSeriesResultValues
                                    WHERE ResultID = :x", 
                              params=list(x=ResultID[,1]))
  
  #Clean up values df
  Values$ValueDateTime<-as.POSIXct(Values$ValueDateTime)
  colnames(Values)<-c("Timestamp", "water_level_m")
  
  #Export Values
  Values
}