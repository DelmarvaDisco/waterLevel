#http://odm2.github.io/ODM2/schemas/ODM2_Current/diagrams/ODM2Core.html

db_get_equip_by_site<-function(db, site_code, variable_code_CV){
  
  #Check if db is compatable
  if (!class(db) %in% c("SQLiteConnection")) {
    stop("sorry, Nate wrote this.  Sooooo, only sqlite database connections are supported for this function until Kelly swoops in to save the day.")}
  
  #SQLite Database 
  if(class(db) == "SQLiteConnection"){
    
    #Retreive Sampling Feature ID for site code
    SamplingFeatureID<-RSQLite::dbGetQuery(db,
                                           "SELECT SamplingFeatureID 
                                              FROM SamplingFeatures 
                                              WHERE SamplingFeatureCode = :x", 
                                           params=list(x=site_code))
    
    #Retreive Feature Action ID[s] for site code
    ActionID<-RSQLite::dbGetQuery(db,
                                  "SELECT ActionID 
                                   FROM FeatureActions 
                                   WHERE SamplingFeatureID = :x", 
                                   params=list(x=SamplingFeatureID[,1]))
    
    #Retreive Feature Action ID[s] for site code
    EquipmentID<-RSQLite::dbGetQuery(db,
                                     "SELECT EquipmentID 
                                      FROM EquipmentUsed 
                                      WHERE ActionID = :x", 
                                      params=list(x=ActionID[,1]))
    
    #Equipment Name
    EquipmentName<-RSQLite::dbGetQuery(db,
                                       "SELECT EquipmentName 
                                        FROM Equipment 
                                        WHERE EquipmentID = :x", 
                                       params=list(x=EquipmentID[,1]))
    
  }
  
  #Create Export
  output<- EquipmentName %>%
    mutate(site_code = site_code)
  
  output
  
}