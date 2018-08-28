############################################################################
#Title: DB Demo
#Coder: Nate Jones 
#Date: 8/26/2018
#Purpose: Demo rodm2 database functionality
############################################################################

############################################################################
#Step 1: Setup workspace~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################################
#Define working dir
working_dir<-getwd()

#Download rodm2 package (note, this may take a few minutes!)
library(devtools)
devtools::install_github("khondula/rodm2")
library(rodm2)

#Create SQLight databse 
db <- create_sqlite(dir = working_dir, 
                    filename="test",
                    connect = F)
db_loc<-"~/test.sqlite"
db_loc <- file.path(working_dir, "test.sqlite")

############################################################################
#Step 2: Describe tables~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################################
#Connect to database
db<-DBI::dbConnect(RSQLite::SQLite(), paste0(db_loc))

#Describe Equipment
db_describe_equipment(db, 
                      equip_name    =   123456789, 
                      serial_no     =   123456789,
                      model_name    =   "U20 Pressure Transducer",
                      vendor        =   "Onset",
                      manufacturer  =   "HOBO",
                      equipment_type=   "pressureTransducer",
                      owner_first   =   "Margaret",
                      owner_last    =   "Palmer",
                      owner_email   =   "mpalmer@sesync.org")

#Describe Site
db_describe_site(db, site_code = "QB Wetland Well Shallow")

#Describe Method
db_describe_method(db, 
                   methodname =   "PT Data Download",
                   methodcode =   "PT Data Download",
                   methodtypecv = "Instrument deployment")  

#Describe Variable
db_describe_variable(db, 
                     variabletypecv = "Hydrology",
                     variablecode   = "water_depth_m",
                     variablenamecv = "waterDepth")
#Disconnect from database
RSQLite::dbDisconnect(db)

############################################################################
#Step 3: Insert Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################################
#Connect to database
db<-DBI::dbConnect(RSQLite::SQLite(), paste0(db_loc))

#Create ts data
df1<-data.frame(Timestamp = seq(ISOdate(2018,1,1), by="day", length.out = 365), 
               water_depth_m = rgamma(365, 0.1))
df1$Timestamp<-as.POSIXct(df$Timestamp)
df2<-data.frame(Timestamp = seq(ISOdate(2019,1,1), by="day", length.out = 365), 
                water_depth_m = rgamma(365, 0.1))
df2$Timestamp<-as.POSIXct(df$Timestamp)

#Create variable list
vars_list<-list("waterDepth" = list(column = "water_depth_m", units = "Meter"))

#Insert data into database
db_insert_results_ts(db = db, # database connecton
                     datavalues = df1, # data frame of time series data
                     method = "PT Data Download", 
                     site_code = "QB Wetland Well Shallow", 
                     variables = vars_list, 
                     sampledmedium = "Water"
                     #actionby = "Margaret", PersonLastName = "Palmer",
                     #equipment_name = "123456789" # optional
)

#Insert data into database
db_insert_results_ts(db = db, # database connecton
                     datavalues = df2, # data frame of time series data
                     method = "PT Data Download", 
                     site_code = "QB Wetland Well Shallow", 
                     variables = vars_list, 
                     sampledmedium = "Water"
                     #actionby = "Margaret", PersonLastName = "Palmer",
                     #equipment_name = "123456789" # optional
)

#Disconnect from database
RSQLite::dbDisconnect(db)

############################################################################
#Step 4: View Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################################
#Connect to database
db<-DBI::dbConnect(RSQLite::SQLite(), paste0(db_loc))

#Create db_get_ts_function
db_get_water_level_ts_function<-function(site_code){
  
  #Retreive Sampling Feature ID for site code
  SamplingFeatureID<-RSQLite::dbGetQuery(db,
                                        "SELECT SamplingFeatureID 
                                          FROM SamplingFeatures 
                                          WHERE SamplingFeatureCode = :x", 
                                        params=list(x=site_code))
  
  #Retreive Feature Action ID[s] for site code
  FeatureActionID<-RSQLite::dbGetQuery(db,
                                       "SELECT FeatureActionID 
                                          FROM FeatureActions 
                                          WHERE SamplingFeatureID = :x", 
                                       params=list(x=SamplingFeatureID[,1]))
  
  #Retreive Result ID[s] for each feature action
  ResultID<-RSQLite::dbGetQuery(db,
                                  "SELECT ResultID 
                                      FROM Results 
                                      WHERE FeatureActionID = :x", 
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

#Retreive Water Level Data from Database
wl<-db_get_water_level_ts_function("QB Wetland Well Shallow")

#Plot
plot(wl)

#Disconnect from database
RSQLite::dbDisconnect(db)
