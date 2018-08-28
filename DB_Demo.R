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
                     variablenamecv = "Water depth")
#Disconnect from database
RSQLite::dbDisconnect(db)

############################################################################
#Step 3: Insert Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################################
#Connect to database
db<-dbConnect(RSQLite::SQLite(), paste0(db_loc))

#Create ts data
df<-data.frame(Timestamp = seq(ISOdate(2018,1,1), by="day", length.out = 365), 
               water_depth_m = rgamma(365, 0.1))
df$Timestamp<-as.POSIXct(df$Timestamp)
# df<-as_tibble(df)

#Create variable list
vars_list<-list("Water depth" = list(column = "water_depth_m", units = "Meter"))

#Insert data into database
db_insert_results_ts(db = db, # database connecton
                     datavalues = df, # data frame of time series data
                     method = "PT Data Download", 
                     site_code = "QB Wetland Well Shallow", 
                     variables = vars_list, 
                     sampledmedium = "Water"
                     # actionby = "Margaret", PersonLastName = "Palmer"
                     # equipment_name = "123456789" # optional
)

#Disconnect from database
RSQLite::dbDisconnect(db)

############################################################################
#Step 4: View Data~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
############################################################################
#Connect to database
db<-dbConnect(RSQLite::SQLite(), paste0(db_loc))

#View Values
dbReadTable(db,"timeseriesresultvalues")
