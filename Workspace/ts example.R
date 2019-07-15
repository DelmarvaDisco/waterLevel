#Organize Data ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-----------------------------------------
#
#Insert site information below
#
#-----------------------------------------

#Identify site and survey data
site<-
survey_temp<-survey %>% filter(Wetland == )

#Identify well info
well_log<-wells %>% filter(Site_Name==site) %>% na.omit()

#Download pressure data
df<-mclapply(paste0(working_dir,well_log$path), download_fun) %>% bind_rows() 

#Estimate gage height~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#-----------------------------------------
#
#Ensure Time Offset between baro and PT is accurate. If not, use "force_diff" 
#dataframe to correct offset
#
#-----------------------------------------

#Estimate barometric pressure
df$barometricPressure<-baro_fun(df$Timestamp, db, 'BARO')

#Define minor offsets
force_diff<-rep(NA, nrow(well_log))

#Estimate water depth
df<-waterHeight_fun(Timestamp = df$Timestamp, 
                    pressureAbsolute = df$pressureAbsolute, 
                    barometricPressure = df$barometricPressure, 
                    temp = df$temp,
                    download_date_ts = df$download_date,
                    download_date_log = well_log$download_date,
                    start_date = well_log$start_date, 
                    end_date = well_log$end_date, 
                    download_datetime = well_log$download_datetime, 
                    force_diff = force_diff)

#Examine waterHeight_fun output [iterate if needed using force_diff or offset_fun]
h_report

#Plot
dygraph_ts_fun(df %>% 
                 mutate(waterHeight=waterHeight*100) %>%
                 select(Timestamp, waterHeight, pressureAbsolute, barometricPressure))

#Estimate water height relative to datum~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#----------------------------------
#
#choose offset for both "depth" and "level"
#   #depth is relative to ground surface
#   #level is relative to wetland invert
#
#note: makes sure to change well height from survey!
#
#----------------------------------

#Estimate water depth and water level 
depths<-waterDepth_fun(
  #From db
  db=db, site = site, 
  #from working df
  Timestamp = df$Timestamp, waterHeight = df$waterHeight,
  #from well log
  download_date = well_log$download_date, Relative_Water_Level_m = well_log$Relative_Water_Level_m, 
  #from survey file
  surveyDate = survey_temp$Date, 
  waterDepth = survey_temp$`Water Depth (m)`, 
  wellHeight = survey_temp$`Upland Well Height (m) - Primary`)
depths

#Define offset
df$offset<-mean(depths$offset[depths$event=='offset'])

#Water depth
df$waterDepth = df$waterHeight + df$offset

#Water Level [datum = wetland invert]
df$waterLevel = df$waterDepth + depths$offset[depths$event=="survey_upland_well"] 

#Manual Edits~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#----------------------------------
#
#Compare updated ts to previous time series
#  #Is the transition smooth
#  #Are there any outliers
#  #other issues? 
#Manualy edit here!
#
#----------------------------------

#Remove NA 
df<-na.omit(df)

#Examine waterDepth accross time sereis
test<-db_get_ts(db, site, variable_code_CV = 'waterDepth', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterDepth = df$waterDepth))
dygraph_ts_fun(test %>% mutate(waterDepth=waterDepth*100+1000) %>% select(Timestamp, waterDepth))

#Examine waterLevel accross time series 
test<-db_get_ts(db, site, variable_code_CV = 'waterLevel', mdy("1/1/1000"), mdy('1/1/3000'))
test<-bind_rows(test, tibble(Timestamp = df$Timestamp, waterLevel = df$waterLevel))
dygraph_ts_fun(test %>% mutate(waterLevel=waterLevel*100+1000) %>% select(Timestamp, waterLevel))

#Insert into database~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#----------------------------------
#
#Insert into the db.  Make sure to only insert variables you used! :)
#
#----------------------------------


#Database insert function
t0<-Sys.time()
rodm2::db_insert_results_ts(db = db,
                            datavalues = df,
                            method = "waterdepth",
                            site_code = site,
                            processinglevel = "Raw data",
                            sampledmedium = "Liquid aqueous", # from controlled vocab
                            #actionby = "Nate",
                            equipment_name = paste(well_log$Sonde_ID),
                            variables = list( # variable name CV term = list("colname", units = "CV units")
                              "offset"     = list(column = "offset",     units = "Meter"),    
                              "waterDepth" = list(column = "waterDepth", units = "Meter"),
                              "waterLevel" = list(column = "waterLevel", units = "Meter"),
                              "Temperature" = list(column = "temp", units = "Degree Celsius")))
tf<-Sys.time()
tf-t0