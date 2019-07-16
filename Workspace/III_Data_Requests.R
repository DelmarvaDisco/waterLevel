#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Name: PT Data Requests
# Coder: C. Nathan Jones
# Date: 14 May 2019
# Purpose: Now that the data is ready, some initial requests
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Create check functions
#  -copy from Tully Lab Reop
#  - Add one to check the number of sondes at each site



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Worskspace---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#clear environment
remove(list=ls())

#load relevant packages
library(xts)
library(dygraphs)
library(parallel)
library(devtools)
devtools::install_github("khondula/rodm2")
library(RSQLite)
library(DBI)
library(rodm2)
library(zoo)
library(lubridate)
library(readxl)
library(tidyverse)

#Read custom R functions
funs<-list.files("functions/", full.names = T)
for(i in 1:length(funs)){source(funs[i]);print(paste(i,"-", funs[i]))}

#Define working dir
working_dir<-"//nfs/palmer-group-data/Choptank/Nate/PT_Data/"

#Set system time zone 
Sys.setenv(TZ="America/New_York")

#Define database connection
db<-dbConnect(RSQLite::SQLite(),"//nfs/palmer-group-data/Choptank/Nate/PT_Data/choptank.sqlite")

#Define sites
sites<-read_csv(paste0(working_dir,"Database Information/sites.csv")) %>% 
  filter(site_code!="TC Wetland Well Shallow")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Wetland Water Level -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create download function
fun<-function(site, 
              start_date = mdy("1-1-2000"), 
              end_date = mdy("9-30-2100")){
  #download data
  temp<-db_get_ts(db, paste(site), "waterLevel", start_date, end_date) %>% as_tibble(.)
  
  #add site collumn
  colnames(temp)<-c("Timestamp", "waterLevel")
  temp$site = paste(site)
  
  #Export to .GlovalEnv
  temp 
}

#Obtain data from all surface water weltand wells
SWL<-sites %>% filter(str_detect(site_code,"Wetland Well Shallow")) %>% as.matrix(.)

#Download Data
SWL<-lapply(SWL, fun) %>% bind_rows(.)

#Sort Data
SWL<-SWL %>% 
  mutate(Timestamp = floor_date(Timestamp, "15 min")) %>% 
  group_by(site, Timestamp) %>% distinct(.) %>%
  summarise(waterLevel=mean(waterLevel)) %>% 
  spread(site,waterLevel)

write.csv(SWL,paste0(working_dir,"SWL.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Upland Water Level --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download function
fun<-function(site, 
              start_date = mdy("1-1-2000"), 
              end_date = mdy("9-30-2100")){
  #download data
  temp<-db_get_ts(db, paste(site), "waterDepth", start_date, end_date) %>% as_tibble(.)
  
  #add site collumn
  colnames(temp)<-c("Timestamp", "waterDepth")
  temp$site = paste(site)
  
  #Export to .GlovalEnv
  temp 
}

#Obtain data from all upland water weltand wells
GWL<-sites %>% filter(str_detect(site_code, "Upland")) %>% as.matrix(.)

#Download Data
GWL<-lapply(GWL, fun) %>% bind_rows(.)

#Sort Data
GWL<-GWL %>% 
  mutate(Timestamp = floor_date(Timestamp, "15 min")) %>% 
  group_by(site, Timestamp) %>% distinct(.) %>%
  summarise(waterDepth=mean(waterDepth)) %>% 
  spread(site,waterDepth)

#export
write.csv(GWL,paste0(working_dir,"GWL.csv"))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Catchment Outlet Water Level ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Download function
fun<-function(site, 
              start_date = mdy("1-1-2000"), 
              end_date = mdy("9-30-2100")){
  #download data
  temp<-db_get_ts(db, paste(site), "waterDepth", start_date, end_date) %>% as_tibble(.)
  
  #add site collumn
  colnames(temp)<-c("Timestamp", "waterDepth")
  temp$site = paste(site)
  
  #Export to .GlovalEnv
  temp 
}

#Obtain data from all upland water weltand wells
OWL<-db_get_sites(db) %>% 
  enframe(name=NULL) %>%
  filter(str_detect(value, "Catchment")) %>% 
  as.matrix(.)

#Download Data
OWL<-lapply(OWL, fun) %>% bind_rows(.)

#Sort Data
OWL<-OWL %>% 
  mutate(Timestamp = floor_date(Timestamp, "15 min")) %>% 
  group_by(site, Timestamp) %>% distinct(.) %>%
  summarise(waterDepth=mean(waterDepth)) %>% 
  spread(site,waterDepth)

#export
write.csv(OWL,paste0(working_dir,"OWL.csv"))
