#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: waterLevel Data for Katie Wardenski
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 2/28/2021
#Purpose: Pull together data for Katie W's analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Next steps: 
#    ) Figure out level issue
#    ) Standardize output
#    ) Complete water level analysis along transect
#    ) Add standardized QAQC data
#    ) Redo 2018-2019 data

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Setup Workspace-------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear workspace 
remove(list=ls())

#Gather libraries of interest
library(dygraphs)
library(xts)
library(lubridate)
library(zoo)
library(tidyverse)
library(stringr)
library(readxl)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Gather data ----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2.1 Gather data (not in the database) ----------------------------------------

#Pull data from output files [eventually pull from from database]
dt1<-list.files(paste0("data//"), full.names =  TRUE, recursive = T) %>% 
  as_tibble() %>% 
  filter(str_detect(value,"output.csv")) %>% 
  as_vector() %>% 
  map_dfr(function(x) read_csv(x, col_types = list('T', 'd', 'd', 'd', 'D', 'c', 'd', 'd', 'd', 'd', 'd', 'd'))) %>% 
  arrange(Site_Name, Timestamp)

# 2.2 Gather data from database ------------------------------------------------
#load required libraries
library(RSQLite)
library(DBI)

#laod custom funs
source("functions//db_get_ts.R")

#Connect to db
db<-dbConnect(RSQLite::SQLite(),"data//choptank.sqlite")

#list sites of interest
index<-read_csv("data//Database Information//baro_assignment.csv") %>% 
  select(site_code) %>% 
  rename(Site_Name = site_code) %>% 
  filter(str_detect(Site_Name,"ND|QB|TB|DB")) %>% 
  as_vector()

#create wrapper for download fun
fun<-function(n){
  #download data
  ts<-db_get_ts(db=db, 
            site=index[n], 
            variable_code_CV = 'waterLevel', 
            mdy("1/1/1900"), 
            mdy('1/1/2100'))
  
  #Add site name
  ts<-ts %>% 
    as_tibble() %>% 
    mutate(Site_Name = index[n])
  
  #Export ts
  ts
}

#Apply function
dt2<-lapply(X=seq(1, length(index)), FUN=fun) %>% bind_rows()

# 2.3 Wrangle data -------------------------------------------------------------
#combine data
dt<-dt1 %>% 
  mutate(waterLevel = if_else(is.na(waterLevel), waterHeight, waterLevel)) %>% 
  select(Timestamp, waterLevel, Site_Name) %>% 
  bind_rows(., dt2) %>% 
  drop_na()

#Summarize by date
dt<-dt %>% 
  mutate(Timestamp = ceiling_date(Timestamp, unit="day")) %>% 
  group_by(Timestamp, Site_Name) %>% 
  summarise(waterLevel=mean(waterLevel)) %>% 
  arrange(Site_Name, Timestamp)

#export
write_csv(dt, "C:\\Workspace\\DOM_Soils\\data\\waterLevel.csv")

