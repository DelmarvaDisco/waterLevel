#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Aggregate
#Coder: James Maze (jtmaze@umd.edu)
#Date: 3/22/2022
#Purpose: Compile and organize processed PT data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. Libraries and workspace ----------------------------------------------


# Purpose: Compile output.csv data
remove(list = ls())

library(dygraphs)
library(xts)
library(readxl)
library(tidyverse)

#Set data directory
data_dir <- "data//"

#Create a big list of files which you can query later
files <- list.files(paste0("data//"), full.names =  TRUE, recursive = T) 

# 2. Prior to April 2019 by N. Jones ------------------------------------------

#Read Choptank WY2019 sheet
owl <- read_xlsx(paste0(data_dir,"Choptank_Wetlands_WY2019.xlsx"), sheet = 3)
gwl <- read_xlsx(paste0(data_dir,"Choptank_Wetlands_WY2019.xlsx"), sheet = 2)
swl <- read_xlsx(paste0(data_dir,"Choptank_Wetlands_WY2019.xlsx"), sheet = 1)

#Merge the sheets 
dx <- full_join(owl, gwl, by = "Timestamp")
dx <- full_join(dx, swl, by = "Timestamp")
rm(owl, gwl, swl)

#Covert everything except "Timestamp" to character, to make merging easier. 
dx <- dx %>% 
  mutate_if(is.numeric, as.character)

#Pivot to long format
dx <- pivot_longer(dx, cols = -c("Timestamp"), 
                   names_to = "Site_Name", 
                   values_to = "waterLevel")

#Check make a tibble with list of site names
site_names <- as_tibble(unique(dx$Site_Name))

# 3. Jones processing Wardinski (Select Sites 2019 - 2020) ------------------------------------------

dt <- files %>% 
  as_tibble() %>% 
  filter(str_detect(value,"output.csv")) %>% 
  as_vector() %>% 
  map_dfr(function(x) read_csv(x, col_types = list('T', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c')))

# 4. After Fall 2019 by J. Maze  -------------------------------------------------


# 4.1 Download the checks ---------------------------------------------------------------------

# Quick funtion to keep the file path as a column for the checks. 
download_checks <- function(file_path){
  #Read the files
  temp <- read_csv(paste0(file_path)) %>% 
    as_tibble() %>% 
    mutate(file = str_sub(file_path)) 
  
  temp
}

#Find checks inside the file list
checks <- files[str_detect(files, "checks_")] 
#Download the checks
checks <- checks %>% 
  map_dfr(download_checks) 

#Goofed up my column names.  
checks <- checks %>% 
  mutate(Relative_water_level_m = coalesce(Relative_Water_Level_m, 
                                           Relative_water_level_m)) %>% 
  select(-Relative_Water_Level_m)

nas <- checks %>% 
  filter(is.na(Relative_water_level_m))

# 4.2 Download the outputs ------------------------------------------------


  

# Xport! ------------------------------------------------------------------

write_csv(df, file = "ouput_data_long.csv")





