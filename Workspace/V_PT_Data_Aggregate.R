#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Aggregate
#Coder: James Maze (jtmaze@umd.edu)
#Date: 3/22/2022
#Purpose: Compile processed PT data and compare the checks.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Notes and issues:
# - How to keep a Notes column after aggregating to daily time step???

# 1.0 Libraries and work space ----------------------------------------------

# Purpose: Compile output.csv data 
remove(list = ls())

library(dygraphs)
library(xts)
library(readxl)
library(tidyverse)

#Functions
source("functions//dygraph_ts_fun.R")

#Set data directory
data_dir <- "data//"

#Create a big list of files which you can query later
files <- list.files(paste0("data//"), full.names =  TRUE, recursive = T) 

# 2.0 Download all data after Fall 2019  -------------------------------------------------

# 2.1 Download the checks ---------------------------------------------------------------------

# Quick function to keep the file path as a column for the checks. This lets you see
# how the checks compare across downloads. 
download_checks <- function(file_path){
  #Read the files
  temp <- read_csv(paste0(file_path)) %>% 
    as_tibble() %>% 
    #Convert to numeric
    mutate(Well_head_m = as.character(Well_head_m),
           Depth_to_water_m = as.character(Depth_to_water_m)) %>% 
    mutate(file = str_sub(file_path)) %>% 
    mutate(Date = lubridate::mdy(Date)) %>%
    rename("Site_ID" = Site_Name)
  
  #Return the checks files as a tibble
  (temp)
  
}

#Find checks inside the files list
checks <- files[str_detect(files, "checks_")]
                     
#Download the checks and combine to a single tibble. 
checks <- checks %>% 
  map_dfr(download_checks) 

# 2.2 Read in the output data ------------------------------------------------

df <- read_csv(file = paste0(data_dir, "output_JM_2019_2022.csv")) %>% 
  #Filter out the bad data
  filter(!Flag == "2") %>%
  #Aggregate data to daily waterlevel
  mutate(Date = str_sub(as.character(Timestamp), start = 1, end = 10)) %>%
  group_by(Date, Site_Name) %>%
  summarise(dly_mean_wtrlvl = mean(waterLevel),
            #If any data point is flagged, then the whole day will be > 0
            Flag = mean(Flag),
            #If there's different flag notes for each day, there will be duplicate data points.
            #Not sure if there's a better way to handle this.
            Notes = print(unique(Notes))) %>% 
  #convert to a tibble instead of grouped data frame. Should be faster and more workable.
  as_tibble() %>% 
  #Flag = 1 for entire day of data if one time point is flagged. 
  mutate(Flag = if_else(Flag > 0,
                        "1",
                        "0")) %>%
  #Rename column to proper name
  rename("Site_ID" = Site_Name) %>% 
  mutate(Date = lubridate::ymd(Date))

# 3.1 Augment original checks file -------------------------------------------------

#!! Some of the checks need to be corrected because, the PTs died or filled up with data.
# Since the processing scrips pull sensor checks from the last 10 observations, the sensor don't match
# the tape-measurements if the sensor dies before downloading data. For sites with dead/full PTs, I use the modeled
# data for those checks. 

#List of sites that filled up with data in Fall 2021
lost_F2021 <- c("BD-CH", "DK-CH", "HB-SW", "HB-UW1", "MB-CH", "MB-SW", "MB-UW1", 
                "OB-CH", "OB-SW", "OB-UW1", "TS-CH", "TS-UW1", "XB-SW", "XB-UW1",
                "TB-SW")

temp <- checks %>% 
  #Filter sites in the lost_F2021 list AND that match the download date.
  filter((file == "data//checks_20211112_JM.csv" &
         Site_ID %in% lost_F2021) |
         (file == "data//checks_20201015_JM.csv" &
         Site_ID == "TB-UW2")) %>% 
  #Notes mentioning use of modeled data
  mutate(Notes = paste0(Notes, ". Used modeled data for check, becuase PT died early.")) %>%
  #Remove columns to change the values. 
  select(-c(measured_diff, sensor_wtrlvl)) %>% 
  #Join df with temp to get modeled values to replace incorrect checks. 
  left_join(., df %>% 
              #Remove the Notes column from df, so there's not duplicate Notes columns after the join. 
              select(-c(Notes)), 
            by = c("Date", "Site_ID")) %>% 
  #Use modeled data to calculate checks
  mutate(sensor_wtrlvl = dly_mean_wtrlvl,
         measured_diff = dly_mean_wtrlvl - Relative_water_level_m) %>% 
  #Remove extra columns
  select(-c(dly_mean_wtrlvl, Flag))

checks <- checks %>% 
  #Remove the incorrect checks from the checks file.
  filter(!(file == "data//checks_20211112_JM.csv" &
         Site_ID %in% lost_F2021)| 
         (file == "data//checks_20201015_JM.csv" &
         Site_ID == "TB-UW2")) 

#Combine original checks with modeled values in temp. 
checks <- bind_rows(checks, temp)
  
#Clean up environment
rm(temp, lost_F2021)

# 3.2 Pull checks values from full data frame -------------------------------------------------

# Since some of the checks were not lining up well, I wanted to try a different method for generating them. Instead of 
# pulling the last 10 values during the processing steps, I'm going to match using the fully processed dataset. 
# This may work better due to off-timing of baro downloads and PT downloads. For example, a baro download three days before 
# the PT download means the file end may not match tape-measurement times. Once the next download is processed,
# the dates can match, hence using the full dataframe could work better. 

checks_from_df <- checks %>% 
  select(-c(measured_diff, sensor_wtrlvl)) %>% 
  #Pull checks values from the day prior to download, 
  #because oftentimes there's limited data on download days. 
  mutate(Date = Date - lubridate::days(1)) %>% 
  left_join(., df %>% 
              #Remove the Notes column from the df to avoid duplicate notes columns after join.
              select(-c(Notes)),
            by = c("Date", "Site_ID")) %>% 
  #Calculate new measured difference values. 
  mutate(sensor_wtrlvl = dly_mean_wtrlvl,
         measured_diff = dly_mean_wtrlvl - Relative_water_level_m) %>% 
  #Remove extra columns
  select(-c(dly_mean_wtrlvl, Flag))

# 3.3 Plot the checks --------------------------------------------------

#!!! Some of the checks values are really wonky from improper measurements and unaligned download dates. 
# In those circumstances, I filter out checks values. 

# Checks taken from specific downloads (more incorrect values here)
checks_interest_from_df <- checks_from_df %>% 
  #These values are clearly incorrect field measurements, and can be removed.
  filter(!(Site_ID == "NB-SW" & file == "data//checks_20201015_JM.csv"),
         !(Site_ID == "ND-UW1" & file == "data//checks_20211112_JM.csv"),
         !(Site_ID == "ND-UW2" & file == "data//checks_20211112_JM.csv"),
         !(Site_ID == "DK-UW1" & file == "data//checks_20210525_JM.csv"),
         !(Site_ID == "TA-SW" & file == "data//checks_20211112_JM.csv"),
         !(Site_ID == "BD-SW" & file == "data//checks_20211112_JM.csv"),
         !(Site_ID == "ND-SW" & file == "data//checks_20201015_JM.csv"),
         !(Site_ID == "Jones Road North Catchment Outlet" & file == "data//checks_20210525_JM.csv"))

# checks_interest_from_processing <- checks 

checks_interest <- checks_interest_from_df %>% 
  filter(!Site_ID %in% c("Jones Road North Catchment Outlet", "Jones Road South Catchment Outlet",
                         "TS-CH"))

#Plot differences between sensors and field measured values for checks. 
checks_plot <- ggplot(data = checks_interest,
                           aes(x = Site_ID,
                               y = measured_diff,
                               fill = file)) +
  geom_col(position = position_dodge(width = 1,
                                     preserve = "single"),
           color = "black",
           width = 1) +
  geom_hline(yintercept = 0.05, 
             color = "tomato", 
             size = 0.75) +
  geom_hline(yintercept = -0.05, 
             color = "tomato",
             size = 0.75) +
  theme_bw() +
  theme(axis.text = element_text(size = 8,
                                 face = "bold",
                                 angle = 89),
        axis.title.x = element_text(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_line(size = 0.25, color = "grey"))+
  ylab("Meters difference (sensor - field)")

(checks_plot)

rm(checks, checks_from_df, checks_plot)

# 4.0 Quick plots of time series together ----------------------------------

df_interest <- df %>%
  #Select sites of interest
  filter(Site_ID %in% c("TB-SW")) %>%
  #Add 100 to waterlevel since dygraphs struggles with negative numbers
  mutate(waterLevel = dly_mean_wtrlvl + 100) %>% 
  rename(Timestamp = Date) %>%
  select(-c(dly_mean_wtrlvl, Flag))

#Pivot wider for dygraph's xts format
df_interest <- pivot_wider(data = df_interest,
                           names_from = c("Site_ID"),
                           values_from = "waterLevel") %>%
  mutate(Timestamp = lubridate::ymd(Timestamp))

#Plot in dygraphs
dygraph_ts_fun(df_interest)

# 5.0 Xport! ------------------------------------------------------------------

#Write aggregated daily data
write_csv(df, paste0(data_dir,"dly_mean_output_JM_2019_2022.csv"))








