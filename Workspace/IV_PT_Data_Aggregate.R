#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Aggregate
#Coder: James Maze (jtmaze@umd.edu)
#Date: 3/22/2022
#Purpose: Compile and organize processed PT data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Notes and issues:
# - How to keep a Notes column after aggregating to daily time step?

# 1.0 Libraries and work space ----------------------------------------------

# Purpose: Compile output.csv data
remove(list = ls())

library(dygraphs)
library(xts)
library(purrr)
library(stringr)
library(readxl)
library(tidyverse)
library(lubridate)

#Functions
source("functions//dygraph_ts_fun.R")

#Set data directory
data_dir <- "data//"

#Create a big list of files which you can query later
files <- list.files(paste0("data//"), full.names =  TRUE, recursive = T) 

# 2.0 Download all data after Fall 2019  -------------------------------------------------

# 2.1 Download the checks ---------------------------------------------------------------------

# Quick funtion to keep the file path as a column for the checks. 
download_checks <- function(file_path){
  #Read the files
  temp <- read_csv(paste0(file_path)) %>% 
    as_tibble() %>% 
    mutate(Well_head_m = as.character(Well_head_m),
           Depth_to_water_m = as.character(Depth_to_water_m)) %>% 
    mutate(file = str_sub(file_path)) %>% 
    mutate(Date = mdy(Date)) %>%
    rename("Site_ID" = Site_Name)
  
  temp
}

#Find checks inside the file list
checks <- files[str_detect(files, "checks_")]
                     
#Download the checks and combine to a single data frame. 
checks <- checks %>% 
  map_dfr(download_checks) 

# 2.2 Download the J. Maze outputs ------------------------------------------------

df <- read_csv(file = paste0(data_dir, "output_JM_2019_2022.csv")) %>% 
  #Filter out the bad data
  filter(!Flag == "2") %>%
  #Aggregate data to daily waterlevel
  mutate(Date = str_sub(as.character(Timestamp), start = 1, end = 10)) %>%
  group_by(Date, Site_Name) %>%
  summarise(dly_mean_wtrlvl = mean(waterLevel),
            Flag = mean(Flag)) %>% 
  #Flag = 1 for entire day of data if one time point is flagged. 
  mutate(Flag = if_else(Flag > 0,
                        "1",
                        "0")) %>%
  #Rename column to proper name
  rename("Site_ID" = Site_Name) %>% 
  mutate(Date = ymd(Date))

df <- as_tibble(df)

# 3.1 Augment original checks file -------------------------------------------------

#!! Some of the checks need to be corrected bc, PTs died or filled up with data. 

#List of sites that filled up with data in Fall 2021
lost_F2021 <- c("BD-CH", "DK-CH", "HB-SW", "HB-UW1", "MB-CH", "MB-SW", "MB-UW1", 
                "OB-CH", "OB-SW", "OB-UW1", "TS-CH", "TS-UW1", "XB-SW", "XB-UW1",
                "TB-SW")

temp <- checks %>% 
  #Select sites to revise checks 
  filter((file == "data//checks_20211112_JM.csv" &
         Site_ID %in% lost_F2021) |
         (file == "data//checks_20201015_JM.csv" &
         Site_ID == "TB-UW2")) %>% 
  #Notes mentioning use of modeled data
  mutate(Notes = paste0(Notes, ". Used modeled data for check, no data on download date.")) %>%
  #Remove columns for correction
  select(-c(measured_diff, sensor_wtrlvl)) %>% 
  #Join df with temp to get modeled values to replace incorrect checks
  left_join(., df, by = c("Date", "Site_ID")) %>% 
  #Use modeled data to calculate checks
  mutate(sensor_wtrlvl = dly_mean_wtrlvl,
         measured_diff = dly_mean_wtrlvl - Relative_water_level_m) %>% 
  #Remove extra columns
  select(-c(dly_mean_wtrlvl, Flag))

checks1 <- checks %>% 
  #Remove the incorrect checks from the checks file. Calculated these values with wrong data periods. 
  filter(!(file == "data//checks_20211112_JM.csv" &
         Site_ID %in% lost_F2021)| 
         (file == "data//checks_20201015_JM.csv" &
         Site_ID == "TB-UW2")) 

#Combine original checks with modeled values in temp
checks1 <- bind_rows(checks1, temp)
  
rm(temp, lost_F2021)


# 3.2 Pull checks values from df -------------------------------------------------

checks2 <- checks %>% 
  select(-c(measured_diff, sensor_wtrlvl)) %>% 
  #Pull checks values from the day pior to download. Oftentimes there's limited data on download days. 
  mutate(Date_minus = Date - days(1)) %>% 
  left_join(., df, by = c("Date", "Site_ID")) %>% 
  mutate(Notes = paste0(Notes, ". Used modeled data for check.")) %>% 
  mutate(sensor_wtrlvl = dly_mean_wtrlvl,
         measured_diff = dly_mean_wtrlvl - Relative_water_level_m) %>% 
  #Remove extra columns
  select(-c(dly_mean_wtrlvl, Flag))

# 3.3 Plot the checks --------------------------------------------------

#!!! Some of the checks values are really wonky from improper measurements 
#!!! and unaligned download dates. In those circumstances, I filter out checks values. 

# Checks taken from specific downloads (more incorrect values here)
checks_interest <- checks1 %>%
  filter(!Site_ID == "HB-CH") %>%
  filter(!(Site_ID %in% c("NB-SW", "ND-SW", "TB-UW2") &
         file == "data//checks_20201015_JM.csv")) %>%
  filter(!(Site_ID %in% c("DK-UW1") &
          file == "data//checks_20210525_JM.csv")) %>%
  filter(!(Site_ID %in% c("ND-UW1", "ND-UW2", "ND-UW3", "TA-SW", "BD-SW", "MB-SW") &
         file == "data//checks_20211112_JM.csv"))

# Checks taken from processed data (slightly more error)
checks_interest <- checks2 %>%
  filter(!Site_ID == "HB-CH") %>%
  filter(!(Site_ID %in% c("NB-SW", "ND-SW") &
          file == "data//checks_20201015_JM.csv")) %>%
  filter(!(Site_ID == "DK-UW1" & 
           file == "data//checks_20210525_JM.csv")) %>% 
  filter(!(Site_ID %in% c("ND-UW1", "ND-UW2", "TA-SW", "BD-SW") &
           file == "data//checks_20211112_JM.csv"))

#Plot differences between sensors and field measured values for checks. 
checks_plot <- ggplot(data = checks_interest,
                           aes(x = Site_ID,
                               y = measured_diff,
                               fill = file)) +
  geom_col(position = position_dodge(width = 0.75,
                                     preserve = "single"),
           color = "black",
           width = 0.75) +
  theme_bw() +
  theme(axis.text = element_text(size = 8,
                                 face = "bold",
                                 angle = 90),
        axis.title.x = element_text(),
        panel.grid = element_blank()) +
  ylab("Meters difference (sensor - field)")

(checks_plot)

rm(checks1, checks2, checks_plot)

# 4.0 Quick plots of timeseries together ----------------------------------


df_interest <- df %>%
  #Select sites of interest
  filter(Site_ID %in% c("HB-SW", "HB-CH", "MB-CH")) %>%
  #Add 100 to waterlevel since dygraphs struggles with negative numbers
  mutate(waterLevel = dly_mean_wtrlvl + 100) %>% 
  rename(Timestamp = Date) %>%
  select(-c(dly_mean_wtrlvl, Flag))

#Pivot wider for dygraph's xts format
df_interest <- pivot_wider(data = df_interest,
                           names_from = c("Site_ID"),
                           values_from = "waterLevel") %>%
  mutate(Timestamp = ymd(Timestamp))

#Plot in dygraphs
dygraph_ts_fun(df_interest)

# 5.0 Xport! ------------------------------------------------------------------

#Write aggregated daily data
write_csv(df, paste0(data_dir,"dly_mean_output_JM_2019_2022.csv"))








