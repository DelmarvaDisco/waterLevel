#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Aggregate
#Coder: James Maze (jtmaze@umd.edu)
#Date: 3/22/2022
#Purpose: Compile and organize processed PT data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# 1. Libraries and work space ----------------------------------------------

# Purpose: Compile output.csv data
remove(list = ls())

library(dygraphs)
library(xts)
library(readxl)
library(tidyverse)
library(lubridate)

#Functions
source("functions//dygraph_ts_fun.R")

#Set data directory
data_dir <- "data//"

#Create a big list of files which you can query later
files <- list.files(paste0("data//"), full.names =  TRUE, recursive = T) 

# 2. All data after Fall 2019 by J. Maze  -------------------------------------------------

# 2.1 Download the checks ---------------------------------------------------------------------

# Quick funtion to keep the file path as a column for the checks. 
download_checks <- function(file_path){
  #Read the files
  temp <- read_csv(paste0(file_path)) %>% 
    as_tibble() %>% 
    mutate(Well_head_m = as.character(Well_head_m),
           Depth_to_water_m = as.character(Depth_to_water_m)) %>% 
    mutate(file = str_sub(file_path)) 
  
  temp
}

#Find checks inside the file list
checks <- files[str_detect(files, "checks_")]
                     
#Download the checks
checks <- checks %>% 
  map_dfr(download_checks) 

# 3. Download the J. Maze outputs ------------------------------------------------

df <- read_csv(file = paste0(data_dir, "output_JM_2019_2022.csv"))


# 4.1 Augment checks file -------------------------------------------------


# 4.2 Plot the checks --------------------------------------------------

# checks_interest <- checks %>%
#   filter(Site_Name %in% c("JA-SW", "NB-SW", "JC-SW", "JB-UW1"))

# checks_plot <- ggplot(data = checks_interest, 
#                            aes(x = Site_Name,
#                                y = measured_diff,
#                                fill = file)) +
#   geom_col(position = position_dodge(width = 0.75,
#                                      preserve = "single"),
#            color = "black",
#            width = 0.75) + 
#   theme_bw() +
#   theme(axis.text = element_text(size = 8,
#                                  face = "bold",
#                                  angle = 90),
#         axis.title.x = element_text()) +
#   ylab("(sensor - field)")
# 
# (checks_plot)


# 5. Aggregate waterLevel to the daily timestep -------------------------------

df <- df %>% 
  filter(!Flag == "2") %>% 
  mutate(Date = str_sub(as.character(Timestamp), start = 1, end = 10)) %>% 
  group_by(Date, Site_Name) %>% 
  summarise(dly_mean_wtrlvl = mean(waterLevel))


# 5.1 Quick plots of timeseries together ----------------------------------


# df_interest <- df %>% 
#   filter(Site_Name %in% c("MB-SW", "OB-SW", "HB-SW")) %>% 
#   mutate(waterLevel = dly_mean_wtrlvl + 100) %>% 
#   filter(waterLevel >= 97) %>% 
#   rename(Timestamp = Date) %>% 
#   select(-dly_mean_wtrlvl)
# 
# df_interest <- pivot_wider(data = df_interest,
#                            names_from = c("Site_Name"),
#                            values_from = "waterLevel") %>% 
#   mutate(Timestamp = ymd(Timestamp))
# 
# dygraph_ts_fun(df_interest)

# 6 Xport! ------------------------------------------------------------------

write_csv(df, paste0(data_dir,"dly_mean_output_JM_2019_2022.csv"))

write_csv(checks, paste0(data_dir, "all_chk_JM.csv"))






