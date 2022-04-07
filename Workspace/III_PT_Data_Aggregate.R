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

# 2. Prior to April 2019 by N. Jones ------------------------------------------

# #Read Choptank WY2019 sheet
# owl <- read_xlsx(paste0(data_dir,"Choptank_Wetlands_WY2019.xlsx"), sheet = 3)
# gwl <- read_xlsx(paste0(data_dir,"Choptank_Wetlands_WY2019.xlsx"), sheet = 2)
# swl <- read_xlsx(paste0(data_dir,"Choptank_Wetlands_WY2019.xlsx"), sheet = 1)
# 
# #Merge the sheets 
# dx <- full_join(owl, gwl, by = "Timestamp")
# dx <- full_join(dx, swl, by = "Timestamp")
# rm(owl, gwl, swl)
# 
# #Covert everything except "Timestamp" to character, to make merging easier. 
# dx <- dx %>% 
#   mutate_if(is.numeric, as.character)
# 
# #Pivot to long format
# dx <- pivot_longer(dx, cols = -c("Timestamp"), 
#                    names_to = "Site_Name", 
#                    values_to = "waterLevel")
# 
# #Check make a tibble with list of site names
# site_names <- as_tibble(unique(dx$Site_Name))

# 3. Jones processing Wardinski (Select Sites 2019 - 2020) ------------------------------------------

# dt <- files %>% 
#   as_tibble() %>% 
#   filter(str_detect(value,"output.csv")) %>% 
#   as_vector() %>% 
#   map_dfr(function(x) read_csv(x, col_types = list('T', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c')))

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

# 4.2 Download the J. Maze outputs ------------------------------------------------

JM_output <- files[str_detect(files, "output_")]

df <- JM_output %>% 
  map_dfr(read_csv) %>%  
  mutate(Timestamp = ymd_hms(Timestamp, tz = "GMT"))
  

# 4.3 Plot the checks --------------------------------------------------

checks_interest <- checks %>% 
  filter(Site_Name == "QB-UW1")
  #Checks from latest download aren't reliable (baro missmatch)
  #filter(!file == "data//checks_20211112_JM.csv")

checks_plot <- ggplot(data = checks_interest, 
                      aes(x = Site_Name,
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
        axis.title.x = element_blank()) +
  ylab("Measured diff meters (sensor - field)")
  

(checks_plot)

# Dygraph of certain sites ------------------------------------------------

df <- df %>% 
  as_tibble() %>% 
  mutate(waterLevel = round(waterLevel, digits = 4))

df <- df %>% 
  unique()

#Some overlapping data points need to be removed
df <- df[!duplicated(df[ , c("Timestamp", "Site_Name")]), ]

df_interest <- df %>% 
  filter(Site_Name %in% c("Jones Road South Catchment Outlet", 
                          "Jones Road North Catchment Outlet",
                          "Tiger Paw Catchment Outlet")) %>% 
  mutate(waterLevel = waterLevel + 100) 

df_interest <- df_interest %>% 
  pivot_wider(names_from = Site_Name,
              values_from = waterLevel)

dygraph_ts_fun(df_interest)

# Xport! ------------------------------------------------------------------

write_csv(df, paste0(data_dir,"all_data_JM.csv"))

write_csv(checks, paste0(data_dir, "all_chk_JM.csv"))






