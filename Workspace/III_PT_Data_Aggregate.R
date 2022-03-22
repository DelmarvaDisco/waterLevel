# Purpose: Compile output.csv data
# !!!Notes: 
# - df works with the Choptank_Wetlands_WY2019.xlsx file, pivoting into a long csv.
# - dt compiles all other output files. STILL needs work compiling. 
# !!!

library(readxl)
library(tidyverse)

#Set data directory
data_dir <- "data//"


# 2. Prior to April 2019 by Nate Jones ------------------------------------------

#Read Choptank WY2019 sheet
owl <- read_xlsx(paste0(data_dir,"Choptank_Wetlands_WY2019.xlsx"), sheet = 3)
gwl <- read_xlsx(paste0(data_dir,"Choptank_Wetlands_WY2019.xlsx"), sheet = 2)
swl <- read_xlsx(paste0(data_dir,"Choptank_Wetlands_WY2019.xlsx"), sheet = 1)

#Merge the sheets 
df <- full_join(owl, gwl, by = "Timestamp")
df <- full_join(df, swl, by = "Timestamp")
rm(owl, gwl, swl)

#Covert everything except "Timestamp" to character, to make merging easier. 
df <- df %>% 
  mutate_if(is.numeric, as.character)

#Pivot to long format
df <- pivot_longer(df, cols = -c("Timestamp"), 
                   names_to = "Site_Name", 
                   values_to = "waterLevel")

#Check make a tibble with list of site names
site_names <- as_tibble(unique(df$Site_Name))

# 3. Nate Jones processing for Katie Wardinski (Select Sites 2019 - 2020) ------------------------------------------


#Aggregate all the other output tables
dt<-list.files(paste0("data//"), full.names =  TRUE, recursive = T) %>% 
  as_tibble() %>% 
  filter(str_detect(value,"output.csv")) %>% 
  as_vector() %>% 
  map_dfr(function(x) read_csv(x, col_types = list('T', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c')))


# 4. After Fall 2019 by James Maze  -------------------------------------------------




# Xport! ------------------------------------------------------------------

write_csv(df, file = "ouput_data_long.csv")





