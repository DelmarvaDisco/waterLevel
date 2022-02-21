# Compile output.csv data

library(readxl)
library(tidyverse)

#Set data directory
data_dir <- "data//"


# 2. Read data prior to 20190422 ------------------------------------------

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

# 3. Read in data after 20190422 ------------------------------------------


#Aggregate all the other output tables
dt<-list.files(paste0("data//"), full.names =  TRUE, recursive = T) %>% 
  as_tibble() %>% 
  filter(str_detect(value,"output.csv")) %>% 
  as_vector() %>% 
  map_dfr(function(x) read_csv(x, col_types = list('T', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c', 'c')))


# 4. Clean up other output tables to merge with df ------------------------

dt <- dt %>% 
  #Remove values from dt (post Apr 2019) included in df (pre Apr 2019)
  #filter(Timestamp > "2019-04-22 20:00:00") %>% 
  #Trim down the data table
  select(c("Timestamp", "name", "value", "Site_Name", "waterHeight", "waterLevel"))

#Find data entries with missing Site_Name and replace those values with values from the "name" column
#See counts for nas
na_Site_Name <- sum(is.na(dt$Site_Name))
not_na_name <- sum(is.na(dt$name))

dt <- dt %>% 
  mutate(Site_Name = ifelse(is.na(dt$Site_Name), 
                            dt$name, 
                            dt$Site_Name)) %>% 
  select(-name)

#Find data entries with a missing "waterLevel" and replace those values from the "value" column
dt <- dt %>% 
  mutate(waterLevel = ifelse(is.na(dt$waterLevel),
                             dt$value,
                             dt$waterLevel)) 


# Xport! ------------------------------------------------------------------

write_csv(df, file = "ouput_data_long.csv")





