#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Gap filling 2020-2022
#Coder: James Maze (jtmaze@umd.edu)
#Date: 6/17/2022
#Purpose: Fill data gaps in waterLevel data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#

# 1. Libraries and workspace ----------------------------------------------

remove(list = ls())

library(tidyverse)
library(lubridate) 
library(purrr)
library(dplyr)
library(xts)
library(dygraphs)

source("functions//dygraph_ts_fun.R")

data_dir <- "data/"


# 2. Read in the data -----------------------------------------------------

output_files <- list.files(path = paste0(data_dir), full.names = TRUE) 

output_files <- output_files[str_detect(output_files, pattern = "output_20")]

outputs <- output_files %>% 
  map_dfr(read_csv) %>% 
  as_tibble() %>% 
  #Make the coluns formating work
  mutate(Site_Name = as.character(Site_Name),
         waterLevel = as.numeric(waterLevel),
         Timestamp = ymd_hms(Timestamp), 
         Flag = as.character(Flag), 
         Notes = as.character(Notes))


# 3. Identify time points with data gaps -----------------------------------

#List of sites to check for data gaps
Sites <- unique(outputs$Site_Name) %>%
  as_tibble() 

#BD_CH
BD_CH <- outputs %>% 
  filter(Site_Name == "BD-CH") %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(Timestamp, waterLevel)

dygraph_ts_fun(BD_CH) 
rm(BD_CH)


# More sites --------------------------------------------------------------

#BD_SW

#DB_SW

#DB_UW1

#DF_SW

#DK_CH

DK_CH <- outputs %>% 
  filter(Site_Name == "DK-CH") %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(Timestamp, waterLevel)

dygraph_ts_fun(BD_CH) 
rm(BD_CH)

#DK_SW

#DK-UW1

#DK-UW2

#FN-SW

#HB-CH

#HB-SW

#HB-UW1

#JA-SW

#JB-SW

#JB-UW1

#JB-UW2

#JC-SW

#JC-UW1

#Jones Road North Catchment Outlet

#Jones Road South Catchment Outlet

#MB-CH

#MB-SW

#MB-UW1

#ND-SW

#ND-UW1

ND_UW1 <- outputs %>% 
  filter(Site_Name == "ND-UW1") %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(Timestamp, waterLevel)

dygraph_ts_fun(ND_UW1) 
rm(ND_UW1)

#ND-UW2

ND_UW2 <- outputs %>% 
  filter(Site_Name == "ND-UW2") %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(Timestamp, waterLevel)

dygraph_ts_fun(ND_UW2) 
rm(ND_UW2)

#ND-UW3

#OB-CH

#OB-SW

#OB-UW1

#QB-SW

#QB-UW1

#QB-UW2

#TA-SW

#TB-SW

#TB-UW1

#TB-UW2

#TB-UW3

#TI-SW

#Tiger Paw Catchment Outlet

#TP-CH

#TS-CH

TS_CH <- outputs %>% 
  filter(Site_Name == "TS-CH") %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(Timestamp, waterLevel)

dygraph_ts_fun(TS_CH) 
rm(TS_CH)

#TS-SW

#TS-UW1

#XB-CH

#XB-SW

#XB-UW1




# 4. Fill gaps with correlations -----------------------------------------

#List of gaps to fix
# 1) BD-CH Fall 2021 
#   - used ND-UW1 as a corr r^2 = .975
# 2) BD-SW 

# 4.1 BD-CH Fall 2021 -----------------------------------------------------

temp <- outputs %>% 
  filter(Site_Name %in% c("BD-CH", "ND-UW1")) %>% 
  #BD_CH not installed until 03-02
  filter(Timestamp >= "2021-03-02 2:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("corr" = `BD-CH`,
         "fill" = `ND-UW1`)

#Plot correlation
(ggplot(data = temp,
        mapping = aes(x = `corr`,
                      y = `fill`)) +
  geom_point())

#Make a model (linear)
model <- lm(`corr` ~ `fill`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(predict = predict(model, data.frame(fill = fill)))

#Compare modeled to data 
test <- ggplot(data = temp,
               mapping = aes(x = Timestamp,
                             y = corr)) +
        geom_line() +
        geom_point(aes(y = predict),
                   size = 0.1,
                   color = "tomato") 
  
(test)


# BD-SW -------------------------------------------------------------------




  




