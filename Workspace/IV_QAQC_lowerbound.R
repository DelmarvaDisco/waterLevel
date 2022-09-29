#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Dry well's lower bound QAQC
#Coder: James Maze (jtmaze@umd.edu)
#Date: 9/28/2022
#Purpose: Identify and Flag Dry Periods for each well
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#


# 1. Libraries and packages -----------------------------------------------

remove(list = ls())

library(tidyverse)
library(xts)
library(dygraphs)

source("functions//dygraph_ts_fun.R")

data_dir <- "data/"

# 2. Read the data ---------------------------------------------------------------------

df <- read_csv(paste0(data_dir, "output_JM_2019_2022.csv"))

#Create a list of lowerbounds for each site. May be helpful later.
sites_lowerbound <- unique(df$Site_Name) %>% data.frame() %>% 
  rename("Site_Name" = ".") %>%
  add_column("lower_bound" = "NA")

# 3. Find and flag dry points for wells.  ---------------------------------

# 3.1 BD-CH -------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name == "BD-CH") %>% 
  mutate(waterLevel = waterLevel +100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Well deep enough no lower bound.

rm(chk)

# 3.2 BD-SW -------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("BD-SW")) %>% 
  mutate(waterLevel = waterLevel +100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Might have lower bound at -0.35m. Check with more data. 

rm(chk)

# 3.3 DK-CH --------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name == "DK-CH") %>%
  mutate(waterLevel = waterLevel +100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Lower bound at -0.60m !!!

#Add lower bound to site's lowerbound table
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "DK-CH",
                               "-0.60",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "DK-CH") %>% 
  mutate(#Rewrite Flag column
         Flag = ifelse(waterLevel <= -0.60, 
                       "2",
                       Flag),
         #Rewrite Notes column
         Notes = ifelse(waterLevel <= -0.60,
                        "Well dry, no data",
                        Notes),
         #Rewrite waterLevel column
         waterLevel = ifelse(waterLevel <= -0.60,
                             "NA",
                             waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 
#Get rid of incorrect site data
df <- df %>% 
  filter(!Site_Name == "DK-CH")

#Add modified/correct site data to df
df <- bind_rows(df, temp)

#Clean up environment 
rm(chk, temp)

# 3.4 DK-SW -------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("DK-SW")) %>% 
  mutate(waterLevel = waterLevel +100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! No lower bound

rm(chk)

# 3.5 DK-UW1 ------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("DK-UW1")) %>% 
  mutate(waterLevel = waterLevel +100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! No lower bound

rm(chk)

# 3.6 DK-UW2 --------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("DK-UW2")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! No lower bound

rm(chk)

# 3.7 HB-CH  --------------------------------------------------------------------

chk <- temp %>% 
  filter(Site_Name %in%  c("HB-CH")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Lower bound at -0.64m !!!

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "HB-CH",
                               "-0.64",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "HB-CH") %>% 
  mutate(#Rewrite Flag column
         Flag = ifelse(waterLevel <= -0.64, 
                       "2",
                       Flag),
         #Rewrite Notes column
         Notes = ifelse(waterLevel <= -0.64,
                        "Well dry, no data",
                        Notes),
         #Rewrite waterLevel colum
         waterLevel = ifelse(waterLevel <= -0.64,
                             "NA",
                             waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "HB-CH")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment 
rm(chk, temp)


# 3.8 HB-SW --------------------------------------------------------------------



# Scatch ------------------------------------------------------------------

temp <- pivot_wider(data = temp,
            names_from = c("Site_Name"),
            values_from = "waterLevel") 

str(temp)

hmm <- df %>% filter(Site_Name %in% c("DK-CH",))

rm(hmm)



