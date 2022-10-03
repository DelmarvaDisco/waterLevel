#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Dry well's lower bound QAQC
#Coder: James Maze (jtmaze@umd.edu)
#Date: 9/28/2022
#Purpose: Identify and Flag Dry Periods for each well
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# !!! Jones Rd Catchment Outlet North moves 
# 5 cm lower between Summer 2021 and Summmer 2022


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

chk <- df %>% 
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

chk <- df %>% 
  filter(Site_Name %in%  c("HB-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! No lower bound

rm(chk)


# 3.9 HB-UW1 --------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("HB-UW1")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! No lower bound

rm(chk)


# 3.10 MB-CH ---------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("MB-CH")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Lower bound at -0.79 meters !!!

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "MB-CH",
                               "-0.79",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "MB-CH") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -0.79, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -0.79,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -0.79,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "MB-CH")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)

# 3.11 MB-SW --------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("MB-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! No lower bound

rm(chk)

# 3.12 MB-UW1 ------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("MB-UW1")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Lower bound at -1.43 meters !!!

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "MB-UW1",
                               "-1.43",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "MB-UW1") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -1.43, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -1.43,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -1.43,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "MB-UW1")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)

# 3.13 ND-SW --------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("ND-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! No lower bound

rm(chk)

# 3.14 ND-UW1 -------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("ND-UW1")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! No lower bound

rm(chk)

# 3.15 ND-UW2 ------------------------------------------------------------------


chk <- df %>% 
  filter(Site_Name %in%  c("ND-UW2")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! No lower bound

rm(chk)

# 3.16 ND-UW3 --------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("ND-UW3")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! No lower bound

rm(chk)


# 3.17 OB-CH -------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("OB-CH")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Lower bound at -0.90 m

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "OB-CH",
                               "-0.9",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "OB-CH") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -0.9, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -0.9,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -0.9,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "OB-CH")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)


# 3.18 OB-SW -------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("OB-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)


# 3.19 OB-UW1 -------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("OB-UW1")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Lower bound at -1.58m

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "OB-UW1",
                               "-1.58",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "OB-UW1") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -1.58, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -1.58,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -1.58,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "OB-UW1")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)

# 3.20 TP-CH -------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("TP-CH")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Lower bound at -0.29 meters

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "TP-CH",
                               "-0.29",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "TP-CH") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -0.29, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -0.29,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -0.29,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "TP-CH")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)

# 3.21 TS-CH --------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("TS-CH")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

# 3.22 TS-SW  -------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("TS-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

# 3.23 TS-UW1 -------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("TS-UW1")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#Lower bound at -1.28 m

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "TS-UW1",
                               "-1.28",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "TS-UW1") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -1.28, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -1.28,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -1.28,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "TS-UW1")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)

# 3.24 XB-CH ----------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("XB-CH")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)


# 3.25 XB-SW --------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("XB-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)


# 3.26 XB-UW1 -------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("XB-UW1")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#Lower bound at -1.38 meters

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "XB-UW1",
                               "-1.38",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "XB-UW1") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -1.38, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -1.38,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -1.38,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "XB-UW1")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)


# 3.27 DB-SW --------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("DB-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)


# 3.28 DB-UW1 --------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("DB-UW1")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#Possible lower bound at ~ -1.55 meters. See more data. 


# 3.29 DF-SW --------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("DF-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

# 3.30 FN-SW --------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("FN-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

# 3.31 JA-SW --------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("JA-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#Lower bound at -0.18 meters

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "JA-SW",
                               "-0.18",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "JA-SW") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -0.18, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -0.18,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -0.18,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "JA-SW")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)


# 3.32 JB-SW --------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("JB-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)


# 3.33 JB-UW1 --------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("JB-UW1")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Lower bound at 1.51 meters !!!

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "JB-UW1",
                               "-1.50",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "JB-UW1") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -1.50, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -1.50,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -1.50,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "JB-UW1")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)

# 3.34 JB-UW2 -------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("JB-UW2")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#Lower bound at about -1.16 meters

#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "JB-UW2",
                               "-1.16",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "JB-UW2") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -1.16, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -1.16,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -1.16,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "JB-UW2")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)

# 3.35 JC-SW -------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("JC-SW")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

# 3.36 JC-UW1 --------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("JC-UW1")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Lower bound at -1.41 !!!
#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "JC-UW1",
                               "-1.41",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "JC-UW1") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -1.41, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -1.41,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -1.41,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "JC-UW1")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)

# 3.37 Jones Road North Catchment Outlet --------------------------------------------------------------------

# chk <- df %>% 
#   filter(Site_Name %in%  c("Jones Road North Catchment Outlet")) %>% 
#   mutate(waterLevel = waterLevel + 100) %>% 
#   select(-c(Flag, Notes)) 
# 
# dygraph_ts_fun(chk)
# 
# #!!! Lower bound at -1.41 !!!
# #Add lower bound to sites list
# sites_lowerbound <- sites_lowerbound %>% 
#   mutate(lower_bound = if_else(Site_Name == "Jones Road North Catchment Outlet",
#                                "-",
#                                lower_bound))
# 
# #Modify the data using the lower bound
# temp <- df %>% 
#   filter(Site_Name == "Jones Road North Catchment Outlet") %>% 
#   mutate(#Rewrite Flag column
#     Flag = ifelse(waterLevel <= -, 
#                   "2",
#                   Flag),
#     #Rewrite Notes column
#     Notes = ifelse(waterLevel <= -,
#                    "Well dry, no data",
#                    Notes),
#     #Rewrite waterLevel colum
#     waterLevel = ifelse(waterLevel <= -,
#                         "NA",
#                         waterLevel)) %>% 
#   #Reformat waterLevel and Flag columns
#   mutate(waterLevel = as.numeric(waterLevel), 
#          Flag = as.numeric(Flag))
# 
# #Combine temp and df replacing the incorrect data in df. 
# 
# df <- df %>% 
#   #Get rid of incorrect site data
#   filter(!Site_Name == "Jones Road North Catchment Outlet")
# 
# #Add modified/corrected site data to df
# df <- bind_rows(df, temp)
# 
# #Clean up environment
# rm(chk, temp)

# 3.38 Jones Road South Catchment Outlet -------------------------------------------------------------------

chk <- df %>% 
  filter(Site_Name %in%  c("Jones Road South Catchment Outlet")) %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(-c(Flag, Notes)) 

dygraph_ts_fun(chk)

#!!! Lower bound at -0.3 !!!
#Add lower bound to sites list
sites_lowerbound <- sites_lowerbound %>% 
  mutate(lower_bound = if_else(Site_Name == "Jones Road South Catchment Outlet",
                               "-0.3",
                               lower_bound))

#Modify the data using the lower bound
temp <- df %>% 
  filter(Site_Name == "Jones Road South Catchment Outlet") %>% 
  mutate(#Rewrite Flag column
    Flag = ifelse(waterLevel <= -0, 
                  "2",
                  Flag),
    #Rewrite Notes column
    Notes = ifelse(waterLevel <= -0,
                   "Well dry, no data",
                   Notes),
    #Rewrite waterLevel colum
    waterLevel = ifelse(waterLevel <= -0,
                        "NA",
                        waterLevel)) %>% 
  #Reformat waterLevel and Flag columns
  mutate(waterLevel = as.numeric(waterLevel), 
         Flag = as.numeric(Flag))

#Combine temp and df replacing the incorrect data in df. 

df <- df %>% 
  #Get rid of incorrect site data
  filter(!Site_Name == "Jones Road Sounh Catchment Outlet")

#Add modified/corrected site data to df
df <- bind_rows(df, temp)

#Clean up environment
rm(chk, temp)


# Scatch ------------------------------------------------------------------











temp <- pivot_wider(data = temp,
            names_from = c("Site_Name"),
            values_from = "waterLevel") 

str(temp)

hmm <- df %>% filter(Site_Name %in% c("DK-CH",))

rm(hmm)



