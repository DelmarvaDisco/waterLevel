#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Anomaly Remover PT data
#Coder: James Maze (jtmaze@umd.edu)
#Date: 3/3/2022
#Purpose: To remove unusual/low values from PTs
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Function ----------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

fun_anomalous <- function(df, #timeseries data with anomalous values
                          min, #The minimum threshold for residuals from rolling median 
                          max #The maximum threshold for residuals from rolling median
){
  #load packages
  library(tidyverse)
  library(zoo)
  
  #Check values against the residuals of a rolling median. 
  df <- df %>% 
    mutate("rolling_median" = rollmedian(df$waterLevel,
                                         k = 11,
                                         fill = NA, 
                                         align = "center")) %>% 
    
    mutate("residuals" = waterLevel - rolling_median) %>% 
    filter(residuals > min) %>% 
    filter(residuals < max)
  
  #Clean up the dataframe
  df <- df %>% 
   select(waterLevel, Timestamp, Site_Name)
  
  #return the df without anomalous values
  return(df)
}

