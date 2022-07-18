#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Gap filling 2020-2022
#Coder: James Maze (jtmaze@umd.edu)
#Date: 6/17/2022
#Purpose: Fill data gaps in waterLevel data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
#

# 1. Libraries and work space ----------------------------------------------

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

df <- output_files %>% 
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
Sites <- unique(df$Site_Name) %>%
  as_tibble() 

#Sandbox to look at timeseries
XB_SW <- df %>%
  filter(Site_Name == "XB-SW") %>%
  # filter(!Flag == "2") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(XB_SW)
rm(XB_SW)

# 4. Fill gaps with correlations -----------------------------------------

#List of gaps to fix


# 4.1 BD-CH Fall 2021 -----------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("BD-CH", "BD-SW")) %>% 
  #BD_CH not installed until 03-02
  filter(Timestamp >= "2021-03-02 2:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `BD-CH`,
         "fill" = `BD-SW`)

#Plot correlation
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill`)) +
#   geom_point())

#Make a model (linear)
model <- lm(`gap` ~ `fill`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill = fill)))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp, #%>%
#                            # filter(Timestamp >= "2021-09-25 12:00:00" &
#                            #        Timestamp <= "2021-12-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_point(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato") +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  #Only use model for the gap section
  filter(Timestamp >= "2021-10-14 12:00:00" & Timestamp <= "2021-11-03 4:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Gap filled with BD-SW as correllary (r^2 = .9899). No delta value.",
                         Notes),
         Site_Name = "BD-CH") %>% 
  select(-c(gap, fill, prediction)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up environment
rm(model, temp, test_plot)

# 4.2 DK-CH Fall 2021 ---------------------------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("DK-CH", "DK-UW2")) %>% 
  #DK_CH not installed until 03-02, but DK-UW2 installed way longer
  filter(Timestamp >= "2021-03-02 2:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `DK-CH`,
         "fill" = `DK-UW2`)

#Make a model (linear)
#!! Filtering the lowest water points where PT dries improves the model fit. 
model <- lm(`gap` ~ `fill`, data = temp %>% filter(fill > -0.87))
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill = fill)))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                            filter(Timestamp >= "2021-08-25 12:00:00" &
#                                   Timestamp <= "2022-01-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_point(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato")  +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  filter(Timestamp >= "2021-10-14 13:15:00" & Timestamp <= "2021-11-03 18:15:00") %>% 
  #Modeled data over-estimates highest water values
  filter(prediction <= .17) %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Gap filled with DK-UW2 as correllary r^2 = .978. No delta value.",
                         Notes),
         Site_Name = "DK-CH") %>% 
  select(-c(gap, fill, prediction)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)

# 4.3 HB-CH Fall 2021 ---------------------------------------------------------------------

# Couldn't find good model. Skipped filling this site. 

# 4.4 HB-SW Fall 2021 -----------------------------------------------------

#Using multiple sites improves performance
temp <- df %>%
  filter(Site_Name %in% c("HB-SW", "QB-SW", "TI-SW", 
                          "JB-SW", "JC-SW", "DF-SW")) %>%
  #HB-SW not installed until 2021-03-10
  filter(Timestamp >= "2021-03-10 21:30:00") %>%
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>%
  rename("gap" = `HB-SW`,
         "fill1" = `QB-SW`,
         "fill2" = `TI-SW`,
         "fill3" = `JB-SW`,
         "fill4" = `JC-SW`,
         "fill5" = `DF-SW`)

#Plot correlation
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = fill5)) +
#     geom_point())

#Make a model (linear)
model <- lm(`gap` ~ `fill1`+`fill2`+`fill3`+`fill4`+`fill5`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>%
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3,
                                                fill4 = fill4,
                                                fill5 = fill5))) %>% 
  select(-c(fill1, fill2, fill3, fill4, fill5)) %>% 
  mutate(prediction_delta = prediction + 0.06)

#Compare modeled prediction to data
# test_plot <- ggplot(data = temp %>%
#                     filter(Timestamp >= "2021-10-10 12:00:00" &
#                            Timestamp <= "2021-12-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#   geom_line(size = 0.2) +
#   geom_line(aes(y = prediction),
#              size = 0.05,
#              color = "tomato") +
#   geom_line(aes(y = prediction_delta),
#              size = 0.05,
#              color = "blue") +
#   ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>%
  #Using prediction delta
  select(-prediction) %>% 
  #!!! Apply this correlation to the F2021 gap.
  filter(Timestamp >= "2021-10-18 09:00:00" & Timestamp <= "2021-12-19 01:00:00") %>%
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Used multiple linear regression 5 nearby sites (JB, JC, TI, QB, DF) r^ = 0.958. Added +0.06m delta",
                         Notes),
         Site_Name = "HB-SW") %>%
  select(-c(gap, prediction_delta))

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>%
  distinct()

# #Clean up environment
rm(model, temp, test_plot)

# 4.5 HB-UW1 Fall 2021 ----------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("HB-UW1", "QB-UW2", "QB-UW1", 
                          "JB-UW1", "JB-UW2")) %>% 
  #HB-UW1 not installed until 2021-03-10
  filter(Timestamp >= "2021-03-10 21:30:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `HB-UW1`,
         "fill1" = `QB-UW2`,
         "fill2" = `QB-UW1`,
         "fill3" = `JB-UW1`,
         "fill4" = `JB-UW2`)

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill2`)) +
#   geom_point())

#Make a model (linear)
model <- lm(`gap` ~ `fill1` + `fill2` + `fill3` + `fill4`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1,
                                                fill2 = fill2, 
                                                fill3 = fill3,
                                                fill4 = fill4))) %>%
  #Added +0.08 m to connect the corrected timeseries.
  mutate(prediction_delta = prediction + 0.05) %>% 
  select(-c(fill1, fill2, fill3, fill4))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                       filter(Timestamp >= "2021-10-10 12:00:00" &
#                              Timestamp <= "2021-12-11 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_point(aes(y = prediction),
#                         size = 0.01,
#                         color = "tomato") +
#              geom_point(aes(y = prediction_delta),
#                         size = 0.01,
#                         color = "blue") +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  #Using prediction_delta
  select(-prediction) %>% 
  #!!! Since this correlation was crappy, only apply it to the F2021 gap.
  filter(Timestamp >= "2021-10-18 09:00:00" & Timestamp <= "2021-11-19 01:00:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Gap filled with multiple linear regression from 4 sites (QB-UW2, QB-UW1, JB-UW1, JB-UW2) as correllary r^2 = 0.952. Also added +0.05 m delta",
                         Notes),
         Site_Name = "HB-UW1") %>% 
  select(-c(gap, prediction_delta)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up environment
rm(model, temp, test_plot)

# 4.6 MB-CH Fall 2021  --------------------------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("MB-CH", "QB-UW1", "TP-CH", "DF-SW",
                          "Jones Road North Catchment Outlet", "TI-SW")) %>% 
  #MB_CH not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `MB-CH`,
         "fill1" = `QB-UW1`,
         "fill2" = `TP-CH`,
         "fill3" = `Jones Road North Catchment Outlet`,
         "fill4" = `TI-SW`,
         "fill5" = `DF-SW`) 

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill4`)) +
#     geom_point())

#Make a model (linear)
model <- lm(`gap` ~ `fill1`+`fill2`+`fill3`+`fill4`+`fill5`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1,
                                                fill2 = fill2, 
                                                fill3 = fill3, 
                                                fill4 = fill4,
                                                fill5 = fill5))) %>% 
  mutate(prediction_delta = prediction + 0.03) %>% 
  select(-c(fill1, fill2, fill3, fill4, fill5))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp, #%>%
#                            # filter(Timestamp >= "2021-08-25 12:00:00" &
#                            #        Timestamp <= "2022-01-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_line(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato")  +
#              geom_line(aes(y = prediction_delta),
#                         size = 0.1,
#                         color = "blue")  +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  select(-prediction) %>% 
  filter(Timestamp >= "2021-10-14 12:15:00" & Timestamp <= "2021-11-18 19:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 5 sites (QB-UW1, TP-CH, Jones Road North Catchment Outlet, TI-SW, DF-SW) as corollaries r^2 = 0.9719. Also added +0.03 m delta.",
                         Notes),
         Site_Name = "MB-CH") %>% 
  select(-c(gap, prediction_delta)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)

# 4.7 MB-SW Fall 2021 -----------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("MB-SW", "QB-SW", "DF-SW", "TI-SW", "JB-SW")) %>% 
  #MB_SW not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `MB-SW`,
         "fill1" = `QB-SW`,
         "fill2" = `DF-SW`,
         "fill3" = `TI-SW`,
         "fill4" = `JB-SW`) 

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill1`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`+`fill4`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3, 
                                                fill4 = fill4))) %>% 
  mutate(prediction_delta = prediction + 0.03) %>% 
  select(-c(fill1, fill2, fill3, fill4))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                            filter(Timestamp >= "2021-08-25 12:00:00" &
#                                   Timestamp <= "2022-01-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_line(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato")  +
#              geom_line(aes(y = prediction_delta),
#                         size = 0.1,
#                         color = "blue")  +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  select(-prediction) %>% 
  filter(Timestamp >= "2021-10-20 12:15:00" & Timestamp <= "2021-11-19 18:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 4 sites (QB-SW,  DF-SW,  TI-SW,  JB-SW) as corollaries r^2 = 0.979. Also added +0.03 m delta.",
                         Notes),
         Site_Name = "MB-SW") %>% 
  select(-c(gap, prediction_delta)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)

# 4.8 MB-UW1 Fall 2021 --------------------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("MB-UW1", "QB-UW1", "JB-UW2", "JC-UW1")) %>% 
  #MB_UW1 not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `MB-UW1`,
         "fill1" = `QB-UW1`,
         "fill2" = `JB-UW2`,
         "fill3" = `JC-UW1`) 

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill3`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3))) %>% 
  select(-c(fill1, fill2, fill3))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                            filter(Timestamp >= "2021-9-20 12:00:00" &
#                                   Timestamp <= "2021-11-29 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_line(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato")  +
#              # geom_line(aes(y = prediction_delta),
#              #            size = 0.1,
#              #            color = "blue")  +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  filter(Timestamp >= "2021-10-20 12:15:00" & Timestamp <= "2021-11-18 18:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 3 sites (QB-UW1,  JB-UW2, JC-UW1) as corollaries r^2 = 0.954. No delta value.",
                         Notes),
         Site_Name = "MB-UW1") %>% 
  select(-c(gap, prediction)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)

# 4.9 OB-CH Fall 2021 ---------------------------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("OB-CH", "QB-UW2", "JB-UW2", "DF-SW", "TI-SW", "TP-CH")) %>% 
  #OB-CH not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `OB-CH`,
         "fill1" = `QB-UW2`,
         "fill2" = `JB-UW2`,
         "fill3" = `DF-SW`,
         "fill4" = `TI-SW`,
         "fill5" = `TP-CH`) 

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill3`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`+`fill4`+`fill5`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3,
                                                fill4 = fill4,
                                                fill5 = fill5))) %>% 
  mutate(prediction_delta = prediction - 0.02) %>% 
  select(-c(fill1, fill2, fill3, fill4, fill5))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                            filter(Timestamp >= "2021-09-25 12:00:00" &
#                                   Timestamp <= "2021-12-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_line(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato")  +
#              geom_line(aes(y = prediction_delta),
#                         size = 0.1,
#                         color = "blue")  +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  select(-prediction) %>% 
  filter(Timestamp >= "2021-10-22 5:00:00" & Timestamp <= "2021-11-18 18:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 5 sites (QB-UW2, JB-UW2, DF-SW, TI-SW, TP-CH) as corollaries r^2 = 0.959. Also added a -0.02 delta.",
                         Notes),
         Site_Name = "OB-CH") %>% 
  select(-c(gap, prediction_delta)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)


# 4.10 OB-SW Fall 2021 ----------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("OB-SW", "JC-SW", "JA-SW", "TI-SW",
                          "DF-SW")) %>% 
  #OB-SW not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `OB-SW`,
         "fill1" = `JC-SW`,
         "fill2" = `JA-SW`,
         "fill3" = `TI-SW`,
         "fill4" = `DF-SW`)

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill3`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`+`fill4`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3,
                                                fill4 = fill4))) %>% 
  mutate(prediction_delta = prediction + 0.03) %>% 
  select(-c(fill1, fill2, fill3, fill4))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                            filter(Timestamp >= "2021-09-25 12:00:00" &
#                                   Timestamp <= "2021-12-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_line(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato")  +
#              geom_line(aes(y = prediction_delta),
#                         size = 0.1,
#                         color = "blue")  +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  select(-prediction) %>% 
  filter(Timestamp >= "2021-10-22 1:00:00" & Timestamp <= "2021-11-18 18:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 4 sites (JC-SW, JA-SW, TI-SW, DF-SW) as corollaries r^2 = 0.9751. Also added a +0.03 delta.",
                         Notes),
         Site_Name = "OB-SW") %>% 
  select(-c(gap, prediction_delta)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)

# 4.11 OB-UW1 Fall 2021-------------------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("OB-UW1", "QB-UW1", "JB-UW1", "QB-UW2", "QB-SW")) %>% 
  #OB-UW1 not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `OB-UW1`,
         "fill1" = `QB-UW1`,
         "fill2" = `JB-UW1`, 
         "fill3" = `QB-UW2`,
         "fill4" = `QB-SW`)

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill3`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`+`fill4`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3,
                                                fill4 = fill4))) %>% 
  mutate(prediction_delta = prediction + 0.03) %>% 
  select(-c(fill1, fill2, fill3, fill4))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                            filter(Timestamp >= "2021-08-25 12:00:00" &
#                                   Timestamp <= "2022-01-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_line(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato")  +
#              geom_line(aes(y = prediction_delta),
#                         size = 0.1,
#                         color = "blue")  +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  select(-prediction) %>% 
  filter(Timestamp >= "2021-10-22 15:00:00" & Timestamp <= "2021-11-18 16:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 4 sites (QB-UW1, JB-UW1, QB-UW2, QB-SW) as corollaries r^2 = 0.9751. Also added a +0.03m delta.",
                         Notes),
         Site_Name = "OB-UW1") %>% 
  select(-c(gap, prediction_delta)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)


# 4.12 TB-SW Fall 2021 --------------------------------------------------------------------

temp <- df %>% 
  #Filter the bad battery data, because it shouldn't be used for any analysis (Flag = 2)
  filter(!Flag == "2") %>% 
  filter(Site_Name %in% c("TB-SW", "TA-SW", "ND-SW",
                          "FN-SW", "TB-UW3", "Tiger Paw Catchment Outlet")) %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `TB-SW`,
         "fill1" = `TA-SW`,
         "fill2" = `ND-SW`,
         "fill3" = `FN-SW`,
         "fill4" = `TB-UW3`,
         "fill5" = `Tiger Paw Catchment Outlet`)

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill3`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`+`fill4`+`fill5`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3,
                                                fill4 = fill4,
                                                fill5 = fill5))) %>% 
  # mutate(prediction_delta = prediction - 0.00) %>% 
  select(-c(fill1, fill2, fill3, fill4, fill5))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                       filter(Timestamp >= "2021-07-25 12:00:00" &
#                                Timestamp <= "2022-01-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#   geom_line() +
#   geom_line(aes(y = prediction),
#             size = 0.1,
#             color = "tomato")  +
#   # geom_line(aes(y = prediction_delta),
#   #           size = 0.1,
#   #           color = "blue")  +
#   ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  filter(Timestamp >= "2021-09-19 15:00:00" & Timestamp <= "2021-12-14 16:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "!Sub-par model fit @ missing section! Filled with multiple linear regression from 5 sites(TA-SW, ND-SW, FN-SW, TB-UW3, Tiger Paw Catchment Outlet) as corrolaries r^2 = 0.9818. Delta value = 0m",
                         Notes),
         Site_Name = "TB-SW") %>% 
  select(-c(gap, prediction)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)


# 4.13 TB-UW2 Fall 2020 ------------------------------------------------------------------

temp <- df %>% 
  filter(!Flag == "2") %>% 
  filter(Site_Name %in% c("TB-UW2", "DB-UW1", "TB-UW1", "TB-UW3")) %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `TB-UW2`,
         "fill1" = `DB-UW1`,
         "fill2" = `TB-UW1`,
         "fill3" = `TB-UW3`)

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill3`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3))) %>% 
  select(-c(fill1, fill2, fill3))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                       filter(Timestamp >= "2020-07-25 12:00:00" &
#                                Timestamp <= "2020-10-30 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#   geom_line() +
#   geom_line(aes(y = prediction),
#             size = 0.1,
#             color = "tomato")  +
#   ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  filter(Timestamp >= "2020-09-19 15:00:00" & Timestamp <= "2020-11-01 16:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 3 sites (DB-UW1, TB-UW1, TB-UW3) r^2 = 0.9818. No delta value.",
                         Notes),
         Site_Name = "TB-UW2") %>% 
  select(-c(gap, prediction)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)


# 4.14 TS-CH Fall 2021 -------------------------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("TS-CH", "TS-SW", "ND-UW2", "DK-UW1")) %>% 
  #TS-CH not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `TS-CH`,
         "fill1" = `TS-SW`,
         "fill2" = `ND-UW2`,
         "fill3" = `DK-UW1`)

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill3`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3))) %>% 
  mutate(prediction_delta = prediction - 0.03) %>% 
  select(-c(fill1, fill2, fill3))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                            filter(Timestamp >= "2021-09-25 12:00:00" &
#                                   Timestamp <= "2021-12-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_line(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato")  +
#              geom_line(aes(y = prediction_delta),
#                         size = 0.1,
#                         color = "blue")  +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  select(-prediction) %>% 
  filter(Timestamp >= "2021-10-14 1:00:00" & Timestamp <= "2021-11-6 18:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 3 sites (TS-SW, ND-UW2, DK-UW1) as corollaries r^2 = 0.96. Also added a -0.03 m delta.",
                         Notes),
         Site_Name = "TS-CH") %>% 
  select(-c(gap, prediction_delta)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)


# 4.15 TS-UW1 Fall 2021 ---------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("TS-UW1", "TS-SW", "ND-UW1", "DK-UW2")) %>% 
  #ND-UW1 not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `TS-UW1`,
         "fill1" = `TS-SW`,
         "fill2" = `ND-UW1`,
         "fill3" = `DK-UW2`)

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill3`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3))) %>% 
  mutate(prediction_delta = prediction - 0.05) %>% 
  select(-c(fill1, fill2, fill3))

#Compare modeled prediction to data 
# test_plot <- ggplot(data = temp %>%
#                            filter(Timestamp >= "2021-09-25 12:00:00" &
#                                   Timestamp <= "2021-12-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_line(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato")  +
#              geom_line(aes(y = prediction_delta),
#                         size = 0.1,
#                         color = "blue")  +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  select(-prediction) %>% 
  filter(Timestamp >= "2021-10-14 12:00:00" & Timestamp <= "2021-11-3 13:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 3 sites (TS-SW, ND-UW1, DK-UW2) as corollaries r^2 = 0.96. Also added a -0.05 m delta.",
                         Notes),
         Site_Name = "TS-UW1") %>% 
  select(-c(gap, prediction_delta)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)


# 4.16 XB-SW Fall 2021 ----------------------------------------------------------------

temp <- df %>%
  filter(Site_Name %in% c("XB-SW", "QB-SW", "JA-SW", "TI-SW",
                          "JC-SW", "TP-CH", "QB-UW1")) %>%
  #XB-SW not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>%
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>%
  rename("gap" = `XB-SW`,
         "fill1" = `QB-SW`,
         "fill2" = `JA-SW`,
         "fill3" = `TI-SW`,
         "fill4" = `JC-SW`,
         "fill5" = `TP-CH`,
         "fill6" = `QB-UW1`)

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill5`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`+`fill4`+`fill5`+`fill6`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>%
  mutate(prediction = predict(model, data.frame(fill1 = fill1,
                                                fill2 = fill2,
                                                fill3 = fill3,
                                                fill4 = fill4,
                                                fill5 = fill5,
                                                fill6 = fill6))) %>%
  mutate(prediction_delta = prediction + 0.06) %>%
  select(-c(fill1, fill2, fill3, fill4, fill5, fill6))

#Compare modeled prediction to data
# test_plot <- ggplot(data = temp %>%
#                            filter(Timestamp >= "2021-09-25 12:00:00" &
#                                   Timestamp <= "2021-12-31 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#              geom_line() +
#              geom_line(aes(y = prediction),
#                         size = 0.1,
#                         color = "tomato")  +
#              geom_line(aes(y = prediction_delta),
#                         size = 0.1,
#                         color = "blue")  +
#              ylab("waterLevel (m)")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>%
  select(-prediction) %>%
  filter(Timestamp >= "2021-10-20 12:00:00" & Timestamp <= "2021-11-18 13:15:00") %>%
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 6 adjacent sites (QB-SW, JA-SW, TI-SW, JC-SW, TP-CH, QB-UW1) r^ = 0.957. With a + 0.06 m delta value",
                         Notes),
         Site_Name = "XB-SW") %>%
  select(-c(gap, prediction_delta))

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>%
  distinct()

#Clean up the environment
rm(model, temp, test_plot)

# 4.17 XB-UW1 Fall 2021 ----------------------------------------------------------------------

temp <- df %>%
  filter(Site_Name %in% c("XB-UW1", "JB-UW1", "QB-UW1", "QB-UW2", 
                          "TP-CH", "JC-UW1", "JB-UW2")) %>%
  #QB-UW1 not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>%
  rename("gap" = `XB-UW1`,
         "fill1" =`JB-UW1`,
         "fill2" = `QB-UW1`,
         "fill3" = `QB-UW2`,
         "fill4" = `TP-CH`,
         "fill5" = `JC-UW1`,
         "fill6" = `JB-UW2`) 

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill6`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`+`fill3`+`fill4`+`fill5`+`fill6`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>%
  mutate(prediction = predict(model, data.frame(fill1 = fill1,
                                                fill2 = fill2,
                                                fill3 = fill3,
                                                fill4 = fill4,
                                                fill5 = fill5,
                                                fill6 = fill6))) %>% 
  select(-c(fill1, fill2, fill3, fill4, fill5, fill6))

#Compare modeled prediction to data
test_plot <- ggplot(data = temp %>%
                           filter(Timestamp >= "2021-09-25 12:00:00" &
                                  Timestamp <= "2021-12-31 12:00:00"),
                    mapping = aes(x = Timestamp,
                                  y = gap)) +
             geom_line() +
             geom_line(aes(y = prediction),
                        size = 0.1,
                        color = "tomato")  +
             # geom_line(aes(y = prediction_delta),
             #            size = 0.1,
             #            color = "blue")  +
             ylab("waterLevel (m)")

(test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>%
  filter(Timestamp >= "2021-10-20 12:00:00" & Timestamp <= "2021-11-18 13:15:00") %>%
  mutate(waterLevel = if_else(is.na(gap),
                              prediction,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Bad model fit (r^2 = 0.803) using multiple linear regression with 6 sites (JB-UW1, QB-UW1, QB-UW2, TP-CH, JC-UW1, JB-UW2). No delta value",
                         Notes),
         Site_Name = "XB-UW1") %>%
  select(-c(gap, prediction))

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>%
  distinct()

#Clean up the environment
rm(model, temp, test_plot)

# 5. Export gap-filled data ----------------------------------------------

write_csv(df, paste0(data_dir,"output_JM_2019_2022.csv"))

  




