#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Gap filling 2020-2022
#Coder: James Maze (jtmaze@umd.edu)
#Date: 6/17/2022
#Purpose: Fill data gaps in waterLevel data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Notes:
# Is it worth while to incorperate multiple sites into correlary models?

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
OB_UW1 <- df %>%
  filter(Site_Name == "OB-UW1") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(OB_UW1)
rm(OB_UW1)

# 4. Fill gaps with correlations -----------------------------------------

#List of gaps to fix
# 1) BD-CH Fall 2021 Oct 14th - Nov 3rd.
#   - used BD-SW as a correlate r^2 = .989
# 2) DK-CH Fall 2021 Oct 14th - Nov 3rd. 
#   - used DK-UW2 as correlate r^2 = .978
# 3) HB-CH Fall 2021
#   - Still need to find a fix
# 4) HB-SW Fall 2021
#   - Used multiple linear regression 5 nearby sites (JB, JC, TI, QB, DF) r^ = 0.958. Added +0.06m delta
# 5) HB-UW1 Fall 2021
#   - Filled with multiple linear regression from 4 sites (QB-UW2, QB-UW1, JB-UW1, JB-UW2) as corollaries r^2 = 0.952. Also added +0.05 m delta
# 6) MB-CH Fall 2021
#  - Filled with multiple linear regression from 5 sites (QB-UW1, TP-CH, Jones Road North Catchment Outlet, TI-SW, DF-SW) as corollaries r^2 = 0.9719. Also added +0.02 m delta.
# 7) MB-SW Fall 2021
# - Filled with multiple linear regression from 4 sites (QB-SW,  DF-SW,  TI-SW,  JB-SW) as corollaries r^2 = 0.979. Also add +0.03 m delta.
# 8) MB-UW1 Fall 2021
# - 

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
                         "Gap filled with BD-SW as correllary r^2 = .9899",
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
#!! Filtering the lowest water points where PT dries improves the model. 
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
  mutate(waterLevel = if_else(is.na(gap),
                              prediction,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Gap filled with DK-UW2 as correllary r^2 = .978",
                         Notes),
         Site_Name = "DK-CH") %>% 
  select(-c(gap, fill, prediction)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)

# 4.3 HB-CH Fall 2021 ---------------------------------------------------------------------

# Ignore this site for now. Large gap + weird behavior

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
  select(-prediction)
  filter(Timestamp >= "2021-10-14 12:15:00" & Timestamp <= "2021-11-18 19:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 5 sites (QB-UW1, TP-CH, Jones Road North Catchment Outlet, TI-SW, DF-SW) as corollaries r^2 = 0.9719. Also added +0.02 m delta.",
                         Notes),
         Site_Name = "MB-CH") %>% 
  select(-c(gap, predition_delta)) 

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
  filter(Timestamp >= "2021-11-03 12:15:00" & Timestamp <= "2021-11-18 18:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 5 sites (QB-UW1, TP-CH, Jones Road North Catchment Outlet, TI-SW, DF-SW) as corollaries r^2 = 0.979. Also added +0.03 m delta.",
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
  mutate(prediction_delta = prediction + 0.01) %>% 
  select(-c(fill1, fill2, fill3))

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
  filter(Timestamp >= "2021-11-03 12:15:00" & Timestamp <= "2021-11-18 18:15:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction_delta,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Filled with multiple linear regression from 3 sites (QB-UW1,  JB-UW2, JC-UW1) as corollaries r^2 = 0.954. Also add +0.01 m delta.",
                         Notes),
         Site_Name = "MB-UW1") %>% 
  select(-c(gap, prediction_delta)) 

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
  mutate(prediction_delta = prediction - 0.01) %>% 
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
                         "Filled with multiple linear regression from 5 sites (QB-UW2, JB-UW2, DF-SW, TI-SW, TP-CH) as corollaries r^2 = 0.959. Also added a -0.01 delta.",
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
  filter(Timestamp >= "2021-10-22 5:00:00" & Timestamp <= "2021-11-18 18:15:00") %>% 
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
  filter(Site_Name %in% c("OB-UW1", "QB-UW1", "JB-UW1", "QB-UW2")) %>% 
  #OB-UW1 not installed until 03-10, but other sites installed way longer
  filter(Timestamp >= "2021-03-10 10:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `OB-UW1`,
         "fill1" = `QB-UW1`,
         "fill2" = `JB-UW1`, 
         "fill3" = `QB-UW2`)

#Plot correlations
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill3`)) +
#     geom_point())

#Make a model (linear)
model <- lm(gap ~ `fill1`+`fill2`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill1 = fill1, 
                                                fill2 = fill2,
                                                fill3 = fill3))) %>% 
  mutate(prediction_delta = prediction + 0.03) %>% 
  select(-c(fill1, fill2, fill3))

#Compare modeled prediction to data 
test_plot <- ggplot(data = temp %>%
                           filter(Timestamp >= "2021-08-25 12:00:00" &
                                  Timestamp <= "2022-01-31 12:00:00"),
                    mapping = aes(x = Timestamp,
                                  y = gap)) +
             geom_line() +
             geom_line(aes(y = prediction),
                        size = 0.1,
                        color = "tomato")  +
             geom_line(aes(y = prediction_delta),
                        size = 0.1,
                        color = "blue")  +
             ylab("waterLevel (m)")

(test_plot)

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
                         "",
                         Notes),
         Site_Name = "OB-UW1") %>% 
  select(-c(gap, prediction_delta)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up the environment
rm(model, temp, test_plot)


# 4.12 --------------------------------------------------------------------



# XX. Export gap-filled data ----------------------------------------------



  




