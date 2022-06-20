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

#BD_CH
# BD_CH <- df %>%
#   filter(Site_Name == "BD-CH") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(BD_CH)
# rm(BD_CH)

#BD_SW
# BD_SW <- df %>%
#   filter(Site_Name == "BD-SW") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(BD_SW)
# rm(BD_SW)

#DB_SW
# DB_SW <- df %>%
#   filter(Site_Name == "DB-SW") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(DB_SW)
# rm(DB_SW)

#DB_UW1

# DB_UW1 <- df %>%
#   filter(Site_Name == "DB-UW1") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(DB_UW1)
# rm(DB_UW1)

#DF_SW
# 
# DF_SW <- df %>%
#   filter(Site_Name == "DF-SW") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(DF_SW)
# rm(DF_SW)

#DK_CH

# DK_CH <- df %>% 
#   filter(Site_Name == "DK-CH") %>% 
#   mutate(waterLevel = waterLevel + 100) %>% 
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(DK_CH) 
# rm(DK_CH)

#DK_SW

# DK_SW <- df %>%
#   filter(Site_Name == "DK-SW") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(DK_SW)
# rm(DK_SW)

#DK-UW1

# DK_UW1 <- df %>%
#   filter(Site_Name == "DK-UW1") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(DK_UW1)
# rm(DK_UW1)

#DK-UW2

# DK_UW2 <- df %>%
#   filter(Site_Name == "DK-UW2") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(DK_UW2)
# rm(DK_UW2)

#FN-SW

# FN_SW <- df %>%
#   filter(Site_Name == "FN-SW") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(FN_SW)
# rm(FN_SW)

#HB-CH
# HB_CH <- df %>%
#   filter(Site_Name == "HB-CH") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(HB_CH)
# rm(HB_CH)
# 
# #HB-SW
# HB_SW <- df %>%
#   filter(Site_Name == "HB-SW") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(HB_SW)
# rm(HB_SW)
# 
# #HB-UW1
# HB_UW1 <- df %>%
#   filter(Site_Name == "HB-UW1") %>%
#   mutate(waterLevel = waterLevel + 100) %>%
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(HB_UW1)
# rm(HB_UW1)

#JA-SW

#JB-SW

#JB-UW1

#JB-UW2

#JC-SW

#JC-UW1

#Jones Road North Catchment Outlet

#Jones Road South Catchment Outlet
Jones_Rd_S <- df %>%
  filter(Site_Name == "Jones Road South Catchment Outlet") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(Jones_Rd_S)
rm(Jones_Rd_S)

#MB-CH
MB_CH <- df %>%
  filter(Site_Name == "MB-CH") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(MB_CH)
rm(MB_CH)

#MB-SW
MB_SW <- df %>%
  filter(Site_Name == "MB-SW") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(MB_SW)
rm(MB_SW)

#MB-UW1
MB_UW1 <- df %>%
  filter(Site_Name == "MB-UW1") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(MB_UW1)
rm(MB_UW1)

#ND-SW

#ND-UW1

# ND_UW1 <- df %>% 
#   filter(Site_Name == "ND-UW1") %>% 
#   mutate(waterLevel = waterLevel + 100) %>% 
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(ND_UW1) 
# rm(ND_UW1)

#ND-UW2

# ND_UW2 <- df %>% 
#   filter(Site_Name == "ND-UW2") %>% 
#   mutate(waterLevel = waterLevel + 100) %>% 
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(ND_UW2) 
# rm(ND_UW2)

#ND-UW3

#OB-CH
OB_CH <- df %>%
  filter(Site_Name == "OB-CH") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(OB_CH)
rm(OB_CH)

#OB-SW
OB_SW <- df %>%
  filter(Site_Name == "OB_SW") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(OB_SW)
rm(OB_SW)

#OB-UW1
OB_UW1 <- df %>%
  filter(Site_Name == "OB-UW1") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(OB_UW1)
rm(OB_UW1)

#QB-SW
QB_SW <- df %>%
  filter(Site_Name == "QB-SW") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(QB_SW)
rm(QB_SW)

#QB-UW1
QB_UW1 <- df %>%
  filter(Site_Name == "QB-UW1") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(QB_UW1)
rm(QB_UW1)

#QB-UW2
QB_UW2 <- df %>%
  filter(Site_Name == "QB-UW2") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(QB_UW2)
rm(QB_UW2)

#TA-SW

#TB-SW

#TB-UW1

#TB-UW2

#TB-UW3

#TI-SW

TI_SW <- df %>%
  filter(Site_Name == "TI-SW") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(TI_SW)
rm(TI_SW)

#Tiger Paw Catchment Outlet

#TP-CH
TP_CH <- df %>%
  filter(Site_Name == "TP-CH") %>%
  mutate(waterLevel = waterLevel + 100) %>%
  select(Timestamp, waterLevel)

dygraph_ts_fun(TP_CH)
rm(TP_CH)

#TS-CH
# TS_CH <- df %>% 
#   filter(Site_Name == "TS-CH") %>% 
#   mutate(waterLevel = waterLevel + 100) %>% 
#   select(Timestamp, waterLevel)
# 
# dygraph_ts_fun(TS_CH) 
# rm(TS_CH)

#TS-SW

#TS-UW1

#XB-CH

#XB-SW

#XB-UW1


# 4. Fill gaps with correlations -----------------------------------------

#List of gaps to fix
# 1) BD-CH Fall 2021 Oct 14th - Nov 3rd.
#   - used BD-SW as a correlate r^2 = .985
# 2) DK-CH Fall 2021 Oct 14th - Nov 3rd. 
#   - used DK-UW2 as correlate r^2 = .978
# 3) HB-CH Fall 2021
#   - Still need to find a fix
# 4) HB-SW Fall 2021
#   - Tried using multi-regression linear model with TI-SW and QB-SW. Improved r^2 = .87
# 5) HB-UW1 Fall 2021
#   - QB-UW2 as correlate r^2 = 0.800 also added +0.08 m correction up, bc correlation didn't adequately tie timeseries. 
# 6) - MB-CH Fall 
# 


# 4.1 BD-CH Fall 2021 -----------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("BD-CH", "BD-SW")) %>% 
  #BD_CH not installed until 03-02
  filter(Timestamp >= "2021-03-02 2:00:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `BD-CH`,
         "fill" = `BD-SW`)

#Plot correlation
(ggplot(data = temp,
        mapping = aes(x = `gap`,
                      y = `fill`)) +
  geom_point())

#Make a model (linear)
model <- lm(`gap` ~ `fill`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill = fill)))

#Compare modeled prediction to data 
test_plot <- ggplot(data = temp, #%>%
                           # filter(Timestamp >= "2021-09-25 12:00:00" &
                           #        Timestamp <= "2021-12-31 12:00:00"),
                    mapping = aes(x = Timestamp,
                                  y = gap)) +
             geom_line() +
             geom_point(aes(y = prediction),
                        size = 0.1,
                        color = "tomato")

(test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Gap filled with BD-SW as correllary r^2 = .978",
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

#Plot correlation
#!! Filtering the lowest water points where PT dries improves the model. 
# (ggplot(data = temp %>% filter(fill > -0.87),
#         mapping = aes(x = `gap`,
#                       y = `fill`)) +
#   geom_point())

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
#                         color = "tomato")
# 
# (test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Gap filled with DK-UW2 as correllary r^2 = .985",
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
# 
# temp <- df %>% 
#   filter(Site_Name %in% c("HB-SW", "QB-SW", "TI-SW")) %>% 
#   #HB-SW not installed until 2021-03-10
#   filter(Timestamp >= "2021-03-10 21:30:00") %>% 
#   pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
#   rename("gap" = `HB-SW`,
#          "fill1" = `QB-SW`,
#          "fill2" = `TI-SW`)
# 
# #Plot correlation
# (ggplot(data = temp,
#         mapping = aes(x = `gap`,
#                       y = `fill1`)) +
#     geom_point())
# 
# #Make a model (linear)
# model <- lm(`gap` ~ `fill1`+`fill2`, data = temp)
# summary(model)
# 
# model_qb <- lm(gap ~ fill1, data = temp)
# summary(model_qb)
# 
# model_ti <- lm(gap ~ fill2, data = temp)
# summary(model_ti)
# 
# #Apply model to df
# temp <- temp %>% 
#   mutate(prediction = predict(model, data.frame(fill1 = fill1, fill2 = fill2))) %>% 
#   mutate(prediction_qb = predict(model_qb, data.frame(fill1 = fill1))) %>% 
#   mutate(prediction_ti = predict(model_ti, data.frame(fill2 = fill2)))
# 
# #Compare modeled prediction to data 
# test_plot <- ggplot(data = temp, #%>% 
#                     # filter(Timestamp >= "2021-10-10 12:00:00" & 
#                     #        Timestamp <= "2021-12-11 12:00:00"),
#                     mapping = aes(x = Timestamp,
#                                   y = gap)) +
#   geom_line() +
#   geom_point(aes(y = prediction),
#              size = 0.1,
#              color = "tomato") +
#   geom_point(aes(y = prediction_qb),
#              size = 0.1,
#              color = "green") +
#   geom_point(aes(y = prediction_ti),
#              size = 0.1,
#              color = "blue")
# 
# (test_plot)
# 
# #Add predicted values to data and note flags accordingly
# temp <- temp %>% 
#   #!!! Since this correlation was crappy, only apply it to the F2021 gap.
#   filter(Timestamp >= "2021-10-18 09:00:00" & Timestamp <= "2021-11-19 01:00:00") %>% 
#   mutate(waterLevel = if_else(is.na(gap),
#                               prediction,
#                               gap),
#          Flag = if_else(is.na(gap),
#                         "1",
#                         Flag),
#          Notes = if_else(is.na(gap),
#                          "real bad",
#                          Notes),
#          Site_Name = "HB-SW") %>% 
#   select(-c(gap, fill, prediction)) 
# 
# # Combine newly computed values to processed data
# df <- bind_rows(temp, df)  %>% 
#   distinct()
# 
# #Clean up environment
# rm(model, temp, test_plot)


# 4.5 HB-UW1 Fall 2021 ----------------------------------------------------

temp <- df %>% 
  filter(Site_Name %in% c("HB-UW1", "QB-UW2")) %>% 
  #HB-UW1 not installed until 2021-03-10
  filter(Timestamp >= "2021-03-10 21:30:00") %>% 
  pivot_wider(names_from = Site_Name, values_from = waterLevel) %>% 
  rename("gap" = `HB-UW1`,
         "fill" = `QB-UW2`)

#Plot correlation
(ggplot(data = temp,
        mapping = aes(x = `gap`,
                      y = `fill`)) +
  geom_point())

#Make a model (linear)
model <- lm(`gap` ~ `fill`, data = temp)
summary(model)

#Apply model to df
temp <- temp %>% 
  mutate(prediction = predict(model, data.frame(fill = fill))) %>% 
  #Added +0.08 m to connect the corrected timeseries. 
  mutate(prediction = prediction + 0.08)

#Compare modeled prediction to data 
test_plot <- ggplot(data = temp, #%>% 
                      # filter(Timestamp >= "2021-10-10 12:00:00" & 
                      #        Timestamp <= "2021-12-11 12:00:00"),
                    mapping = aes(x = Timestamp,
                                  y = gap)) +
             geom_line() +
             geom_point(aes(y = prediction),
                        size = 0.1,
                        color = "tomato")

(test_plot)

#Add predicted values to data and note flags accordingly
temp <- temp %>% 
  #!!! Since this correlation was crappy, only apply it to the F2021 gap.
  filter(Timestamp >= "2021-10-18 09:00:00" & Timestamp <= "2021-11-19 01:00:00") %>% 
  mutate(waterLevel = if_else(is.na(gap),
                              prediction,
                              gap),
         Flag = if_else(is.na(gap),
                        "1",
                        Flag),
         Notes = if_else(is.na(gap),
                         "Gap filled with QB-UW2 as correllary r^2 = 0.800 also added +0.08 m correction up",
                         Notes),
         Site_Name = "HB-UW1") %>% 
  select(-c(gap, fill, prediction)) 

# Combine newly computed values to processed data
df <- bind_rows(temp, df)  %>% 
  distinct()

#Clean up environment
rm(model, temp, test_plot)


# 4.6 MB-CH Fall 2021  --------------------------------------------------------------------




# XX. Export gap-filled data ----------------------------------------------



  




