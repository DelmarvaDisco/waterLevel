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

ND_UW1 <- df %>% 
  filter(Site_Name == "ND-UW1") %>% 
  mutate(waterLevel = waterLevel + 100) %>% 
  select(Timestamp, waterLevel)

dygraph_ts_fun(ND_UW1) 
rm(ND_UW1)

#ND-UW2

ND_UW2 <- df %>% 
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

TS_CH <- df %>% 
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
# 1) BD-CH Fall 2021 Oct 14th - Nov 3rd.
#   - used BD-SW as a correlate r^2 = .985
# 2) DK-CH Fall 2021 Oct 14th - Nov 3rd. 
#   - used DK-UW2 as correlate r^2 = .978

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
# test_plot <- ggplot(data = temp %>%
#                            filter(Timestamp >= "2021-09-25 12:00:00" &
#                                   Timestamp <= "2021-12-31 12:00:00"),
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
(ggplot(data = temp %>% filter(fill > -0.87),
        mapping = aes(x = `gap`,
                      y = `fill`)) +
  geom_point())

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

# 4.3 ---------------------------------------------------------------------



# XX. Export gap-filled data ----------------------------------------------



  




