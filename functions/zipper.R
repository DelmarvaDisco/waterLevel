well_index<-wells %>% dplyr::filter(Site_Name==Site) %>% na.omit()
df %>%
  filter(Timestamp>ymd("2017-11-18") & Timestamp<ymd("2017-11-21")) %>%
  select(Timestamp, pressureAbsolute, barometricPressure) %>%
  gather("var","val",-Timestamp) %>%
  group_by(var) %>%
  summarise(min_val = min(val, na.rm=T), 
            date = mean(Timestamp[min_val==val])) %>%
  select(var, date) %>%
  spread(var, date) %>%
  mutate(diff =  barometricPressure - pressureAbsolute) %>% select(diff)

dygraph_ts_fun(df %>%
                 select(Timestamp,
                        barometricPressure,
                        pressureAbsolute,
                        waterColumnEquivalentHeightAbsolute) %>%
                 mutate(waterColumnEquivalentHeightAbsolute = waterColumnEquivalentHeightAbsolute*100+100))
