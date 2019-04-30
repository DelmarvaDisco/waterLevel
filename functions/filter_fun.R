filter_fun

df$pressureGauge<-df$pressureAbsolute-df$barometricPressure

#Estimate water collumn height
df<-df %>% mutate(waterColumnEquivalentHeightAbsolute = pressureGauge*0.101972)


df<-df %>%
  #Estimate difference
  mutate(diff = lead(waterColumnEquivalentHeightAbsolute) - waterColumnEquivalentHeightAbsolute) %>%
  #Identify failure points where signal "drops
  mutate(diff = if_else(diff> -0.1, 0, 1)) %>%
  mutate(diff = rollapply(diff, 10, sum, align='right', fill=NA)) %>%
  filter(diff == 0)