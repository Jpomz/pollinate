library(dplyr)
sp_df<-read.csv("Data/sp_df.csv")

pollen<-read.csv("Data/trendy_pollen.csv")

source("mutualism_bins.R")
obs_per_unit<-Add_Box(sp_df) %>% filter(box>0) %>% 
  filter(species == "Archilochus colubris") %>%
  filter(year >= 2004 & year <= 2016) %>%
  left_join(pollen, by=c("month", "year","box"))

source("functions_dom.R")
source("graph_density.R")

ggplot(final_test, aes(diff_temp, no_cooccurrence)) +
  geom_point(aes(color = as.factor(box)), size = 2) +
  geom_smooth(method = lm) +
  scale_color_brewer(palette = 'Accent', labels = c('Southeast', 'Mideast','Northeast'), name = 'Region') +
  labs(x = 'Temperature anomalies (C)', y = 'Number of co-occurring days') +
  theme_bw()