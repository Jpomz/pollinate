library(dplyr)
sp_df<-read.csv("Data/sp_df.csv")
pollen<-read.csv("Data/")
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
  scale_color_brewer(palette = 'Accent', labels = c('Northeast', 'Mideast','Southeast'), name = 'Region') +
  labs(x = 'Temperature anomalies (°C)', y = 'Number of co-occurring days') +
  theme_bw()

for (year in 2004:2016){
  for(month in 1:12){
    test <- rbind (test, data.frame(month = rep(month, 3), year=rep(year,3), box=c(1:3), pollen=runif(3, min = 0, max = 100), occurrence = round(rnorm(3))))
  }
}
test$occurrence = test$occurrence >= 1
