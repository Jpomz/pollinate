library(dplyr)
sp_df<-read.csv("Data/sp_df.csv")
pollen<-read.csv("Data/trendy_pollen.csv")
florida_climate <- read.csv("Data/FL_yearly.csv")
northcarolina_climate <- read.csv("Data/NC_yearly.csv")
maine_climate <- read.csv("Data/ME_yearly.csv")
source("mutualism_bins.R")
obs_per_unit<-Add_Box(sp_df) %>% filter(box>0) %>% 
  filter(species == "Archilochus colubris") %>%
  filter(year >= 2004 & year <= 2016) %>%
  left_join(pollen, by=c("month", "year","box"))
write.csv(obs_per_unit, file="obs_per_uni.csv", row.names = F)

result_table <- Add_Box(sp_df) %>% filter(box>0) %>% 
  filter(species == "Archilochus colubris") %>%
  count(month, year,box) %>%
  right_join(pollen, by=c("month", "year", "box")) %>%
  filter(year >= 2004 & year <= 2016)
result_table[is.na(result_table$n),]$n <- 0 
result_table <- result_table %>%
  select(box, month, year, pollen, bird = n)

write.csv(result_table, file="Data/result_table.csv",row.names=F)

source("functions_dom.R")
source("graph_density.R")

florida_climate <- florida_climate %>%
  mutate(box=1)
northcarolina_climate <- northcarolina_climate %>%
  mutate(box=2)
maine_climate <- maine_climate %>%
  mutate(box=3)

climate <- rbind(florida_climate, northcarolina_climate) %>%
  rbind(maine_climate) %>%
  rename(temp=yearly_average)

scatter_data <- get_cooccurrence_temp(obs_per_unit, 1000, climate)

scatter_plot <- scatter_data %>% filter(diff_temp >-1.5)%>%
  ggplot( aes(diff_temp, no_cooccurrence, color =as.factor(box))) +
  geom_point( size = 2) +
  geom_smooth(method="lm", alpha = 0.1)+
  scale_color_brewer(palette = 'Accent', labels = c('Southeast', 'Mideast','Northeast'), name = 'Region') +
  labs(x = 'Temperature anomalies (C)', y = 'Number of co-occurring days') +
  theme_bw()

png(filename="Figure/scatter_plot.png", width=600, height=600)
scatter_plot
dev.off()
