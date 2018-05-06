library(ggplot2)
library(dplyr)
result_table <- read.csv("Data/result_table.csv")
head(result_table)

summary(result_table$bird)
sort(log10(result_table$bird))
sort(log10(result_table$pollen))
plot(log10(pollen+1)~log10(bird+1), data = result_table)

result_table$bins <- cut(result_table$bird, breaks = 5)

ggplot(result_table, aes(x = bins, y = pollen, group = box, color = as.factor(box))) +
  stat_summary(fun.y = "mean", geom = "point") 

summary_table <- result_table %>%
  group_by(box, bins) %>%
  summarize(mean.pollen = mean(log10(pollen+1)),
         sd.pollen = sd(log10(pollen+1)))

# plot pollen searches ~ binned bird observations
ggplot(summary_table, aes(x = bins, y = mean.pollen,
                          group = as.factor(box),
                          color = as.factor(box)))+
  geom_point(size = 2,
             position = position_dodge(width = 0.25)) +
  geom_linerange(aes(ymin = mean.pollen - sd.pollen,
                    ymax = mean.pollen + sd.pollen),
                 position = position_dodge(width = 0.25)) +
  geom_smooth(method = "lm",
              alpha = 0)+
  scale_color_brewer(palette = 'Accent',
            labels = c('Southeast', 'Mideast','Northeast'),
            name = 'Region') +
  labs(x = 'Log10 Bird observations',
       y = 'Log10 Pollen searches') +
  theme_bw()

# plot bird obs ~ pollen
ggplot(result_table, aes(x = log10(pollen+1),
                         y = log10(bird+1),
                         color = as.factor(box))) +
  geom_point(size = 2) +
  geom_smooth(method = "lm",
              alpha = 0)+
  scale_color_brewer(palette = 'Accent',
                     labels = c('Southeast', 'Mideast','Northeast'),
                     name = 'Region') +
  labs(x = 'Log10 Pollen searches',
       y = 'Log10 Bird observations') +
  theme_bw()

# plot bird obs ~ pollen, filtered out "0" observations
result_table %>% filter(pollen > 0, bird >0) %>%
  ggplot(aes(x = log10(pollen+1),
                           y = log10(bird+1),
                           color = as.factor(box))) +
  geom_point(size = 2) +
  geom_smooth(method = "lm",
              alpha = 0)+
  scale_color_brewer(palette = 'Accent',
                     labels = c('Southeast', 'Mideast','Northeast'),
                     name = 'Region') +
  labs(x = 'Log10 Pollen searches',
       y = 'Log10 Bird observations') +
  theme_bw()

