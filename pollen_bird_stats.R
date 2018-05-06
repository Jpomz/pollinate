library(dplyr)

result_table <- read.csv("Data/result_table.csv")
head(result_table)

# 1 = FL, 2 = NC, 3 = ME
result_table$box <- result_table$box %>% recode('1' = "FL", "2" = "NC", "3" = "ME")

lm_pollen_bird <- lm(log10(bird+1) ~ log10(pollen+1)
                     + box,
                     data = result_table)
summary(lm_pollen_bird)


result_binom <- result_table


result_binom$bird[result_binom$bird >0] = 1

model <- glm(bird ~pollen + box,
             family=binomial(link='logit'),
             data= result_binom)
summary(model)
anova(model, test="Chisq")

newdat <- data.frame(pollen = 
                       rep(seq(min(result_binom$pollen),
                                  max(result_binom$pollen),
                                  len = 100), each = 3))
newdat$box <- rep(unique(result_binom$box))
newdat$fit <- predict(model, newdata = newdat,
                       type = "response")
#newdat$fit <- model$family$linkinv(newdat$fit)


ggplot(result_binom, aes(x = pollen,
                                           y = bird,
                         color = box)) +
  geom_point() +
  geom_line(data = newdat, aes(y=fit), size = 1) +
  scale_color_brewer(palette = 'Accent', labels = c('Southeast', 'Mideast','Northeast'), name = 'Region') +
  labs(x = 'Pollen searches', y = 'Probability of bird sighting') +
  theme_bw()

ggsave("Figure/Logistic_regression.png")
