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
