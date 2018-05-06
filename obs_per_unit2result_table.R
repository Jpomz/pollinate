obs_per_unit<-read.csv("Data/obs_per_unit.csv")

result_table <- obs_per_unit %>%
  group_by(box,month,year,pollen) %>%
  summarise(bird = n())

write.csv(result_table, file="Data/result_table.csv",row.names=F)
