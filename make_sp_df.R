source("get_bird_data.R")
source("get_plants.R")
library(plyr)
library(doParallel)

registerDoParallel(cores = (detectCores() - 1))

ruby <- "Archilochus colubris"

# test_df for Greg's function
# test_df <- get_bird_data(ruby)
# write.csv(test_df, "test_df.csv", row.names = FALSE)

# get ruby occurence data records
ruby_df <- get_bird_data(ruby, limit = 100000)
write.csv(ruby_df, "Data/ruby_df.csv", row.names = FALSE)

# get plant species that interact with ruby
plant_sp <- pollinated_plants(ruby)

# make list of plant occurence data
plant_list <- foreach(sp = 1:length(plant_sp)) %dopar%
  get_bird_data(plant_sp[sp], limit = 100000)

# turn plant_list into a dataframe
plant_df <- ldply(plant_list)

# bind_rows of ruby_df and plant_df
sp_df <- bind_rows(plant_df, ruby_df)
write.csv(sp_df, "Data/sp_df.csv", row.names = FALSE)