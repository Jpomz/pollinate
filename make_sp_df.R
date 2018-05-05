# get occurence records for Archilochus colubris and plants it interacts with 
source("get_bird_data.R")
source("get_plants.R")
library(plyr)
library(doParallel)

registerDoParallel(cores = (detectCores() - 1))

ruby <- "Archilochus colubris"

# get ruby occurence data records
# ruby_df <- get_bird_data(ruby, limit = 100000)
# write.csv(ruby_df, "Data/ruby_df.csv", row.names = FALSE)
# ruby_df only contained data back to ~2016, so I went to the website and downloaded all Archilochus data. The following were my search filters:
# Scientific name: Archilocus colubris
# Human observation
# United States
# year 1900-2018
# downloaded 5 May 2018 
# https://www.gbif.org/occurrence/download/0010942-180412121330197
# this information is saved in Data/ruby_GBIF.csv

# read in ruby_GBIF data
ruby_df <- read.delim("Data/ruby_GBIF.csv")
# subset columns
colnames_string <- c("species", "decimallatitude", "decimallongitude", "month", "day", "year", "countryCode")
ruby_df <- ruby_df[, colnames(ruby_df) %in%
                     colnames_string]
# reorder columns
ruby_df <- ruby_df[,c(1,2,3,5,4,6)]
# rename columns to match plant_sp
names(ruby_df)[2:3] <- c("latitude", "longitude")

# get plant species that interact with ruby
plant_sp <- pollinated_plants(ruby)

# make list of plant occurence data
plant_list <- foreach(sp = 1:length(plant_sp)) %dopar%
  get_bird_data(plant_sp[sp], limit = 100000)

# turn plant_list into a dataframe
plant_df <- ldply(plant_list)
write.csv(plant_df, "Data/plant_df.csv", row.names = FALSE)
#plant_df <- read.csv("Data/plant_df.csv")


# bind_rows of ruby_df and plant_df
sp_df <- bind_rows(plant_df, ruby_df)
# remove NA's
sp_df <- na.omit(sp_df)
# filter out observations before 1980
sp_df <- sp_df[sp_df$year>=1980,]



# spring indicators
spring_plant_names <- c("Lonicera tatarica", "Lonicera korolkowii", "Syringa chinensis")

spring_list <- foreach(sp = 1:length(spring_plant_names)) %dopar%
  get_bird_data(spring_plant_names[sp], limit = 100000)
spring_df <- ldply(spring_list)
# read.csv("Data/sp_df.csv")
sp_df <- bind_rows(sp_df, spring_df)


write.csv(sp_df, "Data/sp_df.csv", row.names = FALSE)




