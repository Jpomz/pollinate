# temperature data
# JPomz
# 5 May 2018
# jfpomeranz@gmail.com

# download temperature data from daymet

get_temperature_data <- function(site, lat, lon, years){
  # site must be a character
  # lat and lon are numeric
  # years is a numeric vector
  library(daymetr)
  library(tidyr)
  library(plyr)
  library(zoo)
  out <- NULL
  for (y in seq_along(years)){
    out[[y]] <- download_daymet(site = site,
                lat = lat,
                lon = lon,
                start = years[y],
                end = years[y],
                internal = TRUE)$data[,
                c("year", "yday",
                  "tmax..deg.c.","tmin..deg.c.")]
    names(out[[y]])[3:4] <- c("tmin", "tmax")
    out[[y]]$year <- years[y]
    out[[y]]$date <- as.Date(out[[y]]$yday,
              format = "%j",origin = paste("1.1.",
              as.character(years[y]), sep = ""))
    out[[y]] <- separate(out[[y]], date, c("y", "month", "day"))
    out[[y]] <- out[[y]][,-which(
      colnames(out[[y]]) %in% "y")]
    out[[y]] <- unite(out[[y]],date, c(1,5,6),
          sep = "-", remove = FALSE)
    out[[y]]$date <- as.Date(out[[y]]$date, "%Y-%m-%d")
    out[[y]]$month <- as.numeric(out[[y]]$month)
    out[[y]]$day <- as.numeric(out[[y]]$day)
    }
  out <- ldply(out)
  return(out)
}

years <- c(1980:2016)

FL <- get_temperature_data(site = "FL",
                           lat = 27.302,
                           lon = -81.443,
                           years = years)
saveRDS(FL, "Data/FL_daily_temp_1980_2016")
NC <- get_temperature_data(site = "NC",
                           lat = 36.600, 
                           lon = -78.959, 
                           years = years)
saveRDS(NC, "Data/NC_daily_temp_1980_2016")
ME <- get_temperature_data(site = "ME",
                           lat = 44.571,
                           lon = -70.520, 
                           years = years)
saveRDS(ME, "Data/ME_daily_temp_1980_2016")


monthly <- function(dat){
  library(dplyr)
  dat %>% group_by(year, month) %>%
    summarize(monthly_average = mean(tmin+tmax)) 
}

FL_monthly <- monthly(FL)
write.csv(FL_monthly, "Data/FL_monthly_average.csv", row.names = FALSE)
NC_monthly <- monthly(NC)
write.csv(NC_monthly, "Data/NC_monthly_average.csv", row.names = FALSE)
ME_monthly <- monthly(ME)
write.csv(ME_monthly, "Data/ME_monthly_average.csv", row.names = FALSE)


yearly <- function(dat){
  library(dplyr)
  dat %>% group_by(year) %>%
    summarize(yearly_average = mean(tmin+tmax)) 
}

FL_yearly <- yearly(FL)
write.csv(FL_yearly, "Data/FL_yearly.csv", row.names = FALSE)
NC_yearly <- yearly(NC)
write.csv(NC_yearly, "Data/NC_yearly.csv", row.names = FALSE)
ME_yearly <- yearly(ME)
write.csv(ME_yearly, "Data/ME_yearly.csv", row.names = FALSE)
