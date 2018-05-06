library(dplyr)
library(ggplot2)

get_cooccurrences <- function(obs_per_unit, nbr_sample){
  obs_per_unit <- obs_per_unit %>%
    group_by(year, box) %>%
    sample_n(nbr_sample, replace=T) %>%
    group_by(month, day, year, box, occurrence) %>%
    summarise(bird_occ = n())
  max_occ <- max(obs_per_unit$bird_occ)
  coocurrence_per_unit <- obs_per_unit %>%
    mutate(bird_presence = between(bird_occ, 1, max_occ)) %>%
    mutate(no_species = bird_presence + occurrence) %>%
    mutate(cooccurrence = between(no_species, 2, 2)) %>%
    group_by(year,box) %>%
    summarise(no_cooccurrence=sum(cooccurrence))
  return(coocurrence_per_unit)
}

get_temp_anomalies <- function(temp_box){
  mean_temp_box <- temp_box %>%
    group_by(box) %>%
    summarise(mean_temp=mean(temp))
  diff_temp_df <- temp_box %>%
    left_join(mean_temp_box, by = "box") %>%
    mutate(diff_temp = temp-mean_temp) %>%
    select(year, box, diff_temp)
  return(diff_temp_df)
}

get_cooccurrence_temp <- function(obs_per_unit, nbr_sample, temp_box){
  cooccurence_df <- get_cooccurrences(obs_per_unit, nbr_sample)
  diff_temp_df <- get_temp_anomalies(temp_box)
  cooccurrence_temp <- inner_join(cooccurence_df, diff_temp_df, by=c("box", "year"))
  return(cooccurrence_temp)
}

