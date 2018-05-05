library(dplyr)

get_cooccurrences <- function(obs_per_unit){
  max_occ <- max(obs_per_unit$n)
  coocurrence_per_unit <- obs_per_unit %>%
    mutate(presence = between(n, 1, max_occ)) %>%
    group_by(month, day, year, box) %>%
    summarise(no_species = sum(presence)) %>%
    mutate(cooccurrence = between(no_species, 2, 2)) %>%
    select(-no_species)
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

get_cooccurrence_temp <- function(obs_per_unit, temp_box){
  cooccurence_df <- get_cooccurrences(obs_per_unit)
  diff_temp_df <- get_temp_anomalies(temp_box)
  cooccurrence_temp <- inner_join(cooccurence_df, diff_temp_df, by=c("box", "year"))
  return(cooccurrence_temp)
}