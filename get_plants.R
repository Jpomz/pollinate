library(dplyr)
library(rglobi)

# Get plants being pollinated by a given species
pollinated_plants <- function(species_name){
  globi_data <- get_interactions(species_name, interaction.type = "pollinates")
  plant_species <- globi_data$target_taxon_name %>% unique()
  return(plant_species)
}
