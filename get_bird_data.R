# Function to download  species occurence data
# Filter only US observations
# JPomz
# 04 May 2018
# jfpomeranz@gmail.com

library(spocc)

my_fun <- function(taxon_name, limit = 500){
  colnames_string <- c("name", "longitude", "latitude", "coordinateUncertaintyInMeters", "year", "month", "day", "eventdate", "countryCode") 
  tax_occ <- occ(query = taxon_name, limit = limit)
  tax_occ_df <- tax_occ$gbif$data[[1]]
  tax_occ_df <- tax_occ_df[, colnames(tax_occ_df)
                           %in% colnames_string]
  tax_occ_df <- tax_occ_df[tax_occ_df$countryCode=="US", ]
  return(tax_occ_df)
}

