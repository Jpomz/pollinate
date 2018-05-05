# Function to download  species occurence data
# Filter only US observations
# JPomz
# 04 May 2018
# jfpomeranz@gmail.com

library(spocc)

get_bird_data <- function(taxon_name, limit = 500){
  colnames_string <- c("name", "latitude", "longitude", "month", "day", "year", "countryCode") 
  tax_occ <- spocc::occ(query = taxon_name, limit = limit)
  tax_occ_df <- tax_occ$gbif$data[[1]]
  tax_occ_df <- tax_occ_df[, colnames(tax_occ_df)
                           %in% colnames_string]
  tax_occ_df <- tax_occ_df[tax_occ_df$countryCode=="US", ]
  tax_occ_df <- tax_occ_df[,-7]
  names(tax_occ_df)[1] <- "species"
  tax_occ_df <- tax_occ_df[,c(1,3,2,5,6,4)]
  return(tax_occ_df)
}

