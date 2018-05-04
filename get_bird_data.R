library(rnpn)

"Archilochus"
species.id <- lookup_names("Archilochus", type = "genus")[,1]

npn_allobssp(species.id[2], startdate = '2008-01-01', enddate = '2008-05-04')

npn_allobssp(speciesid = 352, startdate = '2008-01-01', enddate = '2010-12-01')


npn_allobssp(speciesid = 52, startdate = '2008-01-01', enddate = '2010-12-31')


library(spocc)
ruby_spocc <- occ(query = "Archilochus colubris")
colnames(ruby_spocc$gbif$data$Archilochus_colubris)
ruby_df <- occ2df(ruby_spocc)
colnames(ruby_df)

my_fun <- function(taxon_name, limit = 500){
  colnames_string <- c("name", "longitude", "latitude", "coordinateUncertaintyInMeters", "year", "month", "day", "eventdate", "countryCode") 
  tax_occ <- occ(query = taxon_name, limit = limit)
  tax_occ_df <- tax_occ$gbif$data[[1]]
  tax_occ_df <- tax_occ_df[, colnames(tax_occ_df)
                           %in% colnames_string]
  tax_occ_df <- tax_occ_df[tax_occ_df$countryCode=="US", ]
  return(tax_occ_df)
}

test <- my_fun("Archilochus colubris", limit = 1000)
