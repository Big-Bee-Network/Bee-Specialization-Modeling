rm(list=ls())
library(tidyverse)
library(vroom)
library(rgbif)
library(purrr)


#load the GBIF data:
#DOI's of download
# Apidae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.4rrwub
# Colletidae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.cg6954
# Melittidae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.d9csyb
# Halictidae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.e5yvth
# Megachildae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.c7ufpz
# Andrenidae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.xq2r7f



#load the idigbio and gbif data
my_dir1 = '/Volumes/Seagate/idigbio_usa/'
my_dir2 = '/Volumes/Seagate/gbif_usa/'
(my_paths1 = paste0(my_dir,list.files(my_dir1)))
(my_paths2 = paste0(my_dir2,list.files(my_dir2)))
my_paths = c(my_paths1, my_paths2)
#

#combine the paths together:
bees = my_paths %>% map_dfr(function(path){
  vroom(path) %>% 
    mutate("dwc:recordNumber" = as.character("dwc:recordNumber")) %>%
    rename(scientificName = `dwc:scientificName`,
           coordinateUncertaintyInMeters = `dwc:coordinateUncertaintyInMeters`,
           stateProvince = `dwc:stateProvince`)
  
  })

test=vroom(my_paths2[[1]])

#filtering steps:
# 1 get rid of anything not id'd to species
unique_taxa = bees$`dwc:scientificName`
# 1b get rid of anything without lat-long coordinates
# anything without accurate lat-long coordinates
# (or where they're inaccurate)
# 2 get unique names and align
# 3 filter to one species per lat-long
# 4 get rid of non-natives
# 5 calculate mean longitude of each bee species

#align the bee names

occ_search(scientificName = sci_name,country='US',limit=25,hasCoordinate = T)

bee_occ = slist_globi[!slist_globi %in% usa_bees$scientificName] %>% future_map(function(sci_name){
  gbif_list = occ_search(scientificName = sci_name,country='US',limit=25,hasCoordinate = T)
  
  if(is.null(gbif_list$data)) {
    return(gbif_list$data)
  }else{
    return( gbif_list$data %>% mutate(query=sci_name) %>%
              dplyr::select(query,everything()))
  }
  
},.options = furrr_options(seed=T))


occ_search(scientificName = sci_name,country='US',limit=25,hasCoordinate = T)

bee_occ = slist_globi[!slist_globi %in% usa_bees$scientificName] %>% future_map(function(sci_name){
  gbif_list = occ_search(scientificName = sci_name,country='US',limit=25,hasCoordinate = T)
  
  if(is.null(gbif_list$data)) {
    return(gbif_list$data)
  }else{
    return( gbif_list$data %>% mutate(query=sci_name) %>%
              dplyr::select(query,everything()))
  }
  
},.options = furrr_options(seed=T))