rm(list=ls())
library(tidyverse)
library(vroom)
library(maps)
library(sf)
library(readxl)
library(furrr)
library(rgbif)
#if vphylomaker not installed:
#devtools::install_github("jinyizju/V.PhyloMaker")
library(V.PhyloMaker)


# load the globi data
# this is a file with the globi records and plant names are updated 
globi_r = read_csv('modeling_data/globi_occ_names_updated-19dec2022.csv')
globi_r %>% filter(sourceTaxonRank=='variety') %>% distinct(scientificName)
data.frame(globi_r %>% filter(is.na(sourceTaxonRank)) %>% distinct(scientificName))


#also load the fowler data
diet_breadth = read_csv('modeling_data/bee_diet_breadth.csv')
specialists = diet_breadth[diet_breadth$diet_breadth=='specialist',]$scientificName
fowler_formatted = read_csv('modeling_data/fowler_formatted-19dec2022.csv')

#load list of non-native bee species
nonnative = read_csv("modeling_data/nonnative_bees.csv")

# Let's exclude interactions of non-US bees

#first, load list of US bee species from discover life - the text file is from this link: 
# https://www.discoverlife.org/nh/cl/counts/Apoidea_species.html
usa_bees = read_table('modeling_data/Apoidea_species.txt',col_names=F) %>%
  rename(genus = X1, epithet = X2) %>%
  mutate(scientificName = paste(genus,epithet))

#i don't think the discover life list is complete, 
# so let's also check if these species are US bees by seeing if there
# are at least 5 records in gbif


#let's  remove subgenera in parentheses
have_subgenera = data.frame(scientificName = globi_r$scientificName) %>% filter(grepl('(',scientificName,fixed=T)) %>% distinct(scientificName) %>% 
  mutate(genus = gsub(" .*","",scientificName),
                          epithet = sub(".*)",'', scientificName)) %>%
  mutate(new_scientificName = paste0(genus,epithet))

#update the globi data with the new species names
globi_r_updated = globi_r %>% left_join(have_subgenera,by="scientificName") %>%
  mutate(scientificName = ifelse(is.na(new_scientificName),scientificName,new_scientificName)) %>%
  mutate(scientificName = ifelse(grepl("Apis mellifera",scientificName),"Apis mellifera", scientificName)) %>%#remove the weirdly formatted Apis mellifera
  mutate(scientificName = ifelse(grepl("Lasioglossum zephyrum",scientificName),"Lasioglossum zephyrus", scientificName)) %>% #fix spelling of this lasioglossum
  mutate(scientificName = ifelse(grepl("Megachile giliae",scientificName),"Megachile circumcincta", scientificName)) %>% #fix synonym of this megachile
  mutate(scientificName = ifelse(grepl("Nomada heiligbrodtii",scientificName),"Nomada texana", scientificName)) #fix synonym of this nomada
  


#get the list of species
slist = unique(sort(globi_r_updated$scientificName))


#remove morpho species and species groups
(sp_remove1 = slist[grepl('sp\\.',slist) | grepl('/',slist) | grepl('aff\\.',slist) | grepl('nr\\.',slist) | grepl('unk1',slist) | grepl("cf\\.",slist)])

these_are_genera = which(strsplit(slist," ") %>% purrr::map_lgl(function(str_vec) length(str_vec)==1))

sp_remove = c(slist[these_are_genera],sp_remove1)

slist_globi = slist[!slist %in% sp_remove]



american1 = slist_globi[slist_globi %in% usa_bees$scientificName]
# plan(multisession, workers=6)
# pull the US occurrence records of these species from gbif
"Ceratina calcarata/dupla"  %in% slist_globi[!slist_globi %in% usa_bees$scientificName]

# bee_occ = slist_globi[!slist_globi %in% usa_bees$scientificName] %>% future_map(function(sci_name){
#   gbif_list = occ_search(scientificName = sci_name,country='US',limit=25,hasCoordinate = T)
# 
#   if(is.null(gbif_list$data)) {
#     return(gbif_list$data)
#   }else{
#     return( gbif_list$data %>% mutate(query=sci_name) %>%
#               dplyr::select(query,everything()))
#   }
# 
# },.options = furrr_options(seed=T))
#
# saveRDS(bee_occ,'modeling_data/bee_occ_usa-21dec2022.rds') #gbif queried/this saved on 21dec2022
bee_occ = readRDS('modeling_data/bee_occ_usa-21dec2022.rds')
slist_globi
r_obj=bee_occ[[2]]

which(bee_occ %>% map_lgl(is.null))
nrow(r_obj)
r_obj$query

#to be conservative let's say a bee ha to have at least 5 occurrence records in the US
american_bees_tokeep=which(bee_occ %>% purrr::map_lgl(function(r_obj) {
  
  #bee occurs in america if the object is not null (ie 0 occurrence records in the US)
  condition1 = !is.null(r_obj)
  
  #and there are at least 5 occurrence records
  if(condition1){
    condition2 = nrow(r_obj)>5
  }else{
    condition2 = F
  }
  condition1 & condition2
}))

#get the species names
unknown_bees = slist_globi[!slist_globi %in% usa_bees$scientificName]
unknown_bees[american_bees_tokeep]
american2 = unknown_bees[american_bees_tokeep]

#align bee names
american_all =c(american1,american2)
american_all[duplicated(american_all)]
american_df = data.frame(scientificName = american_all)

#write to csv and add to 'name-alignment' github - to run name alignment, commit the read me
# write_csv(american_df,"modeling_data/bee_names_for_alignment.csv")

#read output from the name alignment
bees_aligned = vroom('modeling_data/names-aligned.tsv') %>%
  mutate(source = ifelse(grepl("ITIS",alignedExternalId),'itis',NA)) %>% #add column with source
  mutate(source = ifelse(grepl("discover",alignedExternalId),'discoverlife',source)) %>%#add column with source
  mutate(source = ifelse(grepl("NCBI",alignedExternalId),'ncbi',source)) #add column with source
  

# View(bees_aligned)

#update names - use DL first, followed by itis, followed by col
#first filter just be discover life bees
dl_bees = bees_aligned %>% filter(source=='discoverlife') %>% distinct(providedName,alignedName)
#add itis bees that are not in the discover life list
itis_bees = bees_aligned %>% filter(source=='itis' & !providedName %in% dl_bees$providedName)%>% distinct(providedName,alignedName)
#add col bees that are not in the discover life list or the itis list
ncbi_bees = bees_aligned %>% filter(source=='ncbi' & !providedName %in% c(dl_bees$providedName,itis_bees$providedName))%>% distinct(providedName,alignedName)

name_update = rbind(dl_bees,itis_bees,ncbi_bees)

#anything duplicated
name_update$providedName[duplicated(name_update$providedName)]
#any bees not on any list?
(missing = american_df %>% filter(!scientificName %in% name_update$providedName) )#the three lasioglossum are newly described species from Joel Gardner's paper


#add to name update
name_update2 = name_update %>% 
  bind_rows(data.frame(providedName = missing$scientificName,alignedName = missing$scientificName))


# #may still be some bees left to check:
# check_native = data.frame(scientificName = american2) %>% 
#   filter(!scientificName %in% c(nonnative$scientificName))
# 
# other_bees = read_csv('modeling_data/unknown_native_status.csv')
# check_native %>% filter(!scientificName %in% other_bees$scientificName)


# #which species do we still need to check the native status for?
# add_me = check_native$scientificName[!check_native$scientificName %in% other_bees$scientificName &
#                               !check_native$scientificName %in% nonnative$scientificName]
# add_me
# # write_csv(data.frame(add_me),'modeling_data/check_native_status.csv')

#let's update globi again
american_old_names = globi_r_updated %>%
  filter(scientificName %in% american_all) #filter to just be american bees

american_new_names = american_old_names %>% 
  rename(old_bee_name = scientificName) %>%
  left_join(name_update2 %>% rename(old_bee_name = providedName,scientificName = alignedName))

#double check no new rows got added:
nrow(american_new_names) == nrow(american_old_names)

introduced_bees = nonnative$scientificName

#
#add a comun to the dataset for genus
unique(gsub(" .*","",globi_usa$scientificName))
globi_usa = american_new_names %>% filter(!scientificName %in% introduced_bees) %>%
  mutate(bee_genus = gsub(" .*","",scientificName))


#save file
# write_csv(globi_usa,'modeling_data/globi_american_native_bees.csv')

