rm(list=ls())
library(tidyverse)
library(vroom)
library(sf)
library(readxl)
library(sp)
library(tigris)



# load the globi data

# this is a file with the globi records filtered to be just bees 
# without plant names updated 
globi_r = vroom("modeling_data/interactions-14dec2022.csv")%>%
  mutate(unique_id = 1:nrow(.)) 

globi_r %>% filter(sourceTaxonRank=='variety') %>% distinct(scientificName)
data.frame(globi_r %>% filter(is.na(sourceTaxonRank)) %>% distinct(scientificName))

#also load the fowler data
diet_breadth = read_csv('modeling_data/bee_diet_breadth.csv')
specialists = diet_breadth[diet_breadth$diet_breadth=='specialist',]$scientificName
fowler_formatted = read_csv('modeling_data/fowler_formatted-19dec2022.csv')

#load list of non-native bee species
nonnative = read_csv("modeling_data/nonnative_bees.csv")

#chesshire et al data for aligning names
chesshire = read_csv('modeling_data/Chesshire2023_nameAlignment-geoUpdate.csv') %>% 
  filter(providedName != "Megachile concinna")
#exclude Megachile conccina --> native species, name change from Chesshire since
#  according to Russo 2016, M coccina is actually non-native  

# format names:
#let's  remove subgenera in parentheses
have_subgenera = data.frame(scientificName = globi_r$scientificName) %>% filter(grepl('(',scientificName,fixed=T)) %>% distinct(scientificName) %>% 
  mutate(genus = gsub(" .*","",scientificName),
         epithet = sub(".*)",'', scientificName)) %>%
  mutate(new_scientificName = paste0(genus,epithet))

#update the globi data with the new species names
globi_r_updated = globi_r %>% left_join(have_subgenera,by="scientificName") %>%
  mutate(scientificName = ifelse(is.na(new_scientificName),scientificName,new_scientificName)) %>%
  mutate(scientificName = ifelse(grepl("Apis mellifera",scientificName),"Apis mellifera", scientificName)) #remove the weirdly formatted Apis mellifera

#get the list of species
slist = unique(sort(globi_r_updated$scientificName))

#remove morpho species and species groups
(sp_remove1 = slist[grepl('sp\\.',slist) | grepl('/',slist) | grepl('aff\\.',slist) | grepl('nr\\.',slist) | grepl('unk1',slist) | grepl("cf\\.",slist)])
these_are_genera = which(strsplit(slist," ") %>% purrr::map_lgl(function(str_vec) length(str_vec)==1))
sp_remove = c(slist[these_are_genera],sp_remove1)
slist_globi = slist[!slist %in% sp_remove]

globi_provided = data.frame(providedName = slist_globi)

# 
# first do alignments not informed by geography
chesshire_noGeo = chesshire %>% filter(!geo_informed)
globi_provided %>%
  filter(providedName %in% chesshire_noGeo$providedName)

#looks like there are some species that are only in the finalName column; will have to deal with these separately
check_final = globi_provided %>%
  filter(providedName %in% chesshire_noGeo$finalName & !providedName %in% chesshire_noGeo$providedName)
chesshire_noGeo %>%
  filter(finalName %in% check_final$providedName)


only_in_final =chesshire_noGeo %>%
  filter(finalName %in% check_final$providedName) %>%
  distinct(finalName,geo_informed) %>%
  mutate(providedName = finalName)

globi_align1 = globi_provided %>%
  left_join(chesshire_noGeo) %>%
  filter(!is.na(finalName)) %>%
  bind_rows(only_in_final)

#there are also bees where the provided names are misspelled or incorrect but the 
# alignment isn't in the chesshire data
# add these
alignment_toAdd = data.frame(providedName = c("Melissodes lupina","Melissodes bimaculata","Melissodes rustica"),
                             finalName = c('Melissodes lupinus','Melissodes bimaculatus',"Melissodes druriellus")) %>%
  mutate(geo_informed=F)
globi_align2 =  globi_provided %>%
  left_join(alignment_toAdd) %>%
  filter(!is.na(finalName)) 

# next do alignments that are informed by geography
chesshire_geo = chesshire %>% filter(geo_informed)
globi_provided %>%
  filter(providedName %in% chesshire_geo$providedName) #only 5

look_closer = globi_r_updated %>% filter(scientificName %in% chesshire_geo$providedName)
rm_uids_na = look_closer %>% filter(is.na(decimalLatitude) | is.na(decimalLongitude)) %>%
  select(scientificName,unique_id) #these are the unique ids of specs without coordinates

#next, figure out which coordinates are in the usa
#let's double check and make sure everything's within the continental USA
check_loc = look_closer %>% filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  select(scientificName,unique_id,decimalLatitude,decimalLongitude)

#we only carry about lower 48 states so get rid of these from our usa polygon
states_rm = c("Alaska","Hawaii","American Samoa","Commonwealth of the Northern Mariana Islands" ,"United States Virgin Islands","Puerto Rico","Guam")
states <- states(cb=F)%>%
  filter(!NAME %in% states_rm)
#convert lat-long coords to sf object
pnts2 <- check_loc %>%
  mutate(x = decimalLongitude, y = decimalLatitude)
point_sf = st_as_sf(pnts2, coords = c("x", "y"), 
                    crs = 4326, agr = "constant")
states_trans = st_transform(states,4326) #change crs of states
check_loc_filter = st_filter(point_sf,states_trans)

#how many were removed?
nrow(data.frame(check_loc_filter))
nrow(check_loc)
nrow(check_loc)- nrow(data.frame(check_loc_filter))

filtered_data = data.frame(check_loc_filter)

#make data
rm_uids_global = check_loc %>%
  filter(!unique_id %in% filtered_data$unique_id) %>%
  select(scientificName,unique_id)

rm_uids = bind_rows(rm_uids_na,rm_uids_global)

#how many species?
n_distinct(rm_uids$scientificName)
rm_uids %>%
  group_by(scientificName) %>% summarize(n=n())
#how many records are removed?
nrow(rm_uids)

#update globi data again
globi_r_updated2 = globi_r_updated %>%
  filter(!unique_id %in% rm_uids$unique_id)
#make third name alignment df for name updates informed by geography
globi_align3 = chesshire_geo %>% filter(providedName %in% filtered_data$scientificName)

globi_align =bind_rows(globi_align1,globi_align2,globi_align3)

#exclude nonnatives and filter to just be usa bees
globi_names1 = globi_r_updated2 %>%
  rename(providedName = scientificName) %>%
  left_join(globi_align) %>%
  filter(!is.na(finalName)) %>%
  rename(scientificName = finalName) %>%
  filter(!scientificName %in% nonnative$scientificName)

#what is our sample size?
globi_names1 %>%
  group_by(scientificName) %>% summarize(n=n()) %>%
  arrange(desc(n))
n_distinct(globi_names1$scientificName)
nrow(globi_names1)

#let's load the old dataset and see if we're missing anything due to spelling changes 
old_globi = read_csv('modeling_data/globi_american_native_bees.csv')
(check_me_bees = old_globi %>% filter(!old_bee_name %in% globi_names1$providedName) %>%
  group_by(old_bee_name, scientificName) %>%
  summarize(n=n()) %>%
  arrange(desc(n)))
check_me_bees %>% filter(old_bee_name != scientificName) #this species is alaskan

#


 # write_csv(globi_names1,'modeling_data/globi_american_native_bees_7march2023.csv')







######
##old
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

