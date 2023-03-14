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




