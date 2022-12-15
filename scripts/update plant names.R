
#update plant names for both globi data and fowler data using catalog of life
library(vroom)
library(tidyverse)

#load the data
options(max.print=1000000)
interactions_b <- vroom('modeling_data/interactions-14dec2022.csv')
fowler <- read_csv("modeling_data/fowler_hostplants.csv") 

for (c in colnames(interactions_b)) {
  interactions_b[[c]] <-  tolower(interactions_b[[c]])
}


capitalize = function(a_string){
  
  beginning = toupper(substr(a_string,1,1))
  ending = substr(a_string,2,nchar(a_string))
  
  return(paste0(beginning,ending))
  
}

interactions_reformat = interactions_b %>% 
  mutate(scientificName = capitalize(sourceTaxonSpeciesName), 
         bee_family = capitalize(sourceTaxonFamilyName),
         plant_family = capitalize(targetTaxonFamilyName), 
         plant_genus = capitalize(targetTaxonGenusName),
         plant_species = capitalize(targetTaxonName)) %>%
  filter(scientificName != '' & plant_family !='' & plant_genus != "") %>%
  filter(targetTaxonRank %in% c('subspecies','species',"genus",'subgenus','variety')) %>%
  filter(sourceTaxonRank %in% c("subspecies",'variety','species')) %>%
  mutate(plant_family = ifelse(plant_family=='Compositae','Asteraceae',plant_family)) %>%
  mutate(plant_family = ifelse(plant_family=='Umbelliferae','Apiaceae',plant_family)) %>%
  mutate(plant_family = ifelse(plant_family=='Labiatae','Lamiaceae',plant_family)) 

plant_df = interactions_reformat %>%
  distinct(plant_species)
test_me = plant_df$plant_species


#load world flora online data
wfo_data = vroom("/Volumes/Seagate/globi_13dec2022/data/WFO_Backbone/classification.txt") %>%
  mutate(scientificName2 = paste(genus,specificEpithet,infraspecificEpithet)) # add a field with a scientific name for subspecies

#filter the wfo data to just be species that are in the globi data
wfo_filtered = wfo_data %>% filter(scientificName %in% test_me | scientificName2 %in% test_me)

#look at the taxa that are not in the data:
test_me[!(test_me %in% wfo_filtered$scientificName|test_me %in% wfo_filtered$scientificName2) ]

#some of these are not plants or not id'd to genus (e.g)
rm_me = c("Coleoptera","White")

View(interactions_reformat %>% filter(plant_species %in% rm_me))

#some of these might be spelling issues
change_spelling = data.frame(old_spelling = c("Aloë"), new_spelling = c('Aloe'))

#for subspecies that aren't in the data, check if the species is in the data
# for hybrids (e.g., "Dicerandra × thinicola" , check if its a formatting issue)
