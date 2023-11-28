rm(list=ls())
library(tidyverse)

bee_phy = read_csv('modeling_data/bee_phylogenetic_data.csv')
plant_phy = read_csv("modeling_data/globi_phyloDiv.csv") %>%
  mutate(bee_genus = sub(" .*","",scientificName))
geo = read_csv('modeling_data/chesshire2023_beeArea-11april2023.csv')
diet_breadth_fowler <- read_csv('modeling_data/bee_diet_breadth-7march2023.csv')
(diet_breadth_russell <- read_excel("modeling_data/specialistsGeneralists_needRefs 11-27-2023.xlsx") %>%
  mutate(scientificName =sub("_", " ", scientificName)))
(diet_breadth_fowler <- read_csv('modeling_data/bee_diet_breadth-28june2023.csv') %>%
  mutate(scientificName = ifelse(is.na(scientificName), old_bee_name, scientificName)) %>%
  filter(!scientificName %in% diet_breadth_russell$scientificName))



##load cuckoo bee data
cuckoos <- read_excel("modeling_data/specialistsGeneralists_needRefs 11-27-2023.xlsx", 
                      sheet = "specialistsGeneralists_needRefs") %>% 
  filter(diet_breadth == 'parasitic')
#the data below is at the genus-level
more_cuckoos = read_csv("modeling_data/cuckoo_genera1.csv") %>%
  filter(cuckoo=='yes') %>% mutate(diet_breadth ='parasitic') %>%
  rename(bee_genus = genus)

#make data.frame with scientificName, bee_genus and parastic as columns
cuckoos_part1 <- plant_phy %>% select(scientificName) %>%
  left_join(cuckoos) %>% filter(!is.na(diet_breadth)) 

cuckoos_part2 <- plant_phy %>% select(scientificName, bee_genus) %>%
      left_join(more_cuckoos %>% select(bee_genus, diet_breadth)) %>%
  filter(!scientificName %in% cuckoos_part1$scientificName) %>% 
  filter(!is.na(diet_breadth)) %>%
  select(-bee_genus)

cuckoos_final <- cuckoos_part1 %>% bind_rows(cuckoos_part2) %>% 
  select(-bee_family)

# add cuckoo bees to jarrod fowler data
diet_breadth_fowler_cuckoo <- diet_breadth_fowler %>% mutate(ref = 'fowler') %>%
  distinct(scientificName, diet_breadth) %>%
  bind_rows(cuckoos_final %>% mutate(ref = "michener"))

dupes <- diet_breadth_fowler_cuckoo$scientificName[duplicated(diet_breadth_fowler_cuckoo$scientificName)]
diet_breadth_fowler_cuckoo %>% filter(scientificName %in% dupes) %>%
  arrange(scientificName)

#we need a column for "diet_breadth_liberal" 
####(generalists are anything not on the fowler list)
#and another column for "diet_breadth_conservative 
######(generalists need to have data in russell lists)
#to make the diet_breadth conservative list we will also be using the fowler 
#data, so combine russell and fowler datasets
diet_breadth_conservative_df <- diet_breadth_russell %>% 
  select(scientificName, diet_breadth) %>%
  mutate(ref = "russell") %>%
  bind_rows(diet_breadth_fowler_cuckoo %>% 
              distinct(scientificName, diet_breadth, ref)  
              )

#are any of the scientificNames duplicated?
dupes <- diet_breadth_conservative_df$scientificName[duplicated(diet_breadth_conservative_df$scientificName)]
diet_breadth_conservative_df %>% filter(scientificName %in% dupes)

unique(diet_breadth_conservative_df$diet_breadth)
diet_breadth_conservative_df %>% filter(scientificName=='Biastes cressoni')

#plant_phy has all the bees in globi
#combine with bee_phy and geo
#add diet breadth info
data = plant_phy %>%
  left_join(geo) %>%
  left_join(bee_phy)  %>% 
  left_join(diet_breadth_fowler_cuckoo %>% 
  distinct(scientificName,diet_breadth)) %>%
  mutate(diet_breadth_liberal = ifelse(is.na(diet_breadth),'generalist',diet_breadth)) %>%
  left_join(diet_breadth_conservative_df %>% 
             rename(diet_breadth_conservative = diet_breadth) %>% select(-ref)) %>%
  select(scientificName,bee_genus,bee_family,diet_breadth_liberal, diet_breadth_conservative,
         everything()) 

unks <- data %>% filter(is.na(diet_breadth_conservative))

unique(data$diet_breadth_conservative)
unique(data$diet_breadth_liberal)

#write final dataset to csv
data %>% filter(is.na(area_m2))

data %>% group_by(diet_breadth_conservative) %>% summarize(n=n())
# write_csv(data,"modeling_data/globi_speciesLevelFinal-27nov2023.csv")


#i'm curious if any of these unknowns are in wood et al 2023 dataset?
remove_parenth = function(sp){
  part1 = sub("\\(.*","",sp)
  part2 = sub(".*\\) ","",sp)
  return(paste0(part1,part2))
}
wood <- read_excel("modeling_data/wood2023_supp.xlsx") %>%
  mutate(Species = ifelse( grepl("\\(", Species),remove_parenth(Species),Species ))
unks %>% filter(scientificName %in%  wood$Species) #only 4...
