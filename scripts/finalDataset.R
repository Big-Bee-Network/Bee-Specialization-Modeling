rm(list=ls())
library(tidyverse)

bee_phy = read_csv('modeling_data/bee_phylogenetic_data.csv')
plant_phy = read_csv("modeling_data/globi_phyloDiv.csv") %>%
  mutate(bee_genus = sub(" .*","",scientificName))
geo = read_csv('modeling_data/chesshire2023_beeArea.csv')
diet_breadth = read_csv('modeling_data/bee_diet_breadth-7march2023.csv')


#plant_phy has all the bees in globi
#combine with bee_phy and geo
#add diet breadth info
data = plant_phy %>%
  left_join(geo) %>%
  left_join(bee_phy)  %>% left_join(diet_breadth %>% 
  distinct(scientificName,diet_breadth,diet_breadth_detailed)) %>%
  mutate(diet_breadth = ifelse(is.na(diet_breadth),'generalist',diet_breadth),
         diet_breadth_detailed = ifelse(is.na(diet_breadth_detailed),'generalist',diet_breadth_detailed)) %>%
  select(scientificName,bee_genus,bee_family,diet_breadth,diet_breadth_detailed,everything())


#write final dataset to csv
# write_csv(data,"modeling_data/globi_speciesLevelFinal.csv")
#variables from the data we care about:
# phylo_rich,phylo_simp,n_chesshire,area_ha,mean_doy,eigen1,eigen2, mean_lat,mean_long
View(data)
View(data %>% 
       select(scientificName,phylo_rich,phylo_simp,n_chesshire,area_ha,med_doy,flight_season,med_lat,med_long,eigen1,eigen2))

#let's see if randomForest works with NA values for area
library(randomForest)
rf = randomForest(as.factor(diet_breadth) ~ phylo_rich + area_ha,importance = T,data=data)
