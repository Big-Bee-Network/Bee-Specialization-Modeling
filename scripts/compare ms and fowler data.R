rm(list = ls())
library(tidyverse)
select = dplyr::select; map = purrr::map; rename = dplyr::rename

ms <- read_xlsx('modeling_data/DATA_pollen-diet-breadth_list_12-5-21_ALR.xlsx', sheet = 3) %>%
  rename_all(tolower) %>%
  mutate(scientificName = paste0(genus, ' ',sub('.*\\. ','',species))) %>%
  select(scientificName,everything()) %>%
  rename(diet_breadth=`redefined pollen diet breadth`)

fowler <- read_csv("modeling_data/fowler") %>%
  mutate(scientificName = paste0(toupper(substr(interestBee,1,1)),
                                 substr(fowler$interestBee,2,nchar(interestBee)))) %>%
  select(scientificName,everything())


#any bees in the fowler list not in the missouri state list?
data.frame(fowler %>% filter(!scientificName %in% ms$scientificName))

#any bees classified differently between the two list
# ie classified as specialists by fowler but polylectic by missouri state
data.frame(ms %>% 
  filter(diet_breadth == 'Polylectic' & scientificName %in% fowler$scientificName))

# (north american) bees on the missouri state list that are classified as specialists and 
# not on jarrod fowlers list
ms %>% 
  filter(diet_breadth != "Polylectic" & !scientificName %in% fowler$scientificName) #the handful of these I checked don't apper to be north american
#note some of the fowler bees have ** astericks near the name so that is something i need to deal with

#to fi