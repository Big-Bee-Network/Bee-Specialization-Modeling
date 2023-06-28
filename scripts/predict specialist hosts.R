rm(list=ls())
library(tidyverse)
library(vroom)
library(randomForest)
set.seed(25432)
# load the globi data

#this data file has bee and plant names updated, and only american bees
globi = vroom("modeling_data/globi_allNamesUpdated.csv") %>%
  mutate(bee_genus = sub(' .*',"",scientificName))

#globi_degree is with the species-level data
globi_degree =read_csv("modeling_data/globi_speciesLevelFinal.csv")%>% #let's remove the columns we don't care about
  select(-c(bee_genus,diet_breadth_detailed,rich_genus,rich_fam,area_m2,spherical_geometry,mean_doy,quant10,quant90,eigen1,eigen2)) %>%
  mutate(diet_breadth = as.factor(diet_breadth))


#also load the fowler data
fowler_formatted = read_csv('modeling_data/fowler_formatted-7march2023.csv')
diet_breadth = read_csv('modeling_data/bee_diet_breadth-7march2023.csv')

#
fowler_formatted %>% filter(host_family == 'Compositae')
fowler_formatted %>% filter(!grepl('aceae',host_family)) %>% distinct(host_family)

#need to go back and figure out why 'compositae' is being considered a host family
#using globi data - get the most visited plant family of each bee
most_visited = globi %>% 
  group_by(scientificName,plant_family) %>%
  summarize(n=n()) %>% 
  group_by(scientificName) %>%
  summarize(most_visited = plant_family[n==max(n)]) %>%
  split(.$scientificName) %>%
  map_dfr(function(df){
    
    #if there are ties - and bee doesn't have a plant it visits the most (nrow>1)
    #then pick plants at random
    
    if(nrow(df)>1){
      rand_index = sample(1:nrow(df),1)
      df = df[rand_index,]
    }
    return(df)
  
    })
 
  
  

#filter the data to just be specialists
# fit a random forest model to see if it wirks:
# host-plant ~ bee phylogeny + eigen value score

#get vector of specialist species names
specialists = diet_breadth[diet_breadth$diet_breadth=='specialist',]$scientificName

#filter globi_degree to just be specialists
# and join with host plant data
# will be multiple rows per bee species...could just make it family level?
gd_specs = globi_degree %>% 
  filter(scientificName %in% specialists)%>% 
  left_join(fowler_formatted %>% dplyr::distinct(scientificName,host_family)) %>%
  mutate(host_family = as.factor(host_family)) %>%
  left_join(most_visited)


#how many host plant families are there?
n_distinct(gd_specs$host_family)

#divide into testing and training folds
gd_specs$fold <- 1:nrow(gd_specs) %>% map_int(function(i) sample(1:5,1))

testing_index = 1
all_indices = 1:5
training_indices = all_indices[all_indices != testing_index]

df_test = droplevels(gd_specs %>% filter(fold==testing_index)) #drop levels bc randomForest can't handle empty factors
df_train = droplevels(gd_specs %>% filter(fold %in% training_indices)) #drop levels because randomForest can't handle empty factors

#now try fitting randomforest model on training data

rf_plants = randomForest(host_family ~ most_visited +eigen1_plantGenus + eigen2_plantGenus + eigen1_plantFam + eigen2_plantFam,data=df_train)

summary(rf_plants)
rf_plants$importance

#how accurate? use model to predict with testing data
pred <- predict(rf_plants, df_test %>% dplyr::select(-diet_breadth,-host_family))
#convert host_family to string to test predictions
df_test$prediction_correct <- as.character(df_test$host_family)==as.character(pred)
mean(df_test$prediction_correct)
