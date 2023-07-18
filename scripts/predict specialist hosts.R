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
fowler_formatted = read_csv('modeling_data/fowler_formatted-28june2023.csv')
diet_breadth = read_csv('modeling_data/bee_diet_breadth-28june2023.csv')

#
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
# fit a random forest model to see if it works:
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
  left_join(most_visited) %>%
  mutate(bee_genus = sub(" .*","",scientificName))


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

rf_plants = randomForest(host_family ~ most_visited +eigen1_plantGenus + eigen2_plantGenus + eigen1_plantFam + eigen2_plantFam + bee_genus + bee_family + med_long+med_lat+n_chesshire+med_doy+area_ha,data=df_train)

summary(rf_plants)
rf_plants$importance

#how accurate? use model to predict with testing data
pred <- predict(rf_plants, df_test %>% dplyr::select(-diet_breadth,-host_family))
#convert host_family to string to test predictions
df_test$prediction_correct <- as.character(df_test$host_family)==as.character(pred)
mean(df_test$prediction_correct)


#try using more data and using spatial cross validation
med_lat_overall = median(gd_specs$med_lat)
data_space = gd_specs %>% mutate(lat_block  = ifelse(med_lat>=med_lat_overall,1,2))

#divide the data in fours by 25, 50 and 75 percentiles
lon_seq = quantile(gd_specs$med_long, probs = c(.25,.5,.75))
data_space$long_block <- 1
data_space[data_space$med_long > lon_seq[1] & data_space$med_long<=lon_seq[2],]$long_block <- 2
data_space[data_space$med_long > lon_seq[2] & data_space$med_long<=lon_seq[3],]$long_block <- 3
data_space[data_space$med_long>=lon_seq[3],]$long_block <- 4

#add spatial block as a variable
data_space$spatial_block = as.numeric(as.factor(paste(data_space$lat_block,data_space$long_block)))

#check and make sure the sample sizes are approximately even in each block
data_space %>% group_by(spatial_block) %>% summarize(n=n())


data_space2 = data_space #%>%
#   select(-all_of(vars_to_remove)) %>%
#   select(-n_globi,-bee_family,-lat_block,-long_block)

#run spatial cross validation
#now run the cross validation
block=2
spatial_blocked_output_ls = 1:n_distinct(data_space2$spatial_block) %>%
  purrr::map(function(block){
    
    #divide the training and testing data
    df_train = droplevels(data_space2 %>% filter(spatial_block != block) %>% select(-spatial_block,-med_lat,-med_long))
    df_test = droplevels(data_space2 %>% filter(spatial_block == block)  %>% select(-spatial_block,-med_lat,-med_long))
    
    rf = randomForest(host_family ~ most_visited +eigen1_plantGenus + eigen2_plantGenus + eigen1_plantFam + eigen2_plantFam + bee_genus + bee_family +n_chesshire+med_doy+area_ha,data=df_train)
    
    pred <- predict(rf, df_test %>% dplyr::select(-diet_breadth,-scientificName))
    
    df_test$prediction_correct <- as.character(df_test$host_family)==as.character(pred)
    
    
   
    
    list(rf,
         tibble(
           block_left_out = block,
           overall_accuracy = mean(df_test$prediction_correct),
           
           
         ))
    
    
    
  })

spatial_blocked_output = spatial_blocked_output_ls %>% map_dfr(function(a_list) a_list[[2]])
with(spatial_blocked_output %>% select(-block_left_out) %>%
       pivot_longer(everything(), names_to = 'performance_type',values_to = 'estimates'),
     boxplot(estimates~performance_type))

#
#let's look at variable importance
i=1
var_importance_space = 1:length(spatial_blocked_output_ls) %>% map_dfr(function(i){
  
  #select the model from the list
  my_rf = spatial_blocked_output_ls[[i]][[1]]
  
  #get variable importance
  new_df = data.frame(my_rf$importance) %>% 
    mutate(predictor_var = rownames(.)) %>%
    select(predictor_var, everything()) %>%
    mutate(model_iteration = i)
  
  #get rid of rownames
  rownames(new_df) <- NULL
  
  return(new_df)
  
  })

#get average mean decrease in accuracy and see which variables have highest average
# then plot variation
top_predictors_space = var_importance_space %>% 
  group_by(predictor_var) %>% 
  summarize(mean_importance = mean(MeanDecreaseGini)) %>%
  arrange(desc(mean_importance)) %>%
  mutate(predictor_var = ordered(factor(predictor_var, levels = predictor_var)))
var_importance_space$predictor_var = ordered(factor(var_importance_space$predictor_var,levels=top_predictors_space$predictor_var))

top_predictors_space$predictor_var
top10_space = top_predictors_space$predictor_var[1:10]
top5_space = top_predictors_space$predictor_var[1:5]

top_predictors_space %>%
  mutate(predictor_var = ordered(predictor_var))
levels(top_predictors_space$predictor_var)
with(top_predictors_space ,
     boxplot(mean_importance~predictor_var))
with(var_importance_space ,
     boxplot(MeanDecreaseGini~predictor_var))

#let's try a simpler model
#where we predict based on the most visited plant
mean(data_space$most_visited==data_space$host_family)

#lets get mean accuracy for each spatial block
simple_mod = unique(data_space$spatial_block) %>%
  map_dbl(function(block){
    df = data_space %>% filter(spatial_block==block)
    mean(df$most_visited==df$host_family)
    
  })
summary(simple_mod)


#make box-plots for a figure
#data needed: simple-mod
#spatial-blocked output

accuracy = data.frame(method = 'rf',accuracy = spatial_blocked_output$overall_accuracy) %>%
  bind_rows(data.frame(
    method = 'simple',
    accuracy = simple_mod
  ))
with(accuracy, boxplot(accuracy~method))

#range of accuracy of the random-forest models
summary(spatial_blocked_output)
