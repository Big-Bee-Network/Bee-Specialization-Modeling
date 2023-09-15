rm(list=ls())
library(tidyverse)
library(vroom)
library(maps)
library(sf)
library(vegan)
library(randomForest)
library(readxl)
library(writexl)
library(furrr)
library(gridExtra)
library(pROC)
library(pROC)
library(ROCR)
library(pdp) # for partial, plotPartial, and grid.arrange functions


# set the seed
set.seed(111342)

##analysis
# for bees on the Jarrod Fowler lists, what % visit their host plants?
# for bees [classified as polylectic in ms dataset, what % visit their host plants?]]
# for bees on the fowler list, how accurate are predictions?
# if we change the sample size how does this change?


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


specialists = diet_breadth[diet_breadth$diet_breadth=='specialist',]$scientificName
globi_u=globi %>% left_join(diet_breadth %>% distinct(scientificName,diet_breadth)) %>%
  mutate(diet_breadth = ifelse(is.na(diet_breadth),'generalist',diet_breadth))

#double check no increase in the number of rows
nrow(globi) ==nrow(globi_u)

#how many records total in our data?
#we excluded 8 species that lacked any date info. Get rid of these bee species when calculating n_plants
#globi_degree already has these species excluded
globi_u_final =globi_u %>%
  filter(scientificName %in% globi_degree$scientificName)
nrow(globi_u_final)
paste0('our data has ', sum(globi_degree$n_globi),' records total, with ', nrow(globi_degree),' bee species, and ',
       n_distinct(globi_u_final$plant_genus), ' plant genera')
#how many records of specialists
spec_globi = globi_degree %>% filter(diet_breadth == 'specialist')
gen_globi = globi_degree %>% filter(diet_breadth == 'generalist')

#44,419 were records of pollen specialist bees, from 459 species, and 162,073 were records of generalist bees from 833 species.
paste0(sum(spec_globi$n_globi),' were records of pollen specialist bees, from ', nrow(spec_globi), ' species')
paste0(sum(gen_globi$n_globi),' were records of pollen generalist bees, from ', nrow(gen_globi), ' species')

#get # of records of pollen specialists and generalists
sum(globi_degree$n_globi); n_distinct(globi_u$scientificName); nrow(globi_degree)
with(globi_degree %>% filter(diet_breadth=="specialist"),
     paste("there are", sum(n_globi), "records of specialist bees from", n_distinct(scientificName), "species"))
with(globi_degree %>% filter(diet_breadth=="generalist"),
     paste("there are", sum(n_globi), "records of generalist bees from", n_distinct(scientificName), "species"))

#some exploratory plotting
with(globi_degree,boxplot(phylo_simp~diet_breadth))
with(globi_degree,boxplot(phylo_rich~diet_breadth))

#make table of specialist and generalist bees for the supplement
head(globi_degree)

# species_list = globi_degree %>% select(scientificName, bee_family, diet_breadth) %>%
#   arrange(bee_family, scientificName) %>%
#   rename('Bee species' = scientificName, Family = bee_family, "Diet breadth" = diet_breadth)
# # write_xlsx(species_list,'modeling_data/species_list.xlsx')

#let's look at correlations btw predictor variables:
#subset data to just be predictor variables - that don't include the phylo distance between plant genera
predictor_df = globi_degree %>% 
  select(phylo_rich,phylo_simp,eigen1_plantGenus,eigen2_plantGenus,eigen1_plantFam,eigen2_plantFam,
         med_lat,med_long,n_chesshire,area_ha,med_doy,flight_season, simpson_fam,simpson_genus)
cor_matrix = cor(predictor_df)
heatmap(cor_matrix)

##hmm phylogenetic richness is strongly  orrelated with several predictors
cond_matrix = cor_matrix>.7 & cor_matrix !=1
strong_cor_i = which(cond_matrix,arr.ind=T)
row_is = as.vector(strong_cor_i[,1])
col_is = as.vector(strong_cor_i[,2])

#make data.frame with two columns - pairs of predictors that are strongly correlated
data.frame(var1 = row.names(cor_matrix)[row_is],
  var2 = colnames(cor_matrix)[col_is])[c(-4,-5,-7,-8),]

#let's get rid of these
vars_to_remove = c("phylo_rich","simpson_fam")

colnames(predictor_df %>% select(-all_of(vars_to_remove)))

##first do baseline analaysis: stratified k-fold cross validation
colnames(globi_degree)[!colnames(globi_degree) %in% colnames(predictor_df)]
data = globi_degree %>% 
  select(-c(bee_family,-n_globi)) %>% 
  select(-all_of(vars_to_remove)) %>%#remove the vars we don't need
  mutate(diet_breadth = as.factor(diet_breadth)) #make diet breadth a factor
k_folds = 10
data_stratified = data %>% split(.$diet_breadth) %>%
  map_dfr(function(df){
    nrow_db = nrow(df) #how many bee sp with that diet breadth?
    n_per_fold_db <- ceiling(nrow_db / k_folds) #sample size per fold? is # of bees sp with that diet breadth divided by the number of folds
    
    #what this line of code does: first repeats a sequence 1through k_folds a bunch of times (a bunch of times == the total sample size for each fold for that diet breadth)
    #then it shuffles them
    fold_assignments = sample(rep(1:k_folds, n_per_fold_db), nrow_db)
    df %>% mutate(fold=fold_assignments)
    
  })

#double check everything looks even
data_stratified %>%
  group_by(diet_breadth,fold) %>%
  summarize(n=n())

#how many bee genera in the dataset?
head(data)
data %>% mutate(genus = sub(" .*","",scientificName)) %>%
  summarize(n_distinct(genus))
nrow(data)
nrow(data_stratified)


#

# save one fold for testing and train the model on the rest of the data
# get distribution of accuracy, importance values etc
i=1
1:k_folds
i=3


#let's see if I can get probability from randmo forest models
##divide the training and testing data
df_train = data_stratified %>% filter(fold != i) %>% select(-fold,-n_globi)
df_test = data_stratified %>% filter(fold == i) %>% select(-fold,-n_globi)

rf = randomForest(diet_breadth ~ .,data= df_train %>% select(-scientificName),importance = T) #,na.action=na.omit,auc=T)

rf$votes

#stratfied random blocking method
stratified_output_ls = 1:k_folds %>%
  purrr::map(function(i){
    
    #divide the training and testing data
    df_train = data_stratified %>% filter(fold != i) %>% select(-fold,-n_globi)
    df_test = data_stratified %>% filter(fold == i) %>% select(-fold,-n_globi)
    
    rf = randomForest(diet_breadth ~ .,data= df_train %>% select(-scientificName),importance = T) #,na.action=na.omit,auc=T)
    pred <- predict(rf, df_test %>% dplyr::select(-diet_breadth,-scientificName))
    
    df_test$prediction_correct <- df_test$diet_breadth==pred
    
    specialists_wrong = df_test[!df_test$prediction_correct & df_test$diet_breadth=='specialist',]$scientificName
    generalists_wrong = df_test[!df_test$prediction_correct & df_test$diet_breadth=='generalist',]$scientificName
    
    #get auc values
    #the test AUC
    rf_p_test <- predict(rf, type="prob",newdata = df_test)[,2]
    rf_pr_test <- prediction(rf_p_test, df_test$diet_breadth)
    r_auc_test <- performance(rf_pr_test, measure = "auc")@y.values[[1]] 
    r_auc_test    #0.956
    
    
    ##auc of the training data
    rf_p_train <- predict(rf, type="prob",newdata = df_train)[,2]
    rf_pr_train <- prediction(rf_p_train, df_train$diet_breadth)
    r_auc_train <- performance(rf_pr_train, measure = "auc")@y.values[[1]] 
    r_auc_train    #0.956
    
    list(rf,
    tibble(
      fold_left_out = i,
      overall_accuracy = mean(df_test$prediction_correct),
      specialist_accuracy = mean(df_test[df_test$diet_breadth=="specialist",]$prediction_correct),
      generalist_accuracy = mean(df_test[df_test$diet_breadth=="generalist",]$prediction_correct), 
      test_auc = r_auc_test,
      train_auc = r_auc_train,
      specialists_wrong = list(specialists_wrong),
      generalists_wrong = list(generalists_wrong)
      ))

    
    
    })
stratified_output = stratified_output_ls %>% map_dfr(function(a_list) a_list[[2]])
with(stratified_output %>% select(-fold_left_out,-specialists_wrong,-generalists_wrong) %>%
  pivot_longer(everything(), names_to = 'performance_type',values_to = 'estimates'),
  boxplot(estimates~performance_type))

specialists_wrong_vec = unlist(stratified_output$specialists_wrong)
generalists_wrong_vec = unlist(stratified_output$generalists_wrong)

mean(stratified_output$overall_accuracy)
#let's look at variable importance
var_importance = 1:length(stratified_output_ls) %>% map_dfr(function(i){
  
  #select the model from the list
  my_rf = stratified_output_ls[[i]][[1]]
  
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
top_predictors = var_importance %>% 
  group_by(predictor_var) %>% 
  summarize(mean_importance = mean(MeanDecreaseAccuracy)) %>%
  arrange(desc(mean_importance))
top10 = top_predictors$predictor_var[1:10]
top5 = top_predictors$predictor_var[1:5]

#plot boxplots of top predictors
with(var_importance %>% filter(predictor_var %in% top5),
     boxplot(MeanDecreaseAccuracy~predictor_var))


########Next
#block the data phylogenetically by family
data_phy = globi_degree %>% 
  select(scientificName,bee_family,diet_breadth,phylo_simp,simpson_genus,eigen1_plantGenus,eigen2_plantGenus,eigen1_plantFam,eigen2_plantFam,med_lat,med_long,n_chesshire,area_ha,med_doy,flight_season) #remove the vars we don't need

#look at sample sizes within family
data_phy %>%
  group_by(bee_family) %>%
  summarize(n=n())

phylo_blocked_output_ls = unique(data_phy$bee_family) %>%
  purrr::map(function(fam){
    
    #divide the training and testing data
    df_train = data_phy %>% filter(bee_family != fam) %>% select(-bee_family)
    df_test = data_phy %>% filter(bee_family == fam) %>% select(-bee_family)
    
    rf = randomForest(diet_breadth ~ .,data= df_train  %>% select(-scientificName),importance = T) #,na.action=na.omit,auc=T)
    pred <- predict(rf, df_test %>% dplyr::select(-diet_breadth,-scientificName))
    
    df_test$prediction_correct <- df_test$diet_breadth==pred
    
    specialists_wrong = df_test[!df_test$prediction_correct & df_test$diet_breadth=='specialist',]$scientificName
    generalists_wrong = df_test[!df_test$prediction_correct & df_test$diet_breadth=='generalist',]$scientificName
    
    #get auc values
    #the test AUC
    rf_p_test <- predict(rf, type="prob",newdata = df_test)[,2]
    rf_pr_test <- prediction(rf_p_test, df_test$diet_breadth)
    r_auc_test <- performance(rf_pr_test, measure = "auc")@y.values[[1]] 
    r_auc_test    #0.956
    
    
    ##auc of the training data
    rf_p_train <- predict(rf, type="prob",newdata = df_train)[,2]
    rf_pr_train <- prediction(rf_p_train, df_train$diet_breadth)
    r_auc_train <- performance(rf_pr_train, measure = "auc")@y.values[[1]] 
    r_auc_train    #0.956
    
    
    list(rf,
         tibble(
           fam_left_out = fam,
           overall_accuracy = mean(df_test$prediction_correct),
           specialist_accuracy = mean(df_test[df_test$diet_breadth=="specialist",]$prediction_correct),
           generalist_accuracy = mean(df_test[df_test$diet_breadth=="generalist",]$prediction_correct), 
           test_auc = r_auc_test,
           train_auc = r_auc_train,
           specialists_wrong = list(specialists_wrong),
           generalists_wrong = list(generalists_wrong)
         ))
    

    
    
    
  })
phylo_blocked_output = phylo_blocked_output_ls %>% map_dfr(function(a_list) a_list[[2]])
with(phylo_blocked_output %>% select(-fam_left_out,-specialists_wrong,-generalists_wrong) %>%
       pivot_longer(everything(), names_to = 'performance_type',values_to = 'estimates'),
     boxplot(estimates~performance_type))


#let's look at variable importance
var_importance_phy = 1:length(phylo_blocked_output_ls) %>% map_dfr(function(i){
  
  #select the model from the list
  my_rf = phylo_blocked_output_ls[[i]][[1]]
  
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
top_predictors_phy =  var_importance_phy %>% 
  group_by(predictor_var) %>% 
  summarize(mean_importance = mean(MeanDecreaseAccuracy)) %>%
  arrange(desc(mean_importance))
top10_phy = top_predictors_phy$predictor_var[1:10]
top5_phy = top_predictors_phy$predictor_var[1:5]

#plot boxplots of top predictors
with(var_importance_phy %>% filter(predictor_var %in% top5_phy),
     boxplot(MeanDecreaseAccuracy~predictor_var))


########Next
#block the data spatially
#next let's spatially block the data into k-folds
#first let's look at the data
with(data,plot(med_long,med_lat))


#let's divide into 4x2 grid 
#divide the data in half by latitude
med_lat_overall = median(globi_degree$med_lat)
data_space = globi_degree %>% mutate(lat_block  = ifelse(med_lat>=med_lat_overall,1,2))


#divide the data in fours by 25, 50 and 75 percentiles
lon_seq = quantile(globi_degree$med_long, probs = c(.25,.5,.75))
data_space$long_block <- 1
data_space[data_space$med_long > lon_seq[1] & data_space$med_long<=lon_seq[2],]$long_block <- 2
data_space[data_space$med_long > lon_seq[2] & data_space$med_long<=lon_seq[3],]$long_block <- 3
data_space[data_space$med_long>=lon_seq[3],]$long_block <- 4

#add spatial block as a variable
data_space$spatial_block = as.numeric(as.factor(paste(data_space$lat_block,data_space$long_block)))

#check and make sure the sample sizes are approximately even in each block
data_space %>% group_by(spatial_block) %>% summarize(n=n())


data_space2 = data_space %>%
  select(-all_of(vars_to_remove)) %>%
  select(-n_globi,-bee_family,-lat_block,-long_block)

block=1
#plot to double check everything:
with(data_space2,plot(med_long,med_lat,col=spatial_block))

#now run the cross validation
spatial_blocked_output_ls = 1:n_distinct(data_space2$spatial_block) %>%
  purrr::map(function(block){
    
    #divide the training and testing data
    df_train = data_space2 %>% filter(spatial_block != block) %>% select(-spatial_block,-med_lat,-med_long)
    df_test = data_space2 %>% filter(spatial_block == block)  %>% select(-spatial_block,-med_lat,-med_long)
    
    rf = randomForest(diet_breadth ~ .,data= df_train %>% select(-scientificName),importance = T) #,na.action=na.omit,auc=T)
    pred <- predict(rf, df_test %>% dplyr::select(-diet_breadth,-scientificName))
    
    df_test$prediction_correct <- df_test$diet_breadth==pred
    
    specialists_wrong = df_test[!df_test$prediction_correct & df_test$diet_breadth=='specialist',]$scientificName
    generalists_wrong = df_test[!df_test$prediction_correct & df_test$diet_breadth=='generalist',]$scientificName
    
    
    #get auc values
    #the test AUC
    rf_p_test <- predict(rf, type="prob",newdata = df_test)[,2]
    rf_pr_test <- prediction(rf_p_test, df_test$diet_breadth)
    r_auc_test <- performance(rf_pr_test, measure = "auc")@y.values[[1]] 
    

    
    list(rf,
    tibble(
      block_left_out = block,
      overall_accuracy = mean(df_test$prediction_correct),
      specialist_accuracy = mean(df_test[df_test$diet_breadth=="specialist",]$prediction_correct),
      generalist_accuracy = mean(df_test[df_test$diet_breadth=="generalist",]$prediction_correct), 
      test_auc = r_auc_test,
      specialists_wrong = list(specialists_wrong),           
      generalists_wrong = list(generalists_wrong)

      
    ),
    df_train,
    df_test)
    
    
    
  })
spatial_blocked_output = spatial_blocked_output_ls %>% map_dfr(function(a_list) a_list[[2]])
with(spatial_blocked_output %>% select(-block_left_out,-specialists_wrong,-generalists_wrong) %>%
       pivot_longer(everything(), names_to = 'performance_type',values_to = 'estimates'),
     boxplot(estimates~performance_type))
mean(spatial_blocked_output$overall_accuracy)

#get probability of specialists and generalists
the_probs = 1:n_distinct(data_space2$spatial_block) %>%
  purrr::map_dfr(function(i){
    my_rf = spatial_blocked_output_ls[[i]][[1]]
    my_sp = spatial_blocked_output_ls[[i]][[3]] %>% select(scientificName, diet_breadth)
    prop_df = my_sp %>% bind_cols(data.frame(my_rf$votes)) %>% mutate(iteration = i)
    
    
  })

mean_probs = the_probs %>% group_by(scientificName, diet_breadth) %>% summarize(prob_generalist = mean(generalist), prob_specialist=mean(specialist))

generalist_probs = mean_probs %>% filter(diet_breadth=='generalist') %>%
  select(scientificName, diet_breadth, prob_generalist) %>% arrange(prob_generalist)
specialist_probs = mean_probs %>% filter(diet_breadth=='specialist') %>%
  select(scientificName, diet_breadth, prob_generalist) %>% arrange(prob_generalist)

# write_csv(specialist_probs, "figures/specialist_probabilities.csv")
# write_csv(generalist_probs, "figures/generalist_probabilities.csv")
# 
#let's look at variable importance
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
  summarize(mean_importance = mean(MeanDecreaseAccuracy)) %>%
  arrange(desc(mean_importance))
top10_space = top_predictors_space$predictor_var[1:10]
top5_space = top_predictors_space$predictor_var[1:5]


specialist_importance =var_importance_space %>% 
  group_by(predictor_var) %>% 
  summarize(mean_importance = mean(specialist)) %>%
  arrange(desc(mean_importance))
generalist_importance =var_importance_space %>% 
  group_by(predictor_var) %>% 
  summarize(mean_importance = mean(generalist)) %>%
  arrange(desc(mean_importance))
top5_specialists = specialist_importance$predictor_var[1:5]
top5_generalists = generalist_importance$predictor_var[1:5]

#plot boxplots of top predictors
with(var_importance_space %>% filter(predictor_var %in% top5_space),
     boxplot(MeanDecreaseAccuracy~predictor_var))
with(var_importance_space %>% filter(predictor_var %in% top5_generalists),
     boxplot(generalist~predictor_var))
with(var_importance_space %>% filter(predictor_var %in% top5_specialists),
     boxplot(specialist~predictor_var))


#aggregate all accuracy & auc values from the models

stratified_sum = stratified_output %>% 
  select(-fold_left_out,-specialists_wrong,-generalists_wrong) %>%
  mutate(blocking_method = 'random')
phylo_sum = phylo_blocked_output %>% 
  select(-fam_left_out,-specialists_wrong,-generalists_wrong) %>%
  mutate(blocking_method = 'phylogenetic')
spatial_sum = spatial_blocked_output %>% 
  select(-block_left_out,-specialists_wrong,-generalists_wrong) %>%
  mutate(blocking_method = 'spatial')

accuracy_sum = stratified_sum %>% bind_rows(phylo_sum) %>% bind_rows(spatial_sum)

accuracy_sum %>%
  dplyr::select(blocking_method, everything()) %>%
  select(-train_auc) %>%
  pivot_longer(cols = !blocking_method,names_to = "performance_measure", values_to = 'estimate') %>%
  group_by(blocking_method, performance_measure) %>%
  summarize(mean = mean(estimate), min = min(estimate), max = max(estimate))


accuracy_long = accuracy_sum %>%
  dplyr::select(blocking_method, everything()) %>%
  rename(AUC = test_auc) %>%
  select(-train_auc) %>%
  pivot_longer(cols = !blocking_method,names_to = "performance_measure", values_to = 'estimate')

#make boxplots of accuracy by blocking method for different performance measures
# pdf('figures/accuracy_estimates.pdf')
par(mfrow=c(2,2), mar = c(4.2,4.2,5,1), cex.lab = 1.5)
for(i in 1:n_distinct(accuracy_long$performance_measure)){
  pm = unique(accuracy_long$performance_measure)[i]
  focal_df = accuracy_long %>%  filter(performance_measure ==pm) 
  pm2 = sub("_"," ",pm)
  title = paste0(toupper(substr(pm2,1,1)), substr(pm2,2,nchar(pm2)))
  my_ylab = 'Accuracy'
  if(pm=="AUC") {title = "AUC"; my_ylab = 'AUC'}
  the_col = adjustcolor('plum4',0.6)
  blocking_cols = adjustcolor(RColorBrewer::brewer.pal(4,'Set3')[c(2,3,4)],.5)
  
  #next add the boxplot
  with(focal_df,boxplot(estimate~blocking_method, 
                        col=blocking_cols,xlab = "Blocking method", 
                        ylab = my_ylab, main = title, cex.main=1.8))
  with(focal_df, stripchart(estimate~blocking_method,          # Data
                            method = "jitter", # Random noise
                            pch = 19,   
                            cex = 1.3,
                            col = the_col,           # Color of the symbol
                            vertical = TRUE,   # Vertical mode
                            add=T))        
  
}
# dev.off()


#next let's aggregate all importance values together from all model runs
#and rank them from most to least important
var_importance_all = var_importance %>% bind_rows(var_importance_phy) %>% bind_rows(var_importance_space)
vars_ranked = var_importance_all %>%
  group_by(predictor_var) %>% 
  summarize(mean_importance = mean(MeanDecreaseAccuracy)) %>%
  arrange(desc(mean_importance))
top10_all = vars_ranked[1:10,]$predictor_var
with(var_importance_all %>% filter(predictor_var %in% top10_all),boxplot(MeanDecreaseAccuracy~predictor_var))

rename = data.frame(predictor_var = top10_all,
                    predictor_factor = c("Phylogenetic \ndiversity", "Simpson \ndiversity",
                                         "Mesoxaea", 'Flight season \nduration', 'Plant identity \n(2nd eigenvalue)', 
                                         "Macrotera",'Regional \nabundance', "Perdita", "Pseudopanurgus", "Calliopsis")) %>%
  mutate(predictor_factor = factor(predictor_factor, levels = predictor_factor[10:1]))

rename2 = data.frame(predictor_var = top10_all,
                    predictor_factor = c("Phylogenetic diversity", "Simpson diversity",
                                         "Mesoxaea", 'Flight season duration', 'Plant identity (2nd eigenvalue)', 
                                         "Macrotera",'Regional abundance', "Perdita", "Pseudopanurgus", "Calliopsis")) %>%
  mutate(predictor_factor = factor(predictor_factor, levels = predictor_factor[10:1]))


vars_ranked$predictor_var[!vars_ranked$predictor_var %in% rename$predictor_var]
rename_add = data.frame(predictor_var = c('med_doy',"med_lat","med_long","area_ha","eigen1_plantFam", "eigen2_plantFam","eigen1_plantGenus"),
                       predictor_factor = c('Median day of activity', 'Median latitude','Median longitude', "Extent of Occurrence", 'Plant family identity (1st eivenvalue)', 'Plant family identity (2nd eigenvalue)','Plant identity (1st eigenvalue)'))

#make table with importance values for the supplement
all_var_table = vars_ranked %>% left_join(rename2 %>% bind_rows(rename_add)) %>%
  mutate(predictor_factor = ifelse(is.na(predictor_factor),predictor_var,predictor_factor)) %>%
  dplyr::select(predictor_factor, mean_importance) %>%
  rename("Predictor variable" = predictor_factor, "Mean importance" = mean_importance)
# write_xlsx(all_var_table,'modeling_data/Importance_predictors.xlsx')

#format figure for the paper
var_top10 = rename %>% 
  left_join(var_importance_all)

my_col = adjustcolor('skyblue4',.6)

#first add the data points
# tiff('figures/var_importance2.tiff', units="in", width=14, height=174, res=1000, compression = 'lzw')
# pdf('figures/testfig.pdf',width = 14.1)
par(cex.lab =1.7, mgp=c(3, 1.5, 0), mar =c(5.5,5.5,5,1))
with(var_top10, stripchart(MeanDecreaseAccuracy~predictor_factor,          # Data
                           method = "jitter", # Random noise
                           pch = 19,   
                           cex = 1.5,
                           col = my_col,           # Color of the symbol
                           vertical = TRUE,   # Vertical mode
                           xlab = "", 
                           ylab = "Importance (mean decrase in model accuracy)"))        
mtext("Predictor Variable", side=1, line=4, cex=1.7)
#next add the boxplot
with(var_top10,boxplot(MeanDecreaseAccuracy~predictor_factor,
                       col=adjustcolor('white',0),add=T))
# dev.off()

##make partial dependence plots for the predictors
i=1
partial_ls = 1:length(spatial_blocked_output_ls) %>% purrr::map(function(i){
  my_rf = spatial_blocked_output_ls[[i]][[1]]
  df_train = spatial_blocked_output_ls[[i]][[3]]

  partial_data1 = partial(my_rf, pred.var = c("phylo_simp"),which.class = 'specialist',prob=T,train=df_train)  %>%
    rename(prob_specialist_phylo = yhat)
  partial_data2 = partial(my_rf, pred.var = c("simpson_genus"),which.class = 'specialist',prob=T,train=df_train) %>%
    rename(prob_specialist_simp = yhat)
  partial_data3 = partial(my_rf, pred.var = c("Mesoxaea"),which.class = 'specialist',prob=T,train=df_train) %>%
    rename(prob_specialist_meso = yhat)
  
  output_data = partial_data1 %>% bind_cols(partial_data2) %>% bind_cols(partial_data3) %>%
    mutate(model_iteration=i)
  return(output_data)
  })
partial_df = partial_ls %>% bind_rows

##calculate average effect size for phylo_simp and simpson_genus
head(partial_df)
#prob for species visiting max phylo diversity of plants
(max_phylo_pred = partial_df[which.max(partial_df$phylo_simp),]$prob_specialist_phylo)
#prob for species visiting min phylo diversity of plants
(min_phylo_pred = partial_df[which.min(partial_df$phylo_simp),]$prob_specialist_phylo)

#percent increase
(min_phylo_pred-max_phylo_pred)/max_phylo_pred

##
#prob for species visiting max simpson diversity of plants
(max_simp_pred = partial_df[which.max(partial_df$simpson_genus),]$prob_specialist_simp)
#prob for species visiting min simpson diversity of plants
(min_simp_pred = partial_df[which.min(partial_df$simpson_genus),]$prob_specialist_simp)

#percent increase
(min_simp_pred-max_simp_pred)/max_simp_pred


#format data to make histograms
break_n=40
phylo_simp_min=min(data_space2$phylo_simp); phylo_simp_max=max(data_space2$phylo_simp)
my_breaks=seq(phylo_simp_min,phylo_simp_max,by=(phylo_simp_max-phylo_simp_min)/break_n) 
data_space2$category_binary =ifelse(data_space2$diet_breadth=='specialist',1,0)

densities_df=data_space2 %>% split(.$category_binary) %>% map_dfr(function(df){
  dens=hist(df$phylo_simp,plot=F,breaks=my_breaks)
  percent_dens=  dens$density/sum(dens$density)
  
  
  data.frame(phylo_simp=dens$mid,percent_dens=percent_dens) %>% 
    mutate(category_binary=df$category_binary[1])
  
}) 

hist_df=densities_df %>% mutate(pct=ifelse(category_binary,1-percent_dens,percent_dens))




my_cols = RColorBrewer::brewer.pal(length(partial_ls),'Paired')
my_cols2 = RColorBrewer::brewer.pal(8,'Set2')[4:8]

# tiff('figures/var_importance.tiff', units="in", width=6, height=6, res=500, compression = 'lzw')
(phylo_simp_graph = ggplot() +
  geom_segment(data=hist_df[hist_df$category_binary==0,], size=4, show.legend=FALSE,colour=my_cols2[1],
               aes(x=phylo_simp, xend=phylo_simp, y=category_binary, yend=pct)) +
  geom_segment(data=hist_df[hist_df$category_binary==1,], size=4, show.legend=FALSE,colour=my_cols2[2],
               aes(x=phylo_simp, xend=phylo_simp, y=category_binary, yend=pct))+
  geom_segment(dat=data_space2[data_space2$category_binary==0,], aes(x=phylo_simp, xend=phylo_simp, y=0, yend=-0.02), size=0.2, colour="grey30") +
  geom_segment(dat=data_space2[data_space2$category_binary==1,], aes(x=phylo_simp, xend=phylo_simp, y=1, yend=1.02), size=0.2, colour="grey30") +
  geom_line(data=partial_ls[[1]],aes(x=phylo_simp,y=prob_specialist_phylo),color=my_cols[partial_ls[[1]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[2]],aes(x=phylo_simp,y=prob_specialist_phylo),color=my_cols[partial_ls[[2]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[3]],aes(x=phylo_simp,y=prob_specialist_phylo),color=my_cols[partial_ls[[3]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[4]],aes(x=phylo_simp,y=prob_specialist_phylo),color=my_cols[partial_ls[[4]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[5]],aes(x=phylo_simp,y=prob_specialist_phylo),color=my_cols[partial_ls[[5]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[6]],aes(x=phylo_simp,y=prob_specialist_phylo),color=my_cols[partial_ls[[6]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[7]],aes(x=phylo_simp,y=prob_specialist_phylo),color=my_cols[partial_ls[[7]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[8]],aes(x=phylo_simp,y=prob_specialist_phylo),color=my_cols[partial_ls[[8]]$model_iteration[1]],lwd=1) +
  theme_bw(base_size=12)+theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    text = element_text(size=20)
  )+
  labs(x='Phylogenetic diversity of \nplants visited',y='Probability of being a \nspecialist bee'))




#format data to make histograms
break_n=40
simp_min=min(data_space2$simpson_genus); simp_max=max(data_space2$simpson_genus)
my_breaks_simp=seq(simp_min,simp_max,by=(simp_max-simp_min)/break_n) 

densities_df_simp=data_space2 %>% split(.$category_binary) %>% map_dfr(function(df){
  dens=hist(df$simpson_genus,plot=F,breaks=my_breaks_simp)
  percent_dens=  dens$density/sum(dens$density)
  
  
  data.frame(simpson_genus=dens$mid,percent_dens=percent_dens) %>% 
    mutate(category_binary=df$category_binary[1])
  
}) 

hist_df_simp=densities_df_simp %>% mutate(pct=ifelse(category_binary,1-percent_dens,percent_dens))


(simpson_genus_graph = ggplot() +
  geom_segment(data=hist_df_simp[hist_df_simp$category_binary==1,], size=4, show.legend=FALSE,colour=my_cols2[1],
               aes(x=simpson_genus, xend=simpson_genus, y=category_binary, yend=pct)) +
  geom_segment(data=hist_df_simp[hist_df_simp$category_binary==0,], size=4, show.legend=FALSE,colour=my_cols2[2],
               aes(x=simpson_genus, xend=simpson_genus, y=category_binary, yend=pct))+
  geom_segment(dat=data_space2[data_space2$category_binary==0,], aes(x=simpson_genus, xend=simpson_genus, y=0, yend=-0.02), size=0.2, colour="grey30") +
  geom_segment(dat=data_space2[data_space2$category_binary==1,], aes(x=simpson_genus, xend=simpson_genus, y=1, yend=1.02), size=0.2, colour="grey30") +
  geom_line(data=partial_ls[[1]],aes(x=simpson_genus,y=prob_specialist_simp),color=my_cols[partial_ls[[1]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[2]],aes(x=simpson_genus,y=prob_specialist_simp),color=my_cols[partial_ls[[2]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[3]],aes(x=simpson_genus,y=prob_specialist_simp),color=my_cols[partial_ls[[3]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[4]],aes(x=simpson_genus,y=prob_specialist_simp),color=my_cols[partial_ls[[4]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[5]],aes(x=simpson_genus,y=prob_specialist_simp),color=my_cols[partial_ls[[5]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[6]],aes(x=simpson_genus,y=prob_specialist_simp),color=my_cols[partial_ls[[6]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[7]],aes(x=simpson_genus,y=prob_specialist_simp),color=my_cols[partial_ls[[7]]$model_iteration[1]],lwd=1) +
  geom_line(data=partial_ls[[8]],aes(x=simpson_genus,y=prob_specialist_simp),color=my_cols[partial_ls[[8]]$model_iteration[1]],lwd=1) +
  theme_bw(base_size=12)+theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    text = element_text(size=20)
  )+
  labs(x='Simpson diversity of plants \nvisited',y='Probability of being a \nspecialist bee'))


#dev.of()


##finally make graph for 3rd most important variable - mesoxaea
break_n=40
meso_min=min(data_space2$Mesoxaea); meso_max=max(data_space2$Mesoxaea)
my_breaks_perd=seq(meso_min,meso_max,by=(meso_max-meso_min)/break_n) 

densities_df_meso=data_space2 %>% split(.$category_binary) %>% map_dfr(function(df){
  dens=hist(df$Mesoxaea,plot=F,breaks=my_breaks_perd)
  percent_dens=  dens$density/sum(dens$density)
  
  
  data.frame(Mesoxaea=dens$mid,percent_dens=percent_dens) %>% 
    mutate(category_binary=df$category_binary[1])
  
}) 

hist_df_meso=densities_df_meso %>% mutate(pct=ifelse(category_binary,1-percent_dens,percent_dens))



(meso_graph = ggplot() +
    geom_segment(data=hist_df_meso[hist_df_meso$category_binary==1,], size=4, show.legend=FALSE,colour=my_cols2[1],
                 aes(x=Mesoxaea, xend=Mesoxaea, y=category_binary, yend=pct)) +
    geom_segment(data=hist_df_meso[hist_df_meso$category_binary==0,], size=4, show.legend=FALSE,colour=my_cols2[2],
                 aes(x=Mesoxaea, xend=Mesoxaea, y=category_binary, yend=pct))+
    geom_segment(dat=data_space2[data_space2$category_binary==0,], aes(x=Mesoxaea, xend=Mesoxaea, y=0, yend=-0.02), size=0.2, colour="grey30") +
    geom_segment(dat=data_space2[data_space2$category_binary==1,], aes(x=Mesoxaea, xend=Mesoxaea, y=1, yend=1.02), size=0.2, colour="grey30") +
    geom_line(data=partial_ls[[1]],aes(x=Mesoxaea,y=prob_specialist_meso),color=my_cols[partial_ls[[1]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[2]],aes(x=Mesoxaea,y=prob_specialist_meso),color=my_cols[partial_ls[[2]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[3]],aes(x=Mesoxaea,y=prob_specialist_meso),color=my_cols[partial_ls[[3]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[4]],aes(x=Mesoxaea,y=prob_specialist_meso),color=my_cols[partial_ls[[4]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[5]],aes(x=Mesoxaea,y=prob_specialist_meso),color=my_cols[partial_ls[[5]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[6]],aes(x=Mesoxaea,y=prob_specialist_meso),color=my_cols[partial_ls[[6]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[7]],aes(x=Mesoxaea,y=prob_specialist_meso),color=my_cols[partial_ls[[7]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[8]],aes(x=Mesoxaea,y=prob_specialist_meso),color=my_cols[partial_ls[[8]]$model_iteration[1]],lwd=1) +
    theme_bw(base_size=12)+theme(
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change axis line
      axis.line = element_line(colour = "black"),
      text = element_text(size=20)
    )+
    labs(x='Phylogenetic distance to the \nbee genus Mesoxaea',y='Probability of being a \nspecialist bee'))


#dev.of()

# tiff('figures/important_vars.tiff', units="in", width=7, height=14, res=1000, compression = 'lzw')
# pdf('figures/important_vars.pdf', width=7, height=14)
grid.arrange(phylo_simp_graph, simpson_genus_graph, meso_graph)
# dev.off()



# partialPlot(my_rf, pred.data = df_train, x.var = "phylo_simp")
# partial_data = partial(my_rf, pred.var = "phylo_simp")  %>% mutate(model_iteration=i)


#how does the spatial model perform without any phylognetic predcitors
colnames(data_space2)
data_space3 = data_space2 %>% select(scientificName,diet_breadth,phylo_simp,eigen1_plantGenus,
                                     eigen2_plantGenus,eigen1_plantFam,eigen2_plantFam,
                                     simpson_genus,med_lat,med_long,n_chesshire,area_ha,med_doy,flight_season,spatial_block)
spatial_blocked_output_np = unique(data_space3$spatial_block) %>%
  purrr::map_dfr(function(block){
    
    #divide the training and testing data
    df_train = data_space3 %>% filter(spatial_block != block) %>% select(-spatial_block,-med_lat, med_long)
    df_test = data_space3 %>% filter(spatial_block == block)  %>% select(-spatial_block,-med_lat, med_long)
    
    rf = randomForest(diet_breadth ~ .,data= df_train %>% select(-scientificName),importance = T) #,na.action=na.omit,auc=T)
    pred <- predict(rf, df_test %>% dplyr::select(-diet_breadth,-scientificName))
    
    df_test$prediction_correct <- df_test$diet_breadth==pred
    
    specialists_wrong = df_test[!df_test$prediction_correct & df_test$diet_breadth=='specialist',]$scientificName
    
    
    #get auc values
    #the test AUC
    rf_p_test <- predict(rf, type="prob",newdata = df_test)[,2]
    rf_pr_test <- prediction(rf_p_test, df_test$diet_breadth)
    r_auc_test <- performance(rf_pr_test, measure = "auc")@y.values[[1]] 
    r_auc_test    #0.956
    
    
    ##auc of the training data
    rf_p_train <- predict(rf, type="prob",newdata = df_train)[,2]
    rf_pr_train <- prediction(rf_p_train, df_train$diet_breadth)
    r_auc_train <- performance(rf_pr_train, measure = "auc")@y.values[[1]] 
    r_auc_train    #0.956
    
    
    tibble(
      block_left_out = block,
      overall_accuracy = mean(df_test$prediction_correct),
      specialist_accuracy = mean(df_test[df_test$diet_breadth=="specialist",]$prediction_correct),
      generalist_accuracy = mean(df_test[df_test$diet_breadth=="generalist",]$prediction_correct), 
      test_auc = r_auc_test,
      train_auc = r_auc_train,
      specialists_wrong = list(specialists_wrong)
      
    )
    
    
    
  })
spatial_blocked_output_np
with(spatial_blocked_output_np %>% select(-block_left_out,-specialists_wrong) %>%
       pivot_longer(everything(), names_to = 'performance_type',values_to = 'estimates'),
     boxplot(estimates~performance_type))

#calculate changes in accuracy with and without phylo predictors
spatil_np_long = spatial_blocked_output_np %>% select(-block_left_out,-specialists_wrong) %>%
  pivot_longer(everything(), names_to = 'performance_type',values_to = 'estimates') %>%
  mutate(phylo_predictors = 'no')
spatial_long = spatial_blocked_output %>% select(-block_left_out,-specialists_wrong,-generalists_wrong) %>%
  pivot_longer(everything(), names_to = 'performance_type',values_to = 'estimates') %>%
  mutate(phylo_predictors = 'yes')

spatil_np_long %>% 
  bind_rows(spatial_long) %>%
  group_by(performance_type, phylo_predictors) %>%
  summarize(mean=mean(estimates)) %>%
  pivot_wider(names_from = phylo_predictors, values_from = mean)


##let's see how our random forest models compare to something simpler
(all_data_props = data_space%>% 
  mutate(bee_genus = sub(" .*","",scientificName)) %>%
  group_by(bee_genus) %>% 
  summarize(prop_specialists = mean(diet_breadth=='specialist')))

all_data_props %>% arrange(prop_specialists)
all_data_props %>% arrange(desc(prop_specialists))
mean(all_data_props$prop_specialists==1)
mean(all_data_props$prop_specialists==0)

i=1
majority_class_predictions = unique(data_space$spatial_block) %>% 
  map_dfr(function(testing_block){
  #get globi sample size and add to data_space2
  df_train = data_space  %>% filter(spatial_block !=testing_block) %>% mutate(bee_genus = sub(" .*","",scientificName))
  df_test = data_space%>% filter(spatial_block ==testing_block) %>% mutate(bee_genus = sub(" .*","",scientificName))
  
  #get prop specialists in each genus and family, using the training data
  genera_majority = df_train %>% 
    group_by(bee_genus) %>% 
    summarize(prop_specialists = mean(diet_breadth=='specialist')) %>%
    mutate(majority_class = ifelse(prop_specialists>0.5,'specialist','generalist'))
  family_majority = df_train %>%
    group_by(bee_family)%>% 
    summarize(prop_specialists = mean(diet_breadth=='specialist')) %>%
    mutate(majority_class = ifelse(prop_specialists>0.5,'specialist','generalist'))
  
  
  #predict bee species will be majority class in its genus, using the testing data
  genus_prediction = df_test %>% dplyr::select(scientificName, bee_genus, bee_family) %>% left_join(genera_majority)  %>%
    select(scientificName, majority_class, bee_family) %>% rename(prediction = majority_class)
  
  #if the genus is not in the training data use the majority class of the family
  family_prediction = genus_prediction %>% filter(is.na(prediction))  %>% left_join(family_majority) %>%
    select(scientificName, majority_class, bee_family) %>% rename(prediction = majority_class)
  
  all_prediction = genus_prediction %>% filter(!is.na(prediction)) %>%
    bind_rows(family_prediction) %>% select(-bee_family)
  #check if predictions are correct
  check_predictions = df_test %>% select(scientificName,diet_breadth) %>% left_join(all_prediction) %>%
    mutate(correct = diet_breadth==prediction)
  
  data.frame(
    testing_block = testing_block,
    overall_accuracy = mean(check_predictions$correct),
    specialist_accuracy = with(check_predictions %>% filter(diet_breadth=='specialist'),mean(correct)),
    generalist_accuracy = with(check_predictions %>% filter(diet_breadth=='generalist'),mean(correct))
    
  )
})
apply(majority_class_predictions, 2, mean)
mean(majority_class_predictions$overall_accuracy)

