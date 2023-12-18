# clear environment
rm(list = ls())

# load libraries
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


# 
print('do you want to define generalists liberally or conservatively?') 
print('Select 1 for conservative and 2 for liberal: ')
i = 1
generalist_criteria <- c('conservative','liberal')[i]
  

#function below is for setting diet breadth to be liberal or conservative criteria
set_diet_breadth = function(df){
  if(generalist_criteria == 'conservative') diet_breadth = df$diet_breadth_conservative
  if(generalist_criteria == 'liberal') diet_breadth = df$diet_breadth_liberal
  
  return(diet_breadth)

  }

#write a function for appending _liberal or _conservative to figure file names
figure_name <- 'example_figure.pdf'; gen_criteria = generalist_criteria
rename_figure <- function(figure_name){
  ext <- sub(".*\\.","",figure_name)
  name <- sub('\\..*','',figure_name)
  new_name <- paste0(name, "_", generalist_criteria,'.',ext)
  return(new_name)
}

#what percentage of bee species are missing diet breadth information?
globi_degree_all <- read_csv("modeling_data/globi_speciesLevelFinal-27nov2023.csv")
mean(is.na(globi_degree_all$diet_breadth_conservative))*100

# what is the phylogenetic breakdown of the unknowns
unknowns <-globi_degree_all  %>% filter(is.na(diet_breadth_conservative))
unknowns %>% 
  group_by(bee_family) %>%
  summarize(n=n()) %>% 
  mutate(per = 100*n/sum(n)) %>%
  ungroup

# globi_degree is with the species-level data
globi_degree <- read_csv("modeling_data/globi_speciesLevelFinal-27nov2023.csv") %>% 
  #first we need to define diet-breadth based on liberal or conservative criteria
  mutate(diet_breadth = set_diet_breadth(.)) %>%
  # let's remove the columns we don't care about:
  select(-c(bee_genus, rich_genus, rich_fam, area_m2, spherical_geometry, 
            mean_doy, quant10, quant90, eigen1, eigen2, 
            diet_breadth_conservative, diet_breadth_liberal)) %>%
  #remove cuckoo bees and bees with NA diet breadths 
  filter(!is.na(diet_breadth)) %>%
  filter(diet_breadth != 'parasitic') %>%
  mutate(diet_breadth = as.factor(diet_breadth)) %>%
  select(scientificName, diet_breadth, everything())
unique(globi_degree$diet_breadth)

# we need to get rid of phylogenetic distances to bee genera not in the data
globi_genera <- sub(" .*", "", globi_degree$scientificName)
genera_cols <- colnames(globi_degree)[19:length(colnames(globi_degree))]
phylo_dist_rm <- genera_cols[!genera_cols %in% globi_genera]
n_distinct(globi_genera)
unique(globi_genera)
globi_degree <- globi_degree %>% select(-phylo_dist_rm)

# load the globi data
# this data file has bee and plant names updated, and only american bees
globi <- vroom("modeling_data/globi_allNamesUpdated.csv") %>%
  mutate(bee_genus = sub(" .*", "", scientificName)) %>%
  filter(scientificName %in% globi_degree$scientificName)


#####
######
sources_table <- globi %>%
  group_by(sourceCitation) %>%
  summarize(n = n()) %>%
  rename(source = sourceCitation)
nrow(sources_table)
globi_broadSources <- globi %>%
  group_by(sourceBasisOfRecordName) %>%
  summarize(n = n())

source_key <- data.frame(
  sourceBasisOfRecordName = c(
    "field capture", "field observation/capture", "field observations", "humanobservation",
    "material_sample", "null", "observation", "pinned specimen",
    "preserved specimen", "preservedspecimen", "primary", "secondary", NA
  ),
  broaderSource = c(rep("field", 4), "museum", "literature_unknown", "field", rep("museum", 3), rep("literature_unknown", 3))
)

total_n <- sum(globi_broadSources$n)
globi_broadSources %>%
  left_join(source_key) %>%
  group_by(broaderSource) %>%
  summarize(percent = sum(n) / total_n)
 # write_csv(sources_table, 'sources_table28nov2023.csv')


paste0(
  "our data has ", sum(globi_degree$n_globi), " records total, with ", nrow(globi_degree), " bee species, and ",
  n_distinct(globi$plant_genus), " plant genera"
)


# how many records of specialists
spec_globi <- globi_degree %>% filter(diet_breadth == "specialist")
gen_globi <- globi_degree %>% filter(diet_breadth == "generalist")

# 44,419 were records of pollen specialist bees, from 459 species..
paste0(sum(spec_globi$n_globi), " were records of pollen specialist bees, from ", nrow(spec_globi), " species")
paste0(sum(gen_globi$n_globi), " were records of pollen generalist bees, from ", nrow(gen_globi), " species")

# some exploratory plotting
with(globi_degree, boxplot(phylo_simp ~ diet_breadth))
with(globi_degree, boxplot(phylo_rich ~ diet_breadth))

# make table of specialist and generalist bees for the supplement
head(globi_degree)

# species_list = globi_degree %>% select(scientificName, bee_family, diet_breadth) %>%
#   arrange(bee_family, scientificName) %>%
#   rename('Bee species' = scientificName, Family = bee_family, "Diet breadth" = diet_breadth)
# # write_xlsx(species_list,'modeling_data/species_list.xlsx')

# let's look at correlations btw predictor variables:
# subset data to just be predictor variables - that don't include the phylo distance between plant genera
predictor_df <- globi_degree %>%
  select(
    phylo_rich, phylo_simp, eigen1_plantGenus, eigen2_plantGenus, eigen1_plantFam, eigen2_plantFam,
    med_lat, med_long, n_chesshire, area_ha, med_doy, flight_season, simpson_fam, simpson_genus
  )
cor_matrix <- cor(predictor_df)
heatmap(cor_matrix)

## hmm phylogenetic richness is strongly  correlated with several predictors
cond_matrix <- cor_matrix > .7 & cor_matrix != 1
strong_cor_i <- which(cond_matrix, arr.ind = T)
row_is <- as.vector(strong_cor_i[, 1])
col_is <- as.vector(strong_cor_i[, 2])

# make data.frame with two columns - pairs of predictors that are strongly correlated
data.frame(
  var1 = row.names(cor_matrix)[row_is],
  var2 = colnames(cor_matrix)[col_is]
)

# let's get rid of these
vars_to_remove <- c("phylo_rich", "simpson_fam")

colnames(predictor_df %>% select(-all_of(vars_to_remove)))

## first do baseline analaysis: stratified k-fold cross validation
colnames(globi_degree)[!colnames(globi_degree) %in% colnames(predictor_df)]
data <- globi_degree %>%
  select(-c(bee_family, -n_globi)) %>%
  select(-all_of(vars_to_remove))  # remove the vars we don't need
k_folds <- 8
data_stratified <- data %>%
  split(.$diet_breadth) %>%
  map_dfr(function(df) {
    nrow_db <- nrow(df) # how many bee sp with that diet breadth?
    n_per_fold_db <- ceiling(nrow_db / k_folds) # sample size per fold? is # of bees sp with that diet breadth divided by the number of folds

    # what this line of code does: first repeats a sequence 1through k_folds a bunch of times (a bunch of times == the total sample size for each fold for that diet breadth)
    # then it shuffles them
    fold_assignments <- sample(rep(1:k_folds, n_per_fold_db), nrow_db)
    df %>% mutate(fold = fold_assignments)
  })

# double check everything looks even
data_stratified %>%
  group_by(diet_breadth, fold) %>%
  summarize(n = n())

# how many bee genera in the dataset?
head(data)
data %>%
  mutate(genus = sub(" .*", "", scientificName)) %>%
  summarize(n_distinct(genus))
nrow(data)
nrow(data_stratified)


#

# save one fold for testing and train the model on the rest of the data
# get distribution of accuracy, importance values etc
i <- 1
1:k_folds
i <- 3


# let's see if I can get probability from random forest models
## divide the training and testing data
df_train <- data_stratified %>%
  filter(fold != i) %>%
  select(-fold, -n_globi)
df_test <- data_stratified %>%
  filter(fold == i) %>%
  select(-fold, -n_globi)

rf <- randomForest(diet_breadth ~ ., data = df_train %>% select(-scientificName), importance = T) # ,na.action=na.omit,auc=T)

rf$votes
i=4



library(caret)

#
get_performance <- function(pred, df_test){
  
  predicted <- as.factor(as.vector(pred))
  actual <- df_test$diet_breadth
  
  #this function is to get these performance metrics
  #f1 scores for specialists and generalists
  (cm_specs <- confusionMatrix(predicted, actual, mode = 'everything', positive ='specialist'))
  (cm_gens <- confusionMatrix(predicted, actual, mode = 'everything', positive ='generalist'))
 output <- tibble(
   f1_specs = cm_specs$byClass[7],
   f1_gens = cm_gens$byClass[7],
   balanced_accuracy = cm_gens$byClass[11]
   
 ) 
 return(output)


  }

# stratfied random blocking method
stratified_output_ls <- 1:k_folds %>%
  purrr::map(function(i) {
    # divide the training and testing data
    df_train <- data_stratified %>%
      filter(fold != i) %>%
      select(-fold, -n_globi)
    df_test <- data_stratified %>%
      filter(fold == i) %>%
      select(-fold, -n_globi)

    rf <- randomForest(diet_breadth ~ ., data = df_train %>% select(-scientificName), importance = T) # ,na.action=na.omit,auc=T)
    pred <- predict(rf, df_test %>% dplyr::select(-diet_breadth, -scientificName))

    df_test$prediction_correct <- df_test$diet_breadth == pred

    specialists_wrong <- df_test[!df_test$prediction_correct & df_test$diet_breadth == "specialist", ]$scientificName
    generalists_wrong <- df_test[!df_test$prediction_correct & df_test$diet_breadth == "generalist", ]$scientificName

    # get auc values
    # the test AUC
    rf_p_test <- predict(rf, type = "prob", newdata = df_test)[, 2]
    rf_pr_test <- prediction(rf_p_test, df_test$diet_breadth)
    r_auc_test <- performance(rf_pr_test, measure = "auc")@y.values[[1]]
    r_auc_test # 0.956


  
    list(
      rf,
      tibble(
        fold_left_out = i,
        overall_accuracy = mean(df_test$prediction_correct),
        specialist_accuracy = mean(df_test[df_test$diet_breadth == "specialist", ]$prediction_correct),
        generalist_accuracy = mean(df_test[df_test$diet_breadth == "generalist", ]$prediction_correct),
        test_auc = r_auc_test,
        specialists_wrong = list(specialists_wrong),
        generalists_wrong = list(generalists_wrong)
      ) %>% bind_cols(get_performance(pred,df_test))
    )
  })
stratified_output <- stratified_output_ls %>% map_dfr(function(a_list) a_list[[2]])
with(
  stratified_output %>% select(-fold_left_out, -specialists_wrong, -generalists_wrong) %>%
    pivot_longer(everything(), names_to = "performance_type", values_to = "estimates"),
  boxplot(estimates ~ performance_type)
)

specialists_wrong_vec <- unlist(stratified_output$specialists_wrong)
generalists_wrong_vec <- unlist(stratified_output$generalists_wrong)

mean(stratified_output$overall_accuracy)
# let's look at variable importance
var_importance <- 1:length(stratified_output_ls) %>% map_dfr(function(i) {
  # select the model from the list
  my_rf <- stratified_output_ls[[i]][[1]]

  # get variable importance
  new_df <- data.frame(my_rf$importance) %>%
    mutate(predictor_var = rownames(.)) %>%
    select(predictor_var, everything()) %>%
    mutate(model_iteration = i)

  # get rid of rownames
  rownames(new_df) <- NULL

  return(new_df)
})

# get average mean decrease in accuracy and see which variables have highest average
# then plot variation
top_predictors <- var_importance %>%
  group_by(predictor_var) %>%
  summarize(mean_importance = mean(MeanDecreaseAccuracy)) %>%
  arrange(desc(mean_importance))
top10 <- top_predictors$predictor_var[1:10]
top5 <- top_predictors$predictor_var[1:5]

# plot boxplots of top predictors
with(
  var_importance %>% filter(predictor_var %in% top5),
  boxplot(MeanDecreaseAccuracy ~ predictor_var)
)


######## Next
# block the data phylogenetically by family

# combine melittidae and colletidae since melittidae only has one generalist
globi_degree %>%
  group_by(diet_breadth, bee_family) %>%
  summarize(n = n())

#fam sample sizes
globi_degree %>%
  group_by(bee_family,diet_breadth) %>%
  summarize(n = n())
globi_degree %>%
  group_by(diet_breadth, bee_family) %>%
  summarize(n = n()) %>%
  group_by(bee_family) %>%
  mutate(per =  100 *n/sum(n)) %>%
  ungroup()

per_genus <- globi_degree %>%
  mutate(bee_genus = sub(" .*","",scientificName)) %>%
  group_by(diet_breadth, bee_genus) %>%
  summarize(n = n()) %>%
  group_by(bee_genus) %>%
  mutate(per =  100 *n/sum(n)) %>%
  ungroup()
all_gens = per_genus %>% filter(per ==100 & diet_breadth=='generalist')
all_specs = per_genus %>% filter(per ==100 & diet_breadth=='specialist')
n_genera = n_distinct(per_genus$bee_genus)
nrow(all_specs)/n_genera
nrow(all_gens)/n_genera

data_phy <- globi_degree %>%
  mutate(bee_family = ifelse(bee_family %in% c("Colletidae", "Melittidae"), "Colletidae_Melittidae", bee_family)) %>%
  select(scientificName, bee_family, diet_breadth, phylo_simp, simpson_genus, eigen1_plantGenus, eigen2_plantGenus, eigen1_plantFam, eigen2_plantFam, med_lat, med_long, n_chesshire, area_ha, med_doy, flight_season) # remove the vars we don't need

# look at sample sizes within family
data_phy %>%
  group_by(bee_family) %>%
  summarize(n = n())

phylo_blocked_output_ls <- unique(data_phy$bee_family) %>%
  purrr::map(function(fam) {
    # divide the training and testing data
    df_train <- data_phy %>%
      filter(bee_family != fam) %>%
      select(-bee_family)
    df_test <- data_phy %>%
      filter(bee_family == fam) %>%
      select(-bee_family)

    rf <- randomForest(diet_breadth ~ ., data = df_train %>% select(-scientificName), importance = T) # ,na.action=na.omit,auc=T)
    pred <- predict(rf, df_test %>% dplyr::select(-diet_breadth, -scientificName))

    df_test$prediction_correct <- df_test$diet_breadth == pred

    specialists_wrong <- df_test[!df_test$prediction_correct & df_test$diet_breadth == "specialist", ]$scientificName
    generalists_wrong <- df_test[!df_test$prediction_correct & df_test$diet_breadth == "generalist", ]$scientificName

    # get auc values
    # the test AUC
    rf_p_test <- predict(rf, type = "prob", newdata = df_test)[, 2]
    rf_pr_test <- prediction(rf_p_test, df_test$diet_breadth)
    r_auc_test <- performance(rf_pr_test, measure = "auc")@y.values[[1]]
    r_auc_test # 0.956


    ## auc of the training data
    rf_p_train <- predict(rf, type = "prob", newdata = df_train)[, 2]
    rf_pr_train <- prediction(rf_p_train, df_train$diet_breadth)
    r_auc_train <- performance(rf_pr_train, measure = "auc")@y.values[[1]]
    r_auc_train # 0.956
    
    specialists_wrong <- df_test[!df_test$prediction_correct & df_test$diet_breadth == "specialist", ]$scientificName
    generalists_wrong <- df_test[!df_test$prediction_correct & df_test$diet_breadth == "generalist", ]$scientificName
  
    list(
      rf,
      tibble(
        fam_left_out = fam,
        overall_accuracy = mean(df_test$prediction_correct),
        specialist_accuracy = mean(df_test[df_test$diet_breadth == "specialist", ]$prediction_correct),
        generalist_accuracy = mean(df_test[df_test$diet_breadth == "generalist", ]$prediction_correct),
        test_auc = r_auc_test,
        specialists_wrong = list(specialists_wrong),
        generalists_wrong = list(generalists_wrong)
      ) %>% bind_cols(get_performance(pred,df_test)),
      df_train,
      df_test
    )

    
    
  })
phylo_blocked_output <- phylo_blocked_output_ls %>% map_dfr(function(a_list) a_list[[2]])
with(
  phylo_blocked_output %>% select(-fam_left_out, -specialists_wrong, -generalists_wrong) %>%
    pivot_longer(everything(), names_to = "performance_type", values_to = "estimates"),
  boxplot(estimates ~ performance_type)
)


# let's look at variable importance
var_importance_phy <- 1:length(phylo_blocked_output_ls) %>% map_dfr(function(i) {
  # select the model from the list
  my_rf <- phylo_blocked_output_ls[[i]][[1]]

  # get variable importance
  new_df <- data.frame(my_rf$importance) %>%
    mutate(predictor_var = rownames(.)) %>%
    select(predictor_var, everything()) %>%
    mutate(model_iteration = i)

  # get rid of rownames
  rownames(new_df) <- NULL

  return(new_df)
})

# get average mean decrease in accuracy and see which variables have highest average
# then plot variation
top_predictors_phy <- var_importance_phy %>%
  group_by(predictor_var) %>%
  summarize(mean_importance = mean(MeanDecreaseAccuracy)) %>%
  arrange(desc(mean_importance))
top10_phy <- top_predictors_phy$predictor_var[1:10]
top5_phy <- top_predictors_phy$predictor_var[1:5]

# plot boxplots of top predictors
with(
  var_importance_phy %>% filter(predictor_var %in% top5_phy),
  boxplot(MeanDecreaseAccuracy ~ predictor_var)
)


######## Next
# block the data spatially
# first let's look at the data
with(data, plot(med_long, med_lat))


# let's divide into 4x2 grid
# divide the data in half by latitude
med_lat_overall <- median(globi_degree$med_lat)
data_space <- globi_degree %>% mutate(lat_block = ifelse(med_lat >= med_lat_overall, 1, 2))


# divide the data in fours by 25, 50 and 75 percentiles
lon_seq <- quantile(globi_degree$med_long, probs = c(.25, .5, .75))
data_space$long_block <- 1
data_space[data_space$med_long > lon_seq[1] & data_space$med_long <= lon_seq[2], ]$long_block <- 2
data_space[data_space$med_long > lon_seq[2] & data_space$med_long <= lon_seq[3], ]$long_block <- 3
data_space[data_space$med_long >= lon_seq[3], ]$long_block <- 4

# add spatial block as a variable
data_space$spatial_block <- as.numeric(as.factor(paste(data_space$lat_block, data_space$long_block)))

# check and make sure the sample sizes are approximately even in each block
data_space_ns <- data_space %>%
  group_by(spatial_block) %>%
  summarize(n = n())



data_space2 <- data_space %>%
  select(-all_of(vars_to_remove)) %>%
  select(-n_globi, -bee_family, -lat_block, -long_block)

data_space_means <- data_space2 %>%
  group_by(spatial_block) %>%
  summarize(latitude = mean(med_lat), longitude = mean(med_long))

block <- 1
# plot to double check everything:
with(data_space2, plot(med_long, med_lat, col = spatial_block))
with(data_space_means, text(longitude,latitude,label=spatial_block,col='black'))

#let's also look at the prop of specialists/generalists in each region
data_space %>%
  group_by(spatial_block, diet_breadth) %>%
  summarize(n_diet = n()) %>% 
  left_join(data_space_ns) %>%
  mutate(prop_diet_breadth= n_diet/n) %>%
  filter(diet_breadth=='specialist')

# now run the cross validation
spatial_blocked_output_ls <- 1:n_distinct(data_space2$spatial_block) %>%
  purrr::map(function(block) {
    # divide the training and testing data
    df_train <- data_space2 %>%
      filter(spatial_block != block) %>%
      select(-spatial_block, -med_lat, -med_long)
    df_test <- data_space2 %>%
      filter(spatial_block == block) %>%
      select(-spatial_block, -med_lat, -med_long)

    rf <- randomForest(diet_breadth ~ ., data = df_train %>% select(-scientificName), importance = T) # ,na.action=na.omit,auc=T)
    pred <- predict(rf, df_test %>% dplyr::select(-diet_breadth, -scientificName))

    df_test$prediction_correct <- df_test$diet_breadth == pred

    specialists_wrong <- df_test[!df_test$prediction_correct & df_test$diet_breadth == "specialist", ]$scientificName
    generalists_wrong <- df_test[!df_test$prediction_correct & df_test$diet_breadth == "generalist", ]$scientificName


    # get auc values
    # the test AUC
    rf_p_test <- predict(rf, type = "prob", newdata = df_test)[, 2]
    rf_pr_test <- prediction(rf_p_test, df_test$diet_breadth)
    r_auc_test <- performance(rf_pr_test, measure = "auc")@y.values[[1]]



    list(
      rf,
      tibble(
        block_left_out = block,
        overall_accuracy = mean(df_test$prediction_correct),
        specialist_accuracy = mean(df_test[df_test$diet_breadth == "specialist", ]$prediction_correct),
        generalist_accuracy = mean(df_test[df_test$diet_breadth == "generalist", ]$prediction_correct),
        test_auc = r_auc_test,
        specialists_wrong = list(specialists_wrong),
        generalists_wrong = list(generalists_wrong)
      )%>% bind_cols(get_performance(pred,df_test)),
      df_train,
      df_test
    )
  })
spatial_blocked_output <- spatial_blocked_output_ls %>% map_dfr(function(a_list) a_list[[2]])
with(
  spatial_blocked_output %>% select(-block_left_out, -specialists_wrong, -generalists_wrong) %>%
    pivot_longer(everything(), names_to = "performance_type", values_to = "estimates"),
  boxplot(estimates ~ performance_type)
)
mean(spatial_blocked_output$overall_accuracy)

# get probability of specialists and generalists
the_probs <- 1:n_distinct(data_phy$bee_family) %>%
  purrr::map_dfr(function(i) {
    my_rf <- phylo_blocked_output_ls[[i]][[1]]
    my_sp <- phylo_blocked_output_ls[[i]][[3]] %>% select(scientificName, diet_breadth)
    prop_df <- my_sp %>%
      bind_cols(data.frame(my_rf$votes)) %>%
      mutate(iteration = i)
  })



mean_probs <- the_probs %>%
  group_by(scientificName, diet_breadth) %>%
  summarize(prob_generalist = mean(generalist), prob_specialist = mean(specialist))

generalist_probs <- mean_probs %>%
  filter(diet_breadth == "generalist") %>%
  select(scientificName, diet_breadth, prob_generalist) %>%
  arrange(prob_generalist)
specialist_probs <- mean_probs %>%
  filter(diet_breadth == "specialist") %>%
  select(scientificName, diet_breadth, prob_generalist) %>%
  mutate(prob_specialist = 1-prob_generalist) %>%
  arrange(prob_specialist)  %>% select(-prob_generalist)

# write_csv(specialist_probs, "figures/specialist_probabilities.csv")
# write_csv(generalist_probs, "figures/generalist_probabilities.csv")

#
# let's look at variable importance
var_importance_space <- 1:length(spatial_blocked_output_ls) %>% map_dfr(function(i) {
  # select the model from the list
  my_rf <- spatial_blocked_output_ls[[i]][[1]]

  # get variable importance
  new_df <- data.frame(my_rf$importance) %>%
    mutate(predictor_var = rownames(.)) %>%
    select(predictor_var, everything()) %>%
    mutate(model_iteration = i)

  # get rid of rownames
  rownames(new_df) <- NULL

  return(new_df)
})

# get average mean decrease in accuracy and see which variables have highest average
# then plot variation
top_predictors_space <- var_importance_space %>%
  group_by(predictor_var) %>%
  summarize(mean_importance = mean(MeanDecreaseAccuracy)) %>%
  arrange(desc(mean_importance))
top10_space <- top_predictors_space$predictor_var[1:10]
top5_space <- top_predictors_space$predictor_var[1:5]


specialist_importance <- var_importance_space %>%
  group_by(predictor_var) %>%
  summarize(mean_importance = mean(specialist)) %>%
  arrange(desc(mean_importance))
generalist_importance <- var_importance_space %>%
  group_by(predictor_var) %>%
  summarize(mean_importance = mean(generalist)) %>%
  arrange(desc(mean_importance))
top5_specialists <- specialist_importance$predictor_var[1:5]
top5_generalists <- generalist_importance$predictor_var[1:5]

# plot boxplots of top predictors
with(
  var_importance_space %>% filter(predictor_var %in% top5_space),
  boxplot(MeanDecreaseAccuracy ~ predictor_var)
)
with(
  var_importance_space %>% filter(predictor_var %in% top5_generalists),
  boxplot(generalist ~ predictor_var)
)
with(
  var_importance_space %>% filter(predictor_var %in% top5_specialists),
  boxplot(specialist ~ predictor_var)
)

# for the spatially blocked output, plot sample size vs whether the classification was right or wrong
# for this we want both specialists and generalists classified incorrectly
# (or actually I think we want just generalists)
gens_wrong_ls <- spatial_blocked_output %>% select(generalists_wrong)
gens_wrong <- gens_wrong_ls[[1]] %>% unlist()

n_accuracy <- globi_degree %>%
  filter(diet_breadth == "generalist") %>%
  select(scientificName, n_globi) %>%
  mutate(predicted_generalist = !scientificName %in% gens_wrong)

# fit logistic regression
lr <- glm(predicted_generalist ~ n_globi, data = n_accuracy)
predict_df <- data.frame(n_globi = seq(min(n_accuracy$n_globi), max(n_accuracy$n_globi), 1))
predict_df2 <- predict_df %>%
  mutate(odds = predict(lr, predict_df, type = "response")) %>%
  mutate(prob = odds / (1 + odds))
with(n_accuracy, plot(n_globi, predicted_generalist))
with(predict_df2, lines(n_globi, prob, add = T))
summary(predict_df2$prob)
with(globi_degree %>% filter(diet_breadth == "generalist"), plot(n_globi, phylo_simp))
with(globi_degree %>% filter(diet_breadth == "generalist"), plot(log(n_globi), phylo_simp))
with(globi_degree %>% filter(diet_breadth == "generalist"), plot(log(n_globi), simpson_genus))


# plot phylogenetic diversity between specialist and generalist bees
pdf(rename_figure("figures/phylo_div_boxplot.pdf"))

with(globi_degree, boxplot(phylo_simp ~ diet_breadth, ylab = "phylogenetic Simpson diversity", xlab = "diet breadth", col = "lightcyan2", cex.lab = 1.3))

 dev.off()
 
# what is the mean difference between specialists and generalists in phylo diversity visited?
avg_phylo_div=globi_degree %>% group_by(diet_breadth) %>% summarize(phylo_simp = mean(phylo_simp))
phylo_gen = avg_phylo_div[avg_phylo_div$diet_breadth=='generalist',]$phylo_simp
phylo_spe = avg_phylo_div[avg_phylo_div$diet_breadth=='specialist',]$phylo_simp
((phylo_gen-phylo_spe)/phylo_spe)*100
# aggregate all accuracy & AUC values from the models
stratified_sum <- stratified_output %>%
  select(-fold_left_out, -specialists_wrong, -generalists_wrong) %>%
  mutate(blocking_method = "random")
phylo_sum <- phylo_blocked_output %>%
  select(-fam_left_out, -specialists_wrong, -generalists_wrong) %>%
  mutate(blocking_method = "phylogenetic")
spatial_sum <- spatial_blocked_output %>%
  select(-block_left_out, -specialists_wrong, -generalists_wrong) %>%
  mutate(blocking_method = "spatial")

accuracy_sum <- stratified_sum %>%
  bind_rows(phylo_sum) %>%
  bind_rows(spatial_sum)

accuracy_sum %>% filter(blocking_method != "random") %>%
  pivot_longer(cols= !'blocking_method', values_to = 'estimate') %>%
  group_by(name) %>% summarize(mean=mean(estimate))

accuracy_sum %>% filter(blocking_method == "phylogenetic") %>%
  pivot_longer(cols= !'blocking_method', values_to = 'estimate') %>%
  group_by(name) %>% summarize(mean=mean(estimate))
accuracy_sum %>% filter(blocking_method == "spatial") %>%
  pivot_longer(cols= !'blocking_method', values_to = 'estimate') %>%
  group_by(name) %>% summarize(mean=mean(estimate))

accuracy_sum %>%
  dplyr::select(blocking_method, everything()) %>%
  pivot_longer(cols = !blocking_method, names_to = "performance_measure", values_to = "estimate") %>%
  group_by(blocking_method, performance_measure) %>%
  summarize(mean = mean(estimate), min = min(estimate), max = max(estimate))


accuracy_long <- accuracy_sum %>%
  dplyr::select(blocking_method, everything()) %>%
  rename(AUC = test_auc) %>%
  pivot_longer(cols = !blocking_method, names_to = "performance_measure", values_to = "estimate")



# next let's aggregate all importance values together from all model runs
# and rank them from most to least important
var_importance_all <- var_importance %>%
  bind_rows(var_importance_phy) %>%
  bind_rows(var_importance_space)
vars_ranked <- var_importance_all %>%
  group_by(predictor_var) %>%
  summarize(mean_importance = mean(MeanDecreaseAccuracy)) %>%
  arrange(desc(mean_importance))
top10_all <- vars_ranked[1:10, ]$predictor_var
with(var_importance_all %>% filter(predictor_var %in% top10_all), boxplot(MeanDecreaseAccuracy ~ predictor_var))



rename_all <- data.frame(
  predictor_var = c(
    "phylo_simp", "simpson_genus", "eigen2_plantGenus", "n_chesshire", "area_ha",
    "med_lat", "eigen2_plantFam", "flight_season", "eigen1_plantFam", "eigen1_plantGenus",
    "med_doy", "med_long"
  ),
  predictor_factor = c(
    "Plant \nphylogenetic \ndiversity", "Plant Simpson \ndiversity",
    "Plant genus \nidentity (2nd \neigenvalue)", "Regional \nabundance", "Area \n(ha)",
    "Median \nlatitude", "Plant family \nidentity (2nd \neigenvalue)", "Flight \nseason", 
    "\nPlant family \nidentity (1st \neigenvalue)", "Plant genus \nidentity (1st \neigenvalue)",
    "Median day \nof activity", "Median longitude"
  )
) %>% left_join(vars_ranked) %>%
  arrange(desc(mean_importance)) 
rename2 <- rename_all[10:1,] %>%
  mutate(predictor_factor = factor(predictor_factor, levels = predictor_factor))


# vars_ranked$predictor_var[!vars_ranked$predictor_var %in% rename$predictor_var]
rename_add <- data.frame(
  predictor_var = c("med_doy", "med_long"),
  predictor_factor = c("Median day \nof activity", "Median longitude")
)

# #make table with importance values for the supplement
all_var_table <- vars_ranked %>%
  left_join(rename_all %>% select(-mean_importance) %>% distinct()) %>%
  mutate(predictor_factor = as.character(predictor_factor)) %>%
  mutate(predictor_factor = ifelse(is.na(predictor_factor), predictor_var, predictor_factor)) %>%
  dplyr::select(predictor_factor, mean_importance) %>%
  rename("Predictor variable" = predictor_factor, "Mean importance" = mean_importance)

# write_xlsx(all_var_table,'modeling_data/Importance_predictors-28nov23.xlsx')

# format figure for the paper
var_top10 <- rename2 %>%
  left_join(var_importance_all )

# var_importance_all %>% left_join(rename)

my_col <- adjustcolor("skyblue4", .6)



# first add the data points
cex_axis=1.3
tiff(rename_figure('figures/var_importance2-main.tiff'), units="in", width=20, height=12, res=1000, compression = 'lzw')
# pdf(rename_figure("figures/var_importance2-main.pdf"), width = 18)
par(cex.lab = 2, mgp = c(5.5, 3.2, 0), mar = c(7, 7.3, 5, 1), mfrow = c(1, 1))
with(var_top10, stripchart(MeanDecreaseAccuracy ~ predictor_factor, # Data
  method = "jitter", # Random noise
  pch = 19,
  cex = 1.5,
  cex.axis=cex_axis,
  col = my_col, # Color of the symbol
  vertical = TRUE, # Vertical mode
  xlab = "",
  ylab = "Importance (mean decrase in model accuracy)"
))
mtext("Predictor Variable", side = 1, line = 5.5, cex = 2)
# next add the boxplot
with(var_top10, boxplot(MeanDecreaseAccuracy ~ predictor_factor,
  col = adjustcolor("white", 0), add = T, cex.axis = cex_axis

  ))
dev.off()


## make partial dependence plots for the predictors
i <- 1
partial_ls <- 1:length(phylo_blocked_output_ls) %>% purrr::map(function(i) {
  my_rf <- phylo_blocked_output_ls[[i]][[1]]
  df_train <- phylo_blocked_output_ls[[i]][[3]]

  partial_data1 <- partial(my_rf, pred.var = c("phylo_simp"), which.class = "specialist", prob = T, train = df_train) %>%
    rename(prob_specialist_phylo = yhat)
  partial_data2 <- partial(my_rf, pred.var = c("simpson_genus"), which.class = "specialist", prob = T, train = df_train) %>%
    rename(prob_specialist_simp = yhat)

  output_data <- partial_data1 %>%
    bind_cols(partial_data2) %>%
    mutate(model_iteration = i)
  return(output_data)
})
partial_df <- partial_ls %>% bind_rows()

## calculate average effect size for phylo_simp and simpson_genus
head(partial_df)
# prob for species visiting max phylo diversity of plants
(max_phylo_pred <- partial_df[which.max(partial_df$phylo_simp), ]$prob_specialist_phylo)
# prob for species visiting min phylo diversity of plants
(min_phylo_pred <- partial_df[which.min(partial_df$phylo_simp), ]$prob_specialist_phylo)

# percent increase
(min_phylo_pred - max_phylo_pred) / max_phylo_pred

##
# prob for species visiting max simpson diversity of plants
(max_simp_pred <- partial_df[which.max(partial_df$simpson_genus), ]$prob_specialist_simp)
# prob for species visiting min simpson diversity of plants
(min_simp_pred <- partial_df[which.min(partial_df$simpson_genus), ]$prob_specialist_simp)

# percent increase
(min_simp_pred - max_simp_pred) / max_simp_pred


# format data to make histograms
break_n <- 40
phylo_simp_min <- min(data_space2$phylo_simp)
phylo_simp_max <- max(data_space2$phylo_simp)
my_breaks <- seq(phylo_simp_min, phylo_simp_max, by = (phylo_simp_max - phylo_simp_min) / break_n)
data_space2$category_binary <- ifelse(data_space2$diet_breadth == "specialist", 1, 0)

densities_df <- data_space2 %>%
  split(.$category_binary) %>%
  map_dfr(function(df) {
    dens <- hist(df$phylo_simp, plot = F, breaks = my_breaks)
    percent_dens <- dens$density / sum(dens$density)


    data.frame(phylo_simp = dens$mid, percent_dens = percent_dens) %>%
      mutate(category_binary = df$category_binary[1])
  })

hist_df <- densities_df %>% 
  mutate(pct = ifelse(category_binary, 1 - percent_dens, percent_dens))




my_cols <- RColorBrewer::brewer.pal(length(partial_ls), "Paired")
my_cols2 <- RColorBrewer::brewer.pal(8, "Set2")[4:8]

# tiff('figures/var_importance.tiff', units="in", width=6, height=6, res=500, compression = 'lzw')
(phylo_simp_graph <- ggplot() +
  geom_segment(
    data = hist_df[hist_df$category_binary == 0, ], size = 4, show.legend = FALSE, colour = my_cols2[1],
    aes(x = phylo_simp, xend = phylo_simp, y = category_binary, yend = pct)
  ) +
  geom_segment(
    data = hist_df[hist_df$category_binary == 1, ], size = 4, show.legend = FALSE, colour = my_cols2[2],
    aes(x = phylo_simp, xend = phylo_simp, y = category_binary, yend = pct)
  ) +
  geom_segment(dat = data_space2[data_space2$category_binary == 0, ], aes(x = phylo_simp, xend = phylo_simp, y = 0, yend = -0.02), size = 0.2, colour = "grey30") +
  geom_segment(dat = data_space2[data_space2$category_binary == 1, ], aes(x = phylo_simp, xend = phylo_simp, y = 1, yend = 1.02), size = 0.2, colour = "grey30") +
  geom_line(data = partial_ls[[1]], aes(x = phylo_simp, y = prob_specialist_phylo), color = my_cols[partial_ls[[1]]$model_iteration[1]], lwd = 1) +
  geom_line(data = partial_ls[[2]], aes(x = phylo_simp, y = prob_specialist_phylo), color = my_cols[partial_ls[[2]]$model_iteration[1]], lwd = 1) +
  geom_line(data = partial_ls[[3]], aes(x = phylo_simp, y = prob_specialist_phylo), color = my_cols[partial_ls[[3]]$model_iteration[1]], lwd = 1) +
  geom_line(data = partial_ls[[4]], aes(x = phylo_simp, y = prob_specialist_phylo), color = my_cols[partial_ls[[4]]$model_iteration[1]], lwd = 1) +
  geom_line(data = partial_ls[[5]], aes(x = phylo_simp, y = prob_specialist_phylo), color = my_cols[partial_ls[[5]]$model_iteration[1]], lwd = 1) +
  # geom_line(data = partial_ls[[6]], aes(x = phylo_simp, y = prob_specialist_phylo), color = my_cols[partial_ls[[6]]$model_iteration[1]], lwd = 1) +
  # geom_line(data = partial_ls[[7]], aes(x = phylo_simp, y = prob_specialist_phylo), color = my_cols[partial_ls[[7]]$model_iteration[1]], lwd = 1) +
  # geom_line(data = partial_ls[[8]], aes(x = phylo_simp, y = prob_specialist_phylo), color = my_cols[partial_ls[[8]]$model_iteration[1]], lwd = 1) +
  theme_bw(base_size = 12) +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    text = element_text(size = 20)
  ) +
  labs(x = "Phylogenetic diversity of \nplants visited", y = "Probability of being a \nspecialist bee"))




# format data to make histograms
break_n <- 40
simp_min <- min(data_space2$simpson_genus)
simp_max <- max(data_space2$simpson_genus)
my_breaks_simp <- seq(simp_min, simp_max, by = (simp_max - simp_min) / break_n)

densities_df_simp <- data_space2 %>%
  split(.$category_binary) %>%
  map_dfr(function(df) {
    dens <- hist(df$simpson_genus, plot = F, breaks = my_breaks_simp)
    percent_dens <- dens$density / sum(dens$density)


    data.frame(simpson_genus = dens$mid, percent_dens = percent_dens) %>%
      mutate(category_binary = df$category_binary[1])
  })

hist_df_simp <- densities_df_simp %>% mutate(pct = ifelse(category_binary, 1 - percent_dens, percent_dens))


(simpson_genus_graph <- ggplot() +
  geom_segment(
    data = hist_df_simp[hist_df_simp$category_binary == 1, ], size = 4, show.legend = FALSE, colour = my_cols2[1],
    aes(x = simpson_genus, xend = simpson_genus, y = category_binary, yend = pct)
  ) +
  geom_segment(
    data = hist_df_simp[hist_df_simp$category_binary == 0, ], size = 4, show.legend = FALSE, colour = my_cols2[2],
    aes(x = simpson_genus, xend = simpson_genus, y = category_binary, yend = pct)
  ) +
  geom_segment(dat = data_space2[data_space2$category_binary == 0, ], aes(x = simpson_genus, xend = simpson_genus, y = 0, yend = -0.02), size = 0.2, colour = "grey30") +
  geom_segment(dat = data_space2[data_space2$category_binary == 1, ], aes(x = simpson_genus, xend = simpson_genus, y = 1, yend = 1.02), size = 0.2, colour = "grey30") +
  geom_line(data = partial_ls[[1]], aes(x = simpson_genus, y = prob_specialist_simp), color = my_cols[partial_ls[[1]]$model_iteration[1]], lwd = 1) +
  geom_line(data = partial_ls[[2]], aes(x = simpson_genus, y = prob_specialist_simp), color = my_cols[partial_ls[[2]]$model_iteration[1]], lwd = 1) +
  geom_line(data = partial_ls[[3]], aes(x = simpson_genus, y = prob_specialist_simp), color = my_cols[partial_ls[[3]]$model_iteration[1]], lwd = 1) +
  geom_line(data = partial_ls[[4]], aes(x = simpson_genus, y = prob_specialist_simp), color = my_cols[partial_ls[[4]]$model_iteration[1]], lwd = 1) +
  geom_line(data = partial_ls[[5]], aes(x = simpson_genus, y = prob_specialist_simp), color = my_cols[partial_ls[[5]]$model_iteration[1]], lwd = 1) +
  # geom_line(data = partial_ls[[6]], aes(x = simpson_genus, y = prob_specialist_simp), color = my_cols[partial_ls[[6]]$model_iteration[1]], lwd = 1) +
  # geom_line(data = partial_ls[[7]], aes(x = simpson_genus, y = prob_specialist_simp), color = my_cols[partial_ls[[7]]$model_iteration[1]], lwd = 1) +
  # geom_line(data = partial_ls[[8]], aes(x = simpson_genus, y = prob_specialist_simp), color = my_cols[partial_ls[[8]]$model_iteration[1]], lwd = 1) +
  theme_bw(base_size = 12) +
  theme(
    # Hide panel borders and remove grid lines
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # Change axis line
    axis.line = element_line(colour = "black"),
    text = element_text(size = 20)
  ) +
  labs(x = "Simpson diversity of plants \nvisited", y = "Probability of being a \nspecialist bee"))


# dev.of()






# tiff('figures/important_vars.tiff', units="in", width=7, height=14, res=1000, compression = 'lzw')
pdf(rename_figure("figures/important_vars-main.pdf"), width = 7, height = 10)
grid.arrange(phylo_simp_graph, simpson_genus_graph)
dev.off()



# partialPlot(my_rf, pred.data = df_train, x.var = "phylo_simp")
# partial_data = partial(my_rf, pred.var = "phylo_simp")  %>% mutate(model_iteration=i)


# how does the spatial model perform without any phylognetic predcitors
colnames(data_space2)
data_space3 <- data_space2 %>% select(
  scientificName, diet_breadth, phylo_simp, eigen1_plantGenus,
  eigen2_plantGenus, eigen1_plantFam, eigen2_plantFam,
  simpson_genus, med_lat, med_long, n_chesshire, area_ha, med_doy, flight_season, spatial_block
)
spatial_blocked_output_np <- unique(data_space3$spatial_block) %>%
  purrr::map_dfr(function(block) {
    # divide the training and testing data
    df_train <- data_space3 %>%
      filter(spatial_block != block) %>%
      select(-spatial_block, -med_lat, med_long)
    df_test <- data_space3 %>%
      filter(spatial_block == block) %>%
      select(-spatial_block, -med_lat, med_long)

    rf <- randomForest(diet_breadth ~ ., data = df_train %>% select(-scientificName), importance = T) # ,na.action=na.omit,auc=T)
    pred <- predict(rf, df_test %>% dplyr::select(-diet_breadth, -scientificName))

    df_test$prediction_correct <- df_test$diet_breadth == pred

    specialists_wrong <- df_test[!df_test$prediction_correct & df_test$diet_breadth == "specialist", ]$scientificName


    # get auc values
    # the test AUC
    rf_p_test <- predict(rf, type = "prob", newdata = df_test)[, 2]
    rf_pr_test <- prediction(rf_p_test, df_test$diet_breadth)
    r_auc_test <- performance(rf_pr_test, measure = "auc")@y.values[[1]]
    r_auc_test # 0.956


    ## auc of the training data
    rf_p_train <- predict(rf, type = "prob", newdata = df_train)[, 2]
    rf_pr_train <- prediction(rf_p_train, df_train$diet_breadth)
    r_auc_train <- performance(rf_pr_train, measure = "auc")@y.values[[1]]
    r_auc_train # 0.956


    tibble(
      block_left_out = block,
      overall_accuracy = mean(df_test$prediction_correct),
      specialist_accuracy = mean(df_test[df_test$diet_breadth == "specialist", ]$prediction_correct),
      generalist_accuracy = mean(df_test[df_test$diet_breadth == "generalist", ]$prediction_correct),
      test_auc = r_auc_test,
      train_auc = r_auc_train,
      specialists_wrong = list(specialists_wrong)
    )%>% bind_cols(get_performance(pred,df_test))
  })
spatial_blocked_output_np
with(
  spatial_blocked_output_np %>% select(-block_left_out, -specialists_wrong) %>%
    pivot_longer(everything(), names_to = "performance_type", values_to = "estimates"),
  boxplot(estimates ~ performance_type)
)

# calculate changes in accuracy with and without phylo predictors
spatil_np_long <- spatial_blocked_output_np %>%
  select(-block_left_out, -specialists_wrong) %>%
  pivot_longer(everything(), names_to = "performance_type", values_to = "estimates") %>%
  mutate(phylo_predictors = "no")
spatial_long <- spatial_blocked_output %>%
  select(-block_left_out, -specialists_wrong, -generalists_wrong) %>%
  pivot_longer(everything(), names_to = "performance_type", values_to = "estimates") %>%
  mutate(phylo_predictors = "yes")

spatil_np_long %>%
  bind_rows(spatial_long) %>%
  group_by(performance_type, phylo_predictors) %>%
  summarize(mean = mean(estimates)) %>%
  pivot_wider(names_from = phylo_predictors, values_from = mean)


## let's see how our random forest models compare to something simpler
(all_data_props <- data_space %>%
  mutate(bee_genus = sub(" .*", "", scientificName)) %>%
  group_by(bee_genus) %>%
  summarize(prop_specialists = mean(diet_breadth == "specialist")))

all_data_props %>% arrange(prop_specialists)
all_data_props %>% arrange(desc(prop_specialists))
mean(all_data_props$prop_specialists == 1)
mean(all_data_props$prop_specialists == 0)

#what are the proportions of specialists in each family?
data_space %>%
  group_by(bee_family) %>%
  summarize(prop_specialists = mean(diet_breadth == "specialist"))

i <- 1
majority_class_predictions <- unique(data_space$spatial_block) %>%
  map_dfr(function(testing_block) {
    # get globi sample size and add to data_space2
    df_train <- data_space %>%
      filter(spatial_block != testing_block) %>%
      mutate(bee_genus = sub(" .*", "", scientificName))
    df_test <- data_space %>%
      filter(spatial_block == testing_block) %>%
      mutate(bee_genus = sub(" .*", "", scientificName))

    # get prop specialists in each genus and family, using the training data
    genera_majority <- df_train %>%
      group_by(bee_genus) %>%
      summarize(prop_specialists = mean(diet_breadth == "specialist")) %>%
      mutate(majority_class = ifelse(prop_specialists > 0.5, "specialist", "generalist"))
    family_majority <- df_train %>%
      group_by(bee_family) %>%
      summarize(prop_specialists = mean(diet_breadth == "specialist")) %>%
      mutate(majority_class = ifelse(prop_specialists > 0.5, "specialist", "generalist"))


    # predict bee species will be majority class in its genus, using the testing data
    genus_prediction <- df_test %>%
      dplyr::select(scientificName, bee_genus, bee_family) %>%
      left_join(genera_majority) %>%
      select(scientificName, majority_class, bee_family) %>%
      rename(prediction = majority_class)

    # if the genus is not in the training data use the majority class of the family
    family_prediction <- genus_prediction %>%
      filter(is.na(prediction)) %>%
      left_join(family_majority) %>%
      select(scientificName, majority_class, bee_family) %>%
      rename(prediction = majority_class)

    all_prediction <- genus_prediction %>%
      filter(!is.na(prediction)) %>%
      bind_rows(family_prediction) %>%
      select(-bee_family)
    # check if predictions are correct
    check_predictions <- df_test %>%
      select(scientificName, diet_breadth) %>%
      left_join(all_prediction) %>%
      mutate(correct = diet_breadth == prediction)

    data.frame(
      testing_block = testing_block,
      overall_accuracy = mean(check_predictions$correct),
      specialist_accuracy = with(check_predictions %>% filter(diet_breadth == "specialist"), mean(correct)),
      generalist_accuracy = with(check_predictions %>% filter(diet_breadth == "generalist"), mean(correct))
    ) %>% mutate(balanced_accuracy = mean(c(specialist_accuracy, generalist_accuracy)))
  })
apply(majority_class_predictions, 2, mean)
mean(majority_class_predictions$overall_accuracy)




# plot accuracy with majority class model added
# add these to accuracy_long:

all_accuracy <- accuracy_long %>%
  mutate(model = "random forest") %>%
  bind_rows(
    majority_class_predictions %>%
      mutate(blocking_method = "spatial") %>%
      select(blocking_method, everything()) %>%
      select(-testing_block) %>%
      pivot_longer(-blocking_method, names_to = "performance_measure", values_to = "estimate") %>%
      mutate(model = "majority class")
  ) %>%
  mutate(blocking_model = paste(blocking_method, model, sep = "_")) %>% filter(!performance_measure %in% c('overall_accuracy', 'f1_specs', 'f1_gens'))
blocking_cols2 <- c("#FFFFB380", "#BEBADA80", "#FB807280", "#FB807280")


#
#how does the simple phylogenetic model compare in performance to 
#the other models?
all_accuracy %>%
  dplyr::select(blocking_model, everything()) %>%
  dplyr::select(-blocking_method,-model) %>%
  group_by(blocking_model, performance_measure) %>%
  summarize(mean = mean(estimate),min=min(estimate), max=max(estimate))
 
# code for double checking things are in the right order in the figs
par(mfrow = c(1, 1))
i <- 3
pm <- unique(all_accuracy$performance_measure)[i]
title <- "Generalist accuracy"
my_ylab <- "Generalist accuracy"
blocking_cols <- adjustcolor(RColorBrewer::brewer.pal(4, "Set3")[c(2, 3, 4, 4)], .5)
focal_df <- all_accuracy %>%
  filter(performance_measure == pm) %>%
  mutate(blocking_model = factor(blocking_model, levels = c("phylogenetic_random forest", "random_random forest", "spatial_random forest", "spatial_majority class")))
pm2 <- sub("_", " ", pm)
with(focal_df, boxplot(estimate ~ blocking_model,
  ylim = c(0.2, 1),
  col = blocking_cols, xlab = "Model type",
  ylab = my_ylab, main = title, cex.main = 1.8, 
))




# make boxplots of accuracy by blocking method for different performance measures
ylim_vec = c(min(all_accuracy$estimate)-.07, 1)
axis_pos = 0.3

# pdf(rename_figure("figures/accuracy_estimates_supp.pdf"), width = 9)
tiff(rename_figure("figures/accuracy_estimates_supp.tiff"), width = 8, height =8, units = 'in', res = 1000, compression = 'lzw')

par(mfrow = c(2, 2), mar = c(4.2, 4.2, 5, 1), cex.lab = 1.5)
cex_lab2 <- 1.8
for (i in 1:n_distinct(all_accuracy$performance_measure)) {

    pm <- unique(all_accuracy$performance_measure)[c(1,2,4,3)][i]
  focal_df <- all_accuracy %>%
    filter(performance_measure == pm) %>%
    mutate(blocking_model = factor(blocking_model, levels = c("phylogenetic_random forest", "random_random forest", "spatial_random forest", "spatial_majority class")))
  pm2 <- sub("_", " ", pm)
  title <- paste0(toupper(substr(pm2, 1, 1)), substr(pm2, 2, nchar(pm2)))
  my_ylab <- "Accuracy"
  if (pm == "AUC") {
    title <- "AUC"
    my_ylab <- "AUC"
  }
  the_col <- adjustcolor("plum4", 0.6)
  blocking_cols <- adjustcolor(RColorBrewer::brewer.pal(4, "Set3")[c(2, 3, 4, 4)], .5)

  # next add the boxplot
  if (i %in% 1:3) {
    with(focal_df, boxplot(estimate ~ blocking_model,
      ylim = ylim_vec,
      at = c(1:3, 5), names = c("", "random forest", "", "majority class"),
      col = blocking_cols, xlab = "Model type",
      ylab = my_ylab, main = title, cex.main = 1.8, xaxt = "n"
    ))
    axis(side = 1, padj = axis_pos, at = c(1:3, 5), labels = c("", "random \nforest", "", "simple \nphylogeny"), tick = F, cex.axis = 1.4)

    with(focal_df, stripchart(estimate ~ blocking_model, # Data
      method = "jitter", # Random noise
      at = c(1:3, 5),
      pch = 19,
      cex = 1.3,
      col = the_col, # Color of the symbol
      vertical = TRUE, # Vertical mode
      add = T
    ))
  }
  if (i == 4) {
    focal_df2 <- focal_df %>% mutate(blocking_model = as.character(blocking_model))
    with(focal_df2, boxplot(estimate ~ blocking_model,
      ylim = ylim_vec,
      names = c("", "random forest", ""),
      col = blocking_cols, xlab = "Model type",
      ylab = my_ylab, main = title, cex.main = 1.8, xaxt = "n"
    ))
    axis(side = 1, padj = axis_pos, at = 1:3, labels = c("", "random \nforest", ""), tick = F, cex.axis = 1.4)

    legend("bottomright", fill = blocking_cols, legend = c("phylogenetic", "random", "spatial"), title = "Blocking method", bty = "n")
    with(focal_df2, stripchart(estimate ~ blocking_model, # Data
      method = "jitter", # Random noise
      pch = 19,
      cex = 1.3,
      col = the_col, # Color of the symbol
      vertical = TRUE, # Vertical mode
      add = T
    ))
  }
}
dev.off()

most_accuracy <- all_accuracy %>% filter(blocking_method != c("random"))

par(mfrow = c(1, 1))
i <- 3
pm <- unique(all_accuracy$performance_measure)[i]
focal_df <- most_accuracy %>%
  filter(performance_measure == pm) %>%
  mutate(blocking_model = factor(blocking_model, levels = c("phylogenetic_random forest", "random_random forest", "spatial_random forest", "spatial_majority class")))
pm2 <- sub("_", " ", pm)
with(focal_df, boxplot(estimate ~ blocking_model,
  ylim = ylim_vec,
  at = c(1:3, 5), ,
  col = blocking_cols, xlab = "Model type",
  ylab = my_ylab, main = title, cex.main = 1.8
))

# tiff(rename_figure('figures/var_importance2-main.tiff'), units="in", width=20, height=12, res=1000, compression = 'lzw')



# pdf(rename_figure("figures/accuracy_estimates_main.pdf"), width = 8)
tiff(rename_figure("figures/accuracy_estimates_main.tiff"), width = 8, height =8, units = 'in', res = 1000, compression = 'lzw')
par(mfrow = c(2, 2), mar = c(4.2, 4.2, 5, 2), cex.lab = 1.5)
cex_lab2 <- 1.8
for (i in 1:n_distinct(most_accuracy$performance_measure)) {
  pm <- unique(all_accuracy$performance_measure)[c(1,2,4,3)][i]
  focal_df <- most_accuracy %>%
    filter(performance_measure == pm) %>%
    mutate(blocking_model = factor(blocking_model, levels = c("phylogenetic_random forest", "spatial_random forest", "spatial_majority class")))

  pm2 <- sub("_", " ", pm)
  title <- paste0(toupper(substr(pm2, 1, 1)), substr(pm2, 2, nchar(pm2)))
  my_ylab <- "Accuracy"
  if (pm == "AUC") {
    title <- "AUC"
    my_ylab <- "AUC"
  }
  the_col <- adjustcolor("plum4", 0.6)
  blocking_cols <- adjustcolor(RColorBrewer::brewer.pal(4, "Set3")[c(2, 4, 4)], .5)

  # next add the boxplot
  if (i %in% 1:3) {
    with(focal_df, boxplot(estimate ~ blocking_model,
      ylim = ylim_vec,
      at = c(1:2, 4),
      col = blocking_cols, xlab = "Model type",
      ylab = my_ylab, main = title, cex.main = 1.8, xaxt = "n"
    ))
    axis(side = 1, padj = -.2, at = c(1.5, 2, 4), labels = c("random forest", "", "simple phylogeny"), tick = F, cex.axis = 1.4)

    with(focal_df, stripchart(estimate ~ blocking_model, # Data
      method = "jitter", # Random noise
      at = c(1:2, 4),
      pch = 19,
      cex = 1.3,
      col = the_col, # Color of the symbol
      vertical = TRUE, # Vertical mode
      add = T
    ))
  }
  if (i == 4) {
    focal_df2 <- focal_df %>% mutate(blocking_model = as.character(blocking_model))
    with(
      focal_df2,
      boxplot(estimate ~ blocking_model,
        ylim = ylim_vec,
        col = blocking_cols, xlab = "Model type",
        ylab = my_ylab, main = title, cex.main = 1.8, xaxt = "n"
      )
    )

    axis(side = 1, padj = -.2, at = c(1.5, 2), labels = c("random forest", ""), tick = F, cex.axis = 1.4)

    legend("bottomright", fill = blocking_cols, legend = c("phylogenetic", "spatial"), title = "Blocking method", bty = "n")
    with(focal_df2, stripchart(estimate ~ blocking_model, # Data
      method = "jitter", # Random noise
      pch = 19,
      cex = 1.3,
      col = the_col, # Color of the symbol
      vertical = TRUE, # Vertical mode
      add = T
    ))
  }
}
dev.off()
