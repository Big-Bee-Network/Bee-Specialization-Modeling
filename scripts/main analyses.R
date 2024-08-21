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
library(ROCR)
library(caret)
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
globi_degree_all <- read_csv("final_data/globi_speciesLevelFinal-12Aug2024_revision2.csv")
mean(is.na(globi_degree_all$diet_breadth_conservative))*100
#30.10836




# what is the phylogenetic breakdown of the unknowns
unknowns <-globi_degree_all  %>% filter(is.na(diet_breadth_conservative))
na_count <- sum(is.na(unknowns$diet_breadth_conservative))
#389
unknowns %>% 
  group_by(bee_family) %>%
  summarize(n=n()) %>% 
  mutate(per = 100*n/sum(n)) %>%
  ungroup

# Andrenidae      59 15.2  
# Apidae         102 26.2  
# Colletidae      28  7.20 
# Halictidae      74 19.0  
# Megachilidae   123 31.6  
# Melittidae       3  0.771

# globi_degree is with the species-level data
globi_degree <- read_csv("final_data/globi_speciesLevelFinal-12Aug2024_revision2.csv") %>% 
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
n_distinct(globi_genera) #58 genera
unique(globi_genera)
   
globi_degree <- globi_degree %>% select(-phylo_dist_rm)
#write_csv(globi_degree,"final_data/globi_speciesLevelFinal-12Aug2024_revision2_genera_removed.csv" )
# final_data/globi_speciesLevelFinal-12Aug2024_revision2.csv = 1292 species, 91 genera
# final dataset used in analysis 682 species, 58 genera

# load the globi data
# this data file has bee and plant names updated, and only american bees
globi <- vroom("modeling_data/globi_allNamesUpdated_Henriquez_Piskulich.csv") %>%
  mutate(bee_genus = sub(" .*", "", scientificName)) %>%
  filter(scientificName %in% globi_degree$scientificName)

#how much of the sex data is filled out
unique(globi$sourceSexName)
# 1] "unknown"         "f"               "m"              
# [4] NA                "female"          "male"           
# [7] "unknown sex"     "male and female" "undetermined"   
# [10] "u"               "m, f"            "female queen"   
# [13] "female (worker)" "female (queen)"  "worker"         
# [16] "queen"           "femlae"          "gyne"           
# [19] "male paratype"   "female holo"  

filtered_glob_sex <- globi %>%
  filter(!sourceSexName %in% c("unknown", NA, "unknown sex", "u", "undetermined"))

count(filtered_glob_sex)/count(globi)*100
#58.26882




#####
###### how many sources
sources_table <- globi %>%
  group_by(sourceCitation) %>%
  summarize(n = n()) %>%
  rename(source = sourceCitation)
nrow(sources_table)
#40 source citations in globi

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
 #write_csv(sources_table, 'sources_table12Aug2024_revision.csv')

# broaderSource      percent
# 1 field                0.226
# 2 literature_unknown   0.121
# 3 museum               0.653

paste0(
  "our data has ", sum(globi_degree$n_globi), " records total, with ", nrow(globi_degree), " bee species, and ",
  n_distinct(globi$plant_genus), " plant genera"
)

#our data has 150,880 records total, with 682 bee species, and 1185 plant genera


# how many records of specialists
spec_globi <- globi_degree %>% filter(diet_breadth == "specialist")
gen_globi <- globi_degree %>% filter(diet_breadth == "generalist")

# 50858 were records of pollen specialist bees, from 477 species
# 100022 were records of pollen generalist bees, from 205 species
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
# write_xlsx(species_list,'modeling_data/species_list_revision.xlsx')

# let's look at correlations btw predictor variables:
# subset data to just be predictor variables - that don't include the phylo distance between plant genera
predictor_df <- globi_degree %>%
  select(
    phylo_rich, phylo_simp, eigen1_plantGenus, eigen2_plantGenus, eigen1_plantFam, eigen2_plantFam,
    med_lat, med_long, n_chesshire, area_ha, med_doy, flight_season, simpson_fam, simpson_genus
  )
cor_matrix <- cor(predictor_df)
heatmap(cor_matrix)


# Find correlations greater than 0.7 and less than 1
indices <- which(cor_matrix > 0.7 & cor_matrix < 1, arr.ind = TRUE)

# Filter out the diagonal elements (correlations of a variable with itself)
indices <- indices[indices[, 1] != indices[, 2], ]

# Get the names of the correlated variables and the correlation values
result <- data.frame(
  Row = rownames(cor_matrix)[indices[, 1]],
  Column = colnames(cor_matrix)[indices[, 2]],
  Correlation = cor_matrix[indices]
)

# Print the result
print(result)

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

#################################
#part 1
#save so can return to this step
save.image(file = "analysis_before_first_analysis_revision.RData")
###################################

## first do baseline analysis: stratified k-fold cross validation
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

globi_degree %>%
  group_by(diet_breadth) %>%
  summarize(n = n())

counts <- table(globi_degree$diet_breadth)
total <- nrow(globi_degree)
percentages <- (counts / total) * 100
print(percentages)

# generalist specialist 
# 30.05865   69.94135


# double check everything looks even, more specialists than generalists but even between
data_stratified %>%
  group_by(diet_breadth, fold) %>%
  summarize(n = n())

total <- nrow(diet_breadth)

# diet_breadth  fold     n
# <fct>        <int> <int>
# 1 generalist       1    26
# 2 generalist       2    26
# 3 generalist       3    26
# 4 generalist       4    25
# 5 generalist       5    26
# 6 generalist       6    26
# 7 generalist       7    24
# 8 generalist       8    26
# 9 specialist       1    60
# 10 specialist       2    60
# 11 specialist       3    60
# 12 specialist       4    58
# 13 specialist       5    59
# 14 specialist       6    60
# 15 specialist       7    60
# 16 specialist       8    60

# how many bee genera in the dataset?
#58 genera, 682 rows
head(data)
data %>%
  mutate(genus = sub(" .*", "", scientificName)) %>%
  summarize(n_distinct(genus))
nrow(data)
nrow(data_stratified)


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


####
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
    r_auc_test #0.9375


  
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
#0.8665354

mean(stratified_output$specialist_accuracy)
#0.9390853

mean(stratified_output$generalist_accuracy)
#0.6969712

mean(stratified_output$test_auc)
#0.8982965

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

#################################
#part 2
#save so can return to this step
save.image(file = "analysis_before_phylogenetic_blocking.RData")
###################################

######## Next
# block the data phylogenetically by family

# combine melittidae and colletidae since melittidae only has one generalist
globi_degree %>%
  group_by(diet_breadth, bee_family) %>%
  summarize(n = n())

# diet_breadth bee_family       n
# <fct>        <chr>        <int>
# 1 generalist   Andrenidae      53
# 2 generalist   Apidae          52
# 3 generalist   Colletidae      14
# 4 generalist   Halictidae      45
# 5 generalist   Megachilidae    38
# 6 generalist   Melittidae       3
# 7 specialist   Andrenidae     219
# 8 specialist   Apidae          85
# 9 specialist   Colletidae      32
# 10 specialist   Halictidae      37
# 11 specialist   Megachilidae    86
# 12 specialist   Melittidae      18

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

# bee_family   diet_breadth     n
# 1 Andrenidae   generalist      53
# 2 Andrenidae   specialist     219
# 3 Apidae       generalist      52
# 4 Apidae       specialist      85
# 5 Colletidae   generalist      14
# 6 Colletidae   specialist      32
# 7 Halictidae   generalist      45
# 8 Halictidae   specialist      37
# 9 Megachilidae generalist      38
# 10 Megachilidae specialist      86
# 11 Melittidae   generalist       3
# 12 Melittidae   specialist      18

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
nrow(all_specs)/n_genera #0.4482759
nrow(all_gens)/n_genera # 0.2068966

data_phy <- globi_degree %>%
  mutate(bee_family = ifelse(bee_family %in% c("Colletidae", "Melittidae"), "Colletidae_Melittidae", bee_family)) %>%
  select(scientificName, bee_family, diet_breadth, phylo_simp, simpson_genus, eigen1_plantGenus, eigen2_plantGenus, eigen1_plantFam, eigen2_plantFam, med_lat, med_long, n_chesshire, area_ha, med_doy, flight_season) # remove the vars we don't need

# look at sample sizes within family
data_phy %>%
  group_by(bee_family) %>%
  summarize(n = n())

# bee_family                n
# 1 Andrenidae              272
# 2 Apidae                  137
# 3 Colletidae_Melittidae    67
# 4 Halictidae               82
# 5 Megachilidae            124

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
    r_auc_train # 1
    
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

# predictor_var  generalist  specialist MeanDecreaseAccuracy MeanDecreaseGini model_iteration
# 1         phylo_simp 0.136453154 0.025192008          0.054873056        43.289571               1
# 2      simpson_genus 0.074620978 0.016106787          0.031378301        28.136291               1
# 3  eigen1_plantGenus 0.005342069 0.011155714          0.009643602         9.594873               1
# 4  eigen2_plantGenus 0.044145248 0.019370781          0.025940739        26.237268               1
# 5    eigen1_plantFam 0.013674775 0.012982311          0.013180789        12.344444               1
# 6    eigen2_plantFam 0.016887124 0.013704777          0.014622088        12.548884               1
# 7            med_lat 0.034333138 0.014229476          0.019598351        17.179332               1
# 8           med_long 0.001270148 0.005350684          0.004250147        10.918293               1
# 9        n_chesshire 0.055770185 0.018953551          0.028721787        21.673696               1
# 10           area_ha 0.056866187 0.029449714          0.036545865        27.167438               1
# 11           med_doy 0.010975740 0.009027945          0.009489134        12.129825               1
# 12     flight_season 0.019704652 0.003289327          0.007668375        13.195547               1
# 13        phylo_simp 0.096922613 0.041107461          0.061658677        32.085339               2
# 14     simpson_genus 0.065210851 0.025993954          0.040155140        25.502534               2
# 15 eigen1_plantGenus 0.007552540 0.004173066          0.005429318         7.017175               2
# 16 eigen2_plantGenus 0.028499155 0.039111961          0.035163729        22.898713               2
# 17   eigen1_plantFam 0.017378020 0.006447816          0.010519416         8.895507               2
# 18   eigen2_plantFam 0.019575712 0.003316148          0.009474760         9.189769               2
# 19           med_lat 0.023571818 0.013736216          0.017346241        13.057191               2
# 20          med_long 0.002619350 0.004009881          0.003470987         9.494514               2
# 21       n_chesshire 0.020158722 0.032698402          0.027740515        19.556433               2
# 22           area_ha 0.024815186 0.026529214          0.025797315        17.524801               2
# 23           med_doy 0.009190931 0.004816207          0.006379295         7.739540               2
# 24     flight_season 0.037401686 0.020099431          0.026484409        17.791698               2
# 25        phylo_simp 0.114389929 0.021786179          0.047270971        33.700352               3
# 26     simpson_genus 0.068303850 0.017166440          0.031494688        26.315026               3
# 27 eigen1_plantGenus 0.005188484 0.016639259          0.013478227        10.579326               3
# 28 eigen2_plantGenus 0.034173450 0.014256736          0.019801723        19.729309               3
# 29   eigen1_plantFam 0.008449909 0.010753309          0.010092326        11.178723               3
# 30   eigen2_plantFam 0.007919356 0.009350280          0.008905324        10.839655               3
# 31           med_lat 0.037902614 0.015301195          0.021615525        18.694757               3
# 32          med_long 0.017744268 0.008429282          0.011026374        14.517595               3
# 33       n_chesshire 0.054775499 0.015664017          0.026708570        23.720814               3
# 34           area_ha 0.028975514 0.027693532          0.027987147        22.122669               3
# 35           med_doy 0.023172697 0.009347039          0.013173695        13.091793               3
# 36     flight_season 0.025290650 0.007733746          0.012568652        15.535406               3
# 37        phylo_simp 0.130565763 0.033596114          0.062480468        48.275376               4
# 38     simpson_genus 0.108053053 0.027886987          0.051638100        33.858817               4
# 39 eigen1_plantGenus 0.004201640 0.013376222          0.010685752         8.707042               4
# 40 eigen2_plantGenus 0.028457674 0.019247442          0.021941085        24.805774               4
# 41   eigen1_plantFam 0.021693532 0.009567073          0.013214189         9.866421               4
# 42   eigen2_plantFam 0.025489818 0.011591998          0.015709787        12.447822               4
# 43           med_lat 0.026811687 0.015204077          0.018563341        13.927167               4
# 44          med_long 0.007943414 0.005925212          0.006481425         9.689435               4
# 45       n_chesshire 0.080105143 0.017407635          0.036142506        26.620799               4
# 46           area_ha 0.030350310 0.022461047          0.025054615        19.122767               4
# 47           med_doy 0.016831457 0.006904366          0.009851175         9.860780               4
# 48     flight_season 0.042552335 0.010636996          0.020090245        16.738575               4
# 49        phylo_simp 0.130605575 0.030515823          0.060953745        51.198403               5
# 50     simpson_genus 0.080914508 0.020412211          0.038851070        34.449446               5
# 51 eigen1_plantGenus 0.011095807 0.011068770          0.011103777        10.041148               5
# 52 eigen2_plantGenus 0.039108115 0.025302202          0.029582805        28.340369               5
# 53   eigen1_plantFam 0.015858467 0.007279215          0.009922112        10.417318               5
# 54   eigen2_plantFam 0.014686649 0.012213559          0.013016557        13.541817               5
# 55           med_lat 0.039990641 0.026747924          0.030834115        20.676680               5
# 56          med_long 0.001116220 0.011422812          0.008286646        11.600896               5
# 57       n_chesshire 0.067764793 0.023588968          0.037029905        26.258196               5
# 58           area_ha 0.039995086 0.031345736          0.033949946        23.818583               5
# 59           med_doy 0.020486045 0.006231085          0.010560754        11.845266               5
# 60     flight_season 0.036505804 0.005479538          0.014875214        17.752077               5

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

#################################
#part 3
#save so can return to this step
save.image(file = "analysis_before_before_spatial_blocking.RData")
###################################

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

# spatial_block diet_breadth n_diet     n prop_diet_breadth
# <dbl> <fct>         <int> <int>             <dbl>
# 1             1 specialist       16    38             0.421
# 2             2 specialist       45    67             0.672
# 3             3 specialist       62    90             0.689
# 4             4 specialist       63   148             0.426
# 5             5 specialist      117   133             0.880
# 6             6 specialist       87   103             0.845
# 7             7 specialist       69    80             0.862
# 8             8 specialist       18    23             0.783

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
mean(spatial_blocked_output$overall_accuracy) # 0.8436593

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
  mutate(prob_generalist = round(prob_generalist, digits = 2)) %>%
  arrange(prob_generalist)
specialist_probs <- mean_probs %>%
  filter(diet_breadth == "specialist") %>%
  select(scientificName, diet_breadth, prob_generalist) %>%
  mutate(prob_specialist = 1-prob_generalist) %>%
  mutate(prob_specialist = round(prob_specialist, digits = 2)) %>%
  arrange(prob_specialist)  %>% select(-prob_generalist)

#write_csv(specialist_probs, "figures/specialist_probabilities_revision.csv")
#write_csv(generalist_probs, "figures/generalist_probabilities_revision.csv")

#write.table(specialist_probs, file = 'figures/specialist_probabilities_revision.tsv', sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)

#write.table(generalist_probs, file = 'figures/generalist_probabilities_revision.tsv', sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)


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
#15.25613
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

# balanced_accuracy   0.761
# 2 f1_gens             0.674
# 3 f1_specs            0.877
# 4 generalist_accuracy 0.606
# 5 overall_accuracy    0.840
# 6 specialist_accuracy 0.916
# 7 test_auc            0.834

accuracy_sum %>% filter(blocking_method == "phylogenetic") %>%
  pivot_longer(cols= !'blocking_method', values_to = 'estimate') %>%
  group_by(name) %>% summarize(mean=mean(estimate))

# name                 mean
# 1 balanced_accuracy   0.783
# 2 f1_gens             0.708
# 3 f1_specs            0.875
# 4 generalist_accuracy 0.652
# 5 overall_accuracy    0.835
# 6 specialist_accuracy 0.914
# 7 test_auc            0.824

accuracy_sum %>% filter(blocking_method == "spatial") %>%
  pivot_longer(cols= !'blocking_method', values_to = 'estimate') %>%
  group_by(name) %>% summarize(mean=mean(estimate))

# name                 mean
# 1 balanced_accuracy   0.747
# 2 f1_gens             0.648
# 3 f1_specs            0.881
# 4 generalist_accuracy 0.575
# 5 overall_accuracy    0.847
# 6 specialist_accuracy 0.918
# 7 test_auc            0.837

accuracy_sum %>%
  dplyr::select(blocking_method, everything()) %>%
  pivot_longer(cols = !blocking_method, names_to = "performance_measure", values_to = "estimate") %>%
  group_by(blocking_method, performance_measure) %>%
  summarize(mean = mean(estimate), min = min(estimate), max = max(estimate)) %>%
  print(n = 21) 

# blocking_method performance_measure  mean   min   max
# 1 phylogenetic    balanced_accuracy   0.785 0.715 0.898
# 2 phylogenetic    f1_gens             0.711 0.6   0.884
# 3 phylogenetic    f1_specs            0.876 0.805 0.939
# 4 phylogenetic    generalist_accuracy 0.656 0.529 0.808
# 5 phylogenetic    overall_accuracy    0.836 0.758 0.920
# 6 phylogenetic    specialist_accuracy 0.914 0.826 0.988
# 7 phylogenetic    test_auc            0.821 0.653 0.971
# 8 random          balanced_accuracy   0.818 0.785 0.849
# 9 random          f1_gens             0.759 0.708 0.809
# 10 random          f1_specs            0.908 0.872 0.928
# 11 random          generalist_accuracy 0.697 0.625 0.731
# 12 random          overall_accuracy    0.867 0.824 0.895
# 13 random          specialist_accuracy 0.939 0.864 0.967
# 14 random          test_auc            0.898 0.857 0.951
# 15 spatial         balanced_accuracy   0.747 0.625 0.829
# 16 spatial         f1_gens             0.648 0.4   0.840
# 17 spatial         f1_specs            0.881 0.8   0.956
# 18 spatial         generalist_accuracy 0.575 0.25  0.8
# 19 spatial         overall_accuracy    0.847 0.783 0.922
# 20 spatial         specialist_accuracy 0.918 0.855 1
# 21 spatial         test_auc            0.837 0.722 0.940

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
  mutate(mean_importance = round(mean_importance,digits = 2))  %>%
  rename("Predictor variable" = predictor_factor, "Mean importance" = mean_importance)


write_xlsx(all_var_table,'figures/Importance_predictors-12Aug24_revision.xlsx')
write.table(all_var_table, file = 'figures/Importance_predictors-12Aug24_revision.tsv', sep = "\t", row.names = FALSE, col.names = TRUE, quote = FALSE)


# format figure for the paper
var_top10 <- rename2 %>%
  left_join(var_importance_all )

# var_importance_all %>% left_join(rename)

my_col <- adjustcolor("skyblue4", .6)



# first add the data points
cex_axis=1.3
tiff(rename_figure('figures/var_importance2-main.tiff'), units="in", width=20, height=12, res=1000, compression = 'lzw')
pdf(rename_figure("figures/var_importance2-main_revision.pdf"), width = 18)
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

# phylo_simp prob_specialist_phylo simpson_genus prob_specialist_simp model_iteration
# 1   390.7025             0.7344800      1.000000            0.7592233               1
# 2   394.9360             0.8104767      2.202129            0.7754433               1
# 3   399.1694             0.8128933      3.404259            0.7585667               1
# 4   403.4028             0.8174700      4.606388            0.7475933               1
# 5   407.6363             0.8175100      5.808518            0.7390533               1
# 6   411.8697             0.8158000      7.010647            0.7391500               1

# prob for species visiting max phylo diversity of plants
(max_phylo_pred <- partial_df[which.max(partial_df$phylo_simp), ]$prob_specialist_phylo)
#0.5584567

# prob for species visiting min phylo diversity of plants
(min_phylo_pred <- partial_df[which.min(partial_df$phylo_simp), ]$prob_specialist_phylo)
# 0.73448

# percent increase
(min_phylo_pred - max_phylo_pred) / max_phylo_pred
#0.315196

##
# prob for species visiting max simpson diversity of plants
(max_simp_pred <- partial_df[which.max(partial_df$simpson_genus), ]$prob_specialist_simp) 
#0.5663756

# prob for species visiting min simpson diversity of plants
(min_simp_pred <- partial_df[which.min(partial_df$simpson_genus), ]$prob_specialist_simp) 
#0.7592233

# percent increase
(min_simp_pred - max_simp_pred) / max_simp_pred
#0.3404944


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

tiff('figures/var_importance_revision.tiff', units="in", width=6, height=6, res=500, compression = 'lzw')
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

dev.off()


#tiff('figures/important_vars.tiff', units="in", width=7, height=14, res=1000, compression = 'lzw')
pdf(rename_figure("figures/important_vars-main.pdf"), width = 7, height = 10)
grid.arrange(phylo_simp_graph, simpson_genus_graph)


#partialPlot(my_rf, pred.data = df_train, x.var = "phylo_simp")
#partial_data = partial(my_rf, pred.var = "phylo_simp")  %>% mutate(model_iteration=i)


# how does the spatial model perform without any phylogenetic predictors
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
spatial_blocked_output_np


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

#without phylogenetic predictors
# performance_type       without with phylo
# 1 balanced_accuracy   0.752  0.747
# 2 f1_gens             0.649  0.648
# 3 f1_specs            0.877  0.881
# 4 generalist_accuracy 0.582  0.575
# 5 overall_accuracy    0.841  0.847
# 6 specialist_accuracy 0.922  0.918
# 7 test_auc            0.803  0.837
# 8 train_auc           1     NA   


#only phylogenetically blocked
# name                 mean
# 1 balanced_accuracy   0.783
# 2 f1_gens             0.708
# 3 f1_specs            0.875
# 4 generalist_accuracy 0.652
# 5 overall_accuracy    0.835
# 6 specialist_accuracy 0.914
# 7 test_auc            0.824


## let's see how our random forest models compare to something simpler
(all_data_props <- data_space %>%
  mutate(bee_genus = sub(" .*", "", scientificName)) %>%
  group_by(bee_genus) %>%
  summarize(prop_specialists = mean(diet_breadth == "specialist")))

all_data_props %>% arrange(prop_specialists)
all_data_props %>% arrange(desc(prop_specialists))
mean(all_data_props$prop_specialists == 1)#0.4482759
mean(all_data_props$prop_specialists == 0)#0.2068966

#what are the proportions of specialists in each family?
data_space %>%
  group_by(bee_family) %>%
  summarize(prop_specialists = mean(diet_breadth == "specialist"))

# bee_family   prop_specialists
# 1 Andrenidae              0.805
# 2 Apidae                  0.620
# 3 Colletidae              0.696
# 4 Halictidae              0.451
# 5 Megachilidae            0.694
# 6 Melittidae              0.857

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

#majority class predictions, our niave approach for comparison.
# testing_block    overall_accuracy specialist_accuracy generalist_accuracy 
# 4.5000000           0.7875089           0.9348012           0.3842401 
# balanced_accuracy 
# 0.6595206 
mean(majority_class_predictions$overall_accuracy)
#0.7875089


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

# blocking_model             performance_measure  mean    min   max
# 1 phylogenetic_random forest AUC                 0.821 0.653  0.971
# 2 phylogenetic_random forest balanced_accuracy   0.785 0.715  0.898
# 3 phylogenetic_random forest generalist_accuracy 0.656 0.529  0.808
# 4 phylogenetic_random forest specialist_accuracy 0.914 0.826  0.988
# 5 random_random forest       AUC                 0.898 0.857  0.951
# 6 random_random forest       balanced_accuracy   0.818 0.785  0.849
# 7 random_random forest       generalist_accuracy 0.697 0.625  0.731
# 8 random_random forest       specialist_accuracy 0.939 0.864  0.967
# 9 spatial_majority class     balanced_accuracy   0.660 0.545  0.778
# 10 spatial_majority class     generalist_accuracy 0.384 0.0909 0.682
# 11 spatial_majority class     specialist_accuracy 0.935 0.875  1    
# 12 spatial_random forest      AUC                 0.837 0.722  0.940
# 13 spatial_random forest      balanced_accuracy   0.747 0.625  0.829
# 14 spatial_random forest      generalist_accuracy 0.575 0.25   0.8  
# 15 spatial_random forest      specialist_accuracy 0.918 0.855  1    
 
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

#pdf(rename_figure("figures/accuracy_estimates_supp.pdf"), width = 9)
#tiff(rename_figure("figures/accuracy_estimates_supp.tiff"), width = 8, height =8, units = 'in', res = 1000, compression = 'lzw')

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
#dev.off()

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

#tiff(rename_figure('figures/var_importance2-main.tiff'), units="in", width=20, height=12, res=1000, compression = 'lzw')



# pdf(rename_figure("figures/accuracy_estimates_main.pdf"), width = 8)
png(rename_figure("figures/accuracy_estimates_main.tiff"), width = 8, height =8, units = 'in', res = 1000)
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


