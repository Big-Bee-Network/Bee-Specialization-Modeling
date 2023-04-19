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


# load the globi data
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
fowler_formatted = read_csv('modeling_data/fowler_formatted-7march2023.csv')
diet_breadth = read_csv('modeling_data/bee_diet_breadth-7march2023.csv')


specialists = diet_breadth[diet_breadth$diet_breadth=='specialist',]$scientificName
globi_u=globi %>% left_join(diet_breadth %>% distinct(scientificName,diet_breadth)) %>%
  mutate(diet_breadth = ifelse(is.na(diet_breadth),'generalist',diet_breadth))

#double check no increase in the number of rows
nrow(globi) ==nrow(globi_u)

#how many records total in our data
paste0('our data has ', nrow(globi_u),' records total, with ', n_distinct(globi_u$scientificName),' bee species, and ',
       n_distinct(globi_u$plant_genus), ' plant genera')


#get # of records of pollen specialists and generalists
sum(globi_degree$n_globi); n_distinct(globi_u$scientificName); nrow(globi_degree)
with(globi_degree %>% filter(diet_breadth=="specialist"),
     paste("there are", sum(n_globi), "records of specialist bees from", n_distinct(scientificName), "species"))
with(globi_degree %>% filter(diet_breadth=="generalist"),
     paste("there are", sum(n_globi), "records of generalist bees from", n_distinct(scientificName), "species"))

#some exploratory plotting
with(globi_degree,boxplot(phylo_simp~diet_breadth))
with(globi_degree,boxplot(phylo_rich~diet_breadth))


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


# save one fold for testing and train the model on the rest of the data
# get distribution of accuracy, importance values etc
i=1
1:k_folds
i=3



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
top_predictors_phy = var_importance_phy %>% 
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
  select(-n_globi,-bee_family)

block=1
#plot to double check everything:
with(data_space2,plot(med_long,med_lat,col=spatial_block))


#now run the cross validation
spatial_blocked_output_ls = 1:length(data_space2$spatial_block) %>%
  purrr::map(function(block){
    
    #divide the training and testing data
    df_train = data_space2 %>% filter(spatial_block != block) %>% select(-spatial_block,-med_lat, -med_long)
    df_test = data_space2 %>% filter(spatial_block == block)  %>% select(-spatial_block,-med_lat, -med_long)
    
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
      block_left_out = block,
      overall_accuracy = mean(df_test$prediction_correct),
      specialist_accuracy = mean(df_test[df_test$diet_breadth=="specialist",]$prediction_correct),
      generalist_accuracy = mean(df_test[df_test$diet_breadth=="generalist",]$prediction_correct), 
      test_auc = r_auc_test,
      train_auc = r_auc_train,
      specialists_wrong = list(specialists_wrong),           
      generalists_wrong = list(generalists_wrong)

      
    ))
    
    
    
  })
spatial_blocked_output = spatial_blocked_output_ls %>% map_dfr(function(a_list) a_list[[2]])
with(spatial_blocked_output %>% select(-block_left_out,-specialists_wrong,-generalists_wrong) %>%
       pivot_longer(everything(), names_to = 'performance_type',values_to = 'estimates'),
     boxplot(estimates~performance_type))


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


##make partial dependence plots for the predictors
i=1
partial_ls = 1:length(spatial_blocked_output_ls) %>% purrr::map(function(i){
  my_rf = spatial_blocked_output_ls[[i]][[1]]
  df_train = data_space2 %>% filter(spatial_block != i) %>% select(-spatial_block,-med_lat, -med_long,-scientificName)
  
  partial_data1 = partial(my_rf, pred.var = c("phylo_simp"),which.class = 'specialist',prob=T,data=df_train)  %>%
    rename(prob_specialist_phylo = yhat)
  partial_data2 = partial(my_rf, pred.var = c("simpson_genus"),which.class = 'specialist',prob=T) %>%
    rename(prob_specialist_simp = yhat)
  partial_data3 = partial(my_rf, pred.var = c("Perdita"),which.class = 'specialist',prob=T) %>%
    rename(prob_specialist_perdita = yhat)
  
  output_data = partial_data1 %>% bind_cols(partial_data2) %>% bind_cols(partial_data3) %>%
    mutate(model_iteration=i)
  return(output_data)
  })
partial_df = partial_ls %>% bind_rows


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


##finally make graph for 3rd most important variable - perdita
break_n=40
perd_min=min(data_space2$Perdita); perd_max=max(data_space2$Perdita)
my_breaks_perd=seq(perd_min,perd_max,by=(perd_max-perd_min)/break_n) 

densities_df_perd=data_space2 %>% split(.$category_binary) %>% map_dfr(function(df){
  dens=hist(df$Perdita,plot=F,breaks=my_breaks_perd)
  percent_dens=  dens$density/sum(dens$density)
  
  
  data.frame(Perdita=dens$mid,percent_dens=percent_dens) %>% 
    mutate(category_binary=df$category_binary[1])
  
}) 

hist_df_perd=densities_df_perd %>% mutate(pct=ifelse(category_binary,1-percent_dens,percent_dens))



(perdita_graph = ggplot() +
    geom_segment(data=hist_df_perd[hist_df_perd$category_binary==1,], size=4, show.legend=FALSE,colour=my_cols2[1],
                 aes(x=Perdita, xend=Perdita, y=category_binary, yend=pct)) +
    geom_segment(data=hist_df_perd[hist_df_perd$category_binary==0,], size=4, show.legend=FALSE,colour=my_cols2[2],
                 aes(x=Perdita, xend=Perdita, y=category_binary, yend=pct))+
    geom_segment(dat=data_space2[data_space2$category_binary==0,], aes(x=Perdita, xend=Perdita, y=0, yend=-0.02), size=0.2, colour="grey30") +
    geom_segment(dat=data_space2[data_space2$category_binary==1,], aes(x=Perdita, xend=Perdita, y=1, yend=1.02), size=0.2, colour="grey30") +
    geom_line(data=partial_ls[[1]],aes(x=Perdita,y=prob_specialist_perdita),color=my_cols[partial_ls[[1]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[2]],aes(x=Perdita,y=prob_specialist_perdita),color=my_cols[partial_ls[[2]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[3]],aes(x=Perdita,y=prob_specialist_perdita),color=my_cols[partial_ls[[3]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[4]],aes(x=Perdita,y=prob_specialist_perdita),color=my_cols[partial_ls[[4]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[5]],aes(x=Perdita,y=prob_specialist_perdita),color=my_cols[partial_ls[[5]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[6]],aes(x=Perdita,y=prob_specialist_perdita),color=my_cols[partial_ls[[6]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[7]],aes(x=Perdita,y=prob_specialist_perdita),color=my_cols[partial_ls[[7]]$model_iteration[1]],lwd=1) +
    geom_line(data=partial_ls[[8]],aes(x=Perdita,y=prob_specialist_perdita),color=my_cols[partial_ls[[8]]$model_iteration[1]],lwd=1) +
    theme_bw(base_size=12)+theme(
      # Hide panel borders and remove grid lines
      panel.border = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # Change axis line
      axis.line = element_line(colour = "black"),
      text = element_text(size=20)
    )+
    labs(x='Phylogenetic distance to the \nbee genus Perdita',y='Probability of being a \nspecialist bee'))


#dev.of()

# tiff('figures/important_vars.tiff', units="in", width=7, height=14, res=1000, compression = 'lzw')
pdf('figures/important_vars.pdf', width=7, height=14)
grid.arrange(phylo_simp_graph, simpson_genus_graph, perdita_graph)
dev.off()

par(mfrow=c(1,3),cex.lab=2)
for(i in 1:length(partial_ls)){
  df=partial_ls[[i]]
  if(i==1)   with(df,plot(phylo_simp,prob_specialist_phylo, type='l',col=my_cols[i],ylim=c(0,1),
                          xlab = 'Phylogenetic simpson diversity of plants visited',
                          ylab='Probability of being a specialist'))
  if(i != 1) with(df,lines(phylo_simp,prob_specialist_phylo, type='l',col=my_cols[i]))
  
  #add data points
  
}
diet_breadth_numeric = ifelse(data_space2$diet_breadth=='specialist',1,0)

with(data_space2,points(phylo_simp,diet_breadth_numeric))

for(i in 1:length(partial_ls)){
  df=partial_ls[[i]]
  if(i==1)   {with(df,plot(simpson_genus,prob_specialist_simp, type='l',col=my_cols[i],ylim=c(0,1),
                          xlab = 'Simpson diversity of plant genera visited',
                          ylab='Probability of being a specialist'))}
  if(i != 1) with(df,lines(simpson_genus,prob_specialist_simp, type='l',col=my_cols[i]))

  
}

for(i in 1:length(partial_ls)){
  df=partial_ls[[i]]
  if(i==1)   {with(df,plot(Perdita,prob_specialist_perdita, type='l',col=my_cols[i],ylim=c(0,1),
                           xlab = 'Phylogenetic distance to the bee genus Perdita',
                           ylab='Probability of being a specialist'))}
  if(i != 1) with(df,lines(Perdita,prob_specialist_perdita, type='l',col=my_cols[i]))

  
}


partialPlot(my_rf, pred.data = df_train, x.var = "phylo_simp")
partial_data = partial(my_rf, pred.var = "phylo_simp")  %>% mutate(model_iteration=i)


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




##old
##

specialist_predictions = data_stratified %>%
  filter(diet_breadth=='specialist' & scientificName %in% specialists_wrong_vec==F)  %>%
  select(scientificName,diet_breadth) %>% mutate(model_prediction = 'specialist') %>%
  bind_rows(data.frame(
    scientificName = specialists_wrong_vec,
    diet_breadth = 'specialist',
    model_prediction = 'generalist'
  )) %>%
  arrange(model_prediction,scientificName)
generalist_predictions = data_stratified %>%
  filter(diet_breadth=='generalist' & scientificName %in% generalists_wrong_vec==F)  %>%
  select(scientificName,diet_breadth) %>% mutate(model_prediction = 'generalist') %>%
  bind_rows(data.frame(
    scientificName = generalists_wrong_vec,
    diet_breadth='generalist',
    model_prediction = 'specialists'
  )) %>%
  arrange(model_prediction,scientificName)

predictions = bind_rows(specialist_predictions,generalist_predictions) %>%
  mutate(genus=sub(" .*","",scientificName)) 
(ah_predictions= predictions%>%
  filter(genus %in% c('Habropoda','Anthophora')))

mean(specialist_predictions$model_prediction=='specialist')
# write_xlsx(ah_predictions,"modeling_data/predictions-AnthophoraHabropoda.xlsx")
andrena = specialist_predictions %>% mutate(genus = gsub(' .*',"",scientificName)) %>%
  filter(genus=="Andrena")
mean(andrena$model_prediction=='specialist')

perdita = specialist_predictions %>% mutate(genus = gsub(' .*',"",scientificName)) %>%
  filter(genus=="Perdita")
mean(perdita$model_prediction=='specialist')

mean(specialist_predictions$model_prediction=='specialist')

# write_xlsx(specialist_predictions,
#            "modeling_data/specialist_predictions.xlsx")

stratified_output$specialists_wrong








##old  
  
globi_degree$region = ifelse(globi_degree$med_long > dividing_long,'east','west')
globi_degree %>%filter(scientificName =='Andrena erigeniae') %>% select(region)
globi_degree %>%filter(scientificName =='Diadasia diminuta') %>% select(region)

##remove columns from the dataset we don't want to model
degree_smaller = globi_degree %>% select(-c('spherical_geometry','n_globi','med_long','bee_genus','bee_family','mean_doy','diet_breadth_detailed', 
                                            'rich_genus','simpson_genus','rich_fam','simpson_fam','area_m2',"quant10","quant90"))

#divide the dataset into training data and testing data
training_globi = degree_smaller %>% filter(region=='west')
testing_globi = degree_smaller %>% filter(region=='east')

##make vectors of eastern and western bees
eastern_bees = globi_degree[globi_degree$region=='east',]
western_bees = globi_degree[globi_degree$region=='west',]
n_distinct(eastern_bees)
n_distinct(western_bees)

#
rf_sp = randomForest(as.factor(diet_breadth) ~ .,data= training_globi %>% select(-c(scientificName,region)),importance = T,na.action=na.omit,auc=T)
pred_east <- predict(rf_sp, testing_globi %>% dplyr::select(-c(scientificName,diet_breadth,region)))
# pdf('figures/test_geo.pdf',width=12)
varImpPlot(rf_sp)
# dev.off()

testing_globi$prediction <- pred_east
testing_globi$prediction_correct <- testing_globi$diet_breadth==testing_globi$prediction

#overall accuracy
mean(testing_globi$prediction_correct)
mean(testing_globi[testing_globi$diet_breadth=="specialist",]$prediction_correct)
mean(testing_globi[testing_globi$diet_breadth=="generalist",]$prediction_correct)



testing_globi %>% select(diet_breadth,prediction,prediction_correct) 

#i'm curious what specialist bees are predicted to be generalists
testing_globi %>% 
  filter(diet_breadth=='specialist' & !prediction_correct) %>%
  distinct(scientificName)

#####################################
##let's also try dividing the data phylogenetically
##remove columns from the dataset we don't want to model
degree_smaller2 = globi_degree %>% 
  select(scientificName,diet_breadth,bee_family, phylo_simp, flight_season, 
         phylo_rich, med_doy,n_chesshire, area_ha, med_long,med_lat,eigen1_plantGenus,eigen2_plantGenus,eigen1_plantFam, eigen2_plantFam) %>%
  filter(!is.na(med_doy))

training_phy = degree_smaller2 %>% filter(bee_family %in% c("Andrenidae","Melittidae","Halictidae","Colletidae"))
testing_phy = degree_smaller2 %>% filter(bee_family %in% c("Apidae","Megachilidae"))
nrow(training_phy)
nrow(testing_phy)

rf_phy = randomForest(as.factor(diet_breadth) ~ .,data= training_phy %>% select(-c(scientificName,bee_family)),importance = T,na.action=na.omit,auc=T)
pred_group2 <- predict(rf_phy,testing_phy %>% select(-c(scientificName,bee_family)))

# pdf('figures/test_phylo.pdf',width=12)
varImpPlot(rf_phy)
# dev.off()


##assess prediction accuracy
testing_phy$prediction <- pred_group2
testing_phy$prediction_correct <- testing_phy$diet_breadth==testing_phy$prediction
testing_phy %>% select(diet_breadth,prediction,prediction_correct) 
testing_phy %>% filter(is.na(prediction_correct)) %>% select(diet_breadth,prediction,prediction_correct) 


#overall accuracy
mean(testing_phy$prediction_correct)
mean(testing_phy[testing_phy$diet_breadth=="specialist",]$prediction_correct)
mean(testing_phy[testing_phy$diet_breadth=="generalist",]$prediction_correct)

#which specialist bees are predicted to be generalists?
data.frame(testing_phy %>% 
  filter(diet_breadth=='specialist' & !prediction_correct) %>%
  distinct(scientificName))


#####
#divide the dataset into training data and testing data
degree_smaller2 = globi_degree %>% select(-c('spherical_geometry','n_globi','bee_genus','bee_family','mean_doy','diet_breadth_detailed', 
                                            'rich_genus','simpson_genus','rich_fam','simpson_fam','area_m2',"quant10","quant90"))


half_size = floor(nrow(degree_smaller2)/2)

training_ind = sample(1:nrow(degree_smaller2),half_size,1)

training = degree_smaller2[training_ind,]
testing = degree_smaller2[-training_ind,]


#
rf = randomForest(as.factor(diet_breadth) ~ .,data= training %>% select(-c(scientificName,region)),importance = T,na.action=na.omit,auc=T)
pred <- predict(rf, testing %>% dplyr::select(-c(scientificName,diet_breadth,region)))
# pdf('figures/test_geo.pdf',width=12)
varImpPlot(rf)
# dev.off()

testing$prediction <- pred
testing$prediction_correct <- testing$diet_breadth==testing$prediction

#overall accuracy
mean(testing$prediction_correct)
mean(testing[testing$diet_breadth=="specialist",]$prediction_correct)
mean(testing[testing$diet_breadth=="generalist",]$prediction_correct)


#########################
# Katja is curious what variables come out as important if we just throw in all the globi data
# let's try that:
eastern_bees = globi_degree[globi_degree$region=='east',]

str(globi_u)
#run the random forest model
ind_test = globi_u %>% select(diet_breadth,"sourceInstitutionCode","sourceBasisOfRecordName",
                              'interactionTypeName',"targetTaxonOrderName","targetTaxonClassName",
                              "decimalLatitude","decimalLongitude","eventDate",
                              "bee_family","bee_genus","plant_genus","plant_species","plant_family")
#how many na's for each column
colMeans(is.na(ind_test))


rf_ind = randomForest(as.factor(diet_breadth) ~ .,data= ind_test,importance = T,na.action=na.omit)
rf_ind$importance

# pdf('figures/var_importance_rf.pdf',width =13)
varImpPlot(rf_ind)
# dev.off()




##old
# par(mfrow=c(1,2))
# rf_roc150 = roc(as.factor(globi_degree$diet_breadth),rf_150$votes[,2])
# plot(rf_roc,main = '20 obs needed')
# plot(rf_roc150,main='150 obs needed')
# auc(rf_roc)
# auc(rf_roc150)

#make boxplots for the ms
point_col = adjustcolor('cadetblue',.5)
box_col = adjustcolor('white',0)
loc_gen=1.2; loc_spec=1.8
cex_lab = 1.5
cex_axis=1.3

# pdf('figures/boxplots_spec_gen_visitation.pdf',width=12)
par(mfrow=c(1,2))
with(globi_degree125,stripchart(simpson_fam~diet_breadth,method = 'jitter',
                             vertical=T,col=point_col,at=c(loc_gen,loc_spec),pch=16,
                             cex.lab = cex_lab, cex.axis=cex_axis,
                             xlab = 'bee diet breadth',ylab = 'simpson diversity of plant families visited'))
with(globi_degree125,boxplot(simpson_fam~diet_breadth,col=box_col,add=T,at=c(loc_gen,loc_spec), axes = F,boxwex=c(.35,.35)))


with(globi_degree125,stripchart(simpson_genus~diet_breadth,method = 'jitter',
                             vertical=T,col=point_col,at=c(loc_gen,loc_spec),pch=16,
                             cex.lab = cex_lab, cex.axis = cex_axis,
                             xlab = 'bee diet breadth',ylab = 'simpson diversity of plant genera visited'))
with(globi_degree125,boxplot(simpson_genus~diet_breadth,col=box_col,add=T,at=c(loc_gen,loc_spec),axes=F,boxwex=c(.35,.35)))
# dev.off()

#mean simpson diversity of plants visited by specialists & gens
(mean_simpsons = globi_degree125 %>% 
    group_by(diet_breadth) %>%
    summarize(simp_fam = mean(simpson_fam),
              simp_genus = mean(simpson_genus)))


(mean_simpsons$simp_fam[1]-mean_simpsons$simp_fam[2])/mean_simpsons$simp_fam[2]*100
(mean_simpsons$simp_genus[1]-mean_simpsons$simp_genus[2])/mean_simpsons$simp_genus[2]*100


# write_csv(globi_degree,'modeling_data/globi_degree.csv')

globi_degree125 %>% 
  filter(diet_breadth=='specialist') %>%
  filter(simpson_fam>6) %>% 
  distinct(scientificName)


#run the random forest model
set.seed(20)
rf_all = randomForest(as.factor(diet_breadth) ~ degree_family + degree_genus + simpson_fam + simpson_genus + bee_family + n ,
                      data = globi_degree125,importance = T,nperm=1)
species_probs = data.frame(predict(rf_all,type='prob'))

#any specialists on our list that have low probability of being specialists?
head(globi_degree125)
prob_output=globi_degree125 %>% bind_cols(species_probs)
with(prob_output,boxplot(generalist~diet_breadth))

data.frame(prob_output %>% 
             filter(diet_breadth=='specialist'&specialist<.5) %>%
             select(scientificName,simpson_fam,specialist))

#let's see if species with <0.5 prob of being specialists have higher simpson div of plant fams visited
with(prob_output %>%
  filter(diet_breadth =='specialist') %>%
  mutate(low_prob = specialist<0.5),boxplot(low_prob,simpson_fam))
with(prob_output %>% filter(diet_breadth=='specialist'),
     plot(specialist,simpson_fam))
head(globi_degree125)
# View(globi_degree125)

rf_w_citations = randomForest(as.factor(diet_breadth) ~ degree_family + degree_genus + simpson_fam + simpson_genus + bee_family + n + simpson_citation_genus+simpson_citation_fam,data = globi_degree125,importance = T)

rf_all
importance(rf_all)
rf_all$confusion
globi_degree %>% filter(scientificName == 'Andrena erigeneae')

#make confusion matrix into a heat  map
(cf_mat = rf_all$confusion[,1:2])
(cf_mat_prop = cf_mat/rowSums(cf_mat))
cf_df = data.frame(cf_mat_prop) 
cf_df$true_db = row.names(cf_df)
row.names(cf_df) <- NULL

#hm <- hm %>% gather(x, value, a:j)
(hm <- cf_df %>% 
    select(true_db, everything()) %>%
  gather(predicted, prop, 2:3))#

cm = ggplot(hm, aes(x=true_db, y=predicted, fill=prop)) +
  geom_tile() + theme_bw() + coord_equal() +
  scale_fill_distiller(palette="Greens", direction=1)+
  guides(fill=F) + # removing legend for `fill`
  labs(title = "Confusion matrix") + # using a title instead
  geom_text(aes(label=round(prop,2)), color="black")+
  theme(axis.text.x = element_text(size=13),
          axis.text.y = element_text(size=13),
        axis.title=element_text(size=14))+
  scale_x_discrete(name = "true diet breadth")+
  scale_y_discrete(name = "predicted diet breadth")
  


# pdf('figures/var_importance_rf.pdf',width =13)
varImpPlot(rf_all)
# dev.off()

imp = data.frame(rf_all$importance)
imp = data.frame(importance(rf_all)) 
imp$predictor = rownames(imp)
imp$labels = c('plant families visited',"plant genera visited","plant families visited \n(simpson)",
               "plant genera visited \n(simpson)","bee family","sample size")
row.names(imp) <- NULL
(imp_long = imp %>% select(predictor,labels,everything()) %>%
  pivot_longer(cols = c(generalist,specialist),names_to = c("diet_breadth"),values_to = "importance") %>%
  arrange(diet_breadth,importance))
imp_long %>% filter(diet_breadth=='generalist')

#plot the data so that the variables are in order of increasing importance for predicting specialists
spec_importance = imp_long %>% filter(diet_breadth=='specialist')
labels_factor = factor(spec_importance$labels)
labels_factor = ordered(labels_factor,levels = spec_importance$labels)
reorder_df = data.frame(labels = spec_importance$labels,labels_factor = labels_factor[labels_factor])
imp_long_ordered = imp_long %>% left_join(reorder_df,by='labels')

(imp_plot = ggplot(imp_long_ordered,aes(y = importance,x=labels_factor,fill=factor(diet_breadth)))+
  geom_dotplot(binaxis = "y", stackgroups = TRUE,binpositions = 'all')+ 
  theme_classic()+
  theme(axis.text.x = element_text(angle = 45,vjust=0.9,hjust=.9,size=11),
        axis.title=element_text(size=14),legend.text=element_text(size=11))+
  #theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))+
  #scale_y_discrete(name = "Variable importance")#+
  scale_x_discrete(name = "predictor variable")+
  scale_y_continuous(name='importance')+  
  guides(fill=guide_legend(title="diet breadth")))
  

# pdf('figures/rf_graphs.pdf',width=12)
grid.arrange(cm,imp_plot,ncol=2)
# dev.off()

library(pROC) 
rf_roc = roc(as.factor(globi_degree125$diet_breadth),rf_all$votes[,2])
# pdf('figures/roc_curve.pdf')
plot(rf_roc)
# dev.off()
auc(rf_roc)

rf_roc_cits = roc(as.factor(globi_degree125$diet_breadth),rf_w_citations$votes[,2],levels=c('specialist','generalist'))
rf_roc = roc(as.factor(globi_degree125$diet_breadth),rf_all$votes[,2],levels=c('specialist','generalist'))

auc(rf_roc)
auc(rf_roc_cits)
rf_all$confusion
rf_w_citations$confusion

# pdf('figures/roc_curves.pdf')
par(mfrow=c(1,1))
plot(rf_roc)
plot(rf_roc_cits,col=adjustcolor('deepskyblue3',.8),add=T)
legend("bottomright",c('citations excluded','citations included'),
       lty=1, box.lty=0,cex = 1,
       col=c('black',adjustcolor('deepskyblue3',.8)))
# dev.off()

#plot difference in sample size between specialsits and generalists
with(globi_degree125,boxplot(log(n)~diet_breadth))
#difference in means? medians?
mean(globi_degree125[globi_degree125$diet_breadth=='generalist',]$n);median(globi_degree125[globi_degree125$diet_breadth=='generalist',]$n)

mean(globi_degree125[globi_degree125$diet_breadth=='specialist',]$n);median(globi_degree125[globi_degree125$diet_breadth=='specialist',]$n)


#incidence of specialists by bee family
globi_degree125 %>% group_by(bee_family) %>% 
  summarize(prop_specialists = mean(diet_breadth=="specialist"),
            prop_generalists = mean(diet_breadth == 'generalist')) %>%
  arrange(prop_specialists)
n_specs = sum(globi_degree125$diet_breadth=='specialist')
globi_degree125 %>%
  filter(diet_breadth=='specialist') %>%
  group_by(bee_family) %>% summarize(n=n(),prop=n()/nrow(.))

#what threshold of observations do we need?
set.seed(4435)
what_n = seq(0,300,10) %>% purrr::map(function(threshold_n){
  
  # only include bees in the data with at least threshold_n observations
  bigN_bees=globi_u %>% group_by(scientificName) %>% summarize(n=n()) %>% filter(n>=threshold_n)
  globi_summ = globi_u%>% filter(scientificName %in% bigN_bees$scientificName) %>%
    group_by(scientificName,bee_family,plant_genus,plant_family) %>% 
    summarize(n_genus=n(),n_citations=n_distinct(sourceCitation)) 
  
  # predict whether a bee is a specialist or a generalist
  # calculate the number of plant families each bee species is observed visiting
  total_obs = bigN_bees
  
  ##calculate family-level degree
  degree_by_fam = globi_summ %>% 
    group_by(scientificName,plant_family) %>%
    summarize(n_family = sum(n_genus),n_citations=sum(n_citations)) %>%
    summarize(degree_family = n_distinct(plant_family), 
              degree5_family = sum(n_family >= 5), 
              simpson_fam = diversity(n_family,index='invsimpson'),
              simpson_citation_fam =  diversity(n_citations,index='invsimpson'))# %>%
  
  #calculate the number of plant genera each bee species is observed visiting
  degree_by_genus = globi_summ %>%
    group_by(scientificName,bee_family) %>%
    summarize(degree_genus = n_distinct(plant_genus), 
              degree5_genus = sum(n_genus >= 5), 
              simpson_genus = diversity(n_genus,index='invsimpson'),
              simpson_citation_genus = diversity(n_citations,index='invsimpson')) 
  
  globi_degree = degree_by_fam %>% left_join(degree_by_genus) %>% 
    left_join(diet_breadth %>% 
                distinct(scientificName,diet_breadth,diet_breadth_detailed)) %>%
    mutate(diet_breadth = ifelse(is.na(diet_breadth),'generalist',diet_breadth),
           diet_breadth_detailed = ifelse(is.na(diet_breadth_detailed),'generalist',diet_breadth_detailed)) %>%
    left_join(total_obs) %>%
    mutate(diet_breadth = as.factor(diet_breadth))
  
  #run the random forest model
  rf_all = randomForest(as.factor(diet_breadth) ~ degree_family + degree_genus + simpson_fam + simpson_genus + bee_family + n + simpson_citation_genus+simpson_citation_fam,data = globi_degree,importance = T)
  
  oob_error = rf_all$err.rate[500,][1]
  
  # get auc of roc curve
  rf_roc = roc(as.factor(globi_degree$diet_breadth),rf_all$votes[,2])
  the_auc = as.numeric(auc(rf_roc))
  
  rf_df=data.frame(rf_all$confusion) %>% 
    mutate(threshold_n=threshold_n) %>%
    mutate(oob=oob_error,n_species_all = nrow(globi_degree),auc=the_auc,
           n_species_gen=nrow(globi_degree %>% filter(diet_breadth=='generalist')),
           n_species_spec=nrow(globi_degree %>% filter(diet_breadth=='specialist')))
  
  rf_df$diet_breadth = row.names(rf_df)
  row.names(rf_df) <- NULL
  
  return(rf_df)

  })

#})

par(mfrow=c(1,1))
with(what_n %>% bind_rows , plot(threshold_n,auc))

with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='specialist'), plot(threshold_n,class.error))

with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='generalist'), plot(threshold_n,class.error))
with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='generalist'), plot(threshold_n,oob))
with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='generalist'), plot(threshold_n,n_species_all))
with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='generalist'), plot(threshold_n,n_species_gen))
with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='generalist'), plot(threshold_n,n_species_spec))

# pdf('figures/change_in_auc.pdf')
with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='specialist'), plot(threshold_n,auc,xlab='sample size required for inclusion'))
# dev.off()

# change in proportion specialists as sample size required for inclusion changes?
with(what_n %>% bind_rows %>% 
      filter(diet_breadth=='specialist') %>%
      mutate(prop_specialists = n_species_spec/(n_species_spec+n_species_gen)), 
    plot(threshold_n,prop_specialists,xlab='sample size required for inclusion'))
#
# Fit gam to the data to see what sample size minimizes classification error
library(gam)
specialist_data = what_n %>% 
  bind_rows %>% 
  filter(diet_breadth=='specialist') %>% 
  mutate(n_classified = generalist+specialist)
generalist_data = what_n %>% 
  bind_rows %>% 
  filter(diet_breadth=='generalist') %>% 
  mutate(n_classified = generalist+specialist)


gam_specs1 = gam(class.error ~ s(threshold_n,3),data=specialist_data) ## should this be binomial??
gam_specs = gam(class.error ~ s(threshold_n,3),family='binomial',data=specialist_data,weights=n_classified) ## should this be binomial??

plot(gam_specs)
gam_gens1 = gam(class.error ~ s(threshold_n,3),data=generalist_data)
gam_gens = gam(class.error ~ s(threshold_n,3),family='binomial',weights=n_classified,data=generalist_data)


gam_auc = gam(auc ~ s(threshold_n,3),data = generalist_data)

plot(gam_auc)

new_data <- data.frame(threshold_n = seq(0,300,by=.1)) # make a data.frame of new explanatory variables
new_data$gam_specs <- predict(gam_specs, newdata=new_data, type='response') # predict on the scale of the response
new_data$gam_gens <- predict(gam_gens, newdata=new_data, type='response') # predict on the scale of the response
new_data$gam_auc <- predict(gam_auc, newdata=new_data, type='response') # predict on the scale of the response

x_axis_lab = 'sample size required for inclusion'
ylims = c(0,100)
my_cex = 2.3
cex_pts = 2
the_point_col = adjustcolor('black',.8)
the_lwd=3

#calculate: where is the sample size that minimizes prediction error?
min_gen_index = which(new_data$gam_gens == min(new_data$gam_gens))
min_spec_index = which(new_data$gam_specs == min(new_data$gam_specs))

#lines of code below give the give the sample size for inclusion that minimizes prediction error
(n_needed_gens = new_data$threshold_n[min_gen_index])# for generalists
(n_needed_specs = new_data$threshold_n[min_spec_index]) # for specialists

#accuracy rate at predicting specialists when all bees re included
new_data %>% filter(gam_specs <.3) %>% summarize(max = max(threshold_n),min=min(threshold_n))
1-new_data[new_data$threshold_n==0,]$gam_specs
1-new_data[new_data$threshold_n==300,]$gam_specs

1-new_data[new_data$threshold_n==300,]$gam_gens
1-new_data[new_data$threshold_n==0,]$gam_gens

new_data[new_data$threshold_n==300,]
new_data[new_data$threshold_n==0,]

what_n %>% bind_rows %>% filter(threshold_n  == round(new_data$threshold_n[min_spec_index]))
what_n %>% bind_rows %>% filter(threshold_n  == 120)

# what_n[[min_gen_index]]

what_n[[1]] %>% mutate(n_species  = n_species_gen + n_species_spec)
what_n %>% bind_rows %>% filter(threshold_n  == 120) %>% mutate(n_species  = n_species_gen + n_species_spec)


# pdf('figures/sample size results-newdata.pdf',width=14)
par(mfrow=c(1,3),mar=c(5,5,4,2))
plot(100-class.error*100~threshold_n,data = specialist_data,pch = 16, ylim = ylims,col = the_point_col,
     xlab= x_axis_lab, ylab='prediction accuracy of specialists (%)',cex.lab=my_cex,cex=cex_pts)
with(new_data,lines(100-gam_specs*100~threshold_n,col="deeppink4",lwd=the_lwd))
# abline(v = n_needed_specs,lty=2)

plot(100-class.error*100~threshold_n,data = generalist_data, pch = 16, ylim = ylims, col = the_point_col,
     xlab= x_axis_lab, ylab='prediction accuracy of generalists (%)',cex.lab=my_cex,cex=cex_pts)
with(new_data,lines(100-gam_gens*100~threshold_n,col='deeppink4',lwd=the_lwd))

plot(auc~threshold_n,data = generalist_data, pch = 16,  col = the_point_col,
     xlab= x_axis_lab, ylab='AUC',cex.lab=my_cex,cex=cex_pts,ylim=c(0,1))
with(new_data,lines(gam_auc~threshold_n,col='deeppink4',lwd=the_lwd))
# dev.off()

#make plot of number of specialist bees included in the analysis
# par(mfrow=c(1,1))
pdf('figures/sample_size_change.pdf')
plot(n_classified~threshold_n,data=specialist_data,pch = 16,  col = the_point_col,
     xlab= x_axis_lab, ylab='number of specialist species included in the anlaysis',cex.lab=1.2,cex=cex_pts)
# dev.off()

#plot with sample size change as a percentage
specialist_data$percent_spec_secies_classified = 100*specialist_data$n_classified/max(specialist_data$n_classified)
par(mfrow=c(1,1))
pdf('figures/sample_size_change_percent.pdf')
plot(percent_spec_secies_classified~threshold_n,data=specialist_data,pch = 16,  col = the_point_col,
     xlab= x_axis_lab, ylab='% of specialist species included in the anlaysis',cex.lab=1.2,cex=cex_pts)
dev.off()


#make a figure with just the specialists
# pdf('figures/specialist_samplesize.pdf')
par(mfrow=c(1,1),mar=c(5,5,4,2))
plot(100-class.error*100~threshold_n,data = specialist_data,pch = 16, ylim = ylims,col = the_point_col,
     xlab= x_axis_lab, ylab='prediction accuracy of specialists (%)',cex.lab=my_cex,cex=cex_pts)
with(new_data,lines(100-gam_specs*100~threshold_n,col="deeppink4",lwd=the_lwd))
abline(v = n_needed_specs,lty=2)
# dev.off()
#



# for bees on the Jarrod Fowler lists, what % visit their host plants?
# for the globi data: categorize interaction partner as host or non-host
# loop through the bee speices in globi_u
# get their host plants
# if the interaciton partner is in the family or genus then categorize it as a host
#fowler_specialists = #fowler[fowler$diet_breadth == 'specialist',]$scientificName


# let's exclude bees with fewer than 20 interactions in the globi dataset
n_threshold = 20
bigN_bees20=globi_u %>% 
  group_by(scientificName) %>% 
  summarize(n=n()) %>% 
  filter(n>=n_threshold)

globi_summ_NoSizeFilter = globi_u %>% 
  group_by(scientificName,bee_family,plant_genus,plant_family) %>% 
  summarize(n_genus=n(),n_citations=n_distinct(sourceCitation)) 

globi_specs = globi_summ_NoSizeFilter %>% 
  filter(scientificName %in% bigN_bees20$scientificName) %>%
  left_join(diet_breadth) %>%
  filter(diet_breadth=='specialist') %>%
  #filter(n_genus >=5) %>% ##let's exclude interactions with fewer than 5 records
  mutate(bee_plant = paste(scientificName,plant_genus))

# double check that specialist bees have either family as host rank or genus
# but not both
which(fowler_formatted %>% split(.$scientificName) %>%
  purrr::map_lgl(function(df) n_distinct(df$host_rank)>1))

my_index = which(globi_specs$scientificName  == 'Dufourea virgata')
a = globi_specs %>% split(1:nrow(globi_specs))
row = a[[my_index]]
empty_list = c()
globi_host = globi_specs %>% split(1:nrow(globi_specs)) %>% 
  purrr::map_dfr(function(row){
  
  (host_plant_df = fowler_formatted %>% 
    filter(scientificName == row$scientificName[1]))
  
  if(unique(host_plant_df$host_rank) =='genus') visiting_host = row$plant_genus  %in% host_plant_df$host
  if(unique(host_plant_df$host_rank) =='family') visiting_host = row$plant_family %in% host_plant_df$host
  if(unique(host_plant_df$host_rank) =='tribe') visiting_host = row$plant_family =='Asteraceae'
  
  return(row %>% mutate(host=visiting_host))

  })   #add 0 for bees that haven't visited their host plant
globi_host %>% filter(scientificName %in% dupes) %>% distinct(diet_breadth_detailed)


# globi_host2 = globi_host %>% bind_rows(add_these)
globi_host2 = globi_host

# let's load bees in the globi host data that are specialists at the genus level
# get a list of their hosts in order to search and see if they produce nectar or not
globi_genus_specs = globi_host2 %>% 
  filter(diet_breadth_detailed=='genus_specialist') %>%
  distinct(scientificName) 
get_nectar = fowler_formatted %>% filter(scientificName %in% globi_genus_specs$scientificName) %>%
  distinct(host,family)
# write_csv(get_nectar,'modeling_data/get_nectar.csv')


plants_nectar <- read_excel("modeling_data/plants_nectar.xlsx") %>%
  filter(nectar_status != 'unknown' & !is.na(nectar_status))
head(plants_nectar)

## what percentage of specialist bees visit their host plants?

globi_host2

host_fidelity=globi_host2 %>%
  group_by(scientificName) %>%
  summarize(sum_n=sum(n_genus[host])/sum(n_genus),sample_size = sum(n_genus),
            sum_cits = sum(n_citations[host])/sum(n_citations)) 

# pdf('figures/histogram_visits_hosts_specialists.pdf')
par(mfrow=c(1,1))
hist(host_fidelity$sum_n*100,main='pollen specialists - visits to host plants',
     xlab='% of visits to host plants',col=point_col,cex.lab=cex_lab)
# dev.off()

paste0('specialist bees visited their host plants on average ',round(mean(host_fidelity$sum_n*100),2),
'% of the time (median = ',round(median(host_fidelity$sum_n*100),2),'%)')

#what percentage of bees visit their host plants 100% of the time?
paste0(round(mean(host_fidelity$sum_n==1)*100,2),'% of specialist bees visit their host plants 100% of the time')

#get the bees that are least faithful to their host plants
host_fidelity %>% arrange(sum_n)
paste('For',sum(host_fidelity$sum_n==0),'specialist bee species, there were no records of the bee visiting their pollen hosts')
paste('these bees were:')
(unfaithful_bees = host_fidelity[host_fidelity$sum_n==0,]$scientificName)
fowler_formatted %>% filter(scientificName %in% unfaithful_bees) %>% distinct(scientificName, host,host_rank)

globi_host %>% filter(scientificName == "Perdita zebrata")
fowler_formatted %>% filter(scientificName == 'Perdita zebrata') %>% distinct(host)

hist(host_fidelity$sum_cits*100)
host_fidelity %>% filter(sum_n<.3)

with(host_fidelity,plot(sample_size,sum_n))

host_fidelity %>% 
  filter(host %in% plants_nectar)

#need to 1) get genus level specs in data what there hosts are
# 2) join with plants_nectar to get the host plant's nectar status
# 3) join with host fidelity data
# 4) plot nectar status of host plant vs host fidelity

#get bee species that are specialists and what there host plants are
head(globi_genus_specs)
globi_genus_specs %>% filter(scientificName == "Andrena arabis")
head(plants_nectar)
head(globi_genus_specs)
globi_host %>% filter(host) %>% filter(scientificName %in% globi_genus_specs$scientificName) %>% group_by(scientificName) %>% summarize(n=n()) %>%
  filter(n !=1)
globi_host %>% filter(scientificName == "Andrena asteris")
nectar_fidelity = plants_nectar %>% 
  rename(plant_genus = host) %>%
  left_join(globi_host2 %>% filter(host)) %>%
  filter(!is.na(scientificName)) %>%
  left_join(host_fidelity, by='scientificName')

#check that each bee species only has one row
n_recs = nectar_fidelity2 %>% split(.$scientificName) %>% map_dbl(nrow)
n_recs[n_recs != 1]

# Andrena rehni - specialist on chesnut; doesn't have enough records with this species to show up in the data
## how to deal with species that have 0% visits to host plant (based on the threshold we're using)
globi_genus_specs %>% filter()

# nectar_fidelity = plants_nectar %>% 
#   rename(plant_genus = host) %>%
#   left_join(globi_genus_specs) %>%#join to get genus level specs in data what there hosts are
#   left_join(host_fidelity, by = 'scientificName')
par(mfrow=c(1,1))
with(nectar_fidelity, boxplot(sum_n~nectar_status))

# pdf("figures/nectar_boxplot.pdf")
with(nectar_fidelity,stripchart(sum_n~nectar_status,
                                  vertical=T,col=point_col,at=c(loc_gen,loc_spec),pch=16,
                                  cex.lab = cex_lab, cex.axis=cex_axis,
                                  xlab = 'plant nectar status',ylab = "specialist bee's fidelity to host plant"))
with(nectar_fidelity,boxplot(sum_n~nectar_status,col=box_col,add=T,at=c(loc_gen,loc_spec), axes = F,boxwex=c(.35,.35)))
# dev.off()

with(nectar_fidelity,boxplot(sum_cits ~ nectar_status))
nectar_fidelity %>% filter(scientificName == 'Andrena erigeniae')
nectar_fidelity %>% filter(sum_n<.2)

## for both nectar analysis and bee m/f analysis, look at host fidelity as % citations?
head(nectar_fidelity)

# look at the data for Andrena erigeniae 
# View(globi_r %>% 
#   filter(scientificName == "Andrena erigeniae"))
globi_r %>% filter(scientificName == 'Andrena erigeniae') %>%
  group_by(referenceCitation) %>%
  summarize(n=n()) %>% arrange(desc(n))

check_me = globi_r %>% filter(scientificName == 'Andrena erigeniae')
nrow(check_me)
n_distinct(check_me$plant_genus)

#does pollen fidelity predict visitation fidelity
host_pollen = pollen_fidelity %>% 
  group_by(bee) %>% 
  summarize(mean_prop_host_pollen = mean(prop_host), n=n()) %>%
  filter(n >= 5)
pollen_data_bees = host_pollen$bee
pollen_data_bees[pollen_data_bees %in% host_fidelity$scientificName]

globi_specs %>% filter(scientificName =='Andrena erythronii')

comp_pollen_visit = host_pollen %>% 
  rename(scientificName = bee,pollen_n=n) %>% 
  left_join(host_fidelity)

# pdf('figures/visitfidelity_pollenfidelity.pdf')
par(mar=c(5,5,4,2))
with(comp_pollen_visit,plot(sum_n~mean_prop_host_pollen,cex=1.3, pch=16,col=adjustcolor('black',.7),
                            cex.lab = 1.5,
                            ylab='proportion of visits to host plants in GLOBI',
                            xlab = 'mean proportion of host pollen in pollen load'))
# dev.off()


#look at difference in host fidelity in males vs females
head(globi_host)
globi_host_u = globi_host2 %>% distinct(scientificName,plant_genus,plant_family,host)
#get male v female for the globi data
globi_r %>% 
  filter(!is.na(sourceSexName))
globi_r %>% 
  filter(!is.na(targetSexName))
globi_r %>% 
  filter(!is.na(sourceSexId))
globi_r %>% 
  filter(!is.na(targetSexId))

str(globi_r)
# View(globi_r %>% 
#   filter(!is.na(sourceSexName)) %>%
#     left_join(sex_codes%>% rename(sourceSexName=sex_name)) %>% 
#     filter(sex != 'female')  %>% 
#   distinct(referenceCitation,sourceCitation))

sex_codes

data.frame(globi_r %>% 
  filter(!is.na(sourceSexName)) %>%
  group_by(scientificName,sourceSexName) %>%
  summarize(n=n()))

sexed = globi_r %>% 
  filter(!is.na(sourceSexName)) %>%
  group_by(scientificName,plant_family,plant_genus,sourceSexName) %>%
  summarize(n=n())
sex_codes  = data.frame(sourceSexName = unique(sexed$sourceSexName)) %>%
  mutate(sex = ifelse(grepl('queen',sourceSexName),'queen',sourceSexName)) %>%
  mutate(sex = ifelse(sourceSexName %in% c('fem','female','femlae','female paratype','female holo',"1 female"),'female',sex)) %>%
  mutate(sex = ifelse(sourceSexName %in% c('male','male paratype','1 male'),'male',sex)) %>%
  mutate(sex = ifelse(grepl('worker',sourceSexName),'female',sex)) %>%
  mutate(sex = ifelse(grepl('adult',sourceSexName),'unknown',sex)) %>%
  mutate(sex = ifelse(grepl('unknown',sourceSexName),'unknown',sex)) %>%
  mutate(sex = ifelse(sourceSexName %in% c('u','undetermined','male and female','1 adult'),'unknown',sex)) %>%
  mutate(sex = ifelse(sourceSexName=='f','female',sex)) %>%
  mutate(sex = ifelse(sourceSexName=='m','male',sex))


globi_summ_bySex = globi_u %>% 
  filter(!is.na(sourceSexName)) %>%
  left_join(sex_codes) %>%
  group_by(scientificName,bee_family,plant_genus,plant_family,sex) %>% 
  summarize(n_genus=n(),n_citations=n_distinct(sourceCitation)) 



globi_specs_sexed = globi_summ_bySex %>% 
  left_join(diet_breadth) %>%
  filter(diet_breadth=='specialist') %>%
  mutate(bee_plant = paste(scientificName,plant_genus)) %>%
  filter(bee_plant %in% globi_specs$bee_plant) ##let's exclude interactions with fewer than 5 records (at the species level though - so use globi_spec df to figure out what that is)

globi_host_bySex = globi_specs_sexed %>% split(1:nrow(globi_specs_sexed)) %>% 
  purrr::map_dfr(function(row){
    
    (host_plant_df = fowler_formatted %>% 
       filter(scientificName == row$scientificName[1]))
    
    if(unique(host_plant_df$host_rank) =='genus') visiting_host = row$plant_genus  %in% host_plant_df$host
    if(unique(host_plant_df$host_rank) =='family') visiting_host = row$plant_family %in% host_plant_df$host
    if(unique(host_plant_df$host_rank) =='tribe') visiting_host = row$plant_family =='Asteraceae'
    
    return(row %>% mutate(host=visiting_host))
    
  }) 


hosts_by_sex = globi_host_bySex %>%
  filter(!is.na(host)) %>%
  group_by(scientificName,sex) %>%
  summarize(sum_n=sum(n_genus[host])/sum(n_genus),sample_size = sum(n_genus)) 


#first just plot as box plots
with(hosts_by_sex %>% filter(sex != 'unknown'),boxplot(sum_n~sex))

# then only include bees with pairs of male and female 
# with sample size above some threshold

paired_sexes=names(which(hosts_by_sex %>% filter(sample_size>20 & sex !='unknown') %>% split(.$scientificName) %>% map_lgl(function(df)nrow(df)==2) ))
with(hosts_by_sex %>% 
  filter(scientificName %in% paired_sexes & sex !='unknown'),boxplot(sum_n~sex))

#pair males and females
hosts_by_sex_bigN=hosts_by_sex %>% 
  filter(scientificName %in% paired_sexes & sex !='unknown')

a=hosts_by_sex_bigN %>%
  split(.$scientificName)
df=a[[1]]
sex_differences = hosts_by_sex_bigN %>%
  split(.$scientificName) %>%
  map_dbl(function(df){
    sex_diff = df[df$sex=='female',]$sum_n-df[df$sex=='male',]$sum_n
    return(sex_diff)
    })
hist(sex_differences)

(sexed_wide = hosts_by_sex_bigN %>% select(-sample_size) %>%
  pivot_wider(values_from=sum_n,names_from=sex,values_fill=0))

#make a paired box-plot
# pdf('figures/males_v_females.pdf')
par(mfrow=c(1,1))
with(hosts_by_sex_bigN,stripchart(sum_n~sex,
                             vertical=T,col=point_col,at=c(loc_gen,loc_spec),pch=16,
                             cex.lab = cex_lab, cex.axis=cex_axis,
                             xlab = 'bee sex',ylab = 'fidelity to host plant'))
for(i in 1:nrow(sexed_wide)){segments(loc_gen, sexed_wide$female[i], loc_spec, sexed_wide$male[i],lty=2,col=adjustcolor('black',.3))}
with(hosts_by_sex_bigN,boxplot(sum_n~sex,col=box_col,add=T,at=c(loc_gen,loc_spec), axes = F,boxwex=c(.35,.35)))
# dev.off()


hosts_by_sex_bigN %>% filter(sum_n<.1)
globi_host_bySex %>% filter(scientificName=="Melissodes tristis" & sex=='male')
globi_host_bySex %>% filter(scientificName=="Melissodes tristis" & sex=='female')

# View(globi_r %>% filter(scientificName=="Melissodes tristis") %>%
#   group_by(plant_family,referenceCitation,sourceCitation) %>%
#   summarize(n=n()))

globi_r %>% filter(plant_family=="Compositae")
globi_r %>% 
  filter(!grepl("aceae",plant_family)) %>% 
  distinct(plant_genus,plant_family)
str(globi_r)

#run a paired t-test
t.test(Pair(female,male)~1, data = sexed_wide)

#just look at males/females for bees in my data
(female_comp=hosts_by_sex_bigN %>%
  filter(scientificName %in%comp_pollen_visit$scientificName & sex=='female') %>%
  left_join(comp_pollen_visit %>% select(scientificName,mean_prop_host_pollen,pollen_n),by='scientificName'))

par(mar=c(5,5,4,2))
with(female_comp,plot(sum_n~mean_prop_host_pollen,cex=1.3, pch=16,col=adjustcolor('black',.7),
                            cex.lab = 1.5,
                            ylab='proportion of visits to host plants in GLOBI',
                            xlab = 'mean proportion of host pollen in pollen load'))
# 

globi_r %>%
  filter(scientificName=='Andrena erythronii')
globi_r %>%
  filter(scientificName=='Dufourea novaeangliae')

#load the discrepancies data 
#(tells us which bees have been found with non-host pollen in their pollen load)
discrepancies = read_csv('modeling_data/discrepancies_fowler_ms.csv')
head(discrepancies)
questionable = discrepancies %>% 
  filter(`This ms` =='generalist' &  Fowler=='specialist')
questionable$scientificName[questionable$scientificName %in% host_fidelity$scientificName]

#load the missouri state data
ms <- read_xlsx('modeling_data/DATA_pollen-diet-breadth_list_12-5-21_ALR-cleaned.xlsx', sheet = 3) %>%
  rename_all(tolower) %>%
  mutate(scientificName = paste0(genus, ' ',sub('.*\\. ','',species))) %>%
  select(scientificName,everything()) %>%
  rename(diet_breadth=`redefined pollen diet breadth`) %>%
  mutate(pollen_host = `pollen hosts`) %>%
  filter(!is.na(pollen_host))

ms_check = ms %>% filter(scientificName %in% fowler$scientificName)

#we just want the bees with primary pollen data - get rid of anything wiht just two refs
# bc will be fowler and droege and the species description ref in fowler and droege (not a primary pollen source)
a_string = ms_check$references[1]
more_sources_i = which(ms_check$references %>% purrr::map_lgl(function(a_string){
  length(strsplit(a_string,';')[[1]]) !=2
}))

#as a first approximation = lets compare these to bees on 'questionable' vec
ms_check[more_sources_i,] %>% select(scientificName, references)

not_questionable = ms_check[more_sources_i,] %>% 
  filter(!scientificName %in% questionable$scientificName)
not_questionable$scientificName[not_questionable$scientificName %in% host_fidelity$scientificName]
also_questionable = c("Andrena erythronii")

bees_w_data = host_fidelity %>%
  mutate(specialist_type = ifelse(scientificName %in% not_questionable$scientificName,'true',NA))%>%
  mutate(specialist_type = ifelse(scientificName %in% c(questionable$scientificName,also_questionable), 'facultative',specialist_type)) %>%
  filter(!is.na(specialist_type))

# pdf('figures/preliminary_facultative_boxplot.pdf')
with(bees_w_data,boxplot(sum_n~specialist_type,names=c('true','false'),ylab = 'proportion of visits to host plant',xlab = 'found with nonhost pollen'))
# dev.off()

#format data and look up primary pollen sources
look_up = ms %>%
  mutate(specialist_type = ifelse(scientificName %in% not_questionable$scientificName,'true',NA))%>%
  mutate(specialist_type = ifelse(scientificName %in% c(questionable$scientificName,also_questionable), 'facultative',specialist_type)) %>%
  filter(!is.na(specialist_type)) %>%
  filter(scientificName %in% host_fidelity$scientificName) %>%
  select(scientificName, specialist_type, diet_breadth,pollen_host,references)

# write_csv(look_up,'modeling_data/look_at_primary_data.csv')




pollen_fidelity
