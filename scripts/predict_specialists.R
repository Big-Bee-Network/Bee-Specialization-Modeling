rm(list=ls())
library(tidyverse)
library(randomForest)

data = read_csv('modeling_data/globi_degree.csv') %>% filter(n>20) %>%
  mutate(genus = sub(" .*","",scientificName))

set.seed(222)
ind <- sample(2, nrow(data), replace = TRUE, prob = c(0.7, 0.3))
train <- data[ind==1,]
test <- data[ind==2,]


##there's an R random-forest package - try that...
rf_family = randomForest(as.factor(diet_breadth) ~ weighted_degree_fam + n,data = train,importance = T)
rf_family5 = randomForest(as.factor(diet_breadth) ~ degree5_family + n,data = train %>% filter(degree5_family !=0))
rf_all = randomForest(as.factor(diet_breadth) ~ degree_family + weighted_degree_fam +genus + bee_family + n,data = train,importance = T)
  
rf_family
rf_family5


rf_all
importance(rf_all)

# pdf('figures/var_importance_rf.pdf',width =13)
varImpPlot(rf_all)
# dev.off()

# What percentage of times do we correctly predict specialists v generalists using the visitation data?
p1 <- predict(rf_all, train)
p2 <- predict(rf_all,test)
library(caret)
confusionMatrix(p1, as.factor(train$diet_breadth))
confusionMatrix(p2, as.factor(test$diet_breadth))

plot(rf_all)

test$prediction = predict(rf_all,test)
test %>% filter(diet_breadth == 'specialist')%>% 
  summarize(correct_ids = mean(prediction =='specialist'))
test %>% filter(diet_breadth == 'generalist')%>% 
  summarize(correct_ids = mean(prediction =='generalist'))
test %>% filter(diet_breadth == 'generalist')%>% 
  summarize(incorrect_ids = mean(prediction =='specialist'))


# percentage of specialists correctly identified as specialists?
ml %>% filter(diet_breadth == 'specialist')  %>% 
  summarize(correct_ids = mean(predicted_rf_weighted_degree_fam =='specialist'))

ml %>% filter(diet_breadth == 'generalist')  %>% 
  summarize(incorrect_ids = mean(predicted_rf_weighted_degree_fam =='specialist'))

#what about other methods?
ml %>% filter(diet_breadth == 'specialist')  %>% 
  summarize(correct_ids = mean(predicted_lr_weighted_degree_fam =='specialist'))

ml %>% filter(diet_breadth == 'generalist')  %>% 
  summarize(incorrect_ids = mean(predicted_lr_weighted_degree_fam =='specialist'))

ml %>% filter(diet_breadth == 'specialist')  %>% 
  summarize(correct_ids = mean(predicted_rf_degree_fam =='specialist'))

ml %>% filter(diet_breadth == 'generalist')  %>% 
  summarize(incorrect_ids = mean(predicted_rf_degree_fam =='specialist'))

ml %>% filter(diet_breadth == 'specialist')  %>% 
  summarize(correct_ids = mean(predicted_lr_degree_fam =='specialist'))

ml %>% filter(diet_breadth == 'generalist')  %>% 
  summarize(incorrect_ids = mean(predicted_lr_degree_fam =='specialist'))



