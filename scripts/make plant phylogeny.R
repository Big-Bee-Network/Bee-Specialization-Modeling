rm(list=ls())
library(tidyverse)
library(vroom)
library(Taxonstand)
library(hillR)
library(purrr)
library(vegan)
#if vphylomaker not installed:
#devtools::install_github("jinyizju/V.PhyloMaker")
library(V.PhyloMaker)


# load the filtered globi data
# this is a file with the globi records and plant and bee names are updated 
# bees are filtered to just be species that are native to the US
globi = vroom('modeling_data/globi_american_native_bees.csv')

#standardize according to the plant list, becaues that's what v.phylomaker uses
plant_df = globi %>% distinct(plant_species,plant_genus,plant_family) %>% 
  rename(genus=plant_genus,family=plant_family)
test_me = plant_df$plant_species

# #these are plants we already updated with TPL (takes a really long time to run)
# check_names = read_csv('modeling_data/plant_list_name_update.csv')
# 
# #any plants not in the tpl data?
# add_me = test_me[!test_me %in% check_names$Taxon] #yes - lots. let's add these
# check_names2 = TPL(add_me) #takes forever to run, download output as csv
# check_names_f = check_names %>% mutate(TPL.version = as.character(TPL.version)) %>%
#   bind_rows(check_names2)

# write_csv(check_names_f,'modeling_data/plant_list_name_update2.csv')
check_names_f = read_csv('modeling_data/plant_list_name_update2.csv')

#reformat the tpl data
tpl_formatted = check_names_f %>% 
  rename(plant_species = Taxon) %>% 
  mutate(accepted_name = ifelse(!is.na(New.Species),paste(New.Genus,New.Species),New.Genus) ) %>%
  mutate(accepted_name = ifelse(is.na(accepted_name), plant_species, accepted_name)) %>%
  select(plant_species,accepted_name) %>%
  filter(plant_species %in% plant_df$plant_species) %>%
  rename(tpl_name = accepted_name)

#make a dataframe with the family (from the wfo data) and the tpl genus and specise
tpl_plants = tpl_formatted %>%
  left_join(plant_df %>% select(plant_species,family)) %>%
  mutate(genus = gsub(" .*","",tpl_name)) %>%
  select(plant_species,tpl_name,genus,family) %>%
  mutate(family = ifelse(genus %in% c("Sambucus",'Viburnum'),'Adoxaceae',family)) #these genera are listed by v.phylomaker as in the adoxaceae fam

#update the globi data to be TPL data
globi_tpl = globi %>% 
  left_join(tpl_plants %>% dplyr::select(plant_species,tpl_name,genus) %>% rename(tpl_genus=genus))

#make the plant phylogeny

#which species are in the mega-tree?
sp_in_megatree=tpl_plants %>% filter(sub(" ","_",tpl_name) %in% tips.info$species)
gen_in_megatree= tpl_plants %>% filter(genus %in% tips.info$genus)

#for genera with our region's species in the megatree, 
#randomly pick among the species that are in the megatree
set.seed(1013)
sp_list_small1=sp_in_megatree %>% group_by(genus,family) %>% 
  summarize(species=sample(tpl_name,1)) 

#which genera don't have species from our study region
#that are on the megatree?
sp_out_megatree=tpl_plants %>% 
  filter(!sub(" ","_",tpl_name) %in% tips.info$species)
gen_out_megatree=sp_out_megatree %>% filter(!genus %in% sp_list_small1$genus)
n_distinct(gen_out_megatree$genus)#how many genera are not on the megatree?
n_distinct(gen_in_megatree$genus)#how many of the genera are in the megatree?

#for genera not in the megatree just randomly pick a species from each
set.seed(9980)
sp_list_small2=gen_out_megatree %>% group_by(genus,family)%>% 
  summarize(species=sample(tpl_name,1))

#bind the two dataframes together
sp_list_small=data.frame(bind_rows(sp_list_small1,sp_list_small2) %>%
  select(species,genus,family)) %>%
  filter(!(species == "Cyrilla racemiflora" & family =='Iteaceae')) #get rid of this species because its duplicated

#double check that all the genera from inat_obs are in sp_list_small
sp_list_small %>% 
  filter(!genus %in% tpl_plants$genus) #should be empty tibble

# # #UNCOMMENT ME TO make the phylogeny
# angio_tree =  phylo.maker(sp_list_small,scenarios = 'S3') # make the phylogeny out of the species list
# saveRDS(angio_tree,'modeling_data/phylogney_plant_genera.rds')
# 

angio_tree = readRDS('modeling_data/phylogney_plant_genera.rds')
scenario3 = angio_tree$scenario.3

#change the tip.labels to be plant genera
scenario3$tip.label <- sub("_.*","",scenario3$tip.label)


#now we need to calculate phylogenetic diversity of plants visited for each bee species
#calculate phylogenetic diversity using hill_phylo
head(globi)
globi_tpl$tpl_genus
a=globi_tpl %>% split(.$scientificName)
df = a[[1]]
globi_tpl %>% split(.$scientificName) %>%
  map_dfr(function(df){
    plant_sad = df %>% group_by(tpl_genus) %>% summarize(n=n())
    plant_sad %>%
      pivot_wider(names_from=tpl_genus,values_from=n)
    
  })

globi_com =data.frame(globi_tpl %>%
  group_by(scientificName,tpl_genus) %>%
  summarize(n=n())%>%
  pivot_wider(names_from=tpl_genus,values_from=n,values_fill = 0))

globi_matrix = as.matrix(globi_com %>% select(-scientificName),nrow=nrow(globi_com))
row.names(globi_matrix) <- globi_com$scientificName

#calculate phylo simpson diversity
phylo_simp = hill_phylo(globi_matrix, scenario3, q = 2)
phylo_rich = hill_phylo(globi_matrix, scenario3, q = 0)

#make data.frame with phylogenetic richness and div
#(see how they correlate with species-neutral div metrics)

phylo_div = tibble(phylo_rich) %>%
  mutate(scientificName = names(phylo_rich)) %>%
  left_join(tibble(phylo_simp) 
            %>% mutate(scientificName = names(phylo_simp))) %>%
  dplyr::select(scientificName, everything())

#next get measures of simpson diversity and n
div_genus = globi %>% group_by(scientificName,plant_genus) %>%
  summarize(n=n()) %>%
  summarize(rich_genus = n(),simpson_genus = diversity(n,index='invsimpson'))
div_fam = globi %>% group_by(scientificName,plant_family) %>%
  summarize(n=n()) %>%
  summarize(rich_fam=n(),simpson_fam = diversity(n,index='invsimpson'),sample_size=sum(n)) %>%
  rename(n=sample_size)


div_df = phylo_div %>%
  left_join(div_genus) %>%left_join(div_fam)
  
with(div_df,plot(phylo_simp,simpson_genus))
with(div_df,plot(phylo_simp,simpson_fam))
with(div_df,plot(phylo_simp,phylo_rich)) #not good
with(div_df,plot(phylo_simp,n))
with(div_df,plot(phylo_rich,rich_genus))
with(div_df,plot(phylo_rich,rich_fam))



