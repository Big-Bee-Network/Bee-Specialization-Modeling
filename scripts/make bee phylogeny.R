rm(list=ls())
library(tidyverse)
library("ape")
library("phytools")
library('picante')

##if U.phylomaker is not installed:
# devtools::install_github("jinyizju/U.PhyloMaker")
library("U.PhyloMaker")

#### Read in phylogenetic tree
mytree <- read.tree("modeling_data/12862_2013_2375_MOESM1_ESM.txt",comment.char = "#", keep.multi = TRUE, tree.names=c("treeA","treeB","treeC","treeD","treeE","treeF","treeG","treeH","treeI","treeJ"))

###read in the globi bee data
globi = read_csv("modeling_data/globi_allNamesUpdated.csv") %>%
  mutate(bee_genus = sub(" .*","",scientificName))
globi_genera = unique(globi$bee_genus)
globi_species2 = unique(globi$scientificName)
globi_species =sub(" ","_",globi_species2)

# abritrarily picked the first tree of a list of 10 to work with
workingtree <- mytree[[1]] 


#make data frame with the bees in the tree and their genus
tree_bees = data.frame(bee = workingtree$tip.label) %>%
  mutate(bee_genus = sub("_.*",'',bee))

#any of the globi bee genera not in the tree?
(genera_out = globi_genera[!globi_genera %in% tree_bees$bee_genus] ) #yup
bees_in_tree = globi_species[globi_species %in% tree_bees$bee]

set.seed(7211)
#for bee genera with species in the tree randomly pick one of each species
bees_in_tree_sampled = globi %>% filter(scientificName %in% sub("_"," ",bees_in_tree)) %>%
  split(.$bee_genus) %>% map_dfr(function(df){
    sampled_index = sample(nrow(df),1) #get a random row index
    output = df[sampled_index,] %>% select(scientificName) %>% 
      rename(bee = scientificName) %>% mutate(bee_genus = sub(" .*","",bee))
    return(output)
  })

#for bee genera that are on the tree (but different species)
#randomly pick a species that's on the tree
bee_gen_in_tree = globi_genera[!globi_genera %in% c(genera_out,bees_in_tree_sampled$bee_genus)]
bees_gen_in_tree_sampled=  tree_bees %>% filter(bee_genus %in% bee_gen_in_tree) %>%
  split(.$bee_genus) %>% map_dfr(function(df){
    sampled_index = sample(nrow(df),1) #get a random row index
    output = df[sampled_index,] %>% select(bee) %>% 
      mutate(bee = sub('_'," ",bee)) %>%
       mutate(bee_genus = sub(" .*","",bee))
    return(output)
  })

#for everything else just randomly pick a species
#for genera not on the tree, randomly pick a species from the globi data
out_bees = globi %>% filter(bee_genus %in% genera_out) %>% split(.$bee_genus) %>%
  map_dfr(function(df){
    sampled_index = sample(nrow(df),1) #get a random row index
    output = df[sampled_index,] %>% select(scientificName) %>% rename(bee = scientificName)
    return(output)
  })

#make genus list,
#keep_bees_df is species list
genus_list = data.frame(globi %>% distinct(bee_genus,bee_family) %>% 
  rename(genus=bee_genus,family=bee_family) %>%
  select(genus,family))
#any genera duplicated?
genus_list[duplicated(genus_list$genus),] #nope

##make species list
species_list = data.frame(bees_in_tree_sampled %>% 
  select(bee) %>% 
  bind_rows(bees_gen_in_tree_sampled %>% select(bee)) %>%
  bind_rows(out_bees))

result <- phylo.maker(species_list , workingtree, genus_list, scenario=2)
pruned_tree = result$phylo
plot(pruned_tree)
phylo_dist = cophenetic(pruned_tree)


#plot phylogenetic distance between the bees
# phylo_dist = cophenetic(pruned_tree)
my_pcoa <- stats:::cmdscale(phylo_dist,eig=T)
plot(my_pcoa$points) # looks pretty weird

my_pcoa$points
my_pcoa$eig[1:6]
bee_fam = data.frame(bee = row.names(my_pcoa$points),eigen1 = my_pcoa$points[,1],eigen2 = my_pcoa$points[,2]) %>%
  mutate(bee = sub("_",' ',bee)) %>%
  left_join(species_list %>%  mutate(bee_genus = sub(" .*","",bee)),by='bee') %>% #get fams
  left_join(globi %>% distinct(bee_genus,bee_family))


#add pairwise phylo_dist
phylo_df = as.data.frame(phylo_dist)
colnames(phylo_df) = sub("_.*",'',colnames(phylo_df))
phylo_df$bee_genus = sub("_.*",'',row.names(phylo_dist))
row.names(phylo_df) <- NULL

bee_fam2 = bee_fam %>% left_join(phylo_df)

# write_csv(bee_fam2 %>% select(bee_genus,everything()) %>% select(-bee),
#           'modeling_data/bee_phylogenetic_data.csv')
