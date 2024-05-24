rm(list=ls())
library(tidyverse)
library("ape")
library("phytools")
library('picante')

##if U.phylomaker is not installed:
# devtools::install_github("jinyizju/U.PhyloMaker")
library("U.PhyloMaker")

#### Read in genus phylogenetic tree from Hedtke, S.M., Patiny, S. & Danforth, B.N. The bee tree of life: a supermatrix approach to apoid phylogeny and biogeography. BMC Evol Biol 13, 138 (2013). https://doi.org/10.1186/1471-2148-13-138

mytree <- read.tree('modeling_data/12862_2013_2375_MOESM3_ESM.txt')

#below would read in species level tree. Not used in this analysis
#mytree <- read.tree("modeling_data/12862_2013_2375_MOESM1_ESM.txt",comment.char = "#", keep.multi = TRUE, tree.names=c("treeA","treeB","treeC","treeD","treeE","treeF","treeG","treeH","treeI","treeJ"))

### read in the globi bee data
globi = read_csv("modeling_data/globi_allNamesUpdated.csv") %>%
  mutate(bee_genus = sub(" .*","",scientificName))
globi_genera = unique(globi$bee_genus)
globi_species2 = unique(globi$scientificName)
globi_species =sub(" ","_",globi_species2)

# arbitrarily picked the a tree from the list of 10 to work with
which_index = 1
bee_tree <- mytree[[which_index]] 

###root with apoid wasp outgroup
workingtree=root(bee_tree,outgroup="Tachysphex")
workingtree=as.phylo(workingtree)
# 
##Make ultrametric
workingtree=chronos(workingtree)
# is.ultrametric(bee_tree)

#remove bee genera that are not in globi
drop_me = bee_tree$tip.label[!bee_tree$tip.label %in% globi_genera]
pruned_tree = drop.tip(workingtree, drop_me)


# add genera not in Hedtke
(genera_out = globi_genera[!globi_genera %in% workingtree$tip.label])#look at them (Ancylandrena,Mesoxaea,Gaesischia,Simanthedon,Syntrichalonia,Brachymelecta,Cemolobus,Micralictoides,Pseudaugochlora,Lithurgopsis)
species_list = globi_genera
genus_list = data.frame(globi %>%
  distinct(bee_genus,bee_family) %>% select(bee_genus,bee_family) %>% rename(genus = bee_genus,family = bee_family))
write.table(genera_out,"genera_not_in_original_tree.txt")

#add them using megatree approach in U.PhyloMaker
result <- phylo.maker(species_list , pruned_tree, genus_list, scenario=2)
new_bee_tree = result$phylo
is.ultrametric(new_bee_tree)

#double check tree looks okay for a few genera in each fam
keep = c("Osmia","Hesperapis", 'Bombus',"Andrena","Perdita",'Colletes','Megachile','Lasioglossum')
all_gen = new_bee_tree$tip.label
(rm_genera = all_gen[!all_gen %in% keep])
very_pruned = drop.tip(new_bee_tree,rm_genera)
plot(very_pruned)

phylo_dist = cophenetic.phylo(new_bee_tree)
#plot phylogenetic distance between the bees
# phylo_dist = cophenetic(pruned_tree)
my_pcoa <- stats:::cmdscale(phylo_dist,eig=T)
plot(my_pcoa$points) # looks pretty weird

my_pcoa$points
my_pcoa$eig[1:6]
bee_fam = data.frame(bee_genus = row.names(my_pcoa$points),eigen1 = my_pcoa$points[,1],eigen2 = my_pcoa$points[,2]) %>%
  left_join(globi %>% distinct(bee_genus,bee_family))

data.frame(bee_genus = row.names(my_pcoa$points),eigen1 = my_pcoa$points[,1],eigen2 = my_pcoa$points[,2])

#add pairwise phylo_dist
phylo_df = as.data.frame(phylo_dist)
phylo_df$bee_genus = row.names(phylo_dist)
row.names(phylo_df) <- NULL

bee_fam2 = globi %>% distinct(scientificName,bee_genus) %>%
  left_join(bee_fam %>% left_join(phylo_df))
color_pal=RColorBrewer::brewer.pal(8,'Set3')[c(1,3:8)]
my_cols = adjustcolor(color_pal[as.factor(bee_fam$bee_family)],.4)
my_cols2 = adjustcolor(color_pal[as.factor(bee_fam$bee_family)],.7)

with(bee_fam,plot(eigen1,eigen2,pch=16,col = my_cols))
legend("bottomleft",unique(bee_fam$bee_family),col=unique(my_cols2),pch=16)


#why is melittidae so close to andrenidae/colletidae??
phylo_df %>% filter(bee_genus=="Andrena") %>% select(bee_genus,"Hesperapis",'Colletes','Megachile','Bombus','Perdita')
plot(very_pruned)
phylo_df %>% filter(bee_genus=="Bombus") %>% select(bee_genus,"Hesperapis",'Colletes','Megachile','Lasioglossum')
phylo_df %>% filter(bee_genus=="Hesperapis") %>% select(bee_genus,"Andrena","Perdita",'Colletes','Megachile','Lasioglossum')

# write_csv(bee_fam2 %>% select(bee_genus,everything()) %>% select(-bee),'modeling_data/bee_phylogenetic_data.csv')
