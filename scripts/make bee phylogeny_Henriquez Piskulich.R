rm(list=ls())
library(tidyverse)
library("ape")
library("phytools")
library('picante')

##if U.phylomaker is not installed:
# devtools::install_github("jinyizju/U.PhyloMaker")
library("U.PhyloMaker")

#### Read in genus phylogenetic tree from Henriquez Piskulich, Patricia Andrea; Hugall, Andrew F.; Stuart-Fox, Devi (2023). A supermatrix phylogeny of the world’s bees (Hymenoptera: Anthophila) [Dataset]. Dryad. https://doi.org/10.5061/dryad.80gb5mkw1

mytree <- read.tree('modeling_data/BEE_mat7gen_p8pmAa_fst.nwk')

### read in the globi bee data
globi = read_csv("modeling_data/globi_allNamesUpdated.csv") %>%
  mutate(bee_genus = sub(" .*","",scientificName))
globi_genera = unique(globi$bee_genus)


workingtree=as.phylo(mytree)

##Make ultrametric
workingtree=chronos(workingtree)

# Extract tip labels from the tree
tree_tips <- workingtree$tip.label

# Extract the genus from the tip labels
# Assuming the format is "genus_species~~family~~subfamily~tribe"
genus_names <- sub("_.*", "", tree_tips)

# Find the intersection of the extracted genus names and your list of names
matching_names <- tree_tips[!genus_names %in% globi_genera]

# Trim the tree to include only the matching names
trimmed_tree <- drop.tip(workingtree, matching_names)

trimmed_tree$tip.label


# add genera not in Henriquez Piskulich
(genera_out = globi_genera[!globi_genera %in% genus_names])#look at them (Ancylandrena,Mesoxaea,Gaesischia,Simanthedon,Syntrichalonia,Brachymelecta,Cemolobus,Micralictoides,Pseudaugochlora,Lithurgopsis)
species_list = globi_genera
genus_list = data.frame(globi %>%
  distinct(bee_genus,bee_family) %>% select(bee_genus,bee_family) %>% rename(genus = bee_genus,family = bee_family))
write.table(genera_out,"genera_not_in_original_tree.txt")

#add them using megatree approach in U.PhyloMaker
result <- phylo.maker(species_list , pruned_tree, genus_list, scenario=2)
new_bee_tree = result$phylo
is.ultrametric(new_bee_tree)
plot(new_bee_tree, cex = .7)

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

# write_csv(bee_fam2 %>% select(bee_genus,everything()),'modeling_data/bee_phylogenetic_data.csv')


#run mantel test to see differences between output
library(vegan)
pruned = read_csv("specialist-manuscript-response/bee_phylogenetic_data-pruned.csv")
nonpruned = read_csv("specialist-manuscript-response/bee_phylogenetic_data-notpruned.csv")

matrix_pruned = select(pruned, -bee_genus, -scientificName, -eigen1, -eigen2, -bee_family)
matrix_nonpruned = select(nonpruned, -bee_genus, -scientificName, -eigen1, -eigen2, -bee_family)

correlation <- cor(matrix_pruned, matrix_nonpruned, method = "kendall")
print(correlation)
