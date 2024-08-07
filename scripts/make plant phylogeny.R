rm(list=ls())
library(tidyverse)
library(vroom)
library(Taxonstand)
library(hillR)
library(purrr)
library(vegan)
#if vphylomaker not installed:
#devtools::install_github("jinyizju/V.PhyloMaker")
install.packages("/Users/katjaseltmann/Downloads/Taxonstand_2.4.tar.gz", repos = NULL, type = "source")
library(V.PhyloMaker)

# load the filtered globi data
# this is a file with the globi records and plant and bee names are updated 
# bees are filtered to just be species that are native to the US
globi_full = vroom('modeling_data/globi_american_native_bees_7march2023.csv')

#standardize according to the plant list, because that's what v.phylomaker uses
plant_df = globi_full %>% 
  distinct(plant_species,plant_genus,plant_family) %>% 
  rename(genus=plant_genus,family=plant_family)
test_me = plant_df$plant_species

#we'll get families from wfo
#and also do genus updates with those, since tpl doesn't have a lot of those
genus_updates = read_csv('modeling_data/wfo_genus_updates.csv')
wfo_fams = read_csv('modeling_data/plant_fam_info.csv')

##let's get rid of bees we don't have phenology data for
bee_geo = read_csv('modeling_data/chesshire2023_beeArea.csv')
bee_geo_na  = bee_geo[is.na(bee_geo$med_doy),]$scientificName
#how many don't we have phenology data for?
globi_full %>% filter(scientificName %in% bee_geo_na) %>% distinct(scientificName)
globi = globi_full %>% filter(!scientificName %in% bee_geo_na) 

# # ##uncomment me to check you have all the names
# # # #these are plants we already updated with TPL (takes a really long time to run)
#check_names_f = read_csv('modeling_data/plant_list_name_update2.csv')
# # # #any plants not in the tpl data?
#add_me = test_me[!test_me %in% check_names_f$Taxon] #yes - lots. let's add these
#check_names2 = TPL(add_me) #takes forever to run, download output as csv
# check_names = check_names_f %>% mutate(TPL.version = as.character(TPL.version)) %>%
#   bind_rows(check_names2)
# write_csv(check_names,'modeling_data/plant_list_name_update3.csv')

check_names = read_csv('modeling_data/plant_list_name_update3.csv')

#reformat the tpl data
tpl_formatted = check_names %>% 
  rename(plant_species = Taxon,tpl_family = Family) %>% 
  mutate(in_tpl = !is.na(Taxonomic.status) & !Taxonomic.status =='') %>%
  mutate(accepted_name = ifelse(!is.na(New.Species),paste(New.Genus,New.Species),New.Genus) ) %>%
  mutate(accepted_name = ifelse(is.na(accepted_name), plant_species, accepted_name)) %>%
  select(plant_species,accepted_name,in_tpl,Taxonomic.status,tpl_family) %>%
  filter(plant_species %in% plant_df$plant_species) 


#look at plants that are not in TPL
tpl_formatted %>% 
  filter(!in_tpl)#most are genera
not_in_tpl_sp=tpl_formatted %>%
  filter(grepl(' ',plant_species)) %>% filter(!in_tpl)
not_in_tpl_gen = tpl_formatted %>%
  filter(!grepl(' ',plant_species)) %>% filter(!in_tpl)

# are genera whose names need changing (according to wfo) 
# changed by the tpl
name_change = genus_updates %>% filter(old_pl_genus != plant_genus)
tpl_formatted %>% filter(plant_species %in% name_change$old_pl_genus) #looks like not


# for genera not in tpl, change their name according to world flora online
tpl_formatted2 = tpl_formatted %>%
  left_join(genus_updates %>%
              rename(plant_species = old_pl_genus,wfo_name =plant_genus)) %>%
  mutate(name_source = ifelse(!in_tpl & !is.na(wfo_name),'wfo',NA)) %>%
  mutate(name_source = ifelse(!in_tpl & is.na(wfo_name),'orig_name',name_source)) %>%
  mutate(name_source = ifelse(in_tpl,'tpl',name_source)) %>%
  mutate(accepted_name = ifelse(name_source == 'wfo',wfo_name,accepted_name)) %>%
  mutate(plant_genus = gsub(' .*','',accepted_name)) %>%
  select(-plant_family,-wfo_name) 

#add family info from world flora online
plant_update = tpl_formatted2 %>%
  left_join(wfo_fams,by="plant_genus") %>%
  rename(plant_species = accepted_name, old_pl_name = plant_species) %>%
  mutate(plant_family = ifelse(plant_genus %in% c("Sambucus",'Viburnum'),'Adoxaceae',plant_family)) %>%
  mutate(family_source = ifelse(is.na(plant_family),'tpl','wfo')) %>%
  mutate(plant_family = ifelse(is.na(plant_family),tpl_family,plant_family)) %>% #for the 12 sp without fams in wfo use tpl
  select(plant_species,plant_genus, plant_family, name_source,family_source,old_pl_name) %>%
  mutate(plant_family = ifelse(plant_genus =="Cleoserrata",'Cleomaceae',plant_family)) %>%
  mutate(plant_family = ifelse(plant_family =="Compositae",'Asteraceae',plant_family)) %>%
  mutate(plant_family = ifelse(plant_family =="Leguminosae",'Fabaceae',plant_family)) 
  
#update the plant names in the globi data 
globi_tpl = globi %>% 
  rename(old_pl_name = plant_species,old_pl_fam = plant_family,old_pl_genus = plant_genus) %>%
  left_join(plant_update)

# write_csv(globi_tpl,'modeling_data/globi_allNamesUpdated.csv')
tree = GBOTB.extended.TPL
tips.info.WP
#make the plant phylogeny
#which species are in the mega-tree?
(sp_in_megatree=plant_update %>% filter(sub(" ","_",plant_species) %in% tips.info.WP$species))
(gen_in_megatree= plant_update %>% filter(plant_genus %in% tips.info.WP$genus))

#write list of genera currently in the megatree
write_csv(gen_in_megatree,'modeling_data/megatree_genera.csv')

#how many genera totally missing from the megatree
nrow(plant_update %>% filter(!plant_genus %in% tips.info.WP$genus) %>% distinct(plant_genus))
nrow(gen_in_megatree %>% distinct(plant_genus))
nrow(plant_update %>% distinct(plant_genus))

#write list of genera missing from the megatree
gen_not_in_megatree<-plant_update %>% filter(!plant_genus %in% tips.info$genus)
distinct_genera <- unique(gen_not_in_megatree$plant_genus)
distinct_genera_df <- data.frame(genus = distinct_genera)
#write_csv(distinct_genera,'modeling_data/megatree_not_genera.csv')

#for genera with our region's species in the megatree, 
#randomly pick among the species that are in the megatree
set.seed(1013)
sp_list_small1=sp_in_megatree %>% group_by(plant_genus,plant_family) %>% 
  summarize(plant_species=sample(plant_species,1)) 

#which genera don't have species from our study region
#that are on the megatree?
sp_out_megatree=plant_update %>% 
  filter(!sub(" ","_",plant_species) %in% tips.info$species)

gen_out_megatree=sp_out_megatree %>% filter(!plant_genus %in% sp_list_small1$plant_genus)

#for genera not in the megatree just randomly pick a species from each
set.seed(9980)
sp_list_small2=gen_out_megatree %>% group_by(plant_genus,plant_family)%>% 
  summarize(plant_species=sample(plant_species,1))


#bind the two dataframes together
sp_list_small=data.frame(bind_rows(sp_list_small1,sp_list_small2) %>%
  select(plant_species,plant_genus,plant_family)) 
sp_list_small %>% filter(plant_species %in% c("Oxytenia_acerosa","Petalostemon_purpureus"))

#double check that all the genera from inat_obs are in sp_list_small
sp_list_small %>% 
  filter(!plant_genus %in% plant_update$plant_genus) #should be empty tibble

# #UNCOMMENT ME TO make the phylogeny
angio_tree =  phylo.maker(sp_list_small,scenarios = 'S2') # make the phylogeny out of the species list
# saveRDS(angio_tree,'modeling_data/phylogney_plant_genera-21march2023.rds')


angio_tree = readRDS('modeling_data/phylogney_plant_genera-21march2023.rds')
scenario2 = angio_tree$scenario.2$run.1

#change the tip.labels to be plant genera
scenario2$tip.label <- sub("_.*","",scenario2$tip.label)


#now we need to calculate phylogenetic diversity of plants visited for each 
# bee species. calculate phylogenetic diversity using hill_phylo
head(globi)
a=globi_tpl %>% split(.$scientificName)
df = a[[1]]

# globi_tpl %>% split(.$scientificName) %>%
#   map_dfr(function(df){
#     plant_sad = df %>% group_by(plant_genus) %>% summarize(n=n())
#     plant_sad %>%
#       pivot_wider(names_from=plant_genus,values_from=n)
#     
#   })

globi_com =data.frame(globi_tpl %>%
  group_by(scientificName,plant_genus) %>%
  summarize(n=n())%>%
  pivot_wider(names_from=plant_genus,values_from=n,values_fill = 0))

globi_matrix = as.matrix(globi_com %>% select(-scientificName),nrow=nrow(globi_com))
row.names(globi_matrix) <- globi_com$scientificName

globi_matrix[1:10,1:5]


#calculate phylo simpson diversity
phylo_simp = hill_phylo(globi_matrix, scenario2, q = 2)
phylo_rich = hill_phylo(globi_matrix, scenario2, q = 0)

###also get plant community composition
horn_dist = vegdist(globi_matrix, method = 'horn')
#make pca fo the horn distance matrix
my_pcoa <- stats:::cmdscale(horn_dist,eig=T)
plot(my_pcoa$points) 

#to check this let's find bees that were mostly visiting Solidago, e.g.
which(colnames(globi_matrix)=='Solidago')
soli_bees = which(globi_matrix[,68] !=0)
soli_prop=globi_matrix[soli_bees,68]/rowSums(globi_matrix[soli_bees,])
really_soli = names(soli_prop[soli_prop>.5])

pcoa_data = data.frame(my_pcoa$points)
colnames(pcoa_data) <- c('eigen1','eigen2')
pcoa_data$scientificName=row.names(pcoa_data)
row.names(pcoa_data) <- NULL
pcoa_data$col = ifelse(pcoa_data$scientificName %in% really_soli,'red','black')

with(pcoa_data, plot(eigen1,eigen2,col=col))


#########
#let's also do at the family-level
globi_com_fam =data.frame(globi_tpl %>%
                        group_by(scientificName,plant_family) %>%
                        summarize(n=n())%>%
                        pivot_wider(names_from=plant_family,values_from=n,values_fill = 0))

globi_fam_matrix = as.matrix(globi_com_fam %>% select(-scientificName),nrow=nrow(globi_com_fam))
row.names(globi_fam_matrix) <- globi_com_fam$scientificName
globi_fam_matrix[1:10,1:5]

###also get plant community composition at family level
horn_dist_fam = vegdist(globi_fam_matrix, method = 'horn')
#make pca fo the horn distance matrix
my_pcoa_fam <- stats:::cmdscale(horn_dist_fam,eig=T)
plot(my_pcoa_fam$points) 

#to check this let's find bees that were mostly visiting Solidago, e.g.
aster_ind = which(colnames(globi_fam_matrix)=='Asteraceae')
aster_bees = which(globi_fam_matrix[,aster_ind] !=0)
aster_prop=globi_fam_matrix[aster_bees, aster_ind]/rowSums(globi_fam_matrix[aster_bees,])
really_aster = names(aster_prop[aster_prop>.8])

pcoa_data_fam = data.frame(my_pcoa_fam$points)
colnames(pcoa_data_fam) <- c('eigen1','eigen2')
pcoa_data_fam$scientificName=row.names(pcoa_data_fam)
row.names(pcoa_data_fam) <- NULL
pcoa_data_fam$col = ifelse(pcoa_data_fam$scientificName %in% really_soli,'red','black')

with(pcoa_data_fam %>% arrange((col)), plot(eigen1,eigen2,col=col))



#make data.frame with phylogenetic richness and div
#(see how they correlate with species-neutral div metrics)

phylo_div = tibble(phylo_rich) %>%
  mutate(scientificName = names(phylo_rich)) %>%
  left_join(tibble(phylo_simp) 
            %>% mutate(scientificName = names(phylo_simp))) %>%
  dplyr::select(scientificName, everything())

#add information about the identity of plant taxa visited
plant_id = phylo_div %>%
  left_join(pcoa_data %>% select(-col) %>% rename(eigen1_plantGenus = eigen1, eigen2_plantGenus=eigen2)) %>% #join with genus-level pcca data
  left_join(pcoa_data_fam %>% select(-col) %>% rename(eigen1_plantFam = eigen1, eigen2_plantFam=eigen2)) #join with family-level pca data
#double check no rows added:
nrow(plant_id)==nrow(phylo_div)

#next get measures of simpson diversity and n
div_genus = globi %>% group_by(scientificName,plant_genus) %>%
  summarize(n=n()) %>%
  summarize(rich_genus = n(),simpson_genus = diversity(n,index='invsimpson'))
div_fam = globi %>% group_by(scientificName,plant_family) %>%
  summarize(n=n()) %>%
  summarize(rich_fam=n(),simpson_fam = diversity(n,index='invsimpson'),sample_size=sum(n)) %>%
  rename(n=sample_size)


div_df = plant_id %>%
  left_join(div_genus) %>%left_join(div_fam)
#again, double check that  no rows were added:
nrow(div_df) == nrow(plant_id)


with(div_df,plot(phylo_simp,simpson_genus))
with(div_df,plot(phylo_simp,simpson_fam))
with(div_df,plot(phylo_simp,phylo_rich)) 
with(div_df,plot(phylo_simp,n))
with(div_df,plot(phylo_rich,rich_genus))
with(div_df,plot(phylo_rich,rich_fam))
with(div_df,plot(phylo_rich,eigen1_plantFam))


# write_csv(div_df %>% rename(n_globi = n),'modeling_data/globi_phyloDiv.csv')

