rm(list=ls())
library(tidyverse)
library(vroom)
library(maps)
library(sf)
library(vegan)

# load the globi data
# exclude interactions of non-US bees
# update the plant family names

##analysis
# for bees on the Jarrod Fowler lists, what % visit their host plants?
# for bees [classified as polylectic in ms dataset, what % visit their host plants?]]
# for bees on the fowler list, how accurate are predictions?
# if we change the sample size how does this change?


# load the globi data
# this is a file with the globi records and plant names are updated (though not plant families)
globi_r = read_csv('modeling_data/globi_occ_names_updated.csv')
globi_r %>% filter(sourceTaxonRank=='variety') %>% distinct(scientificName)
data.frame(globi_r %>% filter(is.na(sourceTaxonRank)) %>% distinct(scientificName))
Senecioneae
#also load the fowler data
#some of these bees have different characterizations despite having the same pollen hosts in both databases
# this is because fowler considers bees to be specialists if they use pollen from genera in two different families
#and we don't. we're going to remove these bees from the fowler list (and won't consider them discrepancies)
generalists_fowler = c("Andrena candidiformis", "Anthidium mormonum", "Dufourea cuprea",
                       "Habropoda laboriosa", "Hesperapis ilicifoliae", "Megachile perihirta",
                       "Peponapis michelbacherorum",
                       "Perdita fieldi","Perdita obscurata",
                       "Pseudopanurgus virginicus", "Xenoglossa kansensis")

#note: some bees on this list are duplicated, if they occur in multiple regions (eg both eastern and central usa)
fowler <- read_csv("modeling_data/fowler_hostplants.csv") %>%
  mutate(diet_breadth = ifelse(scientificName %in% generalists_fowler,'generalist','specialist')) %>%
  mutate(host_plant_rank = ifelse(grepl('aceae',host_plant),'family','genus')) %>%
  mutate(diet_breadth_detailed = ifelse(host_plant_rank == 'family' & diet_breadth =='specialist','family_specialist','genus_specialist')) %>%
  mutate(diet_breadth_detailed = ifelse(diet_breadth=='generalist','generalist',diet_breadth_detailed))

specialists = fowler[fowler$diet_breadth=='specialist',]$scientificName

#some bees have different host plants on the east/west/central lists - we need to make their diet breadths consistent
check_me = fowler %>% distinct(scientificName,diet_breadth,diet_breadth_detailed)
dupes = check_me$scientificName[duplicated(check_me$scientificName)]
fowler %>% filter(scientificName %in% dupes) %>% arrange(scientificName)
change_me = check_me %>% filter(scientificName %in% dupes) %>% arrange(scientificName) %>%
  split(.$scientificName) %>% purrr::map_dfr(function(df){
    new_db='family_specialist'
    db_broad = 'specialist'
    if('family_generalist'%in% df$diet_breadth_detailed) new_db <- 'family_specialist'
    if('generalist' %in% df$diet_breadth_detailed) {new_db <- 'generalist'; db_broad <- 'generalist'}
    
    data.frame(scientificName = df$scientificName[1],diet_breadth = db_broad,diet_breadth_detailed = new_db)
  })

diet_breadth = check_me %>% filter(!scientificName %in% dupes) %>% bind_rows(change_me)


# Let's exclude interactions of non-US bees

#first, load list of US bee species from discover life - the text file is from this link: 
# https://www.discoverlife.org/nh/cl/counts/Apoidea_species.html
usa_bees = read_table('modeling_data/Apoidea_species.txt',col_names=F) %>%
  rename(genus = X1, epithet = X2) %>%
  mutate(scientificName = paste(genus,epithet))

#i don't think the discover life list is complete, 
# so let's also check if these species are US bees by seeing if there
# are at least 5 records in gbif

#get the list of species
slist = unique(sort(globi_r$scientificName))

#remove morpho species and species groups
(sp_remove1 = slist[grepl('sp\\.',slist) | grepl('/',slist) | grepl('aff\\.',slist) | grepl('nr\\.',slist) | grepl('unk1',slist)])
these_are_genera = which(strsplit(slist," ") %>% purrr::map_lgl(function(str_vec) length(str_vec)==1))

sp_remove = c(slist[these_are_genera],sp_remove1)

slist_globi = slist[!slist %in% sp_remove]

american1 = slist_globi[slist_globi %in% usa_bees$scientificName]
plan(multisession, workers=6)
# pull the US occurrence records of these species from gbif
"Ceratina calcarata/dupla"  %in% slist_globi[!slist_globi %in% usa_bees$scientificName]

# bee_occ = slist_globi[!slist_globi %in% usa_bees$scientificName] %>% future_map(function(sci_name){
#   gbif_list = occ_search(scientificName = sci_name,country='US',limit=25,hasCoordinate = T)
# 
#   if(is.null(gbif_list$data)) {
#     return(gbif_list$data)
#   }else{
#     return( gbif_list$data %>% mutate(query=sci_name) %>%
#               dplyr::select(query,everything()))
#   }
# 
# },.options = furrr_options(seed=T))
# #
# saveRDS(bee_occ,'modeling_data/bee_occ_usa.rds')
bee_occ = readRDS('modeling_data/bee_occ_usa.rds')

#to be conservative let's say a bee ha to have at least 5 occurrence records in the US
american_bees_tokeep=which(bee_occ %>% purrr::map_lgl(function(r_obj) {
  
  #bee occurs in america if the object is not null (ie 0 occurrence records in the US)
  condition1 = !is.null(r_obj)
  
  #and there are at least 5 occurrence records
  if(condition1){
    condition2 = nrow(r_obj)>5
  }else{
    condition2 = F
  }
  condition1 & condition2
}))

#get the species names
unknown_bees = slist_globi[!slist_globi %in% usa_bees$scientificName]
unknown_bees[american_bees_tokeep]
american2 = unknown_bees[american_bees_tokeep]

#add to final list, the species that are on jarrod fowler's list
american_sp_keep = c(american1, american2) 
globi_usa = globi_r %>% filter(scientificName %in% american_sp_keep)

# update the plant family names using world checklist for vascular plants
# get_fams = unique(globi_usa$plant_genus) %>% future_map(function(sciname){
#   output = search_wcvp(sciname, filters=c("families", "accepted"),limit = 1)
#   output_df = tidy(output)
#   
#   if(nrow(output_df) !=0){
#     return_df = data.frame(genus = sciname,family = output_df$family)
#     
#   }
#   else{
#     output = search_wcvp(sciname, filters=c("families"),limit = 1)
#     df=tidy(output)
#     
#     if(nrow(df) !=0){
#       new_output_df = df$synonymOf[[1]]
#       return_df = data.frame(genus = sciname,family = new_output_df$family)
#       
#     }else{
#       return_df = data.frame(genus=sciname,family = NA)
#     }
#   }
# }) %>% bind_rows
# saveRDS(get_fams,'modeling_data/globi_plant_fams.rds')
get_fams = readRDS("modeling_data/globi_plant_fams.rds")

#get_fams manually for things that are in the world vascular checklist (note some are tribes not genera)
globi_r %>% filter(plant_genus =='Sarracenia') %>% select(referenceCitation)
globi_r %>% filter(plant_genus =='Geniculiflora') %>% select(referenceCitation)

(still_need_fams = get_fams %>% filter(is.na(family)))
add_fam_info = data.frame(genus = c('Heliantheae',"Dithyraea","Sarracenia","Senecioneae","Eupatorieae","Geniculiflora","Pseudoveronica","Althea","Eleagnus"),
                          family = c('Asteraceae',"Brassicaceae","Sarraceniaceae",'Asteraceae',"Asteraceae",NA,'Plantaginaceae','Malvaceae',"Elaeagnaceae"))

get_fams_final = get_fams %>% 
  filter(!is.na(family)) %>% filter(genus %in% add_fam_info$genus==F) %>% 
  bind_rows(add_fam_info)
globi_u=globi_usa %>% 
  left_join(get_fams_final %>%
              rename(plant_genus = genus, wcvp_family=family)) 
nrow(globi_u) == nrow(globi_usa)


# for bees on the fowler list, how accurate are predictions and how does this change with N?

# start with threshold of 20 observations
threshold_n = 20

bigN_bees=globi_u %>% group_by(scientificName) %>% summarize(n=n()) %>% filter(n>=threshold_n)
globi_summ = globi_u%>% filter(scientificName %in% bigN_bees$scientificName) %>%
  group_by(scientificName,bee_family,plant_genus,plant_family) %>% summarize(n_genus=n()) 

#predict whether a bee is a specialist or a generalist
#calculate the number of plant families each bee species is observed visiting
total_obs = bigN_bees

# degree_by_fam 
degree_by_fam = globi_summ %>% 
  group_by(scientificName,plant_family) %>%
  summarize(n_family = sum(n_genus)) %>%
  summarize(degree_family = n_distinct(plant_family), 
            degree5_family = sum(n_family >= 5), 
            simpson = diversity(n_family,index='invsimpson'))# %>%

diversity(x, index = "simpson")

#calculate the number of plant genera each bee species is observed visiting
degree_by_genus = globi_summ %>%
  group_by(scientificName,bee_family) %>%
  summarize(degree_genus = n_distinct(plant_genus), 
            degree5_genus = sum(n_genus >= 5), weighted_degree_genus = diversity(n_genus,index='invsimpson')) 


globi_degree = degree_by_fam %>% left_join(degree_by_genus) %>% 
  left_join(diet_breadth %>% 
              distinct(scientificName,diet_breadth,diet_breadth_detailed)) %>%
  mutate(diet_breadth = ifelse(is.na(diet_breadth),'generalist',diet_breadth),
         diet_breadth_detailed = ifelse(is.na(diet_breadth_detailed),'generalist',diet_breadth_detailed))

nrow(degree_by_fam) == nrow(globi_degree)
with(globi_degree,boxplot(simpson~diet_breadth_detailed))
with(globi_degree,boxplot(simpson~diet_breadth))

# write_csv(globi_degree,'modeling_data/globi_degree.csv')







##old
str(globi)
globi %>% distinct(localityName,localityId)
nrow(globi)

globi %>% filter(is.na(decimalLatitude)) %>% distinct(sourceTaxonSpeciesName)

# filter for interactions that are only in the US
#first make a map of the us
usa <- map_data("usa")
usa_map <- ggplot() + geom_polygon(data = usa, 
                                 aes(x=long, y = lat, group = group), 
                                 fill = "white", 
                                 color="black")+
  coord_fixed(xlim = c(-130, -85),  ylim = c(15, 60), ratio = 1.2)
#add the globi data to the map
usa_map+
  geom_point(data=globi %>% filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)), aes(x=decimalLongitude, y=decimalLatitude), 
             color = adjustcolor("red",.6), shape=16, size=1)

#
states_sf <- sf::st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  st_transform(roads, crs = 4269)
usa_sf <- st_as_sf(usa,coords = c("long", "lat"),crs=4269) %>%
  st_transform(roads, crs = 4269)
  
globi_sf=globi %>% filter(!is.na(decimalLatitude) & !is.na(decimalLongitude)) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), # note x goes first
                   crs = 4269)

globi_usa <- st_intersects(globi_sf,usa_sf)
ggplot()+geom_sf(globi_usa)
