rm(list=ls())
library(tidyverse)
library(furrr)
library(vroom)

#and also do genus updates with those, since tpl doesn't have a lot of those
genus_updates = read_csv('modeling_data/wfo_genus_updates.csv')
wfo_fams = read_csv('modeling_data/plant_fam_info.csv')

# need to remove bee species that are generalists according to Avery's list
actually_generalists_df = read_csv("modeling_data/actuallyGeneralists_changeFowler.csv")

#load world flora online data for getting accepted species names
wfo_data = vroom("/Volumes/Seagate/globi_13dec2022/data/WFO_Backbone/classification.txt") %>%
  mutate(scientificName2 = paste(genus,specificEpithet,infraspecificEpithet)) %>% # add a field with a scientific name for subspecies
  mutate(scientificName3 = paste(genus,specificEpithet))

# #load world flora online data - from computer instead of seagate
# wfo_data = vroom("modeling_data/WFO_Backbone/classification.txt") %>%
#   mutate(scientificName2 = paste(genus,specificEpithet,infraspecificEpithet)) %>% # add a field with a scientific name for subspecies
#   mutate(scientificName3 = paste(genus,specificEpithet))

#this genus is not in wfo, and I looked it up manually in catalog of life :
not_in_wfo = data.frame(fowler = c('Salazaria'),
                        accepted = c("Scutellaria"),
                        family = c("Lamiaceae"),
                        source = c('col'))

#chesshire et al data for aligning names
chesshire1 = read_csv('modeling_data/Chesshire2023_nameAlignment-geoUpdate.csv') 
#for final names that also aren't in provided name: add them
add_chesshire = data.frame(providedName = chesshire1$finalName[!chesshire1$finalName %in% chesshire1$providedName]) %>%
  mutate(finalName = providedName)

chesshire = bind_rows(chesshire1,add_chesshire) %>% select(-geo_informed) %>%
  distinct(providedName, finalName)

# fowler considers bees to be specialists if they use pollen from genera in two different families
# and we don't. we're going to remove these bees from the fowler list (and won't consider them discrepancies)
generalists_fowler = c("Andrena candidiformis", "Anthidium mormonum", "Dufourea cuprea",
                       "Habropoda laboriosa", "Hesperapis ilicifoliae", "Megachile perihirta",
                       "Peponapis michelbacherorum",
                       "Perdita fieldi","Perdita obscurata",
                       "Pseudopanurgus virginicus", "Xenoglossa kansensis","Florilegus condignus")
generalists_russell = actually_generalists_df$scientificName
generalists_change = c(generalists_fowler, generalists_russell)

#note: some bees on this list are duplicated, if they occur in multiple regions (eg both eastern and central usa)
fowler <- read_csv("modeling_data/fowler_hostplants.csv") %>%
  mutate(diet_breadth = ifelse(scientificName %in% generalists_change,'generalist','specialist')) %>%
  mutate(host_plant_rank = ifelse(grepl('aceae',host_plant) | grepl('ieae',host_plant),'family','genus')) %>%
  mutate(diet_breadth_detailed = ifelse(host_plant_rank == 'family' & diet_breadth =='specialist','family_specialist','genus_specialist')) %>%
  mutate(diet_breadth_detailed = ifelse(diet_breadth=='generalist','generalist',diet_breadth_detailed))

specialists = fowler[fowler$diet_breadth=='specialist',]$scientificName

#some bees have different host plants on the east/west/central lists - we need to make their diet breadths consistent
check_me = fowler %>% distinct(scientificName,diet_breadth,diet_breadth_detailed)
dupes = check_me$scientificName[duplicated(check_me$scientificName)]
check_me %>% filter(scientificName %in% dupes) %>% arrange(scientificName)
fowler %>% filter(scientificName %in% dupes) %>% arrange(scientificName)
change_me = check_me %>% 
  filter(scientificName %in% dupes) %>% arrange(scientificName) %>%
  split(.$scientificName) %>% purrr::map_dfr(function(df){
    new_db='family_specialist'
    db_broad = 'specialist'
    #if('family_generalist'%in% df$diet_breadth_detailed) new_db <- 'family_specialist'
    if('generalist' %in% df$diet_breadth_detailed) {new_db <- 'generalist'; db_broad <- 'generalist'}
    
    data.frame(scientificName = df$scientificName[1],diet_breadth = db_broad,diet_breadth_detailed = new_db)
  })

diet_breadth = check_me %>% 
  filter(!scientificName %in% dupes) %>% 
  bind_rows(change_me)


get_host_fowler = fowler %>%
  filter(diet_breadth=='specialist') %>%
  mutate(fam_host = host_plant_rank=='family')  %>%
  mutate(new_host = ifelse(fam_host,sub(":.*","",host_plant),host_plant)) %>%
  mutate(new_host = ifelse(new_host =="Cichorieae","Asteraceae",new_host)) %>%
  distinct(scientificName,host_plant,new_host,fam_host) 


#separate family-level specialists from genus-level specialists
#genus level specialists will require special formatting to get their pollen hosts
get_host_fowler_fams = get_host_fowler %>% filter(fam_host)
unique(get_host_fowler_fams$new_host)

fix_me_df = get_host_fowler %>% 
  filter(!fam_host)%>% 
  filter(!scientificName %in% get_host_fowler_fams$scientificName)
fix_me = unique(fix_me_df$host_plant)
get_host_fowler %>% filter(scientificName == 'Andrena melanochroa')

# if there are specialists whose host plants are genus-level in one region but 
# family-level in another, go with the family-level host plant
fix_me_df %>% filter(scientificName %in% get_host_fowler_fams$scientificName)

#let's redo this so that each genus gets its own row..
a_string = fix_me[19]
fix_me[grepl("&",fix_me)]

host_genera = fix_me %>% purrr::map_dfr(function(a_string){
  string_vec = strsplit(a_string,",")[[1]]
  host_vec = 1:length(string_vec) %>% purrr::map_chr(function(i){
    modify_me=string_vec[i]
    if(i ==1) {
      keep_me = sub(" .*","",modify_me)

    }
    if(i != 1 ){
      modify_me
      #get rid of first space after second space
      keep_me = sub(" .*", "",substring(modify_me, 2))

    }
    return(keep_me)
    
  })
  data.frame(host_plant = a_string, new_host = host_vec)
  
}) 

host_fams = get_host_fowler %>% 
  filter(fam_host) %>%
  mutate(host_rank = ifelse(grepl("aceae",new_host),'family','tribe'))
hosts_long = host_genera %>% mutate(host_rank = 'genus') %>%
  left_join(fowler %>% distinct(scientificName,host_plant) %>% filter(scientificName %in% get_host_fowler$scientificName)) %>%
  filter(!scientificName %in% get_host_fowler_fams$scientificName) %>%
  select(scientificName,host_plant,new_host,host_rank) %>%
  bind_rows(host_fams) %>%
  arrange(scientificName)


host_fams
host_genera %>% mutate(host_rank = 'genus') %>%
  left_join(fowler %>% distinct(scientificName,host_plant)) %>% 
  filter(scientificName=="Florilegus condignus")


# check that for species with multiple genera, the genera are all in the same family
# update species names with wfo
# then check genera are in the same fam
update_these_names2 = with(hosts_long, unique(new_host))

## this has all the Taxonstand updates from the globi dataset:
#the code below is  copied and pasted from the name updates for the globi dataset
check_names = read_csv('modeling_data/plant_list_name_update3.csv')

#get just the genus level updates:
checked_genera_is = which(strsplit(check_names$Taxon, ' ') %>% map_lgl(function(vec) length(vec)==1))
checked_genera = check_names$Taxon[checked_genera_is]
update_these_names2[update_these_names2 %in% checked_genera]


tpl_formatted = check_names %>% 
  filter(Taxon %in% update_these_names2) %>%
  rename(plant_species = Taxon,tpl_family = Family) %>% 
  mutate(in_tpl = !is.na(Taxonomic.status) & !Taxonomic.status =='') %>%
  mutate(accepted_name = ifelse(!is.na(New.Species),paste(New.Genus,New.Species),New.Genus) ) %>%
  mutate(accepted_name = ifelse(is.na(accepted_name), plant_species, accepted_name)) %>%
  select(plant_species,accepted_name,in_tpl,Taxonomic.status,tpl_family) 

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

#for the  genera and families not in the "check_names" dataset, use wfo to update
##############
update_these_names = update_these_names2[!update_these_names2 %in% checked_genera]

update_these_names[!update_these_names %in% wfo_data$scientificName]
wfo_filtered = wfo_data %>% filter(scientificName %in% update_these_names)

# 4) filter wfo_filtered to be just accepted species and for all accepted names
# make data frame with old_name = scientificName, accepted_name = scientificName, family = 	wfo_family, and source = wfo
# double check there’s no duplicates
wfo_accepted = wfo_filtered %>% 
  filter(taxonomicStatus=='ACCEPTED') %>%
  mutate(old_name = scientificName, accepted_name=scientificName,source = 'wfo') %>%
  rename(accepted_family = family) %>%
  distinct(old_name,accepted_name,accepted_family,taxonomicStatus,source) %>%
  mutate(accepted_family = ifelse(is.na(accepted_family),accepted_name,accepted_family)) %>%
  mutate(accepted_family = ifelse(accepted_family=='Compositae',"Asteraceae",accepted_family))

#any duplicated names within the accepted species? 
wfo_accepted$accepted_name[duplicated(wfo_accepted$accepted_name)]

#let's get a df with synonyms next
wfo_synonyms = wfo_filtered %>% 
  filter(taxonomicStatus %in% c('SYNONYM',"HETEROTYPICSYNONYM") & !scientificName %in% wfo_accepted$accepted_name) %>%
  mutate(old_name = scientificName, accepted_id=acceptedNameUsageID,source = 'wfo') %>%
  distinct(old_name, accepted_id,taxonomicStatus,source) %>% #for duplicated synonyms we'll just pick one accepted name at random since globi doesn't give authors.
  split(.$old_name) %>%
  purrr::map_dfr(function(df)df[1,])

#any duplicated rows among the synonyms?
wfo_synonyms[duplicated(wfo_synonyms$old_name),]

#add unchecked and doubtful species to the data - there are none in this dataset
wfo_unsure = wfo_filtered %>% 
  filter(taxonomicStatus %in% c("UNCHECKED","DOUBTFUL" ) & !scientificName %in% wfo_accepted$accepted_name & !scientificName %in% wfo_synonyms$old_name) %>%
  mutate(taxonomicStatus = 'UNCHECKED_DOUBTFUL') %>%
  mutate(old_name = scientificName, accepted_name=scientificName,source = 'wfo') %>%
  rename(accepted_family = family) %>%
  distinct(old_name,accepted_name,accepted_family,taxonomicStatus,source) %>%
  mutate(accepted_family = ifelse(is.na(accepted_family),accepted_name,accepted_family)) 
wfo_unsure[duplicated(wfo_unsure$old_name),]

# update the synonyms to accepted species
# get id of the accepted names - make data.frame and renmae scientificName as oldName
# left_join with wfo %>% select(id, scientificName,genus,epithet, family) and rename scientificName to accepted_name
wfo_syn_update = wfo_synonyms %>% left_join(
  wfo_data %>% distinct(taxonID,scientificName,genus,family) %>% 
    rename(accepted_name = scientificName,accepted_genus = genus,accepted_family =family,accepted_id = taxonID)
) %>%
  mutate(accepted_genus = ifelse(is.na(accepted_genus),accepted_name,accepted_genus))


# 6) rbind all the rows together
# 
not_in_wfo_reformatted = not_in_wfo %>%
  rename(old_name = fowler, accepted_name=accepted,accepted_family=family) %>%
  mutate(accepted_genus = sub(" .*","",accepted_name),
         taxonomicStatus = ifelse(old_name != accepted_name,"SYNONYM",'ACCEPTED')) 
wfo_name_update = wfo_accepted %>% bind_rows(wfo_syn_update)  %>% 
  bind_rows(not_in_wfo_reformatted) %>%
  rename(old_plant = old_name)


#double check all plants are in the df and that there are no duplicates
wfo_name_update[duplicated(wfo_name_update$old_plant),]
update_these_names[!update_these_names %in% wfo_name_update$old_plant]


wfo_name_update

#reformat plant_update to be in the same format as wfo_name_update
all_updates = wfo_name_update %>% bind_rows(
  plant_update %>% 
    rename(old_plant = old_pl_name, accepted_name = plant_species, 
           accepted_family = plant_family)
  
)  %>% select(-name_source)

fowler_formatted = hosts_long %>% 
  rename(old_plant = new_host) %>%
  left_join(all_updates) %>%
  rename(host=accepted_name,old_name = old_plant, host_string = host_plant,host_family = accepted_family) %>%
  select(-accepted_id,-accepted_genus,-taxonomicStatus,-fam_host) %>%
  select(scientificName,host,host_family,host_rank,old_name,everything())%>% select(-family_source, -source,-plant_genus)

nrow(fowler_formatted) == nrow(hosts_long)

#double check that with the name update, no genera in different families
dupes_f = fowler_formatted$scientificName[duplicated(fowler_formatted$scientificName)]
#View(fowler_formatted %>% filter(scientificName %in% dupes_f))

#looks like there are none... this vector should be empty:
which(fowler_formatted %>%
        split(.$scientificName) %>% purrr::map_lgl(function(df) n_distinct(df$host_family)!=1))



#next we need to update the bee names using chesshire's methods
fowler_names_toAlign = data.frame(providedName = unique(diet_breadth$scientificName))

slist = fowler_names_toAlign$providedName
slist[!slist %in% chesshire$providedName]

fowler_updated = fowler_names_toAlign %>%
  left_join(chesshire) %>%
  mutate(finalName = ifelse(is.na(finalName),providedName,finalName))  #for bees not in chesshire et al, just use the provided name

#now update the fowler dataframes
fowler_formatted2=fowler_formatted %>% 
  rename(old_bee_name = scientificName) %>%
  left_join(fowler_updated %>% 
              rename(old_bee_name = providedName,scientificName = finalName))
diet_breadth2 = diet_breadth %>% 
  rename(old_bee_name = scientificName) %>%
  left_join(fowler_updated %>% 
              rename(old_bee_name = providedName,scientificName = finalName))

"Florilegus condignus" %in% fowler_formatted2$scientificName
diet_breadth2 %>% filter(scientificName == "Andrena geranii")
diet_breadth2 %>% filter(scientificName == "Florilegus condignus")

# write_csv(fowler_formatted2,'modeling_data/fowler_formatted-30nov2023.csv')
# write_csv(diet_breadth2,'modeling_data/fowler_bee_diet_breadth-30nov2023.csv')





