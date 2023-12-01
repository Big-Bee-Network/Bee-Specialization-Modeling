rm(list=ls())
library(tidyverse)
library(vroom)
library(readxl)

#load world flora online data for getting accepted species names
wfo_data = vroom("/Volumes/Seagate/globi_13dec2022/data/WFO_Backbone/classification.txt") %>%
  mutate(scientificName2 = paste(genus,specificEpithet,infraspecificEpithet)) %>% # add a field with a scientific name for subspecies
  mutate(scientificName3 = paste(genus,specificEpithet))
#this genus is not in wfo, and I looked it up manually in catalog of life :
not_in_wfo = data.frame(fowler = c('Salazaria'),
                        accepted = c("Scutellaria"),
                        family = c("Lamiaceae"),
                        source = c('col'))

#and also do genus updates with those, since tpl doesn't have a lot of those
genus_updates = read_csv('modeling_data/wfo_genus_updates.csv')
wfo_fams = read_csv('modeling_data/plant_fam_info.csv')

#load list of Avery's specialists not on fowler list
(diet_breadth_russell <- read_excel("modeling_data/specialistsGeneralists_needRefs 11-27-2023.xlsx") %>%
    mutate(scientificName =sub("_", " ", scientificName)))
(diet_breadth_fowler <- read_csv('modeling_data/bee_diet_breadth-28june2023.csv') %>%
    mutate(scientificName = ifelse(is.na(scientificName), old_bee_name, scientificName)) %>%
    filter(!scientificName %in% diet_breadth_russell$scientificName))

#pull out the specialists from the Russell data
need_update = diet_breadth_russell %>% filter(diet_breadth=='specialist')

#load host data and filter
hosts = read_csv("modeling_data/DATA_bee-taxa-known-hosts_all-plant-taxa_11-27-2023.csv") %>%
  mutate(scientificName = sub("_", " ", bee)) %>%
  filter(scientificName %in% need_update$scientificName)


#double check none of the specialists on the russell list are on the fowler list
diet_breadth_russell %>% filter(scientificName %in% diet_breadth_fowler$scientificName) #nope

#check if any 'specialists' have host plants from multiple families
hosts %>% split(.$scientificName) %>% map(function(df){
  if(nrow(df) != 1){
    count_fams = n_distinct(df$family)
    if(count_fams>1) {my_return = df$scientificName[1]}
    else{my_return = NULL}
  
  }else{
    my_return = NULL
  }
  return(my_return)
}) %>% unlist
#lets look at these two bees more closely
hosts %>% filter(scientificName == 'Megachile circumcincta')
hosts %>% filter(scientificName == 'Hylaeus nelumbonis')


#make data frame with the host plants of these specialists
#exclude the two bees with multiple host plant families
hosts_short = hosts %>% split(.$scientificName) %>% map(function(df){
  if(nrow(df) == 1){
    
    #if a bee has one row and genus is not NA its a genus level specialist
    if(!is.na(df$genus)) new_df = data.frame(scientificName = df$scientificName, 
                                             host = df$genus, 
                                             diet_breadth_detailed = 'genus_specialist')
    
    #if a bee has one row and genus is NA its a family level specialist
    if(is.na(df$genus)) new_df = data.frame(scientificName = df$scientificName, 
                                            host = df$family, 
                                            diet_breadth_detailed = 'family_specialist')
    
  }else{
    #if a bee has more than one row its a family level specialist 
    # double check and make sure there's only one family.
    
    if(n_distinct(df$family)>1) {new_df <- NULL}
    else{
      new_df = data.frame(scientificName = df$scientificName[1], 
                          host = df$family[1], 
                          diet_breadth_detailed = 'family_specialist')
      
    } 
    
  }
  
  return(new_df)
  
})%>% bind_rows


#now update the names of hosts_short
# check that for species with multiple genera, the genera are all in the same family
# update species names with wfo
# then check genera are in the same fam
update_these_names2 = with(hosts_short, unique(host))


## this has all the Taxonstand updates from the globi dataset:
#the code below is  copied and pasted from the name updates for the globi dataset
check_names = read_csv('modeling_data/plant_list_name_update3.csv')

#get just the genus level updates:
checked_genera_is = which(strsplit(check_names$Taxon, ' ') %>% map_lgl(function(vec) length(vec)==1))
checked_genera = check_names$Taxon[checked_genera_is]
update_these_names2[update_these_names2 %in% checked_genera]
update_these_names2[!update_these_names2 %in% checked_genera]


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

#any changes
all_updates %>% filter(old_plant != accepted_name)

russell_formatted = hosts_short %>% 
  rename(old_plant = host) %>%
  left_join(all_updates) %>%
  rename(host=accepted_name,old_name = old_plant, host_family = accepted_family) %>%
  select(-accepted_id,-accepted_genus,-taxonomicStatus,-source) %>%
  mutate(host_rank = ifelse(grepl('family',diet_breadth_detailed),'family','genus'  )  ) %>%
  select(scientificName,host,host_family,host_rank,old_name,everything()) %>% select(-family_source, -plant_genus)

nrow(russell_formatted) == nrow(hosts_short)

# any NAs
russell_formatted %>% filter(is.na(scientificName) | is.na(host))

#double check that with the name update, no genera in different families
dupes_f = russell_formatted$scientificName[duplicated(fowler_formatted$scientificName)]

#looks like there are none... this vector should be empty:
which(russell_formatted %>%
        split(.$scientificName) %>% purrr::map_lgl(function(df) n_distinct(df$host_family)!=1))



 write_csv(russell_formatted,'modeling_data/russell_formatted-30nov2023.csv')





