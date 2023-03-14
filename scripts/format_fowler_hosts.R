rm(list=ls())
library(tidyverse)
library(furrr)
library(vroom)

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

chesshire = bind_rows(chesshire1,add_chesshire)

# fowler considers bees to be specialists if they use pollen from genera in two different families
# and we don't. we're going to remove these bees from the fowler list (and won't consider them discrepancies)
generalists_fowler = c("Andrena candidiformis", "Anthidium mormonum", "Dufourea cuprea",
                       "Habropoda laboriosa", "Hesperapis ilicifoliae", "Megachile perihirta",
                       "Peponapis michelbacherorum",
                       "Perdita fieldi","Perdita obscurata",
                       "Pseudopanurgus virginicus", "Xenoglossa kansensis","Florilegus condignus")


#note: some bees on this list are duplicated, if they occur in multiple regions (eg both eastern and central usa)
fowler <- read_csv("modeling_data/fowler_hostplants.csv") %>%
  mutate(diet_breadth = ifelse(scientificName %in% generalists_fowler,'generalist','specialist')) %>%
  mutate(host_plant_rank = ifelse(grepl('aceae',host_plant) | grepl('ieae',host_plant),'family','genus')) %>%
  mutate(diet_breadth_detailed = ifelse(host_plant_rank == 'family' & diet_breadth =='specialist','family_specialist','genus_specialist')) %>%
  mutate(diet_breadth_detailed = ifelse(diet_breadth=='generalist','generalist',diet_breadth_detailed))

specialists = fowler[fowler$diet_breadth=='specialist',]$scientificName

#some bees have different host plants on the east/west/central lists - we need to make their diet breadths consistent
check_me = fowler %>% distinct(scientificName,diet_breadth,diet_breadth_detailed)
dupes = check_me$scientificName[duplicated(check_me$scientificName)]
check_me %>% filter(scientificName %in% dupes) %>% arrange(scientificName)
fowler %>% filter(scientificName %in% dupes) %>% arrange(scientificName)
change_me = check_me %>% filter(scientificName %in% dupes) %>% arrange(scientificName) %>%
  split(.$scientificName) %>% purrr::map_dfr(function(df){
    new_db='family_specialist'
    db_broad = 'specialist'
    #if('family_generalist'%in% df$diet_breadth_detailed) new_db <- 'family_specialist'
    if('generalist' %in% df$diet_breadth_detailed) {new_db <- 'generalist'; db_broad <- 'generalist'}
    
    data.frame(scientificName = df$scientificName[1],diet_breadth = db_broad,diet_breadth_detailed = new_db)
  })

diet_breadth = check_me %>% filter(!scientificName %in% dupes) %>% bind_rows(change_me)


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
  left_join(fowler %>% distinct(scientificName,host_plant)) %>%
  filter(!scientificName %in% get_host_fowler_fams$scientificName) %>%
  select(scientificName,host_plant,new_host,host_rank) %>%
  bind_rows(host_fams) %>%
  arrange(scientificName)
hosts_long %>% filter(is.na(new_host))
hosts_long %>% filter(scientificName=='Andrena melanochroa')

# check that for species with multiple genera, the genera are all in the same family
# update species names with wfo
# then check genera are in the same fam
update_these_names = with(hosts_long, unique(new_host))

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
  mutate(accepted_family = ifelse(is.na(accepted_family),accepted_name,accepted_family))

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
wfo_name_update = wfo_accepted %>% bind_rows(wfo_syn_update)  %>% bind_rows(not_in_wfo_reformatted) %>%
  rename(old_plant = old_name)

#double check all plants are in the df and that there are no duplicates
wfo_name_update[duplicated(wfo_name_update$old_plant),]
update_these_names[!update_these_names %in% wfo_name_update$old_plant]


fowler_formatted = hosts_long %>% rename(old_plant = new_host) %>%
  left_join(wfo_name_update) %>%
  rename(host=accepted_name,old_name = old_plant, host_string = host_plant,host_family = accepted_family,name_source = source) %>%
  select(-accepted_id,-accepted_genus,-taxonomicStatus,-fam_host) %>%
  select(scientificName,host,host_family,host_rank,name_source,old_name,everything())

nrow(fowler_formatted) == nrow(hosts_long)

#double check that with the name update, no genera in different families
dupes_f = fowler_formatted$scientificName[duplicated(fowler_formatted$scientificName)]
View(fowler_formatted %>% filter(scientificName %in% dupes_f))
#looks like there are none... this vector should be empty:
which(fowler_formatted %>%
  split(.$scientificName) %>% purrr::map_lgl(function(df) n_distinct(df$host_family)!=1))

#next we need to update the bee names using chesshire's methods
fowler_names_toAlign = data.frame(providedName = unique(fowler_formatted$scientificName))

slist = fowler_names_toAlign$providedName
slist[!slist %in% chesshire$providedName]

folwer_updated = fowler_names_toAlign %>%
  left_join(chesshire) %>%
  mutate(finalName = ifelse(is.na(finalName),providedName,finalName)) %>% #for bees not in chesshire et al, just use the provided name
  dplyr::select(-geo_informed)

#now update teh fowler dataframes
fowler_formatted2=fowler_formatted %>% 
  rename(old_bee_name = scientificName) %>%
  left_join(folwer_updated %>% rename(old_bee_name = providedName,scientificName = finalName))
diet_breadth2 = diet_breadth %>% 
  rename(old_bee_name = scientificName) %>%
  left_join(folwer_updated %>% rename(old_bee_name = providedName,scientificName = finalName))


# write_csv(fowler_formatted2,'modeling_data/fowler_formatted-7march2023.csv')
# write_csv(diet_breadth2,'modeling_data/bee_diet_breadth-7march2023.csv')







##old 
#do this using name alignment template
#save file for name alignment on github
fowler_names_toAlign = data.frame(scientificName = unique(fowler_formatted$scientificName))
# write_csv(fowler_names_toAlign,'modeling_data/fowler_toAlign.csv')

#upload the output of the name alignment
fowler_aligned = vroom("modeling_data/fowler_names-aligned.csv")%>%
  mutate(source = ifelse(grepl("ITIS",alignedExternalId),'itis',NA)) %>% #add column with source
  mutate(source = ifelse(grepl("discover",alignedExternalId),'discoverlife',source)) %>%#add column with source
  mutate(source = ifelse(grepl("NCBI",alignedExternalId),'ncbi',source)) #add column with source

#update names - use DL first, followed by itis, followed by col
#first filter just be discover life bees
dl_bees = fowler_aligned %>% filter(source=='discoverlife') %>% distinct(providedName,alignedName)
#add itis bees that are not in the discover life list
itis_bees = fowler_aligned %>% filter(source=='itis' & !providedName %in% dl_bees$providedName)%>% distinct(providedName,alignedName)
#add col bees that are not in the discover life list or the itis list
ncbi_bees = fowler_aligned %>% filter(source=='ncbi' & !providedName %in% c(dl_bees$providedName,itis_bees$providedName))%>% distinct(providedName,alignedName)

name_update = rbind(dl_bees,itis_bees,ncbi_bees)

#check no duplicates
name_update[duplicated(name_update$providedName),]

#any bees missing?
(missing = fowler_names_toAlign %>% filter(!scientificName %in% name_update$providedName)) #these are spelling mistakes
add_me = data.frame(providedName = missing$scientificName,alignedName = c('Andrena osmioides','Melissodes robustior'))

name_update2 = name_update %>% bind_rows(add_me) 

#now update teh fowler dataframes
fowler_formatted2=fowler_formatted %>% 
  rename(old_bee_name = scientificName) %>%
  left_join(name_update2 %>% rename(old_bee_name = providedName,scientificName = alignedName))
diet_breadth2 = diet_breadth %>% 
  rename(old_bee_name = scientificName) %>%
  left_join(name_update2 %>% rename(old_bee_name = providedName,scientificName = alignedName))
  

# write_csv(fowler_formatted2,'modeling_data/fowler_formatted.csv')
# write_csv(diet_breadth2,'modeling_data/bee_diet_breadth.csv')
# 
