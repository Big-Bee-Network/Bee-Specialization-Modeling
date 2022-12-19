rm(list=ls())
library(tidyverse)
library(furrr)

#load world flora online data for getting accepted species names
wfo_data = vroom("/Volumes/Seagate/globi_13dec2022/data/WFO_Backbone/classification.txt") %>%
  mutate(scientificName2 = paste(genus,specificEpithet,infraspecificEpithet)) %>% # add a field with a scientific name for subspecies
  mutate(scientificName3 = paste(genus,specificEpithet))

# #load world flora online data - from computer instead of seagate
# wfo_data = vroom("modeling_data/WFO_Backbone/classification.txt") %>%
#   mutate(scientificName2 = paste(genus,specificEpithet,infraspecificEpithet)) %>% # add a field with a scientific name for subspecies
#   mutate(scientificName3 = paste(genus,specificEpithet))

#these genus is not in wfo, and I looked it up manually in catalog of life :
not_in_wfo = data.frame(fowler = c('Salazaria'),
                        accepted = c("Scutellaria"),
                        family = c("Lamiaceae"),
                        source = c('col'))

# fowler considers bees to be specialists if they use pollen from genera in two different families
#and we don't. we're going to remove these bees from the fowler list (and won't consider them discrepancies)
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
# update species names with taxonstand
# update family names with wvcp?
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
write_csv(fowler_formatted,'modeling_data/fowler_formatted.csv')


##old
# fowler_genus_update = Taxonstand::TPL(update_these_names)
# fowler_genus_update %>% select(Genus,New.Genus) %>% filter(Genus != New.Genus) #no changes

#get families for genera 
# update the plant family names using world checklist for vascular plants
plan(multisession,workers=6)
get_fams = update_these_names %>% future_map(function(sciname){
  
  output = search_wcvp(sciname, filters=c("families", "accepted"),limit = 1)
  output_df = tidy(output)

  if(nrow(output_df) !=0){
    return_df = data.frame(new_host = sciname,family = output_df$family)
  }else{
    output = search_wcvp(sciname, filters=c("families"))
    df=tidy(output)

    if(nrow(df) !=0 & n_distinct(df$family)==1){
      new_output_df = df$synonymOf[[1]]
      return_df = data.frame(new_host = sciname,family = new_output_df$family)

    }
    if(nrow(df) !=0 & n_distinct(df$family)!=1){
      df_sum = df %>% group_by(family) %>% summarize(n=n()) %>% arrange(desc(n))
      return_df = data.frame(new_host = sciname,family = df_sum$family[1])
      
      }
    if(nrow(df)==0){
      return_df = data.frame(new_host=sciname,family = NA)
    }
  }
  return(return_df)

  }) %>% bind_rows
get_fams %>% filter(new_host == "Horkelia")

fowler_formatted = hosts_long %>% left_join(get_fams) %>%
  mutate(family = ifelse(host_rank == 'family',new_host,family)) %>%
  mutate(family = ifelse(host_rank == 'tribe',new_host,family)) %>%
  rename(host=new_host,host_string = host_plant)
head(fowler_formatted)

#double check there's no genera in different families
fowler_list = fowler_formatted %>% split(.$scientificName) 
a = fowler_formatted %>% split(.$scientificName)
df = a[[1]]
check_these = which(fowler_formatted %>% split(.$scientificName) %>% map_lgl(function(df) length(unique(df$host))>1))
generalists_i_think = which(check_these %>% purrr::map_dbl(function(i){
  n_distinct(fowler_list[[i]]$family)
}) >1)

check_these[generalists_i_think] %>% purrr::map(function(i) fowler_list[[i]])
fowler_formatted %>% filter(scientificName ==  'Andrena melanochroa')

# for bees with multiple genera double check they're in the same family
# write_csv(fowler_formatted,'modeling_data/fowler_formatted-19dec2022.csv')
# write_csv(diet_breadth,'modeling_data/bee_diet_breadth.csv')
