rm(list = ls())
library(tidyverse)
library(readxl)
library(furrr)
library(rgbif)

select = dplyr::select; map = purrr::map; rename = dplyr::rename

#load the missouri state data
ms <- read_xlsx('modeling_data/DATA_pollen-diet-breadth_list_12-5-21_ALR-cleaned.xlsx', sheet = 3) %>%
  rename_all(tolower) %>%
  mutate(scientificName = paste0(genus, ' ',sub('.*\\. ','',species))) %>%
  select(scientificName,everything()) %>%
  rename(diet_breadth=`redefined pollen diet breadth`) %>%
  mutate(pollen_host = `pollen hosts`) %>%
  filter(!is.na(pollen_host))


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
  mutate(diet_breadth_fowler = ifelse(scientificName %in% generalists_fowler,'generalist','specialist'))



#any bees in the fowler list not in the missouri state list?
data.frame(fowler %>% filter(!scientificName %in% ms$scientificName)) #yes
fowler %>% filter(!scientificName %in% ms$scientificName) %>% distinct(scientificName)
fowler_not_ms = fowler %>% filter(!scientificName %in% ms$scientificName)

#any bees classified differently between the two list
# ie classified as specialists by fowler but polylectic by missouri state
data.frame(ms %>% 
  filter(diet_breadth == 'Polylectic' & scientificName %in% fowler$scientificName[fowler$diet_breadth_fowler=="specialist"])) 
fowlerSpec_msGen = ms %>% 
  filter(diet_breadth == 'Polylectic' & scientificName %in% fowler$scientificName[fowler$diet_breadth_fowler=="specialist"])


# (north american) bees on the missouri state list that are classified as specialists and 
# not on jarrod fowlers list
(not_on_fowler = ms %>% 
  filter(diet_breadth != "Polylectic" & !scientificName %in% fowler$scientificName)) #the handful of these I checked don't appear to be north american


#load list of US bee species from discover life - the text file is from this link: 
# https://www.discoverlife.org/nh/cl/counts/Apoidea_species.html

usa_bees = read_table('modeling_data/Apoidea_species.txt',col_names=F) %>%
  rename(genus = X1, epithet = X2) %>%
  mutate(scientificName = paste(genus,epithet))

#any bees on the jarrod fowler list that are not in the list of usa bees?
fowler_notinUS = data.frame(fowler %>% filter(!scientificName %in% usa_bees$scientificName)) #yes
ms_notinUS = data.frame(ms %>% filter(!scientificName %in% usa_bees$scientificName)) #yes

not_on_fowler %>% filter(scientificName %in% usa_bees$scientificName)


#i don't think the discover life list is complete, 
# so let's also check if these species are US bees by seeing if there
# are at least 5 records in gbif

#get the list of species
slist = unique(sort(ms_notinUS$scientificName))

#remove morpho species
rm_list = c("sp ",as.character(1:10)) %>% purrr::map(function(numb) which(grepl(numb,slist)) )
(sp_remove = c(slist[rm_list %>% unlist],"NA NA","Megachile sp","Exoneura sp","Dialictus sp","Augochlora sp.")   )
slist[grepl(' sp',slist)]
slist_ms = slist[!slist %in% sp_remove]


plan(multisession, workers=6)
#pull the US occurrence records of these species from gbif
bee_occ = slist_ms %>% future_map(function(sci_name){
  gbif_list = occ_search(scientificName = sci_name,country='US',limit=25,hasCoordinate = T)
  
  if(is.null(gbif_list$data)) {
    return(gbif_list$data)
  }else{
      return( gbif_list$data %>% mutate(query=sci_name) %>%
                dplyr::select(query,everything()))
    }
     
  },.options = furrr_options(seed=T))

#to be conservative let's say a bee ha to have at least 5 occurrence records in the US
american_bees_tokeep_ms=which(bee_occ %>% purrr::map_lgl(function(r_obj) {
  
  #bee occurs in america if the boject is not null (ie 0 occurrence records in the US)
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
american_sp2 = slist_ms[american_bees_tokeep_ms]
not_american = c("Bombus pascuorum","Lasioglossum punctatissimum") #these don't look american based on discvoer life data

#add to final list, the species that are on jarrod fowler's list
american_sp_keep = c(american_sp2[!american_sp2 %in% not_american],slist[slist %in% fowler$scientificName])

#filter the missouri state data to just be american bees
ms_usa = ms %>% filter(scientificName %in% c(usa_bees$scientificName,american_sp_keep))

#how many specialists are there in fowler and ms list combined?
ms_specialists = ms_usa %>% filter(diet_breadth %in% c("Oligolectic","Monolectic"))
fowler_specialists = fowler %>% filter(diet_breadth_fowler=='specialist')
(how_many = n_distinct(c(ms_specialists$scientificName,fowler_specialists$scientificName)))

#how many are on both lists
on_both = unique(intersect(ms_specialists$scientificName,fowler_specialists$scientificName))
(n_on_both=n_distinct(on_both))

fowler$scientificName[!fowler$scientificName %in% on_both]
ms_specialists$scientificName[!ms_specialists$scientificName %in% on_both]

(percent_consistent = round(n_on_both/how_many*100,1))

#
#how many are specialists on the misouri state list but not on fowler?
paste0("Of the ",how_many, " specialist bee species that was on at least one list, ",n_on_both," or approximately ",percent_consistent,"% were on both lists")
specialists_ms_notFowler = ms_specialists$scientificName[!ms_specialists$scientificName %in% fowler$scientificName]
n_specialistsMSnotF = n_distinct(specialists_ms_notFowler)

# how many specialists classified as generalists by missouri state that are 
# specialists according to fowler?
ms_generalists = ms_usa %>% filter(diet_breadth == 'Polylectic')
generalistsMSonF = ms_generalists$scientificName[ms_generalists$scientificName %in% fowler$scientificName]
n_generalistsMSonF = n_distinct(generalistsMSonF)

#any bees on fowler list that are not on the missouri state list?
specialistsMSnotF = unique(fowler$scientificName[!fowler$scientificName %in% ms$scientificName])
(n_specialistsMSnotF = n_distinct(specialistsMSnotF))

# make a table with all these species that have discrepancies
# columns: species name, classification fowler, classification MS, host plants Fowler, 
# host plants MS
fowler %>% filter(!scientificName %in% ms$scientificName)


#tables I need
ms_not_fowler =  ms_usa %>% 
  filter(!scientificName %in% fowler$scientificName & diet_breadth %in% c("Monolectic","Oligolectic"))

discrepancies <- fowlerSpec_msGen %>% mutate(This_ms = "generalist",Fowler = 'specialist') %>% 
  bind_rows(
      ms_not_fowler %>% mutate(This_ms = "specialist",Fowler = 'not on list')
    ) %>% select(scientificName, This_ms, Fowler, "pollen hosts") %>%
  rename("Hosts (this ms)" = 'pollen hosts')

# i still need to add a column with the fowler hosts
# and to add the species on the fowler list that are not on the ms list
add_me = fowler_not_ms %>% 
  mutate("Hosts (Fowler)" = sub(" .*","",sub(":.*","",host_plant)))%>% 
  mutate(This_ms = "not on list",Fowler = 'specialist')
head(fowler)
get_host_fowler = fowler %>% filter(scientificName %in% discrepancies$scientificName ) %>% 
  mutate(new_host = ifelse(grepl('aceae',host_plant),sub(":.*","",host_plant),host_plant)) %>%
  select(scientificName,host_plant,new_host)
fix_me_df = get_host_fowler %>% filter(!grepl('aceae',host_plant))
fix_me = unique(fix_me_df$host_plant)
a_string = "Cercis L., Lupinus L., Vaccinium L."   

fix_me[4]

host_names_formatted = fix_me %>% purrr::map_dfr(function(a_string){
  
  if(!a_string %in% c("Asteraceae, Fabaceae",'Asteraceae?, Fabaceae?')){
    string_vec = strsplit(a_string,"\\.")
    
    for(i in 1:length(string_vec[[1]])){
      
      if(i ==1) {
        keep_this = sub(" .*","",string_vec[[1]][i])
        new_string=keep_this
        
      }
      if(i != 1 & !grepl("&",string_vec[[1]][i])){
        #get rid of name after second space
        keep_this = sub("^(\\S*\\s+\\S+).*", "\\1",string_vec[[1]][i])
        new_string = paste0(new_string,keep_this)
        
      }
      
    }
  }else{
    new_string = a_string
  }
  
  return(data.frame(host_plant = a_string,new_host2 = new_string))

  }) #%>% unlist

#these three dfs need to be combined together
fowler_name_update = get_host_fowler %>% distinct(scientificName,host_plant,new_host) %>% 
  left_join(host_names_formatted) %>% 
  distinct(scientificName,host_plant,new_host,new_host2) %>%
  mutate(new_host2 = ifelse(is.na(new_host2),new_host,new_host2)) %>%
  mutate(new_host2 = ifelse(new_host2 == "Cryptantha ex Don",'Cryptantha',new_host2)) %>%
  distinct(scientificName,new_host2) 
#double check that no bee names are duplicated
fowler_name_update%>%
  filter(duplicated(scientificName))
data.frame(fowler_name_update)

#combine fowler_name_update with discrepancies
fowler_missing_msFormatted = add_me %>% mutate("Hosts (this ms)" = NA) %>% select(scientificName,This_ms,Fowler,"Hosts (this ms)","Hosts (Fowler)")
nrow(discrepancies) == nrow(discrepancies %>% left_join(fowler_name_update)) #double check that joining doesn't duplicate any rows
nrow(discrepancies)

discrepancies_final = discrepancies %>% left_join(fowler_name_update %>% rename("Hosts (Fowler)" = new_host2)) %>% 
  bind_rows(fowler_missing_msFormatted)

# write_csv(discrepancies_final,'modeling_data/discrepancies_Fowler_MS.csv')

ms %>% filter(`cited pollen diet breadth`=="Polylectic" & diet_breadth != "Polylectic")
#let's reformat the ms data so that it's just specialists and each host-plant is a row
# and there's info about whether it's at the family level or genus,
# and if it's genus, what family it's a part of
ms_usa %>%
  filter(is.na("pollen hosts"))
ms_hosts = ms_usa %>% 
  filter(diet_breadth !='Polylectic') %>%
  distinct(pollen_host)

which(ms_hosts_arranged$pollen_host == "Larrea, Larrea tridentata")
a=ms_hosts %>% split(.$pollen_host)
(my_row = a[[604]])
(my_row = a[[10]])

my_row = ms_hosts %>% filter(grepl('Boykinia',pollen_host))

ms_hosts_formatted= ms_hosts %>% split(.$pollen_host) %>% 
  purrr::map_dfr(function(my_row){
    host_string = as.character(my_row$pollen_host)
    hosts = strsplit(host_string,", ")[[1]] #remove commas and spaces
    #hosts[substr(hosts,1,1)== " "]
    
    #if anything is full species get rid of the epithet
    hosts_no_species = sub(" .*","",hosts)

    #get rid of anything that's duplicated
    hosts_final = unique(hosts_no_species)
    
    #return as a dataframe
    data.frame(pollen_host_vec = host_string, host = hosts_final)
    }) %>%
  mutate(rank = ifelse(grepl('aceae',host),'family','genus'))
ms_hosts_formatted %>% filter(grepl('tridentata',host))
ms_hosts_formatted %>% filter(grepl('greggii,',host))
ms_hosts_formatted %>% filter(grepl('Physalis?',host))
"Helinathus"
Hymenopappus.
Boltonia.

unique(ms_hosts_formatted$host)

# #use taxonstand to check for mis-spellings
# library(Taxonstand)
host_genera = unique(ms_hosts_formatted[ms_hosts_formatted$rank == 'genus',]$host)
# check_spelling = TPL(host_genera)
# check_spelling %>% filter(Typo) %>% dplyr::select(Taxon,New.Genus)

# #get families of genera
library(taxize)
# divide_by = ceiling(length(host_genera)/6)
# tax_name(sci='Acer', get="family",messages=T,ask=F)
# 
# 1:6 %>% purrr::map(function(i) indices = ((i-1)*divide_by+1):(i*divide_by))
# plan(multisession,workers=6)
# get_fams_ls = 1:6 %>% future_map((function(i){
#   
#   indices = ((i-1)*divide_by+1):(i*divide_by)
#   sci_names = host_genera[indices]
#   get_fams = tax_name(sci=sci_names[!is.na(sci_names)], get="family",messages=T,ask=F)
#   return(get_fams)
# 
#   }),.options = furrr_options(seed=T))
# saveRDS(get_fams_ls,'modeling_data/ms_plant_fams.rds')
get_fam_ls = readRDS('modeling_data/ms_plant_fams.rds')
find_elsewhere = get_fam_ls %>% bind_rows %>% filter(is.na(family))

get_fams_again = tax_name(sci = find_elsewhere$query,get = 'family',db = 'ncbi',division_filter = "eudicots")

still_need = get_fams_again %>% filter(is.na(family))
Taxonstand::TPL('Acer rubrum')


get_fams_tstand = Taxonstand::TPL(still_need$query)

#
clist = read_table("modeling_data/wcvp_v9_jun_2022 copy.txt")
devtools::install_github("barnabywalker/kewr")
library("kewr")
poa= search_wcvp("Poa")

download_wcvp()
poa$results[[1]]
clist[10908,]

clist[10909,]
clist[10910,]
clist[10911,]

download_wcvp()



