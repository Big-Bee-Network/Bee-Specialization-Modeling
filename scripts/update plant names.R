rm(list=ls())
#update plant names for both globi data and fowler data using catalog of life
library(vroom)
library(tidyverse)

#load the data
options(max.print=1000000)
interactions_b <- vroom('modeling_data/interactions-14dec2022.csv') #note this is non-filtered data (includes non-usa & non-native bees)
fowler <- read_csv("modeling_data/fowler_hostplants.csv") 


plant_df = interactions_b %>%
  distinct(plant_species)
test_me = plant_df$plant_species

#load world flora online data
wfo_data = vroom("/Volumes/Seagate/globi_13dec2022/data/WFO_Backbone/classification.txt") %>%
  mutate(scientificName2 = paste(genus,specificEpithet,infraspecificEpithet)) %>% # add a field with a scientific name for subspecies
  mutate(scientificName3 = paste(genus,specificEpithet))

# #load world flora online data
# wfo_data = vroom("modeling_data/WFO_Backbone/classification.txt") %>%
#   mutate(scientificName2 = paste(genus,specificEpithet,infraspecificEpithet)) %>% # add a field with a scientific name for subspecies
#   mutate(scientificName3 = paste(genus,specificEpithet))


#these species were not in wfo, and I looked them up manually in catalog of life or itis:
not_in_wfo = data.frame(globi_name = c("Spiranthes arcisepala","Lisianthus skinneri","Cuphea viscossima","Aphanostephus riddellii","Jacquemontia curtissii","Lathyrus cabrerianus","Oncosiphon suffruticosum","Spergularia fasciculata","Ageratina theifolia","Salvia palifolia","Callianthe striata","Rabelera holostea","Kewa salsoloides","Doellingeria vialis","Antirrhinum thompsonii","Ipomoea cuneata","Oncosiphon piluliferum","Quadrella indica","Baccharis glabrata","Euphorbia bourgaeana","Austroeupatorium inulifolium","Cirsium nuttallii","Salvia polystachia","Erythranthe cardinalis","Senna marilandica","Sabulina michauxii","Indigofera rautanenii" ),
                        accepted = c("Spiranthes arcisepala","Lisianthus skinneri","Cuphea viscosissima","Aphanostephus riddellii","Jacquemontia curtissii","Lathyrus cabrerianus","Oncosiphon suffruticosum","Spergularia fasciculata","Ageratina theifolia","Salvia palifolia","Callianthe striata","Rabelera holostea","Kewa salsoloides","Doellingeria vialis","Sairocarpus multiflorus","Ipomoea cuneata","Oncosiphon piluliferum","Quadrella indica","Baccharis glabrata","Euphorbia bourgaeana","Austroeupatorium inulifolium","Cirsium nuttallii","Salvia polystachia",'Mimulus cardinalis',"Senna marilandica","Sabulina michauxii","Indigofera rautanenii"),
                        family=c("Orchidaceae","Gentianaceae","Lythraceae","Asteraceae","Convolvulaceae","Fabaceae","Asteraceae",'Caryophyllaceae','Asteraceae','Lamiaceae','Malvaceae',"Caryophyllaceae","Kewaceae","Asteraceae", "Plantaginaceae","Convolvulaceae","Asteraceae","Capparaceae","Asteraceae","Euphorbiaceae","Asteraceae",'Asteraceae',"Lamiaceae",'Phrymaceae','Fabaceae','Caryophyllaceae','Fabaceae'),
                        source= c("col",'col',"itis","itis","itis","col","col","col","col",'col',"col",'col',"col","col", "col","col","itis",'itis',"col",'itis',"col",'itis',"col",'itis',"itis",'col','itis'))
# write_csv(not_in_wfo,'modeling_data/plant_search1.csv')


### 2) update spelling mistakes in globi - new data.frame is called 'interactions_updated'
# first let's look at the taxa that are not in the data to try to catch any spelling mistakes:
# filter the wfo data to just be species that are in the globi data
wfo_filtered = wfo_data %>% filter(scientificName %in% test_me | scientificName2 %in% test_me | scientificName3 %in% test_me)
#look at the taxa that are not in the data:
(missing_plants = test_me[!(test_me %in% wfo_filtered$scientificName|test_me %in% wfo_filtered$scientificName2 | test_me %in%wfo_filtered$scientificName3) ])

different_naming = data.frame(globi_name = test_me[test_me %in% wfo_filtered$scientificName2])
change_formatting = wfo_filtered[wfo_filtered$scientificName2 %in% different_naming$globi_name,] %>%
  rename(globi_name = scientificName2,wfo_name = scientificName) %>% select(globi_name,wfo_name)%>%
  split(.$globi_name) %>%
  purrr::map_dfr(function(df) df[1,])#for duplicates - just pick the first species (since will ge genus-level analysis anyway)


#these are plants that are only sub-species or hybrids in the wfo data, but are species-level in ours
different_naming2 = data.frame(globi_name = test_me[test_me %in% wfo_filtered$scientificName3 & !test_me %in% c(wfo_filtered$scientificName,wfo_filtered$scientificName2)])
change_formatting2 = wfo_filtered[wfo_filtered$scientificName3 %in% different_naming2$globi_name,] %>%
  rename(globi_name = scientificName3,wfo_name = scientificName) %>% select(globi_name,wfo_name) %>%
  split(.$globi_name) %>%
  purrr::map_dfr(function(df) df[1,])#for duplicates - just pick the first species (since will ge genus-level analysis anyway)

#for subspecies that aren't in the data, check if the species is in the data
#code for removing all after second space:
chop_off_word3 = sub("^\\s*(\\S+\\s+\\S+).*", "\\1", missing_plants)
change_formatting3 = data.frame(globi_name = missing_plants,wfo_name = chop_off_word3) %>%
  filter(wfo_name %in% wfo_data$scientificName & !globi_name %in% c(change_formatting$globi_name,change_formatting2$globi_name)) 
# these_in_data = change_formatting3[chop_off_word3 %in% wfo_data$scientificName,]$old_name

#(missing_plants2 = missing_plants[!(missing_plants %in% these_in_data | missing_plants %in% rm_me | missing_plants %in% change_spelling$old_spelling)]) #update missing plants list so these species are not incldued




#some of these might be spelling issues, change spelling for hybrids as well
# first look at hybrids 
(hybrids = missing_plants[grepl("×",missing_plants)])
#code for checking:
interactions_b %>% filter(plant_species=="Xanthoxylum") %>% distinct(targetTaxonPathNames)

#data frame of all the species with spelling mistakes:
change_spelling = data.frame(globi_name = c("Aloë","Sanvitalia abertii","Salvia cuatrecasasiana","Phacelia caerulea","Exodeconus maritimus","Rhus copallina","Adelinia grande","Helianthus maximilliani","Citrus paridisi","Quesnelia strobilispica","Atropa belladonna","Oncosiphon grandiflorum","Stevia satureiifolia","Nephrosperma van-houtteanum","Layia gaillardioides","Lachnanthes caroliana","Potentilla pulcherima","Datura inoxia","Achyrocline satureoides","Salpingostylis caelestina","Nama hispidum","Salvia ballotaeflora","Polemonium vanbruntiae","Mitracarpus villosa","Zephyranthes atamasca","Dorycnopsis gerardi","Simsia lagascaeformis","Arctomecon californica","Disperis wealei","Iva xanthifolia","Nepeta faassenii","Eleagnus","Spirea","Pseudoveronica","Xanthoxylum","Serpyllum","Astralagus","Chicorium",'Clerodendron',"Aesculus × carnea", "Dicerandra × thinicola","Bougainvillea × buttiana","Neuradopsis austro-africana"), 
                             wfo_name = c('Aloe',"Sanvitalia aberti","Salvia cuatrecasana",'Phacelia coerulea',"Exodeconus maritima","Rhus copallinum","Adelinia grandis","Helianthus maximiliani","Citrus paradisi","Quesnelia strobilospica","Atropa bella-donna","Oncosiphon grandiflorus","Stevia","Nephrosperma vanhoutteanum","Layia galardioides","Lachnanthes caroliniana","Potentilla pulcherrima","Datura innoxia", "Achyrocline","Salpingostylis coelestina","Nama hispida","Salvia ballotiflora","Polemonium van-bruntiae",	
                                              "Mitracarpus villosum var. glabrescens","Zephyranthes atamasco","Dorycnopsis gerardii","Simsia lagasceiformis","Arctomecon californicum",	"Disperis wealii","Iva xanthiifolia","Nepeta ×faasenii","Elaeagnus","Spiraea","Veronica","Zanthoxylum","Thymus serpyllum" ,'Astragalus',"Cichorium",'Clerodendrum',"Aesculus ×carnea","Dicerandra thinicola","Bougainvillea buttiana","Neuradopsis austroafricana"))

change_formatting_all = change_formatting %>% bind_rows(change_formatting2) %>% bind_rows(change_formatting3) %>% bind_rows(change_spelling)

#check for duplicates - should be none
(dupes = change_formatting_all$globi_name[duplicated(change_formatting_all$globi_name)])


interactions_updated = interactions_b %>% 
  left_join(change_formatting_all %>% rename(plant_species=globi_name)) %>%
  mutate(plant_species = ifelse(is.na(wfo_name),plant_species,wfo_name)) %>%
  select(-wfo_name)


# 2) filter wfo data to be plant species in globi. data.frame is wfo_filtered2
wfo_filtered2 = wfo_data %>% filter(scientificName %in% interactions_updated$plant_species)

# 3) double check that all species in globi are either in the wfo data or in the ‘not_in_wfo’ dataframe
the_plants = unique(interactions_updated$plant_species)
the_plants[!(the_plants %in% wfo_filtered2$scientificName | the_plants %in% not_in_wfo$globi_name)]

# 4) filter wfo_filtered to be just accepted species and for all accepted names
# make data frame with old_name = scientificName, accepted_name = scientificName, family = 	wfo_family, and source = wfo
# double check there’s no duplicates
wfo_accepted = wfo_filtered2 %>% 
  filter(taxonomicStatus=='ACCEPTED') %>%
  mutate(old_name = scientificName, accepted_name=scientificName,source = 'wfo') %>%
  rename(accepted_genus=genus,accepted_family = family) %>%
  distinct(old_name,accepted_name,accepted_family,accepted_genus,taxonomicStatus,source) %>%
  mutate(accepted_genus = ifelse(is.na(accepted_genus),accepted_name,accepted_genus)) %>%
  mutate(genus_fam = paste(accepted_family,accepted_genus,sep="_")) %>%
  filter(!genus_fam %in% c("Aquifoliaceae_Ehretia","Boraginaceae_Ipomoea","Meteoriaceae_Tricholepis")) %>% #get rid of wrong genus fam combos
  select(-genus_fam)

"Ilex" %in% wfo_accepted$accepted_name
#any duplicated names within the accepted species? there shouldn't be
wfo_accepted$accepted_name[duplicated(wfo_accepted$accepted_name)]
#View(wfo_accepted %>% filter(accepted_name == "Ehretia rigida subsp. nervifolia" ))

#let's get a df with synonyms next
wfo_synonyms = wfo_filtered2 %>% 
  filter(taxonomicStatus %in% c('SYNONYM',"HETEROTYPICSYNONYM") & !scientificName %in% wfo_accepted$accepted_name) %>%
  mutate(old_name = scientificName, accepted_id=acceptedNameUsageID,source = 'wfo') %>%
  distinct(old_name, accepted_id,taxonomicStatus,source) %>% #for duplicated synonyms we'll just pick one accepted name at random since globi doesn't give authors.
  split(.$old_name) %>%
  purrr::map_dfr(function(df)df[1,])

#any duplicated rows among the synonyms?
wfo_synonyms[duplicated(wfo_synonyms$old_name),]
# wfo_synonyms %>% filter(old_name == "Aster tripolium") 


#add unchecked and doubtful species to the data
wfo_unsure = wfo_filtered2 %>% 
  filter(taxonomicStatus %in% c("UNCHECKED","DOUBTFUL" ) & !scientificName %in% wfo_accepted$accepted_name & !scientificName %in% wfo_synonyms$old_name) %>%
  mutate(taxonomicStatus = 'UNCHECKED_DOUBTFUL') %>%
  mutate(old_name = scientificName, accepted_name=scientificName,source = 'wfo') %>%
  rename(accepted_genus=genus,accepted_family = family) %>%
  distinct(old_name,accepted_name,accepted_family,accepted_genus,taxonomicStatus,source) %>%
  mutate(accepted_genus = ifelse(is.na(accepted_genus),accepted_name,accepted_genus)) 
wfo_unsure[duplicated(wfo_unsure$old_name),]
wfo_unsure %>% filter(old_name == "Taraxacum vulgare") 

# 5) for all synonyms:
#   filter wfo_filtered to just be these species (filter so  that none are also accepted)


# View(wfo_filtered2 %>% filter(taxonomicStatus=="SYNONYM")) #doubtful and unchecked don't have accepted usage-ids, so we'll just go with those
# View(wfo_filtered2 %>% filter(taxonID=="wfo-4000035688"))
# View(wfo_filtered2 %>% filter(taxonomicStatus=="SYNONYM" & is.na(acceptedNameUsageID)))



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
  rename(old_name = globi_name, accepted_name=accepted,accepted_family=family) %>%
  mutate(accepted_genus = sub(" .*","",accepted_name),
         taxonomicStatus = ifelse(old_name != accepted_name,"SYNONYM",'ACCEPTED')) 
wfo_name_update = wfo_accepted %>% bind_rows(wfo_syn_update) %>% bind_rows(wfo_unsure) %>% bind_rows(not_in_wfo_reformatted) %>%
  rename(old_plant = old_name)

#double check all plants are in the df and that there are no duplicates
wfo_name_update[duplicated(wfo_name_update$old_plant),]
the_plants[!the_plants %in% wfo_name_update$old_plant]

# make data frame with ‘globi_name’ and name in wfo
# update the globi data once more, with the new names

interactions_updated_final = interactions_updated %>%
  rename(old_plant = plant_species,old_pl_genus = plant_genus, old_pl_family = plant_family) %>%
  left_join(wfo_name_update %>% select(-accepted_id,-taxonomicStatus) %>% rename(plant_family = accepted_family,
                                       plant_genus = accepted_genus,plant_species = accepted_name, plant_name_source = source))
#double check the old and new data.frames are the same  number of rows
nrow(interactions_updated_final) == nrow(interactions_updated)

#how many changes
with(interactions_updated_final,mean(plant_species != old_plant)) #about 5%

#save the file as a csv
# what we want is a data.frame with wfo data with genus and family
wfo_fams = interactions_updated_final %>% distinct(plant_genus,plant_family)%>%
  select(plant_genus,plant_family)

#make sure no genera are duplicated
(dupes2 = wfo_fams$plant_genus[duplicated(wfo_fams$plant_genus)])


#filter to just be genera
just_gen = interactions_updated_final[!grepl(' ',interactions_updated_final$plant_species),]
genus_updates = just_gen %>% distinct(old_pl_genus,plant_genus,plant_family) %>%
  select(old_pl_genus,plant_genus,plant_family) %>%
  arrange(old_pl_genus)
genus_updates %>% filter(old_pl_genus != plant_genus)

# write_csv(genus_updates,'modeling_data/wfo_genus_updates.csv')
# write_csv(wfo_fams, 'modeling_data/plant_fam_info.csv')


# write_csv(interactions_updated_final,"modeling_data/globi_occ_names_updated-19dec2022.csv")

