rm(list=ls())
library(tidyverse)
library(vroom)

# we need to exclude non-native species
# # first use list from USDA traits database (Diller et al 2020)
# nonnative = read_csv("/Users/colleen/Dropbox/My Mac (MacBook-Air.local)/Downloads/BeeGap_Taxonomy and General Traits.csv") %>% 
#   filter(`Native?` == "N") %>%  #N == 'no'
#   distinct(Genus,Species) %>%
#   mutate(scientificName = paste(Genus,Species), source = "Diller et al. 2020")


# the list below is one I compiled from Russo 2016
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5198217/
nonnatives = c("Hylaeus variegates", "Hylaeus albonitens",
                    "Hylaeus hyalinatus", "Hylaeus leptocephalus","Hylaeus punctatus",
                    "Hylaeus strenuus","Andrena wilkella", "Halictus tectus", "Lasioglossum eleutherense",
                    "Lasioglossum leucozonium", "Lasioglossum zonulum", "Anthidium manicatum",
                    "Anthidium oblongatum", "Chelostoma campanularum", "Coelioxys coturnix", "Heriades truncorum", "Hoplitis anthocopoides",
                    "Lithurgus chrysurus", "Lithurgus scabrosus", "Megachile apicalis", "Megachile chlorura", "Megachile concinna", "Megachile ericetorum",
                    "Megachile fullawayi", "Megachile lanata",	"Megachile rotundata", "Megachile sculpturalis", "Osmia caerulescens",
                    "Osmia cornifrons", "Osmia taurus", "Anthophora villosula", "Apis mellifera", "Ceratina cobaltina",
                    "Ceratina dallatorreana", "Euglossa dilemma", "Plebia frontalis", "Xylocopa appendiculata")
#convert to data frame
nonnatives_df = data.frame(scientificName = nonnatives ,source="Russo 2016") %>%
  arrange(scientificName)


#load chesshire et al, which has the name update info
chesshire = read_csv('modeling_data/Chesshire2023_nameAlignment-geoUpdate.csv')



#update names using chesshire df
#first exclude species where John Ascher seemed to think the species does not occur in the USA
#these are all the bees where geo_informed ==T in the chesshire et al df
chesshire_geo = chesshire %>% filter(geo_informed)
(im_nonnative = chesshire %>%
  filter(providedName %in% nonnatives & providedName != finalName))
chesshire_native1 = chesshire %>% 
  filter(!providedName %in% im_nonnative)

#now update names on non-native list - see if any have changed
chesshire_join = nonnatives_df %>%
  rename(providedName = scientificName) %>%
  left_join(chesshire_native1)
check_for_changes = chesshire_join %>% filter(finalName != providedName) #any name changes?

if(nrow(check_for_changes) != 0) print("stop: you need to update some names")

#finally, write csv of the non-native species
# write_csv(nonnatives_df,'modeling_data/nonnative_bees.csv')




#old 
# write_csv(chesshire_join %>%
#   filter(!is.na(finalName)) %>%
#   filter(source == "Diller et al. 2020"),'modeling_data/check_me_nonnatives.csv')

chesshire_join %>%
  filter(!is.na(finalName)) %>%
  select(finalName,providedName,source) %>% filter(finalName != providedName)

#now upload the bees we aligned separately
bees_aligned = vroom("modeling_data/nonnative_names-aligned.csv")%>%
  mutate(source = ifelse(grepl("ITIS",alignedExternalId),'itis',NA)) %>% #add column with source
  mutate(source = ifelse(grepl("discover",alignedExternalId),'discoverlife',source)) %>%#add column with source
  mutate(source = ifelse(grepl("NCBI",alignedExternalId),'ncbi',source)) #add column with source

#update names - use DL first, followed by itis, followed by col
#first filter just be discover life bees
dl_bees = bees_aligned %>% filter(source=='discoverlife') %>% 
  distinct(providedName,alignedName,source)






##old

# last is bees I looked up using discover life:
# other_bees = read_csv('modeling_data/unknown_native_status.csv')
# even_more_nonnatives = other_bees %>%
#   filter(status == 'introduced') %>%
#   mutate(source='Discover Life')

all_nonnatives = data.frame(scientificName = more_nonnatives,source="Russo 2016") %>%
  bind_rows(nonnative %>% dplyr::select(scientificName,source)) 

dupes = all_nonnatives$scientificName[duplicated(all_nonnatives$scientificName)]
all_nonnatives[all_nonnatives$scientificName %in% dupes,]$source <- "Diller et al. 2020; Russo 2016"

(nonnatives_distinct = all_nonnatives %>% distinct() %>%
  arrange(scientificName))

#now update names using the name alignment template
#save as csv and align  names on github
# write_csv(nonnatives_distinct %>% select(-source),'modeling_data/nonnative_bees_forAlignment.csv')

#upload csv file with the name changes from Chesshire et al 2023
chesshire_bees = vroom('modeling_data/Chesshire2023_nameAlignment.csv')

#any of these bees not in the Chesshire 2023 data?
#yes quite a lot:
nonnatives_distinct$scientificName[!nonnatives_distinct$scientificName %in% c(chesshire_bees$providedName,chesshire_bees$finalName)]
nonnatives_distinct$scientificName[nonnatives_distinct$scientificName %in% c(chesshire_bees$providedName,chesshire_bees$finalName)]

bees_aligned = vroom("modeling_data/nonnative_names-aligned.csv")%>%
  mutate(source = ifelse(grepl("ITIS",alignedExternalId),'itis',NA)) %>% #add column with source
  mutate(source = ifelse(grepl("discover",alignedExternalId),'discoverlife',source)) %>%#add column with source
  mutate(source = ifelse(grepl("NCBI",alignedExternalId),'ncbi',source)) #add column with source

#update names - use DL first, followed by itis, followed by col
#first filter just be discover life bees
dl_bees = bees_aligned %>% filter(source=='discoverlife') %>% 
  distinct(providedName,alignedName,source)

#add itis bees that are not in the discover life list
itis_bees = bees_aligned %>% filter(source=='itis' & !providedName %in% dl_bees$providedName) %>% 
  distinct(providedName,alignedName,source)
#add col bees that are not in the discover life list or the itis list
ncbi_bees = bees_aligned %>% 
  filter(source=='ncbi' & !providedName %in% c(dl_bees$providedName,itis_bees$providedName)) %>% 
  distinct(providedName,alignedName,source)

#any bees not in any catalog?
name_update = rbind(dl_bees,itis_bees,ncbi_bees)
# both of these are spelling mistakes, found correct spelling on discoverlife
(missing = nonnatives_distinct %>% filter(!scientificName %in% name_update$providedName))
#add to name update
name_update2 = name_update %>% 
  bind_rows(data.frame(providedName = missing$scientificName,
                       alignedName = c('Hylaeus variegatus','Plebeia frontalis'),
                       source = c('discoverlife','discoverlife')))
name_update2 %>% filter(alignedName != providedName)
nonnatives_distinct2 = nonnatives_distinct %>%
  rename(old_bee_name = scientificName) %>%
  left_join(name_update2 %>% rename(old_bee_name = providedName,scientificName = alignedName) %>% select(scientificName,old_bee_name))


#lets see what species on the chesshire et al list are non-native
(chesshire_nonnative = chesshire_bees %>% filter(finalName %in% name_update2$alignedName ))
chesshire_bees %>% filter(!finalName %in% name_update2$alignedName & finalName %in% name_update2$providedName)

#what about bees that are in the chesshire data for the original name but not for the 
(whats_happening = chesshire_bees %>% 
    filter(!providedName %in% chesshire_nonnative$finalName & providedName %in% name_update2$alignedName)) #interesting

name_update2 %>% filter(alignedName %in% whats_happening$providedName)

#let's download these and see what's going on here
# write_csv(whats_happening,'modeling_data/nonnative_name_discrepancies.csv')

#anthophora furcata - valid species on discover life and anthophora terminalis appears to be native
# natureserve:  A terminata previously considered a subspecies of the Old World species, Anthophora furcata.

# bombus bohemicus - eurasian sister(?) of bombus ashtoni - 
####some sources suggest they're the same species: https://explorer.natureserve.org/Taxon/ELEMENT_GLOBAL.2.946793/Bombus_bohemicus

#bombus flavidus and fernaldae recently synonymized: https://academic.oup.com/isd/article/5/2/5/6239767?login=true
# write_csv(nonnatives_distinct2,'modeling_data/nonnative_bees.csv')
