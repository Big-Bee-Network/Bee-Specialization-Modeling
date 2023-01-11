rm(list=ls())
library(tidyverse)

# we need to exclude non-native species
# first use list from USDA traits database (Diller et al 2020)
nonnative = read_csv("/Users/colleen/Dropbox/My Mac (MacBook-Air.local)/Downloads/BeeGap_Taxonomy and General Traits.csv") %>% 
  filter(`Native?` == "N") %>%  #N == 'no'
  distinct(Genus,Species) %>%
  mutate(scientificName = paste(Genus,Species), source = "Diller et al. 2020")


# the list below is one I compiled from Russo 2016
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5198217/
more_nonnatives = c("Hylaeus variegates", "Hylaeus albonitens",
                    "Hylaeus hyalinatus", "Hylaeus leptocephalus","Hylaeus punctatus",
                    "Hylaeus strenuus","Andrena wilkella", "Halictus tectus", "Lasioglossum eleutherense",
                    "Lasioglossum leucozonium", "Lasioglossum zonulum", "Anthidium manicatum",
                    "Anthidium oblongatum", "Chelostoma campanularum", "Coelioxys coturnix", "Heriades truncorum", "Hoplitis anthocopoides",
                    "Lithurgus chrysurus", "Lithurgus scabrosus", "Megachile apicalis", "Megachile chlorura", "Megachile concinna", "Megachile ericetorum",
                    "Megachile fullawayi", "Megachile lanata",	"Megachile rotundata", "Megachile sculpturalis", "Osmia caerulescens",
                    "Osmia cornifrons", "Osmia taurus", "Anthophora villosula", "Apis mellifera", "Ceratina cobaltina",
                    "Ceratina dallatorreana", "Euglossa dilemma", "Plebia frontalis", "Xylocopa appendiculata")
# last is bees I looked up using discover life:
other_bees = read_csv('modeling_data/unknown_native_status.csv')
even_more_nonnatives = other_bees %>%
  filter(status == 'introduced') %>%
  mutate(source='Discover Life')

all_nonnatives = data.frame(scientificName = more_nonnatives,source="Russo 2016") %>%
  bind_rows(nonnative %>% dplyr::select(scientificName,source)) %>%
  bind_rows(even_more_nonnatives %>% dplyr::select(scientificName, source))

dupes = all_nonnatives$scientificName[duplicated(all_nonnatives$scientificName)]
mean(dupes %in% even_more_nonnatives$scientificName)
all_nonnatives[all_nonnatives$scientificName %in% dupes,]$source <- "Diller et al. 2020; Russo 2016"

(nonnatives_distinct = all_nonnatives %>% distinct() %>%
  arrange(scientificName))

#now update names using the name alignment template
#save as csv and align  names on github
# write_csv(nonnatives_distinct %>% select(-source),'modeling_data/nonnative_bees_forAlignment.csv')

#upload csv file with the aligned names
bees_aligned = vroom("modeling_data/nonnative_names-aligned.csv")%>%
  mutate(source = ifelse(grepl("ITIS",alignedExternalId),'itis',NA)) %>% #add column with source
  mutate(source = ifelse(grepl("discover",alignedExternalId),'discoverlife',source)) %>%#add column with source
  mutate(source = ifelse(grepl("NCBI",alignedExternalId),'ncbi',source)) #add column with source

#update names - use DL first, followed by itis, followed by col
#first filter just be discover life bees
dl_bees = bees_aligned %>% filter(source=='discoverlife')
#add itis bees that are not in the discover life list
itis_bees = bees_aligned %>% filter(source=='itis' & !providedName %in% dl_bees$providedName)
#add col bees that are not in the discover life list or the itis list
ncbi_bees = bees_aligned %>% filter(source=='ncbi' & !providedName %in% c(dl_bees$providedName,itis_bees$providedName))

#any bees not in any catalog?
name_update = rbind(dl_bees,itis_bees,ncbi_bees)
# both of these are valid according to manual search on dl & itis though(weird they're not showing up)
missing = nonnatives_distinct %>% filter(!scientificName %in% name_update$providedName) #the three lasioglossum are newly described species from Joel Gardner's paper

#add to name update
name_update2 = name_update %>% 
  bind_rows(data.frame(providedName = missing$scientificName,alignedName = missing$scientificName))

nonnatives_distinct %>%
  rename(old_bee_name = scientificName) %>%
  left_join(name_update2 %>% rename(old_bee_name = providedName,scientificName = alignedName) %>% select(scientificName,old_bee_name))

# write_csv(nonnatives_distinct,'modeling_data/nonnative_bees.csv')
