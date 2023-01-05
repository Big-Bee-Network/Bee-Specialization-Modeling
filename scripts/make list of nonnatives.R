#let's exclude non-native species
nonnative = read_csv("/Users/colleen/Dropbox/My Mac (MacBook-Air.local)/Downloads/BeeGap_Taxonomy and General Traits.csv") %>% 
  filter(`Native?` == "N") %>%  #N == 'no'
  distinct(Genus,Species) %>%
  mutate(scientificName = paste(Genus,Species), source = "Diller et al. 2020")


#list below from Russo 2016
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

all_nonnatives = data.frame(scientificName = more_nonnatives,source="Russo 2016") %>%
  bind_rows(nonnative %>% dplyr::select(scientificName,source))

dupes = all_nonnatives$scientificName[duplicated(all_nonnatives$scientificName)]
all_nonnatives[all_nonnatives$scientificName %in% dupes,]$source <- "Diller et al. 2020; Russo 2016"

(nonnatives_distinct = all_nonnatives %>% distinct() %>%
  arrange(scientificName))
# write_csv(nonnatives_distinct,'modeling_data/nonnative_bees.csv')
