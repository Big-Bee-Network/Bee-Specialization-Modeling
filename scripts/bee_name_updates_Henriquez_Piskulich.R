rm(list=ls())
library(tidyverse)

#updates to globi dataset, fowler datasets according to Henriquez Piskulich taxonomy. 
#(Pseudopanurgus,Peponapis,Tetraloniella,Syntrichalonia,Cemolobus,Micralictoides)
#Eucerini treated according to https://academic.oup.com/isd/article/7/4/3/7222693?login=true 
#Pseudopanurgus https://resjournals.onlinelibrary.wiley.com/doi/epdf/10.1111/syen.12530
#Micralictoides treated as subgenus of Duforea
#Dufourea subg. Micralictoides Timberlake, 1939. Ent. Soc. Amer., Ann. 32: 397.

#globi_allNamesUpdated <- read_csv("modeling_data/globi_allNamesUpdated.csv")
chesshire2023_beeArea11april2023 <- read_csv("modeling_data/chesshire2023_beeArea-11april2023.csv")
#globi_phyloDiv <- read_csv("modeling_data/globi_phyloDiv.csv")
DATA_beetaxaknownhostsallplanttaxa_11272023 <- read_csv("modeling_data/DATA_bee-taxa-known-hosts_all-plant-taxa_11-27-2023.csv")


# Perform multiple generic replacements
replace_species <- function(x) {
  x <- gsub("Pseudopanurgus", "Protandrena", x)
  x <- gsub("Peponapis", "Xenoglossa", x)
  x <- gsub("Tetraloniella", "Xenoglossa", x)
  x <- gsub("Syntrichalonia", "Xenoglossa", x)
  x <- gsub("Cemolobus", "Xenoglossa", x)
  x <- gsub("Micralictoides", "Dufourea", x)
  return(x)
}

# Apply the replacement function to all columns in the dataframe
globi_allNamesUpdated <- globi_allNamesUpdated %>%
  mutate_all(~ replace_species(.))

# Apply the replacement function to all columns in the dataframe
chesshire2023_beeArea11april2023 <- chesshire2023_beeArea11april2023 %>%
  mutate_all(~ replace_species(.))

# Apply the replacement function to all columns in the dataframe
globi_phyloDiv <- globi_phyloDiv %>%
  mutate_all(~ replace_species(.))

# Apply the replacement function to all columns in the dataframe
DATA_beetaxaknownhostsallplanttaxa_11272023 <- DATA_beetaxaknownhostsallplanttaxa_11272023 %>%
  mutate_all(~ replace_species(.))


#write updated dataframe
#write_csv(globi_allNamesUpdated,'modeling_data/globi_allNamesUpdated_Henriquez_Piskulich.csv')
write_csv(chesshire2023_beeArea11april2023,'modeling_data/chesshire2023_beeArea11april2023_revision.csv')
write_csv(globi_phyloDiv,'modeling_data/globi_phyloDiv_revision.csv')
write_csv(DATA_beetaxaknownhostsallplanttaxa_11272023,'modeling_data/DATA_bee-taxa-known-hosts_all-plant-taxa_11-27-2023_revision.csv')
