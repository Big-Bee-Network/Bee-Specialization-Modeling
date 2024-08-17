rm(list=ls())
library(tidyverse)

#updates to globi dataset, fowler datasets according to Henriquez Piskulich taxonomy. 
#(Pseudopanurgus,Peponapis,Tetraloniella,Syntrichalonia,Cemolobus,Micralictoides)
#Eucerini treated according to https://academic.oup.com/isd/article/7/4/3/7222693?login=true 
#Pseudopanurgus https://resjournals.onlinelibrary.wiley.com/doi/epdf/10.1111/syen.12530
#Micralictoides treated as subgenus of Duforea
#Dufourea subg. Micralictoides Timberlake, 1939. Ent. Soc. Amer., Ann. 32: 397.

globi_allNamesUpdated <- read_csv('modeling_data/globi_allNamesUpdated_Henriquez_Piskulich.csv')

russell_formatted <- read_csv("final_data/other_data_files/russell_formatted-30nov2023.csv")

fowler_formatted <- read_csv("final_data/other_data_files/fowler_formatted-30nov2023.csv")


# Perform multiple generic replacements
replace_species <- function(x) {
  x <- gsub("Pseudopanurgus", "Protandrena", x, ignore.case = TRUE)
  x <- gsub("Peponapis", "Xenoglossa", x, ignore.case = TRUE)
  x <- gsub("Tetraloniella", "Xenoglossa", x, ignore.case = TRUE)
  x <- gsub("Syntrichalonia", "Xenoglossa", x, ignore.case = TRUE)
  x <- gsub("Cemolobus", "Xenoglossa", x, ignore.case = TRUE)
  x <- gsub("Micralictoides", "Dufourea", x, ignore.case = TRUE)
  return(x)
}

# Apply the replacement function to all columns in the dataframe
globi_allNamesUpdated <- globi_allNamesUpdated %>%
  mutate_all(~ replace_species(.))

# Apply the replacement function to all columns in the dataframe
fowler_formatted <- fowler_formatted %>%
  mutate_all(~ replace_species(.))

# Apply the replacement function to all columns in the dataframe
russell_formatted <- russell_formatted %>%
  mutate_all(~ replace_species(.))



#write updated dataframe
write_csv(globi_allNamesUpdated,'modeling_data/globi_allNamesUpdated_Henriquez_Piskulich.csv')

write_csv(fowler_formatted,'modeling_data/fowler_formatted-15Aug2024.csv')

write_csv(russell_formatted,'modeling_data/russell_formatted-15Aug2024.csv')

