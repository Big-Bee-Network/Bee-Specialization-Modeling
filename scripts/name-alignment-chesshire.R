rm(list=ls())
library(tidyverse)
library(readxl)

chesshire2 = read_csv('modeling_data/Chesshire2023_nameAlignment.csv')
chesshire = read_excel('modeling_data/Chesshire2023-appendixs3.xlsx') %>%
  rename(finalName = "Final Name Selected",
         providedName = 'Step 1: Original Scientific Name in Data Download',
         secondRoundName = "Step 2: Chosen Name after comparing with DL List and/or first round of manual sleuthing",
         reasonSecondRound = "Reason for Initial Name Change (or no change)",
         JohnAscherReason = "John Notes and Final Decision")
str(chesshire)
unique(chesshire$reasonSecondRound)


# let's look at records where the secondRoundName doesn't equal the final  name
# these are most likely records where John Ascher disagreed with the DiscoverLife name
# write_csv(chesshire %>%
#   filter(finalName != secondRoundName) %>%
#   select(providedName,secondRoundName,finalName,reasonSecondRound,JohnAscherReason),"modeling_data/look_at_me.csv")

# let's upload same spreadsheet but with column saying whether the name alignment was informed by geography or not
geo_informed = read_excel('modeling_data/Chesshire et al 2023 bees name alignment unsure.xlsx') %>%
  filter(geographic_informed == 'yes')


#filter original chesshire data to not include these species
final = chesshire %>%
  mutate(geo_informed = providedName %in% geo_informed$providedName) %>%
  dplyr::select(providedName,finalName,geo_informed)

#how many are informed by geography?
nrow(geo_informed)

  
# write_csv(final,'modeling_data/Chesshire2023_nameAlignment-geoUpdate.csv')
