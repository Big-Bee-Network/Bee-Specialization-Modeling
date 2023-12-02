rm(list=ls())
library(tidyverse)

##how much overlap between specialist bees and natureserve lists?

#load natureserve data
natureserve=read_csv("/Users/colleen/Dropbox/Fall_2014/ucsb/bees_restorations/raw_data/nsExplorer-Export-2022-06-28-06-52.csv") %>%
  #filter('Species Group (Broad)'=="Insects - Bees") #%>%
  rename(group='Species Group (Broad)',scientificName="Scientific Name",ns_rank='NatureServe Global Rank',
         ns_rank_rounded="NatureServe Rounded Global Rank") %>%
  filter(group=="Insects - Bees" )

str(natureserve)

#load pollen specialist bee data
#fowler_formatted and russell_formatted have the lists of our specialists
fowler_hosts = read_csv("modeling_data/fowler_formatted-30nov2023.csv")
russell_hosts = read_csv("modeling_data/russell_formatted-30nov2023.csv")
hosts = russell_hosts %>% bind_rows(fowler_hosts)

# specs=read_csv("~/Dropbox/Fall_2014/postdoc/inat project/data_current/bee_hosts_allUS.csv") %>% 
#   mutate(scientificName=sub('_',' ',bee))


#filter natureserve data just to be specialists
spec_status = natureserve %>% 
  filter(scientificName %in% hosts$scientificName) 

spec_status%>%
  group_by(ns_rank_rounded) %>%
  summarize(n=n())

imperiled_specs = spec_status %>% filter(ns_rank_rounded %in% c('G1', 'G2') ) %>% distinct(scientificName)
paste0('In the United States, there are ', nrow(imperiled_specs),  ' specialist bee species currently rated by the website NatureServe as critically imperiled or imperiled')


View(spec_status %>% filter(ns_rank_rounded =='G1') )

#how many have no status
# GNR = not ranked; GU ==unrankable
unique(spec_status$ns_rank_rounded)
rank_sums = hosts %>% left_join(natureserve) %>% group_by(ns_rank_rounded) %>% summarize(n=n()) %>%
  mutate(prop_rank = n/nrow(hosts)*100)
31.7+ 55.3 + 0.570
