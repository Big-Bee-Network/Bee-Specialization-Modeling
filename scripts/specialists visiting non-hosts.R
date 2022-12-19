rm(list = ls())
library(tidyverse)
library(readxl)
library(kewr)
library(furrr)

# Research Qs this R script will answer:
# What percentage of oligolectic bees visit non-host plants (where visit == at least 5 visits)
# How often are oligolectic bees observed visiting non-host plants?

# load formatted MS data with list of specialist bees and their hosts
ms = read_csv('modeling_data/MS_data_simplified.csv') %>%
  filter(diet_breadth != "Polylectic")
# load globi data
globi = read_csv("modeling_data/globi_names_updated.csv") %>% 
  filter(scientificName %in% ms$scientificName) %>%
  mutate(plant_genus = ifelse(plant_genus == "Ipomaea","Ipomoea",plant_genus))

#load discrepancies data
discrepancies = read_csv('modeling_data/discrepancies_fowler_ms.csv')
only_ms_specialist = discrepancies %>% filter(`This ms` =='specialist' & Fowler =="not on list")

# update the family names of the globi data so it's consistent with how i named families for the ms data
get_fams = unique(globi$plant_genus) %>% future_map(function(sciname){
  output = search_wcvp(sciname, filters=c("families", "accepted"),limit = 1)
  output_df = tidy(output)
  
  if(nrow(output_df) !=0){
    return_df = data.frame(genus = sciname,family = output_df$family)
    
  }
  else{
    output = search_wcvp(sciname, filters=c("families"),limit = 1)
    df=tidy(output)
    
    if(nrow(df) !=0){
      new_output_df = df$synonymOf[[1]]
      return_df = data.frame(genus = sciname,family = new_output_df$family)
      
    }else{
      return_df = data.frame(genus=sciname,family = NA)
    }
  }
}) %>% bind_rows

still_need_fams = get_fams %>% filter(is.na(family))
add_fam_info = data.frame(genus = c('Heliantheae'),
                          family = c('Asteraceae'))

get_fams_final = get_fams %>% 
  filter(!is.na(family)) %>% filter(genus %in% add_fam_info$genus==F) %>% 
  bind_rows(add_fam_info)

(bees_bigN = globi %>% group_by(scientificName) %>% summarize(n=sum(n)) %>% filter(n>20))
globi_u=globi %>% 
  left_join(get_fams_final %>%
              rename(plant_genus = genus, wcvp_family=family)) %>%
  filter(n>5 & scientificName %in% bees_bigN$scientificName) #let's exclude interactions less than or equal to five and bees with les than or equal to 20 records total

# for the globi data: categorize interaction partner as host or non-host
# loop through the bee speices in globi_u
# get their host plants
# if the interaciton partner is in the family or genus then categorize it as a host
a = globi_u %>% split(1:nrow(globi_u))
row = a[[1]]
empty_list = c()
globi_host = globi_u %>% split(1:nrow(globi_u)) %>% purrr::map_dfr(function(row){
  host_plant_df = ms %>% filter(scientificName == row$scientificName[1])
  if(unique(host_plant_df$rank =='genus')) visiting_host = row$plant_genus  %in% host_plant_df$host
  if(unique(host_plant_df$rank =='family')) visiting_host = row$wcvp_family %in% host_plant_df$host
  return(row %>% mutate(host=visiting_host))
  }) %>%
  mutate(only_ms = scientificName %in% only_ms_specialist$scientificName)
check_these = which(ms %>% split(.$scientificName) %>% map_lgl(function(df) nrow(df)>1))

#what percentage of oligolects visit non-host plants?
nonhost_visits=globi_host %>% group_by(scientificName,only_ms) %>% summarize(visiting_nonhost = F %in% host)
mean(nonhost_visits$visiting_nonhost)
nonhost_visits %>% group_by(only_ms) %>% summarize(mean(visiting_nonhost))

#how [frequently] do they visit non-host plants
head(globi_host)
prop_nonhost=globi_host %>% group_by(scientificName,only_ms) %>%
  summarize(prop_nonhost_visits=sum(n[!host])/sum(n),total_n=sum(n))
with(prop_nonhost,hist(prop_nonhost_visits))
with(prop_nonhost,plot(prop_nonhost_visits~total_n))

with(prop_nonhost,boxplot(prop_nonhost_visits~only_ms))

# format the box-plot graph:
# pdf('figures/compare_fowler_ms.pdf')
with(prop_nonhost %>% mutate(on_fowler = factor(!only_ms,levels = c(T,F))),
     boxplot(prop_nonhost_visits~on_fowler, ylab = 'proportion of bees visiting non-hosts',
                          xlab = 'On Fowler list'))
# dev.off()


# is there a difference between bees on fowler list vs MS list?
only_ms_specialist$scientificName

## old:
# note: for fowler df, bees on this list are duplicated if they occur in multiple regions (eg both east and central america)
fowler <- read_csv("modeling_data/fowler_hostplants.csv") 
globi <- read_csv('modeling_data/globi_names_updated.csv')

# we want to reformat the fowler data so that there's only one row for each bee species
# and so that the host plants are in a consistent format
fowler %>% filter(host_plant ==  "Pontederia L., Fabaceae?")
host_plants = unique(fowler$host_plant)
fam_hosts = host_plants[grepl(":",host_plants)]
sub(":.*","",fam_hosts)
which(sub(":.*","",fam_hosts)=="Lyonia Nutt., Vaccinium L.")
gen_hosts=host_plants[!grepl(":",host_plants)]
gen_hosts[grepl('aceae' , gen_hosts)]
fam_hosts[51]
head(fowler)
fowler %>% split(.$host_plant)
authorities = c()