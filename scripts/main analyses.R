rm(list=ls())
library(tidyverse)
library(vroom)
library(maps)
library(sf)
library(vegan)
library(randomForest)
library(readxl)

# load the globi data
# exclude interactions of non-US bees
# update the plant family names

##analysis
# for bees on the Jarrod Fowler lists, what % visit their host plants?
# for bees [classified as polylectic in ms dataset, what % visit their host plants?]]
# for bees on the fowler list, how accurate are predictions?
# if we change the sample size how does this change?


# load the globi data
# this is a file with the globi records and plant names are updated (though not plant families)
globi_r = read_csv('modeling_data/globi_occ_names_updated.csv')
globi_r %>% filter(sourceTaxonRank=='variety') %>% distinct(scientificName)
data.frame(globi_r %>% filter(is.na(sourceTaxonRank)) %>% distinct(scientificName))
Senecioneae
#also load the fowler data
#some of these bees have different characterizations despite having the same pollen hosts in both databases
# this is because fowler considers bees to be specialists if they use pollen from genera in two different families
#and we don't. we're going to remove these bees from the fowler list (and won't consider them discrepancies)
generalists_fowler = c("Andrena candidiformis", "Anthidium mormonum", "Dufourea cuprea",
                       "Habropoda laboriosa", "Hesperapis ilicifoliae", "Megachile perihirta",
                       "Peponapis michelbacherorum",
                       "Perdita fieldi","Perdita obscurata",
                       "Pseudopanurgus virginicus", "Xenoglossa kansensis","Florilegus condignus")

#note: some bees on this list are duplicated, if they occur in multiple regions (eg both eastern and central usa)
fowler <- read_csv("modeling_data/fowler_hostplants.csv") %>%
  mutate(diet_breadth = ifelse(scientificName %in% generalists_fowler,'generalist','specialist')) %>%
  mutate(host_plant_rank = ifelse(grepl('aceae',host_plant),'family','genus')) %>%
  mutate(diet_breadth_detailed = ifelse(host_plant_rank == 'family' & diet_breadth =='specialist','family_specialist','genus_specialist')) %>%
  mutate(diet_breadth_detailed = ifelse(diet_breadth=='generalist','generalist',diet_breadth_detailed))

specialists = fowler[fowler$diet_breadth=='specialist',]$scientificName
fowler_formatted = read_csv('modeling_data/fowler_formatted.csv')

#some bees have different host plants on the east/west/central lists - we need to make their diet breadths consistent
check_me = fowler %>% distinct(scientificName,diet_breadth,diet_breadth_detailed)
dupes = check_me$scientificName[duplicated(check_me$scientificName)]
fowler %>% filter(scientificName %in% dupes) %>% arrange(scientificName)
change_me = check_me %>% filter(scientificName %in% dupes) %>% arrange(scientificName) %>%
  split(.$scientificName) %>% purrr::map_dfr(function(df){
    new_db='family_specialist'
    db_broad = 'specialist'
    if('family_generalist'%in% df$diet_breadth_detailed) new_db <- 'family_specialist'
    if('generalist' %in% df$diet_breadth_detailed) {new_db <- 'generalist'; db_broad <- 'generalist'}
    
    data.frame(scientificName = df$scientificName[1],diet_breadth = db_broad,diet_breadth_detailed = new_db)
  })

diet_breadth = check_me %>% filter(!scientificName %in% dupes) %>% bind_rows(change_me)


# Let's exclude interactions of non-US bees

#first, load list of US bee species from discover life - the text file is from this link: 
# https://www.discoverlife.org/nh/cl/counts/Apoidea_species.html
usa_bees = read_table('modeling_data/Apoidea_species.txt',col_names=F) %>%
  rename(genus = X1, epithet = X2) %>%
  mutate(scientificName = paste(genus,epithet))

#i don't think the discover life list is complete, 
# so let's also check if these species are US bees by seeing if there
# are at least 5 records in gbif

#get the list of species
slist = unique(sort(globi_r$scientificName))

#remove morpho species and species groups
(sp_remove1 = slist[grepl('sp\\.',slist) | grepl('/',slist) | grepl('aff\\.',slist) | grepl('nr\\.',slist) | grepl('unk1',slist)])
these_are_genera = which(strsplit(slist," ") %>% purrr::map_lgl(function(str_vec) length(str_vec)==1))

sp_remove = c(slist[these_are_genera],sp_remove1)

slist_globi = slist[!slist %in% sp_remove]

american1 = slist_globi[slist_globi %in% usa_bees$scientificName]
plan(multisession, workers=6)
# pull the US occurrence records of these species from gbif
"Ceratina calcarata/dupla"  %in% slist_globi[!slist_globi %in% usa_bees$scientificName]

# bee_occ = slist_globi[!slist_globi %in% usa_bees$scientificName] %>% future_map(function(sci_name){
#   gbif_list = occ_search(scientificName = sci_name,country='US',limit=25,hasCoordinate = T)
# 
#   if(is.null(gbif_list$data)) {
#     return(gbif_list$data)
#   }else{
#     return( gbif_list$data %>% mutate(query=sci_name) %>%
#               dplyr::select(query,everything()))
#   }
# 
# },.options = furrr_options(seed=T))
# #
# saveRDS(bee_occ,'modeling_data/bee_occ_usa.rds')
bee_occ = readRDS('modeling_data/bee_occ_usa.rds')
r_obj=bee_occ[[2]]

which(bee_occ %>% map_lgl(is.null))
nrow(r_obj)
r_obj$query

#to be conservative let's say a bee ha to have at least 5 occurrence records in the US
american_bees_tokeep=which(bee_occ %>% purrr::map_lgl(function(r_obj) {
  
  #bee occurs in america if the object is not null (ie 0 occurrence records in the US)
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
unknown_bees = slist_globi[!slist_globi %in% usa_bees$scientificName]
unknown_bees[american_bees_tokeep]
american2 = unknown_bees[american_bees_tokeep]


#let's exclude non-native species
nonnative = read_csv("/Users/colleen/Dropbox/My Mac (MacBook-Air.local)/Downloads/BeeGap_Taxonomy and General Traits.csv") %>% 
  filter(`Native?` == "N") %>% 
  distinct(Genus,Species) %>%
  mutate(scientificName = paste(Genus,Species))

#list below from Russo 2016
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5198217/
more_nonnatives = c("Hylaeus variegates", "Hylaeus albonitens",
                    "Hylaeus hyalinatus", "Hylaeus leptocephalus"," Hylaeus punctatus",
                    "Hylaeus strenuus","Andrena wilkella", "Halictus tectus", "Lasioglossum eleutherense",
                    "Lasioglossum leucozonium", "Lasioglossum zonulum", "Anthidium manicatum",
                    "Anthidium oblongatum", "Chelostoma campanularum", "Coelioxys coturnix", "Heriades truncorum", "Hoplitis anthocopoides",
                    "Lithurgus chrysurus", "Lithurgus scabrosus", "Megachile apicalis", "Megachile chlorura", "Megachile concinna", "Megachile ericetorum",
                    "Megachile fullawayi", "Megachile lanata",	"Megachile rotundata", "Megachile sculpturalis", "Osmia caerulescens",
                    "Osmia cornifrons", "Osmia taurus", "Anthophora villosula", "Apis mellifera", "Ceratina cobaltina",
                    "Ceratina dallatorreana", "Euglossa dilemma", "Plebia frontalis", "Xylocopa appendiculata")
#still some bees left to check:
check_native = data.frame(scientificName = american2) %>% 
  filter(!scientificName %in% c(nonnative$scientificName,more_nonnatives))

# write_csv(check_native,'modeling_data/check_native_status.csv')
even_more_nonnatives = read_csv('modeling_data/unknown_native_status.csv') %>%
  filter(status == 'introduced')

introduced_bees = c(even_more_nonnatives$scientificName,more_nonnatives,nonnative$scientificName)

#
american_sp_keep = c(american1, american2)[!c(american1,american2) %in% introduced_bees]
globi_usa = globi_r %>% filter(scientificName %in% american_sp_keep) 


# update the plant family names using world checklist for vascular plants
# get_fams = unique(globi_usa$plant_genus) %>% future_map(function(sciname){
#   output = search_wcvp(sciname, filters=c("families", "accepted"),limit = 1)
#   output_df = tidy(output)
#   
#   if(nrow(output_df) !=0){
#     return_df = data.frame(genus = sciname,family = output_df$family)
#     
#   }
#   else{
#     output = search_wcvp(sciname, filters=c("families"),limit = 1)
#     df=tidy(output)
#     
#     if(nrow(df) !=0){
#       new_output_df = df$synonymOf[[1]]
#       return_df = data.frame(genus = sciname,family = new_output_df$family)
#       
#     }else{
#       return_df = data.frame(genus=sciname,family = NA)
#     }
#   }
# }) %>% bind_rows
# saveRDS(get_fams,'modeling_data/globi_plant_fams.rds')
get_fams = readRDS("modeling_data/globi_plant_fams.rds")

#get_fams manually for things that are in the world vascular checklist (note some are tribes not genera)
globi_r %>% filter(plant_genus =='Sarracenia') %>% select(referenceCitation)
globi_r %>% filter(plant_genus =='Geniculiflora') %>% select(referenceCitation)

(still_need_fams = get_fams %>% filter(is.na(family)))
add_fam_info = data.frame(genus = c('Heliantheae',"Dithyraea","Sarracenia","Senecioneae","Eupatorieae","Geniculiflora","Pseudoveronica","Althea","Eleagnus"),
                          family = c('Asteraceae',"Brassicaceae","Sarraceniaceae",'Asteraceae',"Asteraceae",NA,'Plantaginaceae','Malvaceae',"Elaeagnaceae"))

get_fams_final = get_fams %>% 
  filter(!is.na(family)) %>% filter(genus %in% add_fam_info$genus==F) %>% 
  bind_rows(add_fam_info)
globi_u=globi_usa %>% 
  left_join(get_fams_final %>%
              rename(plant_genus = genus, wcvp_family=family)) 
nrow(globi_u) == nrow(globi_usa)


# for bees on the fowler list, how accurate are predictions and how does this change with N?

# start with threshold of 20 observations
threshold_n = 20

bigN_bees=globi_u %>% group_by(scientificName) %>% summarize(n=n()) %>% filter(n>=threshold_n)
globi_summ = globi_u%>% filter(scientificName %in% bigN_bees$scientificName) %>%
  group_by(scientificName,bee_family,plant_genus,plant_family) %>% summarize(n_genus=n()) 

#predict whether a bee is a specialist or a generalist
#calculate the number of plant families each bee species is observed visiting
total_obs = bigN_bees

# degree_by_fam 
degree_by_fam = globi_summ %>% 
  group_by(scientificName,plant_family) %>%
  summarize(n_family = sum(n_genus)) %>%
  summarize(degree_family = n_distinct(plant_family), 
            degree5_family = sum(n_family >= 5), 
            simpson_fam = diversity(n_family,index='invsimpson'))# %>%

diversity(x, index = "simpson")

#calculate the number of plant genera each bee species is observed visiting
degree_by_genus = globi_summ %>%
  group_by(scientificName,bee_family) %>%
  summarize(degree_genus = n_distinct(plant_genus), 
            degree5_genus = sum(n_genus >= 5), 
            simpson_genus = diversity(n_genus,index='invsimpson')) 


globi_degree = degree_by_fam %>% left_join(degree_by_genus) %>% 
  left_join(diet_breadth %>% 
              distinct(scientificName,diet_breadth,diet_breadth_detailed)) %>%
  mutate(diet_breadth = ifelse(is.na(diet_breadth),'generalist',diet_breadth),
         diet_breadth_detailed = ifelse(is.na(diet_breadth_detailed),'generalist',diet_breadth_detailed)) %>%
  left_join(total_obs)

nrow(degree_by_fam) == nrow(globi_degree)
with(globi_degree,boxplot(simpson_fam~diet_breadth_detailed))
with(globi_degree,boxplot(simpson_fam~diet_breadth))
with(globi_degree,boxplot(simpson_genus~diet_breadth))

#make boxplots for the ms
point_col = adjustcolor('cadetblue',.5)
box_col = adjustcolor('white',0)
loc_gen=1.2; loc_spec=1.8
cex_lab = 1.5
cex_axis=1.3

# pdf('figures/boxplots_spec_gen_visitation.pdf',width=12)
par(mfrow=c(1,2))
with(globi_degree,stripchart(simpson_fam~diet_breadth,method = 'jitter',
                             vertical=T,col=point_col,at=c(loc_gen,loc_spec),pch=16,
                             cex.lab = cex_lab, cex.axis=cex_axis,
                             xlab = 'bee diet breadth',ylab = 'simpson diversity of plant families visited'))
with(globi_degree,boxplot(simpson_fam~diet_breadth,col=box_col,add=T,at=c(loc_gen,loc_spec), axes = F,boxwex=c(.35,.35)))


with(globi_degree,stripchart(simpson_genus~diet_breadth,method = 'jitter',
                             vertical=T,col=point_col,at=c(loc_gen,loc_spec),pch=16,
                             cex.lab = cex_lab, cex.axis = cex_axis,
                             xlab = 'bee diet breadth',ylab = 'simpson diversity of plant genera visited'))
with(globi_degree,boxplot(simpson_genus~diet_breadth,col=box_col,add=T,at=c(loc_gen,loc_spec),axes=F,boxwex=c(.35,.35)))
# dev.off()

#mean simpson diversity of plants visited by specialists & gens
(mean_simpsons = globi_degree %>% 
    group_by(diet_breadth) %>%
    summarize(simp_fam = mean(simpson_fam),
              simp_genus = mean(simpson_genus)))


(mean_simpsons$simp_fam[1]-mean_simpsons$simp_fam[2])/mean_simpsons$simp_fam[2]*100
(mean_simpsons$simp_genus[1]-mean_simpsons$simp_genus[2])/mean_simpsons$simp_genus[2]*100


# write_csv(globi_degree,'modeling_data/globi_degree.csv')

globi_degree %>% 
  filter(diet_breadth=='specialist') %>%
  filter(simpson>6) %>% 
  distinct(scientificName)


#run the random forest model
rf_all = randomForest(as.factor(diet_breadth) ~ degree_family + degree_genus + simpson_fam + simpson_genus + bee_family + n,data = globi_degree,importance = T)
importance(rf_all)
rf_all$confusion

# pdf('figures/var_importance_rf.pdf',width =13)
varImpPlot(rf_all)
# dev.off()

threshold_n= 60

#what threshold of observations do we need?
set.seed(4435)
what_n = seq(0,300,10) %>% purrr::map(function(threshold_n){
  
  # only include bees in the data with at least threshold_n observations
  bigN_bees=globi_u %>% group_by(scientificName) %>% summarize(n=n()) %>% filter(n>=threshold_n)
  globi_summ = globi_u%>% filter(scientificName %in% bigN_bees$scientificName) %>%
    group_by(scientificName,bee_family,plant_genus,plant_family) %>% summarize(n_genus=n()) 
  
  # predict whether a bee is a specialist or a generalist
  # calculate the number of plant families each bee species is observed visiting
  total_obs = bigN_bees
  
  # degree_by_fam 
  degree_by_fam = globi_summ %>% 
    group_by(scientificName,plant_family) %>%
    summarize(n_family = sum(n_genus)) %>%
    summarize(degree_family = n_distinct(plant_family), 
              degree5_family = sum(n_family >= 5), 
              simpson_fam = diversity(n_family,index='invsimpson'))# %>%
  
  #calculate the number of plant genera each bee species is observed visiting
  degree_by_genus = globi_summ %>%
    group_by(scientificName,bee_family) %>%
    summarize(degree_genus = n_distinct(plant_genus), 
              degree5_genus = sum(n_genus >= 5), 
              simpson_genus = diversity(n_genus,index='invsimpson')) 
  
  
  globi_degree = degree_by_fam %>% left_join(degree_by_genus) %>% 
    left_join(diet_breadth %>% 
                distinct(scientificName,diet_breadth,diet_breadth_detailed)) %>%
    mutate(diet_breadth = ifelse(is.na(diet_breadth),'generalist',diet_breadth),
           diet_breadth_detailed = ifelse(is.na(diet_breadth_detailed),'generalist',diet_breadth_detailed)) %>%
    left_join(total_obs) %>%
    mutate(diet_breadth = as.factor(diet_breadth))
  
  #run the random forest model
  rf_all = randomForest(as.factor(diet_breadth) ~ degree_family + degree_genus + simpson_fam + simpson_genus + bee_family + n,data = globi_degree,importance = T)
  
  oob_error = rf_all$err.rate[500,][1]
  rf_df=data.frame(rf_all$confusion) %>% 
    mutate(threshold_n=threshold_n) %>%
    mutate(oob=oob_error,n_species_all = nrow(globi_degree),
           n_species_gen=nrow(globi_degree %>% filter(diet_breadth=='generalist')),
           n_species_spec=nrow(globi_degree %>% filter(diet_breadth=='specialist')))
  
  rf_df$diet_breadth = row.names(rf_df)
  row.names(rf_df) <- NULL
  
  return(rf_df)

  })

#})

par(mfrow=c(1,1))
with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='specialist'), plot(threshold_n,class.error))

with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='generalist'), plot(threshold_n,class.error))
with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='generalist'), plot(threshold_n,oob))
with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='generalist'), plot(threshold_n,n_species_all))
with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='generalist'), plot(threshold_n,n_species_gen))
with(what_n %>% bind_rows %>% 
       filter(diet_breadth=='generalist'), plot(threshold_n,n_species_spec))

# Fit gam to the data to see what sample size minimizes classification error
library(gam)
specialist_data = what_n %>% bind_rows %>% 
  filter(diet_breadth=='specialist') %>% mutate(n_classified = generalist+specialist)
generalist_data = what_n %>% bind_rows %>% 
  filter(diet_breadth=='generalist')%>% mutate(n_classified = generalist+specialist)

gam()
gam_specs = gam(class.error ~ s(threshold_n,3),data=specialist_data) ## should this be binomial??
gam_specs = gam(class.error ~ s(threshold_n,3),family='binomial',data=specialist_data,weights=n_classified) ## should this be binomial??

plot(gam_specs)
gam_gens = gam(class.error ~ s(threshold_n,3),data=generalist_data)
gam_gens = gam(class.error ~ s(threshold_n,3),family='binomial',weights=n_classified,data=generalist_data)


plot(gam_gens)

new_data <- data.frame(threshold_n = seq(0,300,by=.1)) # make a data.frame of new explanatory variables
new_data$gam_specs <- predict(gam_specs, newdata=new_data, type='response') # predict on the scale of the response
new_data$gam_gens <- predict(gam_gens, newdata=new_data, type='response') # predict on the scale of the response

x_axis_lab = 'sample size required for inclusion'
ylims = c(30,100)
my_cex = 1.7
cex_pts = 1.5
the_point_col = adjustcolor('black',.8)
the_lwd=3
# pdf('figures/sample size results.pdf',width=12)
par(mfrow=c(1,2),mar=c(5,5,4,2))
plot(100-class.error*100~threshold_n,data = specialist_data,pch = 16, ylim = ylims,col = the_point_col,
     xlab= x_axis_lab, ylab='prediction accuracy of specialists (%)',cex.lab=my_cex,cex=cex_pts)
with(new_data,lines(100-gam_specs*100~threshold_n,col="deeppink4",lwd=the_lwd))

plot(100-class.error*100~threshold_n,data = generalist_data, pch = 16, ylim = ylims, col = the_point_col,
     xlab= x_axis_lab, ylab='prediction accuracy of generalists (%)',cex.lab=my_cex,cex=cex_pts)
with(new_data,lines(100-gam_gens*100~threshold_n,col='deeppink4',lwd=the_lwd))
# dev.off()

#calculate: where is the sample size that minimizes prediction error?
min_gen_index = which(new_data$gam_gens == min(new_data$gam_gens))
min_spec_index = which(new_data$gam_specs == min(new_data$gam_specs))

#lines of code below give the give the sample size for inclusion that minimizes prediction error
new_data$threshold_n[min_gen_index] # for generalists
new_data$threshold_n[min_spec_index] # for specialists

what_n %>% bind_rows %>% filter(threshold_n  == round(new_data$threshold_n[min_spec_index]))
what_n %>% bind_rows %>% filter(threshold_n  == 130)

what_n[[min_gen_index]]


# for bees on the Jarrod Fowler lists, what % visit their host plants?
# for the globi data: categorize interaction partner as host or non-host
# loop through the bee speices in globi_u
# get their host plants
# if the interaciton partner is in the family or genus then categorize it as a host
#fowler_specialists = #fowler[fowler$diet_breadth == 'specialist',]$scientificName

# let's exclude bees with fewer than 20 interactions in the globi dataset
bigN_bees
globi_specs = globi_summ %>% 
  #filter(scientificName %in% bigN_bees$scientificName) %>%
  left_join(diet_breadth) %>%
  filter(diet_breadth=='specialist') %>%
  filter(n_genus >=5)##let's exclude interactions wiht fewer than 5 records

# double check that specialist bees have either farmily as host rank or genus
# but not both
which(fowler_formatted %>% split(.$scientificName) %>%
  purrr::map_lgl(function(df) n_distinct(df$host_rank)>1))

a = globi_specs %>% split(1:nrow(globi_specs))
row = a[[188]]
empty_list = c()
globi_host = globi_specs %>% split(1:nrow(globi_specs)) %>% 
  purrr::map_dfr(function(row){
  
  (host_plant_df = fowler_formatted %>% 
    filter(scientificName == row$scientificName[1]))
  
  if(unique(host_plant_df$host_rank) =='genus') visiting_host = row$plant_genus  %in% host_plant_df$host
  if(unique(host_plant_df$host_rank) =='family') visiting_host = row$plant_family %in% host_plant_df$host
  if(unique(host_plant_df$host_rank) =='tribe') visiting_host = row$plant_family =='Asteraceae'
  
  return(row %>% mutate(host=visiting_host))

  }) 

##what percentage of specialist bees visit their host plants?
globi_host %>%
  group_by(scientificName,host) %>%
  summarize(sum_n=sum(n_genus)) 

host_fidelity=globi_host %>%
  group_by(scientificName) %>%
  summarize(sum_n=sum(n_genus[host])/sum(n_genus),sample_size = sum(n_genus)) 
#pdf('figures/histogram_visits_hosts_specialists-nothreshold.pdf')
par(mfrow=c(1,1))
hist(host_fidelity$sum_n*100,main='pollen specialists - visits to host plants',
     xlab='% of visits to host plants',col=point_col,cex.lab=cex_lab)
# dev.off()
host_fidelity %>% filter(sum_n<.3)

with(host_fidelity,plot(sample_size,sum_n))

#load the discrepancies data 
#(tells us which bees have been found with non-host pollen in their pollen load)
discrepancies = read_csv('modeling_data/discrepancies_fowler_ms.csv')
head(discrepancies)
questionable = discrepancies %>% 
  filter(`This ms` =='generalist' &  Fowler=='specialist')
questionable$scientificName[questionable$scientificName %in% host_fidelity$scientificName]

#load the missouri state data
ms <- read_xlsx('modeling_data/DATA_pollen-diet-breadth_list_12-5-21_ALR-cleaned.xlsx', sheet = 3) %>%
  rename_all(tolower) %>%
  mutate(scientificName = paste0(genus, ' ',sub('.*\\. ','',species))) %>%
  select(scientificName,everything()) %>%
  rename(diet_breadth=`redefined pollen diet breadth`) %>%
  mutate(pollen_host = `pollen hosts`) %>%
  filter(!is.na(pollen_host))

ms_check = ms %>% filter(scientificName %in% fowler$scientificName)

#we just want the bees with primary pollen data - get rid of anything wiht just two refs
# bc will be fowler and droege and the species description ref in fowler and droege (not a primary pollen source)
a_string = ms_check$references[1]
more_sources_i = which(ms_check$references %>% purrr::map_lgl(function(a_string){
  length(strsplit(a_string,';')[[1]]) !=2
}))

#as a first approximation = lets compare these to bees on 'questionable' vec
ms_check[more_sources_i,] %>% select(scientificName, references)

not_questionable = ms_check[more_sources_i,] %>% 
  filter(!scientificName %in% questionable$scientificName)
not_questionable$scientificName[not_questionable$scientificName %in% host_fidelity$scientificName]
also_questionable = c("Andrena erythronii")

bees_w_data = host_fidelity %>%
  mutate(specialist_type = ifelse(scientificName %in% not_questionable$scientificName,'true',NA))%>%
  mutate(specialist_type = ifelse(scientificName %in% c(questionable$scientificName,also_questionable), 'facultative',specialist_type)) %>%
  filter(!is.na(specialist_type))

pdf('figures/preliminary_facultative_boxplot.pdf')
with(bees_w_data,boxplot(sum_n~specialist_type,names=c('true','false'),ylab = 'proportion of visits to host plant',xlab = 'found with nonhost pollen'))
dev.off()

#format data and look up primary pollen sources
look_up = ms %>%
  mutate(specialist_type = ifelse(scientificName %in% not_questionable$scientificName,'true',NA))%>%
  mutate(specialist_type = ifelse(scientificName %in% c(questionable$scientificName,also_questionable), 'facultative',specialist_type)) %>%
  filter(!is.na(specialist_type)) %>%
  filter(scientificName %in% host_fidelity$scientificName) %>%
  select(scientificName, specialist_type, diet_breadth,pollen_host,references)

# write_csv(look_up,'modeling_data/look_at_primary_data.csv')
look_up
