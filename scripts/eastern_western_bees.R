rm(list=ls())
library(tidyverse)
library(vroom)
library(purrr)


#load the GBIF data:
#DOI's of download
# Apidae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.4rrwub
# Colletidae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.cg6954
# Melittidae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.d9csyb
# Halictidae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.e5yvth
# Megachildae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.c7ufpz
# Andrenidae: GBIF.org (14 February 2023) GBIF Occurrence Download https://doi.org/10.15468/dl.xq2r7f



#load the idigbio and gbif data
my_dir1 = '/Volumes/Seagate/idigbio_usa/'
my_dir2 = '/Volumes/Seagate/gbif_usa/'
(my_paths1 = paste0(my_dir1,list.files(my_dir1)))
(my_paths2 = paste0(my_dir2,list.files(my_dir2)))
my_paths = c(my_paths1, my_paths2)
#

#load the idigbio data and rename columns
bees_idig = my_paths1 %>% map_dfr(function(path){
  vroom(path) %>% 
    mutate("dwc:recordNumber" = as.character("dwc:recordNumber")) 
  })%>%
  rename(scientificName = `dwc:scientificName`,
         coordinateUncertaintyInMeters = `dwc:coordinateUncertaintyInMeters`,
         stateProvince = `dwc:stateProvince`,
         coords = `idigbio:geoPoint`,
         genus = `dwc:genus`,
         specificEpithet = `dwc:specificEpithet`) %>%
  mutate(decimalLongitude = as.numeric(sub("}.*","",sub(".*: ","",coords)))) %>% #format longitude
  mutate(decimalLatitude = as.numeric(sub(",.*","",sub(".*at\": ","",coords)))) %>%#format latitude
  filter(!'dwc:basisOfRecord' %in% c( 'fossilspecimen','machineobservation'))


#since there are so many columns for the gbif data
#we'll just select the ones we want
my_paths3 = my_paths2[!grepl("apid",my_paths2)]#since apidae is huge and takes a long time to load let's try doing it separately from the other files
apidae_path = my_paths2[grepl("apid",my_paths2)]
bees_gbif2 = my_paths3 %>% 
  map_dfr(function(path){
  vroom(path, col_select = c(genus,specificEpithet,scientificName, stateProvince, coordinateUncertaintyInMeters, decimalLatitude, decimalLongitude,basisOfRecord))
    })%>% filter(!basisOfRecord%in% c( 'FOSSIL_SPECIMEN','MACHINE_OBSERVATION'))

gbif_apidae = vroom(apidae_path, col_select = c(genus,specificEpithet,scientificName, stateProvince, coordinateUncertaintyInMeters, decimalLatitude, decimalLongitude,basisOfRecord))%>%
  filter(!basisOfRecord%in% c( 'FOSSIL_SPECIMEN','MACHINE_OBSERVATION'))
bees_gbif = bind_rows(bees_gbif2,gbif_apidae)

rm(bees_gbif2); rm(gbif_apidae)

                                                                                                           decimalLatitude, decimalLongitude)
# unique(bees_gbif$basisOfRecord)
# bees_gbif %>% filter(basisOfRecord%in% c( 'FOSSIL_SPECIMEN','MACHINE_OBSERVATION'))
# bees_gbif %>% filter(basisOfRecord =="MATERIAL_CITATION")
# bees_gbif2 = bees_gbif %>% filter(!basisOfRecord%in% c( 'FOSSIL_SPECIMEN','MACHINE_OBSERVATION'))
# 
# bees_gbif2 %>%
#   filter(genus=='Andrena' & grepl("sepulta",specificEpithet))
#now combine the three datasets

#columns we care about:
# scientificName, stateProvice, coordinateUncertaintyInMeters, decimalLatitude, decimalLongitude
bees = bees_gbif %>% 
  bind_rows(bees_idig %>% 
              select(genus,specificEpithet,scientificName, stateProvince, coordinateUncertaintyInMeters, 
                     decimalLatitude, decimalLongitude)) %>%
  mutate(genus = paste0(toupper(substr(genus,1,1)), substr(genus,2,nchar(genus)))) %>% #make first letter of genus uppercase
  mutate(sub(" .*",'',genus)) #remove any subgenera in the genus

test = "Anthophora (anthophoroides)"
sub(" .*",'',test)

# 1b get rid of anything without lat-long coordinates
# anything without accurate lat-long coordinates?
# or where they're inaccurate?
n_distinct(bees$coordinateUncertaintyInMeters)
length(bees$coordinateUncertaintyInMeters)

coord_uncertainties = unique(bees$coordinateUncertaintyInMeters)[!is.na(unique(bees$coordinateUncertaintyInMeters))]
summary(coord_uncertainties)

# this is arbitrary but let's get rid of bees where coordinate uncertainty
# is greater than 50k (about 30 mi). this seens conservative to me given our goal
# being 30 mi off isn't going to take you from the western half of the US to the eastern half
nrow(bees %>% filter(coordinateUncertaintyInMeters>50000)) #gets rid of about 7000 obs
bees2 = bees %>% filter(coordinateUncertaintyInMeters<50000) 
bees_coords = bees2 %>% 
  filter(!is.na(decimalLatitude)) %>% 
  filter(!is.na(decimalLongitude))
  
rm(bees) #remove bees since its so big
rm(bees2)
places_to_exclude = c("Hawaii","hawaii" ,"alberta" ,"virgin islands" ,"puerto rico","american samoa","guam","Alaska","alaska" )


# View(bees_coords %>%
#   filter(scientificName == "anthophora (melea) bomboides" ))
# 


bees_remove = c("Colletes aff annae","Lasioglossum macoupinense sensu gibbs aka former divergens","Agapostemon angelicus x texanus",
                "Lasioglossum undet", "Lasioglossum undetermined",
                "Stelis 6-maculata","Ashmeadiella c-astragali","Hylaeus undefinable")
bees = bees_coords %>% 
  distinct(genus, specificEpithet, stateProvince, 
                          decimalLatitude, decimalLongitude) %>%#get unique species at each lat-long
  filter(!stateProvince %in% places_to_exclude) %>% #also filter to just be bees in l48 states
  filter(!is.na(specificEpithet)) %>%#1 get rid of anything not id'd to species
  filter(!is.na(genus)) %>%
  mutate(genus = sub(' .*','',genus)) %>%
  mutate(scientificName = paste(genus,specificEpithet)) %>%
  filter(!grepl(", nr" ,scientificName)) %>% #get rid of uncertain species determinations
  filter(!grepl(" or " ,scientificName)) %>%
  filter(!grepl(" form " ,scientificName)) %>%
  filter(!scientificName %in% bees_remove) %>%
  filter(specificEpithet!='sp.') %>%
  filter(!grepl('sensu lato',scientificName)) %>%
  filter(!grepl('[(]',specificEpithet)) %>%
  filter(specificEpithet != 'undetermined' & specificEpithet != 'unplaced') 


bees %>%
  filter(grepl('unplaced',scientificName))

unique(bees$stateProvince)
nrow(bees);nrow(bees_coords) #reduces dataset size a lot

#filtering steps:
# 2 get unique names and align
unique_taxa = unique(bees$scientificName)
to_align = data.frame(scientificName = unique_taxa)
# write_csv(to_align,'modeling_data/bee_names_usa-unaligned.csv')
unique_taxa[1:100]

#look at bees with subspecies
look_at_me = which(strsplit(to_align$scientificName,' ') %>%
  purrr::map_int(length)>2)
to_align[look_at_me,]

to_align %>%
  filter(grepl('sensu lato',scientificName))

#read output from the name alignment
to_align %>% filter(grepl('striata',scientificName))
to_align %>% filter(grepl('[(]',scientificName))

bees_aligned = vroom('modeling_data/bee_names_usa-aligned.tsv') %>%
  filter(providedName %in% to_align$scientificName)

bees_aligned %>%
  filter(alignRelation == "NONE")

bees %>% filter(!scientificName %in% bees_aligned$providedName)

# View(bees_aligned %>%
#   filter(alignRelation=='SYNONYM_OF'))

#update names - use DL first, followed by itis, followed by col
#first filter just be discover life bees
dl_bees = bees_aligned %>% filter(alignedExternalId=='discoverlife' & alignRelation != 'NONE') %>% distinct(providedName,alignedAuthority) %>%
  mutate(source = 'discoverlife')

#add itis bees that are not in the discover life list
itis_bees = bees_aligned %>% filter(alignedExternalId=='itis' & alignRelation != 'NONE'& !providedName %in% dl_bees$providedName)%>% 
  distinct(providedName,alignedAuthority) %>%
  mutate(source = 'itis')
#add col bees that are not in the discover life list or the itis list
ncbi_bees = bees_aligned %>% filter(alignedExternalId=='ncbi' & alignRelation != 'NONE' & !providedName %in% c(dl_bees$providedName,itis_bees$providedName))%>% 
  distinct(providedName,alignedAuthority) %>% #hmmm eucera pruinosa is pepanapis pruinosa not sure why that's not showing up in discover life
  mutate(source = "ncbi") 
ncbi_bees[ncbi_bees$providedName == "Eucera pruinosa",]$alignedAuthority <- "Pepanapis pruinosa"
ncbi_bees[ncbi_bees$providedName == "Pepanapis pruinosa",]$source <- "CS"

name_update = rbind(dl_bees,itis_bees,ncbi_bees)


"Augochloropsis metallica" %in% to_align$scientificName
View(bees %>% filter(scientificName=='Augochloropsis metallica'))

#anything duplicated
name_update$providedName[duplicated(name_update$providedName)]

#any bees not on any list?
(missing = to_align %>% filter(!scientificName %in% name_update$providedName) )#the three lasioglossum are newly described species from Joel Gardner's paper
(missing2 = bees %>% filter(scientificName %in% missing$scientificName) %>%
  group_by(scientificName) %>% summarize(n=n()) %>% arrange(desc(n)))

# write_csv(missing2,'modeling_data/missing_alignment_gbif.csv')
checked_names = read_csv("modeling_data/missing_alignment_gbif-lookedUp.csv")

name_update2 = name_update %>% 
  bind_rows(checked_names %>% filter(!is.na(alignedAuthority)))


# 4 get rid of non-natives and bees we could align names for
nonnative=read_csv('modeling_data/nonnative_bees.csv')
bees_filtered = bees %>% filter(scientificName %in% name_update2$providedName) %>% # only bees we have aligned names for
  filter(!scientificName %in% nonnative$scientificName)  #get rid of non-natives

#now update the names
bees_named = bees_filtered %>%
  rename(providedName = scientificName) %>%
  left_join(name_update2 %>% rename(scientificName = alignedAuthority))

#make sure no extra rows were added:
nrow(bees_named) == nrow(bees_filtered)

# 5 combine things with subspecies with their species and get unique lat-long-species combs again
native_idig_gbif = bees_named %>% mutate(specificEpithet2 = sub(' .*',"",specificEpithet)) %>%
  mutate(scientificName2 = paste(genus,specificEpithet2)) %>%
  rename(full_name = scientificName) %>%
  rename(scientificName = scientificName2)

n_distinct(native_idig_gbif$scientificName) #how many species?
nrow(native_idig_gbif) #how many records
  
# write_csv(native_idig_gbif,'modeling_data/native_idig_gbif.csv')
library(tidyverse)
native_idig_gbif = read_csv('modeling_data/native_idig_gbif.csv')
data_distinct = native_idig_gbif %>% distinct(scientificName,decimalLatitude,decimalLongitude)


#let's double check and make sure everything's within the continental USA
states <- states(cb=F)
pnts2 <- data_distinct %>%
  mutate(x = decimalLongitude, y = decimalLatitude)
point_sf = st_as_sf(pnts2, coords = c("x", "y"), 
                    crs = 4326, agr = "constant")
states_trans = st_transform(states,4326)
# a=st_join(point_sf,states_trans)
sf_filter = st_filter(point_sf,states_trans)

#how many were removed?
nrow(data.frame(sf_filter))
filtered_data = data.frame(sf_filter)
nrow(data_distinct)
n_distinct(data_distinct$scientificName)
n_distinct(filtered_data$scientificName)

###
library(grDevices)
library(sp)
#code from Chesshire et al 2023

filtered_list = filtered_data %>% split(.$scientificName)
(sp = filtered_list[[2]])

bees_4ormore = filtered_data %>% group_by(scientificName) %>%
  summarize(n=n()) %>%
  filter(n>=4)
library(BBmisc)
my_list = filtered_data %>% filter(scientificName %in% bees_4ormore$scientificName) %>%
  split(.$scientificName)
sp=my_list[[2]]
sp = my_list$`Megachile xerophila`
bee_area = filtered_data %>% filter(scientificName %in% bees_4ormore$scientificName) %>%
  split(.$scientificName) %>%
  purrr::map_dfr(function(sp){
    ch <- chull(sp[,2:1])
    coords <- sp[,2:1][c(ch, ch[1]),] #closed polygon
    pols <- SpatialPolygons(list(Polygons(list(Polygon(coords)), 1)), proj4string = CRS("+proj=longlat +datum=WGS84"))
    check_error = is.error(try(st_as_sf(pols) %>% #some are having an error with spherical geometry and not sure why. this is fixed by turning spherical geomtery off
                                 st_area,silent = T))
    if(check_error){ #if there's an error turn off spherical geometry
      sf_use_s2(FALSE)
      area_m2 = st_as_sf(pols) %>%
        st_area
      sf_use_s2(T) # turn back on spherical geometry
      
    }else{
      area_m2 = st_as_sf(pols) %>%
        st_area
    }
    
    data.frame(scientificName = sp$scientificName[1],
                                    area_m2 = as.numeric(area_m2)) %>%
      mutate(area_ha = area_m2*0.0001, spherical_geometry= !check_error)
    
  })

bee_area
bee_area %>% filter(!spherical_geometry)

#what happens for bees wiht <4points?

#make df with area, mean lat, mean long, and n

