#get bee area
rm(list=ls())
library(tidyverse)
library(vroom)
library(purrr)
library(sf)
library(sp)
library(grDevices)
library(BBmisc)

#
my_dir = '/Volumes/Seagate/Chesshire2023'
list.files(my_dir)

#cleaned occurence records from chesshire et al
bees = fread(paste0(my_dir,"contiguousRecords_high_Only.csv"))

#get distinct updated names
bee_names = bees %>% distinct(scientificName, finalName) %>%
  rename(providedName = scientificName)
# write_csv(bee_names,paste0(my_dir,'Chesshire2023_nameAlignment.csv'))
# write_csv(bee_names,'modeling_data/Chesshire2023_nameAlignment.csv')

# get rid of non-natives 
nonnative=read_csv('modeling_data/nonnative_bees.csv') #need to realign using same naming conventions
bees_filtered = bees  %>% # only bees we have aligned names for
  filter(!finalName %in% nonnative$scientificName)  #get rid of non-natives
n_distinct(bees_filtered$finalName)
bees_filtered_distinct = bees_filtered %>%
  distinct(finalName,finalLatitude,finalLongitude) 
nrow(bees_filtered)#weird, shouldn't these be the same? I thought unique lat-longs were taken for each bee
nrow(bees_filtered_distinct)

#get rid of bees with fewer than 4 records
bees_4ormore = bees_filtered_distinct %>% group_by(finalName) %>%
  summarize(n=n()) %>%
  filter(n>=4)

my_list = bees_filtered_distinct %>% filter(finalName %in% bees_4ormore$finalName) %>%
  split(.$finalName)
sp = my_list[[1]]

bee_area = bees_filtered_distinct %>% 
  filter(finalName %in% bees_4ormore$finalName) %>%
  split(.$finalName) %>%
  purrr::map_dfr(function(sp){
    ch <- chull(sp$finalLongitude,sp$finalLatitude)
    coords = sp %>% select(finalLongitude,finalLatitude) %>% slice(ch,ch[1])
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
    
    data.frame(finalName = sp$finalName[1],
               area_m2 = as.numeric(area_m2)) %>%
      mutate(area_ha = area_m2*0.0001, spherical_geometry= !check_error)
    
  })


##
#double check the convex hull code by plotting
plot(coords,pch=16,col=adjustcolor("black",.8))
lines(coords[ch,])



##
# Data
sp = my_list[[10]]

ch <- chull(sp$finalLongitude,sp$finalLatitude)
coords = sp %>% select(finalLongitude,finalLatitude) %>% slice(ch,ch[1])
pols <- SpatialPolygons(list(Polygons(list(Polygon(coords)), 1)), proj4string = CRS("+proj=longlat +datum=WGS84"))



# plot data and convex-hull
ggplot(sp, aes(finalLatitude, finalLongitude, colour="black", fill="black")) + 
  geom_point() + 
  geom_density2d(alpha=.5) + 
  labs(x = "Latitude", y = "Longitude")+ 
  geom_polygon(data=coords, alpha=.2)


##for each species get median lat, long and area
#for species with not enough records to caclualte area , put NA

bee_geographic = bees_filtered_distinct %>%
  group_by(finalName) %>%
  summarize(med_lat = median(finalLatitude),med_long = median(finalLongitude),
            n_records = n()) %>% 
  left_join(bee_area)
bee_area

