#get bee area
rm(list=ls())
library(tidyverse)
library(vroom)
library(purrr)
library(sf)
library(sp)
library(grDevices)
library(BBmisc)
library(data.table)
library(readxl)
library(lubridate)

#
my_dir = '/Volumes/Seagate/Chesshire2023/'
list.files(my_dir)

#cleaned occurence records from chesshire et al
#to be used for usa species list:
bees = fread(paste0(my_dir,"contiguousRecords_high_Only.csv")) #this contains only contiguous USA records

#get distinct updated names
bee_names = bees %>% distinct(scientificName, finalName) %>%
  rename(providedName = scientificName)
# write_csv(bee_names,paste0(my_dir,'Chesshire2023_nameAlignment.csv'))
# write_csv(bee_names,'modeling_data/Chesshire2023_nameAlignment.csv')


#rm bees since its so big
rm(bees)
#upload data with records for all of NA
#to be used for estimating extent of occurrence
allNA = fread(paste0(my_dir,"NorAmer_highQual_only_ALLfamilies.csv"))#this contains records records from Mexico, Canada, Alaska 

# get rid of non-natives 
nonnative=read_excel('modeling_data/nonnative_bees.xlsx') 

#filter North American dataset to just be bees in bee_names
#and just be native species
#also add bee average collection date
#dates in this df have two different formats that we need to account for
bees_filtered = allNA  %>% # only bees we have aligned names for
  filter(!finalName %in% nonnative$scientificName) %>%  #get rid of non-natives
  filter(finalName %in% bee_names$finalName) %>%
  mutate(fdate = ifelse(grepl('/',eventDate),as.character(mdy(eventDate)),as.character(as.Date(eventDate)))) %>%
  mutate(doy = yday(as.character(fdate)))

n_distinct(bees_filtered$finalName)
bees_filtered[1498290,]
bees_filtered %>%
  filter(is.na(fdate)) %>%
  distinct(eventDate) %>% #double check that the only thing that isn't getting reformatted is NAs
  mutate(mdy(eventDate))

#check that dates appear to be formatted correctly
bees_filtered %>% filter(grepl('/',eventDate)) %>%
  distinct(fdate,eventDate,doy)
bees_filtered %>% filter(!grepl('/',eventDate)) %>%
  distinct(fdate,eventDate,doy)

bees_filtered_distinct = bees_filtered %>%
  distinct(finalName,finalLatitude,finalLongitude,fdate,doy) 
nrow(bees_filtered)#
nrow(bees_filtered_distinct)

#split dataset - bees with 4 or more records
bees_4ormore = bees_filtered_distinct %>% group_by(finalName) %>%
  summarize(n=n()) %>%
  filter(n>=4) %>%
  mutate(fake_data=F)

#bees with less than four records
# for these bees we'll randomly sample coordiantes within a 100km buffer
# to get to the cour coordiantes needed to estiamte extent of occurrence
bees_3orless = bees_filtered_distinct %>% group_by(finalName) %>%
  summarize(n=n()) %>%
  filter(n<4) # there's a lot

#set buffersize
buffer_km = 100
buffer_m = buffer_km*1000

#let's just randomly add coords within a 100km radius of existing points
#function for randomly sampling within a polygon: spsample(columbus,n=12,"random")
new_list = bees_filtered_distinct %>% 
  filter(finalName %in% bees_3orless$finalName) %>%
  split(.$finalName)
sp = new_list[[9]]

set.seed(1832)
random_points_added= bees_filtered_distinct %>% 
  filter(finalName %in% bees_3orless$finalName) %>%
  split(.$finalName) %>%
  map_dfr(function(sp){
    
    new_points_needed = 4-nrow(sp) #calcualte how many points are needed to get to 4 points total
    
    #convert to a sf object, and than change to nad83 albers crs, which has units in  meters 
    point_sf = st_as_sf(sp, coords = c("finalLongitude", "finalLatitude"), crs = 4326)
    crs_meters <- st_as_sf(point_sf) %>% st_transform(5070) #transform to 
    
    #make buffer around each point
    buffer_sf=st_union(st_buffer(crs_meters,buffer_m)) #union merges the geometries together. needed for spsample
    buffer_sp = as_Spatial(buffer_sf) #convert to a spatial object
    
    #randomly pick points within the buffers
    new_points = spsample(buffer_sp,n=new_points_needed,"random",iter=20) #randomly sample the new points from within the buffer
    
    #convert to a dataframe
    new_coords_df = data.frame(new_points %>% st_as_sf %>% st_transform(4326)) %>% 
      extract(geometry, c('finalLongitude', 'finalLatitude'), '\\((.*), (.*)\\)', convert = TRUE) %>%
      mutate(eventDate = NA,doy = NA,finalName =sp$finalName[1],fake_data=T)
    
    sp %>% mutate(fake_data = F) %>% bind_rows(new_coords_df)
    
  })


# #code for plotting buffers and random points
# ggplot() +
#   geom_sf(data = buffer_sf,color = 'red') +
#   geom_sf(data=crs_meters,color='green')+
#   geom_sf(data=new_points %>% st_as_sf,col='blue')



##
#double check no names duplicated between bees_4ormore and random_points_added
random_points_added[random_points_added$finalName %in% bees_4ormore$finalName,]$finalName
bees_4ormore[bees_4ormore$finalName %in% random_points_added$finalName,]$finalName

bees_hasFakeData = bees_filtered_distinct %>%
  filter(finalName %in% bees_4ormore$finalName) %>%
  bind_rows(random_points_added)
my_list = bees_hasFakeData %>% 
  split(.$finalName)
sp = my_list[[5]]


bee_area = bees_hasFakeData %>% 
  split(.$finalName) %>%
  purrr::map_dfr(function(sp){
    ch <- chull(sp$finalLongitude,sp$finalLatitude)
    coords = sp %>% select(finalLongitude,finalLatitude) %>% slice(ch,ch[1])
    pols <- SpatialPolygons(list(Polygons(list(Polygon(coords)), 1)), proj4string = CRS("+proj=longlat +datum=WGS84"))
    check_error = is.error(try(st_as_sf(pols) %>% #some are having an error with spherical geometry and not sure why. this is fixed by turning spherical geomtery off
                                 st_transform(5070) %>% #transform to equal area projection
                                 st_area,silent = T))
    if(check_error){ #if there's an error turn off spherical geometry
      sf_use_s2(FALSE)
      area_m2 = st_as_sf(pols) %>%
        st_transform(5070) %>%
        st_area
      sf_use_s2(T) # turn back on spherical geometry
      
    }else{
      area_m2 = st_as_sf(pols) %>%
        st_transform(5070) %>%
        st_area
    }
    
    data.frame(finalName = sp$finalName[1],
               area_m2 = as.numeric(area_m2)) %>%
      mutate(area_ha = area_m2*0.0001, spherical_geometry= !check_error)
    
  })




##
#double check the convex hull code by plotting
a=bees_filtered_distinct %>% 
  filter(finalName %in% bees_4ormore$finalName) %>%
  split(.$finalName)
sp = a[[2]]
ch <- chull(sp$finalLongitude,sp$finalLatitude)
coords = sp %>% select(finalLongitude,finalLatitude) %>% slice(ch,ch[1])
# plot(coords,pch=16,col=adjustcolor("black",.8))
pols <- SpatialPolygons(list(Polygons(list(Polygon(coords)), 1)), proj4string = CRS("+proj=longlat +datum=WGS84"))

# plot data and convex-hull
ggplot(sp, aes(finalLatitude, finalLongitude, colour="black", fill="black")) + 
  geom_point() + 
  geom_density2d(alpha=.5) + 
  labs(x = "Latitude", y = "Longitude")+ 
  geom_polygon(data=coords, alpha=.2)


bee_geographic2 = bees_filtered_distinct %>% 
  group_by(finalName) %>%
  summarize(med_lat = median(finalLatitude),med_long = median(finalLongitude),
              n_chesshire = n()) %>% left_join(bee_area) %>%
  rename(scientificName = finalName)
with(bee_geographic2,plot(log(n_chesshire),area_ha))


mean_doy=bees_filtered_distinct %>%
  group_by(finalName) %>%
  summarize(mean_doy = mean(doy,na.rm=T),
            med_doy = median(doy,na.rm=T),
            quant10 = quantile(doy,.1,na.rm=T),
            quant90 = quantile(doy,.9,na.rm = T)) %>%
  mutate(flight_season = quant90-quant10)




#add day of year to bee_geographic
bee_geographic = bee_geographic2 %>%
  left_join(mean_doy %>% rename(scientificName = finalName))


# write_csv(bee_geographic,'modeling_data/chesshire2023_beeArea.csv')
# ##
# # Data
# sp = my_list[[10]]
# 
# ch <- chull(sp$finalLongitude,sp$finalLatitude)
# coords = sp %>% select(finalLongitude,finalLatitude) %>% slice(ch,ch[1])
# pols <- SpatialPolygons(list(Polygons(list(Polygon(coords)), 1)), proj4string = CRS("+proj=longlat +datum=WGS84"))
# 
# 
# 
# # plot data and convex-hull
# ggplot(sp, aes(finalLatitude, finalLongitude, colour="black", fill="black")) + 
#   geom_point() + 
#   geom_density2d(alpha=.5) + 
#   labs(x = "Latitude", y = "Longitude")+ 
#   geom_polygon(data=coords, alpha=.2)
# 
# 
# ##for each species get median lat, long and area
# #for species with not enough records to caclualte area , put NA
# 
# bee_geographic = bees_filtered_distinct %>%
#   group_by(finalName) %>%
#   summarize(med_lat = median(finalLatitude),med_long = median(finalLongitude),
#             n_records = n()) %>% 
#   left_join(bee_area)
# bee_area
# 
