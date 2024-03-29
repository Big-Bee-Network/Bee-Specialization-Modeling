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
library(giscoR)
library(tigris)
library(furrr)
library(geodist)

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
allNA = fread(paste0(my_dir,"NorAmer_highQual_only_ALLfamilies.csv")) %>%#this contains records records from Mexico, Canada, Alaska 
  mutate(finalLatitude=round(finalLatitude,3),finalLongitude=round(finalLongitude,3)) %>%  #let's round to 3, corresponds to approx 100m accuracy
  mutate(lat_long = paste(finalLatitude,finalLongitude))

#exclude observations not in North America
distinct_coords = allNA %>% distinct(lat_long,finalLatitude,finalLongitude)

#load map 
na_map <- gisco_get_countries(country = c("United States of America",'Canada',"Mexico"),resolution = 01)
#also get low-res NA map for plotting
na_map_lowRes <- gisco_get_countries(country = c("United States of America",'Canada',"Mexico"))
world <- gisco_get_countries()
pnts2 <- distinct_coords %>%
  mutate(x = finalLongitude, y = finalLatitude)
point_sf = st_as_sf(pnts2, coords = c("x", "y"), 
                    crs = 4326, agr = "constant")
sf_filter = st_filter(point_sf,na_map)

#how many were removed?
nrow(data.frame(sf_filter)); nrow(distinct_coords)
nrow(distinct_coords)-nrow(data.frame(sf_filter))

filtered_data = data.frame(sf_filter) %>%
  filter(finalLongitude<9)
excluded_data = distinct_coords %>% filter(!lat_long %in% filtered_data$lat_long)

#plot data-points excluded
ggplot(world) +
  geom_sf(fill = "blue", col = "white") +
  theme_minimal()+
  geom_point(data=excluded_data,aes(x=finalLongitude,y=finalLatitude))



#let's also exclude data in hawaii, american samoa etc
places_to_exclude = c("Hawaii","United States Virgin Islands" ,"Puerto Rico","American Samoa","Guam","Commonwealth of the Northern Mariana Islands")

islands <- states(cb=F) %>% filter(NAME %in% places_to_exclude)

islands_trans = st_transform(islands,4326)
islands_filter = st_filter(point_sf,islands_trans)
filtered_data2 = filtered_data %>% filter(!lat_long %in% islands_filter$lat_long)

#plot data points included
ggplot(na_map_lowRes) +
  geom_sf(fill = "blue", col = "white") +
  theme_minimal()+
  geom_point(data=filtered_data2,aes(x=finalLongitude,y=finalLatitude))

na <- map_data("world", region = c("Mexico","Canada")) %>% bind_rows(map_data("state"))
ggplot(na, aes(x = long, y = lat))+
  geom_polygon(aes( group = group, fill = region),show.legend = F)+
  geom_point(data=filtered_data2[1:100,] ,aes(x=finalLongitude,y=finalLatitude))


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
  mutate(doy = yday(as.character(fdate))) %>%
  filter(lat_long %in% filtered_data2$lat_long) #filters to just be bees in usa/canada/mexico

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


#look for outliers in the data
##try writing my own function
bee_mediumN = bees_filtered_distinct %>% 
  distinct(finalName, finalLongitude,finalLatitude) %>%
  group_by(finalName) %>% summarize(n=n()) %>%
  filter(n>=5 & n<10000)

bees_filtered_distinct %>% 
  distinct(finalName, finalLongitude,finalLatitude) %>%
  group_by(finalName) %>% summarize(n=n()) %>%
  filter(n>10000)

#for bees with very large sample sizes (eg bombus impatiens)
# doing pairwise distances exhausts vector memory limit
# we'll inspect these manually for outliers
manual_inspect = bees_filtered_distinct %>% 
  distinct(finalName, finalLongitude,finalLatitude) %>%
  filter(!finalName %in% bee_mediumN$finalName) %>%
  rename(species = finalName,longitude = finalLongitude, latitude=finalLatitude)

df_outlier_test = bees_filtered_distinct %>%
  distinct(finalName, finalLongitude,finalLatitude) %>%
  filter(finalName %in% bee_mediumN$finalName) %>%
  rename(species = finalName,longitude = finalLongitude, latitude=finalLatitude)

a=df_outlier_test %>%
  split(.$species)
df=a[[2]]
sp=df$species[1]
# my_map = ggplot(na, aes(x = long, y = lat))+
#   geom_polygon(aes( group = group, fill = region),show.legend = F)+
#   scale_fill_manual(values=rep('darkgreen',51))+
#   geom_point(data=df ,aes(x=decimallongitude,y=decimallatitude))+
#   scale_color_manual(values=c('black','red'))+theme_minimal()+
#   ggtitle(sp)
# my_map

df = filter(df_outlier_test,species=="Ptiloglossa arizonensis")
get_outliers = function(df){
  flag_dist=1500
  
  dist_mat = geodist(df, measure='haversine') 
  
  min_dists = dist_mat %>% apply(1,function(row) min(row[row!=0])  )/1000
  
  #flags coordinates >1000km from the nearest coordinate
  flag_i = which(min_dists>flag_dist) 
  df$test_passed <- TRUE
  
  #if there are flags, add test_passed =F to df
  if(length(flag_i) > 0){
    df[flag_i,]$test_passed <- F
  }
  
  return(df)
}

##this takes a long time to run:
plan(multisession, workers = 6)
check_outliers_ls = df_outlier_test %>% split(.$species) %>%
  future_map(get_outliers)
check_outliers = check_outliers_ls %>% map_dfr(function(df) df %>% mutate(id=1:nrow(.)))

##filter data to just be species with flagged specimens and plot
have_outliers = check_outliers %>% group_by(species) %>%
  summarize(prop_passing = mean(test_passed), sum_flagged=sum(test_passed==F)) %>%
  filter(prop_passing < 1.00) 
nrow(have_outliers)
(so_many_flagged = have_outliers %>% filter(sum_flagged>10)) #check if any species with lots of outliers
i=1
random_is = sample(1:nrow(have_outliers),10)
random_is_ordered = random_is[order(random_is)]


# pdf('figures/outlier_maps.pdf')
for(i in 1:nrow(have_outliers)){
  sp = have_outliers$species[i]
  df = check_outliers %>% filter(species == sp) %>%
    mutate(my_col = ifelse(test_passed,adjustcolor('black',.5),'red'),
           my_label = ifelse(test_passed, '',as.character(id)))
  df_rearranged = df %>% arrange(desc(my_col))

  
  my_map = ggplot(na, aes(x = long, y = lat))+
    geom_polygon(aes( group = group, fill = region),show.legend = F)+
    scale_fill_manual(values=rep('darkgreen',51))+
    geom_point(data=df_rearranged ,aes(x=longitude,y=latitude,col=my_col))+
    geom_text(data=df_rearranged,label=df_rearranged$my_label,nudge_y = 1.5,aes(x=longitude,y=latitude,col=my_col))+
    scale_color_manual(values=c('black','red'))+theme_minimal()+
    ggtitle(sp)
  
  print(my_map)
  
}
# dev.off()

not_an_outlier = data.frame(
  species = c("Osmia aquilonaria", "Osmia laticeps", "Osmia laticeps"),
  id = c(5, 4, 3)
)

#these are the species that we will keep
check_outliers %>% filter(species %in% not_an_outlier$species & test_passed ==F)
check_outliers[check_outliers$species %in% not_an_outlier$species & check_outliers$test_passed ==F,]$test_passed <- T

#visually inspect the bees that we didn't do the outlier test for
#(becuase >10000 specimens or fewer than 5)
# pdf('figures/outlier_maps.pdf')
manual_inspect_sp = unique(manual_inspect$species)

# pdf("figures/small_bigN_maps.pdf")
# for(i in 1:length(manual_inspect_sp)){
#   sp = manual_inspect_sp[i]
#   df = manual_inspect %>% filter(species == sp) %>%
#     mutate(my_col = "black")
# 
#   
#   my_map = ggplot(na, aes(x = long, y = lat))+
#     geom_polygon(aes( group = group, fill = region),show.legend = F)+
#     scale_fill_manual(values=rep('darkgreen',51))+
#     geom_point(data=df ,aes(x=longitude,y=latitude,col=my_col))+
#     scale_color_manual(values=c('black'))+theme_minimal()+
#     ggtitle(sp)
#   
#   print(my_map)
#   
# }
# dev.off()


#remove all other outliers
#double check 
outliers_removed = check_outliers %>% filter(test_passed) %>%
  bind_rows(manual_inspect) %>%
  mutate(latlongname = paste(latitude, longitude, species)) 

bees_filtered_noOutliers = bees_filtered_distinct %>% 
  mutate(latlongname = paste(finalLatitude,finalLongitude,finalName)) %>%
  filter(latlongname %in% outliers_removed$latlongname)

#split dataset - bees with 4 or more records
bees_4ormore = bees_filtered_noOutliers %>% group_by(finalName) %>%
  summarize(n=n_distinct(finalLatitude,finalLongitude)) %>%
  filter(n>=4) %>%
  mutate(fake_data=F)


#bees with less than four records
# for these bees we'll randomly sample coordiantes within a 100km buffer
# to get to the cour coordiantes needed to estiamte extent of occurrence
bees_3orless = bees_filtered_noOutliers %>% group_by(finalName) %>%
  summarize(n=n_distinct(finalLatitude,finalLongitude)) %>%
  filter(n<4) # there's a lot

#set buffersize
buffer_km = 100
buffer_m = buffer_km*1000

#let's just randomly add coords within a 100km radius of existing points
#function for randomly sampling within a polygon: spsample(columbus,n=12,"random")
new_list = bees_filtered_noOutliers %>% 
  filter(finalName %in% bees_3orless$finalName) %>%
  split(.$finalName)
sp = new_list[[9]]

set.seed(1832)
random_points_added= bees_filtered_noOutliers %>% 
  filter(finalName %in% bees_3orless$finalName) %>%
  split(.$finalName) %>%
  map_dfr(function(sp){
    
    new_points_needed = 4-n_distinct(sp$finalLatitude,sp$finalLongitude) #calcualte how many points are needed to get to 4 points total
    
    #convert to a sf object, and than change to nad83 albers crs, which has units in  meters 
    point_sf = st_as_sf(sp %>% distinct(finalName,finalLatitude,finalLongitude), coords = c("finalLongitude", "finalLatitude"), crs = 4326)
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

bees_hasFakeData = bees_filtered_noOutliers %>%
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

# find the elements which produced warnings
bee_area %>% purrr::map("warnings") %>% unlist
bees_hasFakeData %>% filter(finalName == 'Dufourea neovernalis')


ns=bees_hasFakeData %>% split(.$finalName) %>% map(nrow) %>% unlist
ns[ns<4]
##
#double check the convex hull code by plotting
a=bees_filtered_noOutliers %>% 
  filter(finalName %in% bees_4ormore$finalName) %>%
  split(.$finalName)
sp = a[[19]]
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


bee_geographic2 = bees_filtered_noOutliers %>% 
  group_by(finalName) %>%
  summarize(med_lat = median(finalLatitude),med_long = median(finalLongitude),
            n_chesshire = n()) %>% left_join(bee_area) %>%
  rename(scientificName = finalName)
with(bee_geographic2,plot(log(n_chesshire),area_ha))

bee_geographic2 %>%
  filter(grepl("Nomada park",scientificName))


mean_doy=bees_filtered_noOutliers %>%
  group_by(finalName) %>%
  summarize(mean_doy = mean(doy,na.rm=T),
            med_doy = median(doy,na.rm=T),
            quant10 = quantile(doy,.1,na.rm=T),
            quant90 = quantile(doy,.9,na.rm = T)) %>%
  mutate(flight_season = quant90-quant10)


#add day of year to bee_geographic
bee_geographic = bee_geographic2 %>%
  left_join(mean_doy %>% rename(scientificName = finalName))

bee_phen_nas = bee_geographic %>% filter(is.na(mean_doy))
#for a random sample of bees plot doy vs latitude
sp_list = unique(bees_filtered_noOutliers$finalName)[!unique(bees_filtered_noOutliers$finalName) %in% bee_phen_nas$scientificName]
plot_species = sample(sp_list,15)
for(my_sp in plot_species){
  focal_df = bees_filtered_noOutliers %>% filter(finalName == my_sp) %>% filter(!is.na(doy))
  with(focal_df, plot(finalLatitude,doy,main=my_sp))
  
}

# write_csv(bee_geographic,'modeling_data/chesshire2023_beeArea-11april2023.csv')



#compare old mehtod to new method:
old = read_csv('modeling_data/chesshire2023_beeArea.csv') %>%
  rename(old_area = area_ha)

area_comp = bee_geographic %>% rename(new_area=area_ha) %>% select(scientificName,new_area) %>%
  left_join(old %>% select(scientificName,old_area))

with(area_comp, plot(old_area,new_area))
area_comp %>% filter(old_area==0)
bees_filtered_noOutliers %>% filter(finalName=='Ashmeadiella chumashae')
area_comp %>% filter(old_area<1e09 & new_area>2e09)

#plot distribution of Bombus sylvicola
sp = 'Bombus sylvicola'
df =  check_outliers %>% filter(species == sp) %>%
    mutate(my_col = "black")


(my_map = ggplot(na, aes(x = long, y = lat))+
    geom_polygon(aes( group = group, fill = region),show.legend = F)+
    scale_fill_manual(values=rep('darkgreen',51))+
    geom_point(data=df ,aes(x=longitude,y=latitude,col=my_col))+
    scale_color_manual(values=c('black'))+theme_minimal()+
    ggtitle(sp))


sp = bees_hasFakeData %>% filter(finalName==sp)
ch <- chull(sp$finalLongitude,sp$finalLatitude)
coords = sp %>% select(finalLongitude,finalLatitude) %>% slice(ch,ch[1])
pols <- SpatialPolygons(list(Polygons(list(Polygon(coords)), 1)), proj4string = CRS("+proj=longlat +datum=WGS84"))
area_m2 = st_as_sf(pols) %>%
  st_transform(5070) %>%
  st_area
(area_ha=area_m2/10000)
area_comp %>% filter(old_area<1e09 & new_area>2e09)

# plot data and convex-hull
ggplot(sp, aes(finalLatitude, finalLongitude, colour="black", fill="black")) + 
  geom_point() + 
  geom_density2d(alpha=.5) + 
  labs(x = "Latitude", y = "Longitude")+ 
  geom_polygon(data=coords, alpha=.2)



##
##old
library(CoordinateCleaner)
bee_bigN = bees_filtered_distinct %>% group_by(finalName) %>% summarize(n=n()) %>%
  filter(n>=7)

df_outlier_test = bees_filtered_distinct %>%
  select(finalName, finalLongitude,finalLatitude) %>%
  filter(finalName %in% bee_bigN$finalName) %>%
  rename(species = finalName,decimallongitude = finalLongitude, decimallatitude=finalLatitude)
a=df_outlier_test %>%
  split(.$species)
df=a[[1]]
##this takes a long time to run:
plan(multisession, workers = 6)

check_outliers_ls = df_outlier_test %>%
  split(.$species) %>%
  future_map(function(df){
    
    outliers = cc_outl(df, method='distance',value = 'flagged')
    df %>% mutate(test_passed=outliers)
  
    })
check_outliers = check_outliers_ls %>% bind_rows


##filter data to just be species with flagged specimens and plot
have_outliers = check_outliers %>% group_by(species) %>%
  summarize(prop_passing = mean(test_passed), sum_flagged=sum(test_passed==F)) %>%
  filter(prop_passing < 1.00)
nrow(have_outliers)
so_many_flagged = have_outliers %>% filter(sum_flagged>10) #hmm, some have thousands that are flagged

i=1
random_is = sample(1:nrow(have_outliers),100)
random_is_ordered = random_is[order(random_is)]

for(i in random_is_ordered){
  sp = have_outliers$species[i]
  df = check_outliers %>% filter(species == sp) %>%
    mutate(my_col = ifelse(test_passed,adjustcolor('black',.5),'red'))
  df_rearranged = df %>% arrange(desc(my_col))
  # with(df %>% arrange(desc(my_col)),
  #      plot(decimallongitude,decimallatitude, col=my_col,pch=16 ,
  #      main=sp))
  
  my_map = ggplot(na, aes(x = long, y = lat))+
    geom_polygon(aes( group = group, fill = region),show.legend = F)+
    scale_fill_manual(values=rep('darkgreen',51))+
    geom_point(data=df_rearranged ,aes(x=decimallongitude,y=decimallatitude,col=my_col))+
    scale_color_manual(values=c('black','red'))+theme_minimal()+
    ggtitle(sp)

  print(my_map)
  
}


##based on these plots I don't think the cc_outl function is working...
for(i in 1:nrow(so_many_flagged)){
  sp = so_many_flagged$species[i]
  df = check_outliers %>% filter(species == sp) %>%
    mutate(my_col = ifelse(test_passed,adjustcolor('black',.5),'red'))
  df_rearranged = df %>% arrange(desc(my_col))
 
  my_map = ggplot(na, aes(x = long, y = lat))+
    geom_polygon(aes( group = group, fill = region),show.legend = F)+
    scale_fill_manual(values=rep('darkgreen',51))+
    geom_point(data=df_rearranged ,aes(x=decimallongitude,y=decimallatitude,col=my_col))+
    scale_color_manual(values=c('black','red'))+theme_minimal()+
    ggtitle(sp)
  
  print(my_map)
  
}





# plot data and convex-hull
ggplot(df %>% arrange(desc(my_col)), aes(finalLatitude, finalLongitude, colour=my_col, fill=my_col)) + 
  geom_point() #+ 
  geom_density2d(alpha=.5) + 
  labs(x = "Latitude", y = "Longitude")#+ 
  geom_polygon(data=coords, alpha=.2)


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
