##############
## Code to snap selected sites to the closest hydrography of two environmental 
# databases (RiverATLAS and Free Flowing Rivers).
# 
# RiverATLAS: https://www.hydrosheds.org/hydroatlas
# Free-Flowing Rivers: https://figshare.com/articles/dataset/Mapping_the_world_s_free-flowing_rivers_data_set_and_technical_documentation/7688801 
#
# 33t

# Required packages:
library(rgdal)
library(tidyverse)
library(sf)

data.eu <- readOGR (dsn = ".",layer = "RiverATLAS_v10_eu") 

data_eu_sf <- data.eu %>% st_as_sf()


# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z33 <- utm.zones[utm.zones$ZONE == "33", ]
z33t <- z33[z33$ROW_ == "T",]
z33t_sf<-z33t %>% st_as_sf() 

#clip riverATLAS with the zone 
clip_data_eu_z33t <- data_eu_sf %>% sf::st_intersection(z33t_sf) %>% 
  as("Spatial")

# select variables of interest
clip_data_eu_z33t_sf <- clip_data_eu_z33t %>% st_as_sf()

select_cols <- subset(clip_data_eu_z33t_sf, select = c(dis_m3_pyr,ele_mt_cav,tmp_dc_cyr,
                                                       pre_mm_cyr,hft_ix_c93, 
                                                       hft_ix_c09))
view(select_cols)

select_cols_sp <- select_cols %>% as_Spatial()

# read and join count and density data:
count <- read.csv("count.csv",header=T,sep=",")
dens <- read.csv("dens.csv",header=T,sep=",")
points <- rbind(count,dens)

points.sf <- points %>% 
  dplyr::mutate(longitude=long,latitude=lat) %>% 
  st_as_sf(coords = c(x = "longitude", y = "latitude"), crs = 4326)

# clip sites with the zone
clip_points_33t<- points.sf %>% sf::st_intersection(z33t_sf)%>%  
  as("Spatial") 

# convert
atlas_33t_utm<- spTransform(select_cols_sp,CRS("+proj=utm +zone=33 +datum=WGS84 +ellps=WGS84"))
points_33t_utm<- spTransform(clip_points_33t,CRS("+proj=utm +zone=33 +datum=WGS84 +ellps=WGS84"))

# check if is the same projection
st_crs(atlas_33t_utm)==st_crs(points_33t_utm) #TRUE

# snap
snap_33t <- maptools::snapPointsToLines(points_33t_utm, atlas_33t_utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points_33t_utm@data) %>% 
  st_as_sf()  

# buffer 
atlas_33t_utm_sf <-atlas_33t_utm %>% st_as_sf()
buffer_33t <- st_buffer(atlas_33t_utm_sf, 10) 

# intersection
intersection <- snap_33t %>% sf::st_intersection(buffer_33t)

# remove unnecessary columns
remov_cols <- subset(intersection, select = -c(FID:EAST_VALUE,nearest_line_id,snap_dist,
                                               Quarter.y))

hydro_atlas_33t <- remov_cols

#
### ok, now we will also clip the Free Flowing Rivers

ffr_eu <- st_read(dsn="FFR_eu_river_network_v1.shp") %>% # FFR Europe
  dplyr::select(CSI, CSI_FF2)

# clip 
clip_z33t_ffr <- ffr_eu %>% sf::st_intersection(z33t_sf) %>% 
  as("Spatial")

# convert 
clip_z33t_ffr_utm<- spTransform(clip_z33t_ffr,CRS("+proj=utm +zone=33 +datum=WGS84 +ellps=WGS84"))

# check if is the same projection
st_crs(points_33t_utm)==st_crs(clip_z33t_ffr_utm) #TRUE

# snap
snap_ffr_33t <- maptools::snapPointsToLines(points_33t_utm, clip_z33t_ffr_utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points_33t_utm@data) %>% 
  st_as_sf()  

# buffer 
clip_z33t_ffr_utm_sf <- clip_z33t_ffr_utm %>% st_as_sf() 

buffer10 <- st_buffer(clip_z33t_ffr_utm_sf, 10)

# Intersection 
points_info <- snap_ffr_33t %>% sf::st_intersection(buffer10)

# select columns
ffr_select_columns <- points_info %>% 
  dplyr::select(HYBAS_ID,SiteID, CSI, CSI_FF2) %>% 
  unique() %>% 
  st_drop_geometry

# Now, join the two tables:
environment.33t<- full_join(hydro_atlas_33t,ffr_select_columns)
view(environment.33t)
environment.33t.drop.geo <- st_drop_geometry(environment.33t)

write.table(environment.33t.drop.geo, file="environment.33t.csv",sep = ",") # export
