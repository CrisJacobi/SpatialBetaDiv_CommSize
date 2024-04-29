##############
## Code to snap selected sites to the closest hydrography of two environmental 
# databases (RiverATLAS and Free Flowing Rivers).
# 
# RiverATLAS: https://www.hydrosheds.org/hydroatlas
# Free-Flowing Rivers: https://figshare.com/articles/dataset/Mapping_the_world_s_free-flowing_rivers_data_set_and_technical_documentation/7688801 
#
# zone 15u

# Required packages:
library(rgdal)
library(tidyverse)
library(sf)

data.na <- readOGR (dsn = ".",layer = "RiverATLAS_v10_na") # North America

data_na_sf <- data.na %>% st_as_sf()


# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z15 <- utm.zones[utm.zones$ZONE == "15", ]
z15u <- z15[z15$ROW_ == "U",]
z15u_sf<-z15u %>% st_as_sf() 

#clip riverATLAS with the zone 
clip_data_na_z15u <- data_na_sf %>% sf::st_intersection(z15u_sf) %>% 
  as("Spatial")

# select variables of interest
clip_data_na_z15u_sf <- clip_data_na_z15u %>% st_as_sf()

select_cols <- subset(clip_data_na_z15u_sf, select = c(dis_m3_pyr,ele_mt_cav,tmp_dc_cyr,
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

# intersection
clip_points_15u<- points.sf %>% sf::st_intersection(z15u_sf)%>%  
  as("Spatial") 

# convert
atlas_15u_utm<- spTransform(select_cols_sp,CRS("+proj=utm +zone=15 +datum=WGS84 +ellps=WGS84"))
points_15u_utm<- spTransform(clip_points_15u,CRS("+proj=utm +zone=15 +datum=WGS84 +ellps=WGS84"))

# check if it are in the same projection:
st_crs(atlas_15u_utm)==st_crs(points_15u_utm) #TRUE

# snap
snap_15u <- maptools::snapPointsToLines(points_15u_utm, atlas_15u_utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points_15u_utm@data) %>% 
  st_as_sf()  

# buffer
atlas_15u_utm_sf <-atlas_15u_utm %>% st_as_sf()
buffer_15u <- st_buffer(atlas_15u_utm_sf, 10)

# intersection
intersection <- snap_15u %>% sf::st_intersection(buffer_15u)

# remove unnecessary columns:
remov_cols <- subset(intersection, select = -c(FID:EAST_VALUE,nearest_line_id,snap_dist,
                                               Quarter.y))

hydro_atlas_15u <- remov_cols

#
### ok, now we will also clip the Free Flowing Rivers

ffr_na <- st_read(dsn="FFR_na_river_network_v1.shp") %>% # FFR Nort America
  dplyr::select(CSI, CSI_FF2)

# clip 
clip_z15u_ffr <- ffr_na %>% sf::st_intersection(z15u_sf) %>% 
  as("Spatial")

# convert
clip_z15u_ffr_utm<- spTransform(clip_z15u_ffr,CRS("+proj=utm +zone=15 +datum=WGS84 +ellps=WGS84"))

# check projection
st_crs(points_15u_utm)==st_crs(clip_z15u_ffr_utm) #TRUE

# snap
snap_ffr_15u <- maptools::snapPointsToLines(points_15u_utm, clip_z15u_ffr_utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points_15u_utm@data) %>% 
  st_as_sf()  

# buffer 
clip_z15u_ffr_utm_sf <- clip_z15u_ffr_utm %>% st_as_sf() 

buffer10 <- st_buffer(clip_z15u_ffr_utm_sf, 10)

# Intersection 
points_info <- snap_ffr_15u %>% sf::st_intersection(buffer10)

# select columns
ffr_select_columns <- points_info %>% 
  dplyr::select(HYBAS_ID,SiteID, CSI, CSI_FF2) %>% 
  unique() %>% 
  st_drop_geometry

# Now, join the two tables:
environment.15u <- full_join(hydro_atlas_15u,ffr_select_columns)
view(environment.15u)
environment.15u.drop.geo <- st_drop_geometry(environment.15u)

write.table(environment.15u.drop.geo, file="environment.15u.csv",sep = ",") # export
