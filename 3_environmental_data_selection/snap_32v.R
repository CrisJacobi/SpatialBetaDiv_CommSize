##############
## Code to snap selected sites to the closest hydrography of two environmental 
# databases (RiverATLAS and Free Flowing Rivers).
# 
# RiverATLAS: https://www.hydrosheds.org/hydroatlas
# Free-Flowing Rivers: https://figshare.com/articles/dataset/Mapping_the_world_s_free-flowing_rivers_data_set_and_technical_documentation/7688801 
#
# 32v

# Required packages
library(rgdal)
library(tidyverse)
library(sf)

data.eu <- readOGR (dsn = ".",layer = "RiverATLAS_v10_eu") 

data_eu_sf <- data.eu %>% st_as_sf()


# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z32 <- utm.zones[utm.zones$ZONE == "32", ]
z32v <- z32[z32$ROW_ == "V",]
z32v_sf<-z32v %>% st_as_sf() 

#clip riverATLAS with the zone 
clip_data_eu_z32v <- data_eu_sf %>% sf::st_intersection(z32v_sf) %>% 
  as("Spatial")

# select variables of interest
clip_data_eu_z32v_sf <- clip_data_eu_z32v %>% st_as_sf()

select_cols <- subset(clip_data_eu_z32v_sf, select = c(dis_m3_pyr,ele_mt_cav,tmp_dc_cyr,
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
clip_points_32v<- points.sf %>% sf::st_intersection(z32v_sf)%>%  
  as("Spatial") 

# convert
atlas_32v_utm<- spTransform(select_cols_sp,CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84"))
points_32v_utm<- spTransform(clip_points_32v,CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84"))

# check if is the same projection
st_crs(atlas_32v_utm)==st_crs(points_32v_utm) #TRUE

# snap
snap_32v <- maptools::snapPointsToLines(points_32v_utm, atlas_32v_utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points_32v_utm@data) %>% 
  st_as_sf()  

# buffer 
atlas_32v_utm_sf <-atlas_32v_utm %>% st_as_sf()
buffer_32v <- st_buffer(atlas_32v_utm_sf, 10) 

# intersection
intersection <- snap_32v %>% sf::st_intersection(buffer_32v)

# remove unnecessary columns
remov_cols <- subset(intersection, select = -c(FID:EAST_VALUE,nearest_line_id,snap_dist,
                                               Quarter.y))

hydro_atlas_32v <- remov_cols

#
### ok, now we will also clip the Free Flowing Rivers

ffr_eu <- st_read(dsn="FFR_eu_river_network_v1.shp") %>% # FFR Europe
  dplyr::select(CSI, CSI_FF2)

# clip 
clip_z32v_ffr <- ffr_eu %>% sf::st_intersection(z32v_sf) %>% 
  as("Spatial")

# convert
clip_z32v_ffr_utm<- spTransform(clip_z32v_ffr,CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84"))

# check if is the same projection
st_crs(points_32v_utm)==st_crs(clip_z32v_ffr_utm) #TRUE

# snap
snap_ffr_32v <- maptools::snapPointsToLines(points_32v_utm, clip_z32v_ffr_utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points_32v_utm@data) %>% 
  st_as_sf()  

# buffer 
clip_z32v_ffr_utm_sf <- clip_z32v_ffr_utm %>% st_as_sf() 

buffer10 <- st_buffer(clip_z32v_ffr_utm_sf, 10)

# Intersection
points_info <- snap_ffr_32v %>% sf::st_intersection(buffer10)

# select columns
ffr_select_columns <- points_info %>% 
  dplyr::select(HYBAS_ID,SiteID, CSI, CSI_FF2) %>% 
  unique() %>% 
  st_drop_geometry

# Now, join the two tables:
environment.32v<- full_join(hydro_atlas_32v,ffr_select_columns)
view(environment.32v)
environment.32v.drop.geo <- st_drop_geometry(environment.32v)

write.table(environment.32v.drop.geo, file="environment.32v.csv",sep = ",") # export
