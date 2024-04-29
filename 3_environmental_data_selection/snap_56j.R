##############
## Code to snap selected sites to the closest hydrography of two environmental 
# databases (RiverATLAS and Free Flowing Rivers).
# 
# RiverATLAS: https://www.hydrosheds.org/hydroatlas
# Free-Flowing Rivers: https://figshare.com/articles/dataset/Mapping_the_world_s_free-flowing_rivers_data_set_and_technical_documentation/7688801 
#
# zone 56j

# Required packages:
library(rgdal)
library(tidyverse)
library(sf)

data_au <- readOGR (dsn = ".",layer = "RiverATLAS_v10_au")  # Australia

data_au_sf <- data_au %>% st_as_sf()

# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z56 <- utm.zones[utm.zones$ZONE == "56", ]
z56j <- z56[z56$ROW_ == "J",]
z56j_sf<-z56j %>% st_as_sf() 

#clip riverATLAS with the zone 
clip_data_au_z56j <- data_au_sf %>% sf::st_intersection(z56j_sf) %>% 
  as("Spatial")

# select variables of interest
clip_data_au_z56j_sf <- clip_data_au_z56j %>% st_as_sf()

select_cols <- subset(clip_data_au_z56j_sf, select = c(dis_m3_pyr,ele_mt_cav,tmp_dc_cyr,
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
clip_points_56j<- points.sf %>% sf::st_intersection(z56j_sf)%>%  
  as("Spatial") 

# convert to UTM 56
atlas_56j_utm<- spTransform(select_cols_sp,CRS("+proj=utm +zone=56 +datum=WGS84 +ellps=WGS84"))
points_56j_utm<- spTransform(clip_points_56j,CRS("+proj=utm +zone=56 +datum=WGS84 +ellps=WGS84"))

# check if is the same projection
st_crs(atlas_56j_utm)==st_crs(points_56j_utm) #TRUE

# snap
snap_56j <- maptools::snapPointsToLines(points_56j_utm, atlas_56j_utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points_56j_utm@data) %>% 
  st_as_sf()  

# buffer 
atlas_56j_utm_sf <-atlas_56j_utm %>% st_as_sf()
buffer_56j <- st_buffer(atlas_56j_utm_sf, 10)

# intersection
intersection <- snap_56j %>% sf::st_intersection(buffer_56j)

# remove unnecessary column
remov_cols <- subset(intersection, select = -c(FID:EAST_VALUE,nearest_line_id,snap_dist,
                                              Quarter.y))
  
hydro_atlas_56j <- remov_cols

#
### ok, now we will also clip the Free Flowing Rivers

ffr_au <- st_read(dsn="FFR_au_river_network_v1.shp") %>% # FFR Australia
  dplyr::select(CSI, CSI_FF2)

# clip 
clip_z56j_ffr <- ffr_au %>% sf::st_intersection(z56j_sf) %>% 
  as("Spatial")

# convert
clip_z56j_ffr_utm<- spTransform(clip_z56j_ffr,CRS("+proj=utm +zone=56 +datum=WGS84 +ellps=WGS84"))

# check if is the same projection
st_crs(points_56j_utm)==st_crs(clip_z56j_ffr_utm) #TRUE

# snap
snap_ffr_56j <- maptools::snapPointsToLines(points_56j_utm, clip_z56j_ffr_utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points_56j_utm@data) %>% 
  st_as_sf()  

# buffer
clip_z56j_ffr_utm_sf <- clip_z56j_ffr_utm %>% st_as_sf() 

buffer10 <- st_buffer(clip_z56j_ffr_utm_sf, 10)

# Intersection
points_info <- snap_ffr_56j %>% sf::st_intersection(buffer10)

# select columns
ffr_select_columns <- points_info %>% 
  dplyr::select(HYBAS_ID,SiteID, CSI, CSI_FF2) %>% 
  unique() %>% 
  st_drop_geometry

# Now, join the two tables:
environment.56j <- full_join(hydro_atlas_56j,ffr_select_columns)
view(environment.56j)
environment.56j.drop.geo <- st_drop_geometry(environment.56j)

write.table(environment.56j.drop.geo, file="environment.56j.csv",sep = ",") # export
