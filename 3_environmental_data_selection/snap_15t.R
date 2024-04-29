##############
## Code to snap selected sites to the closest hydrography of two environmental 
# databases (RiverATLAS and Free Flowing Rivers).
# 
# RiverATLAS: https://www.hydrosheds.org/hydroatlas
# Free-Flowing Rivers: https://figshare.com/articles/dataset/Mapping_the_world_s_free-flowing_rivers_data_set_and_technical_documentation/7688801 
#
# 15t

# Required packages:
library(rgdal)
library(tidyverse)
library(sf)

data.na <- readOGR (dsn = ".",layer = "RiverATLAS_v10_na") 

data_na_sf <- data.na %>% st_as_sf()


# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z15 <- utm.zones[utm.zones$ZONE == "15", ]
z15t <- z15[z15$ROW_ == "T",]
z15t_sf<-z15t %>% st_as_sf() 

# clip riverATLAS with the zone 
clip_data_na_z15t <- data_na_sf %>% sf::st_intersection(z15t_sf) %>% 
  as("Spatial")

# select variables of interest
clip_data_na_z15t_sf <- clip_data_na_z15t %>% st_as_sf()

select_cols <- subset(clip_data_na_z15t_sf, select = c(dis_m3_pyr,ele_mt_cav,tmp_dc_cyr,
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

# clip of sites with the zone
clip_points_15t<- points.sf %>% sf::st_intersection(z15t_sf)%>%  
  as("Spatial") 

# convert
atlas_15t_utm<- spTransform(select_cols_sp,CRS("+proj=utm +zone=15 +datum=WGS84 +ellps=WGS84"))
points_15t_utm<- spTransform(clip_points_15t,CRS("+proj=utm +zone=15 +datum=WGS84 +ellps=WGS84"))

# check if it are in the same projection:
st_crs(atlas_15t_utm)==st_crs(points_15t_utm) #TRUE

# snap
snap_15t <- maptools::snapPointsToLines(points_15t_utm, atlas_15t_utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points_15t_utm@data) %>% # combina com as informa??es dos dados da tabela de atributos dos pontos
  st_as_sf()  

# buffer 
atlas_15t_utm_sf <-atlas_15t_utm %>% st_as_sf()
buffer_15t <- st_buffer(atlas_15t_utm_sf, 10)

# clip
intersection <- snap_15t %>% sf::st_intersection(buffer_15t)

# remove unnecessary columns:
remov_cols <- subset(intersection, select = -c(FID:EAST_VALUE,nearest_line_id,snap_dist,
                                               Quarter.y))

hydro_atlas_15t <- remov_cols

#
### ok, now we will also clip the Free Flowing Rivers

ffr_na <- st_read(dsn="FFR_na_river_network_v1.shp") %>% # FFR North America
  dplyr::select(CSI, CSI_FF2)

# clip 
clip_z15t_ffr <- ffr_na %>% sf::st_intersection(z15t_sf) %>% 
  as("Spatial")

# convert
clip_z15t_ffr_utm<- spTransform(clip_z15t_ffr,CRS("+proj=utm +zone=15 +datum=WGS84 +ellps=WGS84"))

# check if is the same projection:
st_crs(points_15t_utm)==st_crs(clip_z15t_ffr_utm) #TRUE

# snap
snap_ffr_15t <- maptools::snapPointsToLines(points_15t_utm, clip_z15t_ffr_utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points_15t_utm@data) %>% 
  st_as_sf()  

# buffer 
clip_z15t_ffr_utm_sf <- clip_z15t_ffr_utm %>% st_as_sf() 

buffer10 <- st_buffer(clip_z15t_ffr_utm_sf, 10)

# Intersection
points_info <- snap_ffr_15t %>% sf::st_intersection(buffer10)

# select columns of interest:
ffr_select_columns <- points_info %>% 
  dplyr::select(HYBAS_ID,SiteID, CSI, CSI_FF2) %>% 
  unique() %>% 
  st_drop_geometry

# Now, join the two tables:
environment.15t <- full_join(hydro_atlas_15t,ffr_select_columns)
view(environment.15t)
environment.15t.drop.geo <- st_drop_geometry(environment.15t)

write.table(environment.15t.drop.geo, file="environment.15t.csv",sep = ",") # export
