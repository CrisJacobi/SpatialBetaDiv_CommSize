# zone 22j

library(rgdal)
library(tidyverse)
library(sf)
library(mapview)

# read the dataframe from the script (data_selection_1).

quarter.final <- read.csv("quarter.final.csv",header=T,sep=",")

points.sf <- quarter.final %>% 
  dplyr::mutate(longitude=long,latitude=lat) %>% 
  st_as_sf(coords = c(x = "longitude", y = "latitude"), crs = 4326)

# South America hydrography (https://www.hydrosheds.org/products/hydrorivers)
hydro.sa <- readOGR (dsn = ".",layer = "HydroRIVERS_v10_sa") 
hydro.sa.sf <- hydro.sa %>% st_as_sf() 

# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z22 <- utm.zones[utm.zones$ZONE == "22", ]
z22j <- z22[z22$ROW_ == "J",]
z22j_sf<-z22j %>% st_as_sf() 

# clip the sites with the zone
points22j <- points.sf %>% sf::st_intersection(z22j_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro22j <- hydro.sa.sf %>% sf::st_intersection(z22j_sf) %>% 
  as("Spatial")

# convert to UTM 22
hydro22j.utm<- spTransform(hydro22j,CRS("+proj=utm +zone=22 +datum=WGS84 +ellps=WGS84"))
points22j.utm<- spTransform(points22j,CRS("+proj=utm +zone=22 +datum=WGS84 +ellps=WGS84"))

points22j.utm.sf <- points22j.utm %>% st_as_sf() 
hydro22j.utm.sf <- hydro22j.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points22j.utm.sf)==st_crs(hydro22j.utm.sf) #TRUE

# view data in mapview
mapview::mapview(hydro22j.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points22j.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
points22j.snap <- maptools::snapPointsToLines(points22j.utm, hydro22j.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points22j.utm@data) %>% # combina com as informa??es dos dados da tabela de atributos dos pontos
  st_as_sf()  # ja transformo p sf p olhar no mapviews

# check in mapviews:
mapview::mapview(hydro22j.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points22j.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points22j.snap, col.regions="green",layer.name="Snapped sites")

# buffer
buffer <- st_buffer(hydro22j.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points22j.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) #zero

# add a new column to change the order, if necessary
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist, 
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))

points.order.4 <- st_drop_geometry(points.order.3)

write.table(points.order.4, file="points.22j.csv",sep = ",") # export
