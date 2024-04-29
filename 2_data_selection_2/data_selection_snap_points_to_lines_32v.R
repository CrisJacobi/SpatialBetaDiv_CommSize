# zone 32v

# Required packages:
library(rgdal)
library(tidyverse)
library(sf)
library(mapview)

# read the dataframe from the script (data_selection_1).

quarter.final <- read.csv("quarter.final.csv",header=T,sep=",")

points.sf <- quarter.final %>% 
  dplyr::mutate(longitude=long,latitude=lat) %>% 
  st_as_sf(coords = c(x = "longitude", y = "latitude"), crs = 4326)

# Europe hydrography (https://www.hydrosheds.org/products/hydrorivers)
hydro.eu <- readOGR (dsn = ".",layer = "HydroRIVERS_v10_eu") 
hydro.eu.sf <- hydro.eu %>% st_as_sf() 

# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z32 <- utm.zones[utm.zones$ZONE == "32", ]
z32v <- z32[z32$ROW_ == "V",]
z32v_sf<-z32v %>% st_as_sf() 

# clip the sites with the zone
points32v <- points.sf %>% sf::st_intersection(z32v_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro32v <- hydro.eu.sf %>% sf::st_intersection(z32v_sf) %>% 
  as("Spatial")

# convert to UTM 33
hydro32v.utm<- spTransform(hydro32v,CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84"))
points32v.utm<- spTransform(points32v,CRS("+proj=utm +zone=32 +datum=WGS84 +ellps=WGS84"))

points32v.utm.sf <- points32v.utm %>% st_as_sf() 
hydro32v.utm.sf <- hydro32v.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points32v.utm.sf)==st_crs(hydro32v.utm.sf) #TRUE

# view data in mapviews
mapview::mapview(hydro32v.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points32v.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
points32v.snap <- maptools::snapPointsToLines(points32v.utm, hydro32v.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points32v.utm@data) %>% 
  st_as_sf()

#  view data in mapview 
mapview::mapview(hydro32v.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points32v.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points32v.snap, col.regions="green",layer.name="Snapped sites")

# buffer
buffer <- st_buffer(hydro32v.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points32v.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) 

# add a new column to change the order
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist, 
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))

points.order.4 <- st_drop_geometry(points.order.3)

write.table(points.order.4, file="points.32v.csv",sep = ",") # export
