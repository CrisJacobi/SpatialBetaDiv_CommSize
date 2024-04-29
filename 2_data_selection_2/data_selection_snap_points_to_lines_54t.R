# zone 54t

# Required packages
library(rgdal)
library(tidyverse)
library(sf)
library(mapview)

# read the dataframe from the script (data_selection_1).

quarter.final <- read.csv("quarter.final.csv",header=T,sep=",")

points.sf <- quarter.final %>% 
  dplyr::mutate(longitude=long,latitude=lat) %>% 
  st_as_sf(coords = c(x = "longitude", y = "latitude"), crs = 4326)

# Asia hydrography (https://www.hydrosheds.org/products/hydrorivers)
hydro.as <- readOGR (dsn = ".",layer = "HydroRIVERS_v10_as") 
hydro.as.sf <- hydro.as %>% st_as_sf() 

# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z54 <- utm.zones[utm.zones$ZONE == "54", ]
z54t <- z54[z54$ROW_ == "T",]
z54t_sf<-z54t %>% st_as_sf() 

# clip the sites with the zone
points54t <- points.sf %>% sf::st_intersection(z54t_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro54t <- hydro.as.sf %>% sf::st_intersection(z54t_sf) %>% 
  as("Spatial")

# convert to UTM 54
hydro54t.utm<- spTransform(hydro54t,CRS("+proj=utm +zone=54 +datum=WGS84 +ellps=WGS84"))
points54t.utm<- spTransform(points54t,CRS("+proj=utm +zone=54 +datum=WGS84 +ellps=WGS84"))

points54t.utm.sf <- points54t.utm %>% st_as_sf() 
hydro54t.utm.sf <- hydro54t.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points54t.utm.sf)==st_crs(hydro54t.utm.sf) #TRUE

# view data in mapview
mapview::mapview(hydro54t.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points54t.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
points54t.snap <- maptools::snapPointsToLines(points54t.utm, hydro54t.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points54t.utm@data) %>% 
  st_as_sf()  

# check in mapview 
mapview::mapview(hydro54t.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points54t.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points54t.snap, col.regions="green",layer.name="Snapped sites")

# buffer 
buffer <- st_buffer(hydro54t.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points54t.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) 

# add a new column to change the order
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist, 
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))

points.order.4 <- st_drop_geometry(points.order.3)

write.table(points.order.4, file="points.54t.csv",sep = ",") # export

