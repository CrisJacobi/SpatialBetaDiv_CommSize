# zone 11u

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

# North America hydrography (https://www.hydrosheds.org/products/hydrorivers)
hydro.na <- readOGR (dsn = ".",layer = "HydroRIVERS_v10_na") 
hydro.na.sf <- hydro.na %>% st_as_sf() 

# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z11 <- utm.zones[utm.zones$ZONE == "11", ]
z11u <- z11[z11$ROW_ == "U",]
z11u_sf<-z11u %>% st_as_sf() 

# clip the sites with the zone
points11u <- points.sf %>% sf::st_intersection(z11u_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro11u <- hydro.na.sf %>% sf::st_intersection(z11u_sf) %>% 
  as("Spatial")

# convert to UTM 11
hydro11u.utm<- spTransform(hydro11u,CRS("+proj=utm +zone=11 +datum=WGS84 +ellps=WGS84"))
points11u.utm<- spTransform(points11u,CRS("+proj=utm +zone=11 +datum=WGS84 +ellps=WGS84"))

points11u.utm.sf <- points11u.utm %>% st_as_sf() 
hydro11u.utm.sf <- hydro11u.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points11u.utm.sf)==st_crs(hydro11u.utm.sf) #TRUE

# view data in mapview:
mapview::mapview(hydro11u.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points11u.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
points11u.snap <- maptools::snapPointsToLines(points11u.utm, hydro11u.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points11u.utm@data) %>% 
  st_as_sf() 

# check in mapviews:
mapview::mapview(hydro11u.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points11u.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points11u.snap, col.regions="green",layer.name="Snapped sites")

# buffer 
buffer <- st_buffer(hydro11u.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points11u.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) 

# add a new column to change the order, if necessary
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist,
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))

points.order.4 <- st_drop_geometry(points.order.3)

write.table(points.order.4, file="points.11u.csv",sep = ",") # export
