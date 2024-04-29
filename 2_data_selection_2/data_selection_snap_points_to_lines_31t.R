# zone 31t

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

# Europe hydrography (https://www.hydrosheds.org/products/hydrorivers)
hydro.eu <- readOGR (dsn = ".",layer = "HydroRIVERS_v10_eu") 
hydro.eu.sf <- hydro.eu %>% st_as_sf() 

# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z31 <- utm.zones[utm.zones$ZONE == "31", ]
z31t <- z31[z31$ROW_ == "T",]
z31t_sf<-z31t %>% st_as_sf() 

# clip the sites with the zone
points31t <- points.sf %>% sf::st_intersection(z31t_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro31t <- hydro.eu.sf %>% sf::st_intersection(z31t_sf) %>% 
  as("Spatial")

# convert to UTM 31
hydro31t.utm<- spTransform(hydro31t,CRS("+proj=utm +zone=31 +datum=WGS84 +ellps=WGS84"))
points31t.utm<- spTransform(points31t,CRS("+proj=utm +zone=31 +datum=WGS84 +ellps=WGS84"))

points31t.utm.sf <- points31t.utm %>% st_as_sf() 
hydro31t.utm.sf <- hydro31t.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points31t.utm.sf)==st_crs(hydro31t.utm.sf) #TRUE

# view data in mapview 
mapview::mapview(hydro31t.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points31t.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
points31t.snap <- maptools::snapPointsToLines(points31t.utm, hydro31t.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points31t.utm@data) %>% 
  st_as_sf()  

# check in mapviews:
mapview::mapview(hydro31t.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points31t.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points31t.snap, col.regions="green",layer.name="Snapped sites")

# buffer 
buffer <- st_buffer(hydro31t.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points31t.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) 

# add a new column to change the order
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist, 
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))

points.order.4 <- st_drop_geometry(points.order.3)

write.table(points.order.4, file="points.31t.csv",sep = ",") #export
