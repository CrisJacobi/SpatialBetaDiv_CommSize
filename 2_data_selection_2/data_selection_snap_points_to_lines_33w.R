# zone 33w

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
z33 <- utm.zones[utm.zones$ZONE == "33", ]
z33w <- z33[z33$ROW_ == "W",]
z33w_sf<-z33w %>% st_as_sf() 

# clip the sites with the zone
points33w<- points.sf %>% sf::st_intersection(z33w_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro33w <- hydro.eu.sf %>% sf::st_intersection(z33w_sf) %>% 
  as("Spatial")

# convert to UTM 33
hydro33w.utm<- spTransform(hydro33w,CRS("+proj=utm +zone=33 +datum=WGS84 +ellps=WGS84"))
points33w.utm<- spTransform(points33w,CRS("+proj=utm +zone=33 +datum=WGS84 +ellps=WGS84"))

points33w.utm.sf <- points33w.utm %>% st_as_sf() 
hydro33w.utm.sf <- hydro33w.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points33w.utm.sf)==st_crs(hydro33w.utm.sf) #TRUE

# view data
mapview::mapview(hydro33w.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points33w.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
points33w.snap <- maptools::snapPointsToLines(points33w.utm, hydro33w.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points33w.utm@data) %>% 
  st_as_sf()  

# check in mapviews:
mapview::mapview(hydro33w.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points33w.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points33w.snap, col.regions="green",layer.name="Snapped sites")

# buffer 
buffer <- st_buffer(hydro33w.utm.sf, 10)

# select columns
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points33w.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) 

# with mapview we can look if the snapped points were moved to a stream of appropriate order
mapview::mapview(hydro33w.utm.sf,zcol="ORD_STRA", legend=TRUE, layer.name="Stream Order")+ 
  mapview::mapview(points33w.utm.sf,col.regions="red", legend=F,layer.name= "Sites") +
  mapview::mapview(distant,col.regions="green",layer.name="Snapped sites")

# add a new column to change the order, if needed
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist, 
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))

points.order.4 <- st_drop_geometry(points.order.3)

# change the order of this sites in the dataframe:
points.order.4$ORD_STRA2[grepl("ser11770",points.order.4$SurveyID)]<- 2

write.table(points.order.4, file="points.33w.csv",sep = ",") # export

