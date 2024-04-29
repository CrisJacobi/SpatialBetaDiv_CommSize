# zone 15t

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
hydro.na.sf <- hydro.na %>% st_as_sf() # sf

# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z15 <- utm.zones[utm.zones$ZONE == "15", ]
z15t <- z15[z15$ROW_ == "T",]
z15t_sf<-z15t %>% st_as_sf() 

# clip the sites with the zone
points15t <- points.sf %>% sf::st_intersection(z15t_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro15t <- hydro.na.sf %>% sf::st_intersection(z15t_sf) %>% 
  as("Spatial")

# convert to UTM 15
hydro15t.utm<- spTransform(hydro15t,CRS("+proj=utm +zone=15 +datum=WGS84 +ellps=WGS84"))
points15t.utm<- spTransform(points15t,CRS("+proj=utm +zone=15 +datum=WGS84 +ellps=WGS84"))

points15t.utm.sf <- points15t.utm %>% st_as_sf() 
hydro15t.utm.sf <- hydro15t.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points15t.utm.sf)==st_crs(hydro15t.utm.sf) #TRUE

# view data in mapview
mapview::mapview(hydro15t.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points15t.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
points15t.snap <- maptools::snapPointsToLines(points15t.utm, hydro15t.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points15t.utm@data) %>% 
  st_as_sf()  

# check in mapviews:
mapview::mapview(hydro15t.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points15t.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points15t.snap, col.regions="green",layer.name="Snapped sites")

# buffer
buffer <- st_buffer(hydro15t.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points15t.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) #zero

# add a new column to change the order, if necessary
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist, 
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))

points.order.4 <- st_drop_geometry(points.order.3) # export


write.table(points.order.4, file="points.15t.csv",sep = ",")
