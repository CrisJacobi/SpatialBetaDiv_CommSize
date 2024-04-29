##############
## Code to snap selected sites to the closest hydrography from the HydroRIVERS 
# database(https://www.hydrosheds.org/products/hydrorivers) to atributte a Strahler 
# stream order to each site.
# 
# In the first script we selected basins with 10 or more sites. Now we want to assign 
# stream orders to the sampled sites to select only basins with 10 or more sites
# composed of first to third-order streams.

# We also checked if there were sites more distant than 1000m from the hydrography and
# in positive case, we checked in mapview if the assigned order made sense. When 
# appropriate we changed the order in the dataframe.

# Required packages:
library(rgdal)
library(tidyverse)
library(sf)
library(mapview)

# read the dataframe obtained in the script "data_selection_1".
quarter.final <- read.csv("quarter.final.csv",header=T,sep=",")

points.sf <- quarter.final %>% 
  dplyr::mutate(longitude=long,latitude=lat) %>% 
  st_as_sf(coords = c(x = "longitude", y = "latitude"), crs = 4326)

hydro.na <- readOGR (dsn = ".",layer = "HydroRIVERS_v10_na") # North America hydrography
hydro.na.sf <- hydro.na %>% st_as_sf() 

# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
                    verbose = FALSE, stringsAsFactors = TRUE)
z11 <- utm.zones[utm.zones$ZONE == "11", ]
z11t <- z11[z11$ROW_ == "T",]
z11t_sf<-z11t %>% st_as_sf() 

# clip of the sites with the zone 11t:
points11t <- points.sf %>% sf::st_intersection(z11t_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone 11t:
hydro11t <- hydro.na.sf %>% sf::st_intersection(z11t_sf) %>% 
  as("Spatial")

# convert to UTM z11t:
hydro11t.utm<- spTransform(hydro11t,CRS("+proj=utm +zone=11 +datum=WGS84 +ellps=WGS84"))
points11t.utm<- spTransform(points11t,CRS("+proj=utm +zone=11 +datum=WGS84 +ellps=WGS84"))

## Transform to sf to check in mapviews:
points11t.utm.sf <- points11t.utm %>% st_as_sf() 
hydro11t.utm.sf <- hydro11t.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points11t.utm.sf)==st_crs(hydro11t.utm.sf) #TRUE

# view data in mapview:
mapview::mapview(hydro11t.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points11t.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
points11t.snap <- maptools::snapPointsToLines(points11t.utm, hydro11t.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points11t.utm@data) %>% 
  st_as_sf() 

# check in mapviews:
mapview::mapview(hydro11t.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points11t.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points11t.snap, col.regions="green",layer.name="Snapped sites")

#  to be possible to obtain the order for the selected sites we need to do a buffer 
# in the database to intersect with the sites.
buffer <- st_buffer(hydro11t.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points11t.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) #zero

# add a new column to change the order, if necessary (in this zone it would not be
# necessary, but in others zones some points are more distant than 1000m and we checked the 
# order using the mapview. When we changed the order, this was made in ORD_STRA2
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist, 
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))

points.order.4 <- st_drop_geometry(points.order.3)

write.table(points.order.4, file="points.11t.csv",sep = ",") # export
