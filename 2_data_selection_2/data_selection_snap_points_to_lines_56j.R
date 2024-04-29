# zone 56j

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

# Australia hydrography (https://www.hydrosheds.org/products/hydrorivers)
hydro.au <- readOGR (dsn = ".",layer = "HydroRIVERS_v10_au")
hydro.au.sf <- hydro.au %>% st_as_sf()

# shapefile of world UTM zones (https://hub.arcgis.com/datasets/esri::world-utm-grid/about)
utm.zones <-readOGR(dsn = ".", layer = "0f893164-d038-48ff-98dd-9fefb26127d3202034-1-145zfwr.nwf1", 
            verbose = FALSE, stringsAsFactors = TRUE)
z56 <- utm.zones[utm.zones$ZONE == "56", ]
z56j <- z56[z56$ROW_ == "J",]
z56j_sf<-z56j %>% st_as_sf() 

# clip the sites with the zone
points56j<- points.sf %>% sf::st_intersection(z56j_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro56j <- hydro.au.sf %>% sf::st_intersection(z56j_sf) %>% 
  as("Spatial")

# convert to UTM 56
hydro56j.utm<- spTransform(hydro56j,CRS("+proj=utm +zone=56 +datum=WGS84 +ellps=WGS84"))
points56j.utm<- spTransform(points56j,CRS("+proj=utm +zone=56 +datum=WGS84 +ellps=WGS84"))

points56j.utm.sf <- points56j.utm %>% st_as_sf() 
hydro56j.utm.sf <- hydro56j.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points56j.utm.sf)==st_crs(hydro56j.utm.sf) #TRUE

# view data in mapview
mapview::mapview(hydro56j.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points56j.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
memory.limit(9999999999)
points56j.snap <- maptools::snapPointsToLines(points56j.utm, hydro56j.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points56j.utm@data) %>% 
  st_as_sf()  

# check in mapviews:
mapview::mapview(hydro56j.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points56j.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points56j.snap, col.regions="green",layer.name="Snapped sites")

# buffer 
buffer <- st_buffer(hydro56j.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points56j.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) 

# add a new column to change the order
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist,
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))
                                             
points.order.4 <- st_drop_geometry(points.order.3)

write.table(points.order.4, file="points.56j.csv",sep = ",") # exportar tabela


 