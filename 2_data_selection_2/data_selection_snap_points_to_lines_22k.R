# zone 22k

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
z22k <- z22[z22$ROW_ == "K",]
z22k_sf<-z22k %>% st_as_sf() 

# clip the sites with the zone
points22k <- points.sf %>% sf::st_intersection(z22k_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro22k <- hydro.sa.sf %>% sf::st_intersection(z22k_sf) %>% 
  as("Spatial")

# convert to UTM 22
hydro22k.utm<- spTransform(hydro22k,CRS("+proj=utm +zone=22 +datum=WGS84 +ellps=WGS84"))
points22k.utm<- spTransform(points22k,CRS("+proj=utm +zone=22 +datum=WGS84 +ellps=WGS84"))

points22k.utm.sf <- points22k.utm %>% st_as_sf() 
hydro22k.utm.sf <- hydro22k.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points22k.utm.sf)==st_crs(hydro22k.utm.sf) #TRUE

# view data in mapview
mapview::mapview(hydro22k.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points22k.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
points22k.snap <- maptools::snapPointsToLines(points22k.utm, hydro22k.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points22k.utm@data) %>% # combina com as informa??es dos dados da tabela de atributos dos pontos
  st_as_sf()  # ja transformo p sf p olhar no mapviews

# check in mapviews:
mapview::mapview(hydro22k.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points22k.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points22k.snap, col.regions="green",layer.name="Snapped sites")

# buffer
buffer <- st_buffer(hydro22k.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points22k.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) 

# with mapview we can look if the snapped points were moved to a stream of appropriate order
mapview::mapview(hydro22k.utm.sf,zcol="ORD_STRA", legend=TRUE, layer.name="Stream Order")+ 
  mapview::mapview(points22k.utm.sf,col.regions="red", legend=F,layer.name= "Sites") +
  mapview::mapview(distant,col.regions="green",layer.name="Snapped sites")

# add a new column to change the order
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist,
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))

points.order.4 <- st_drop_geometry(points.order.3)

# change the order of this sites in the dataframe:
points.order.4$ORD_STRA2[grepl("alt51",points.order.4$SurveyID)]<- 3
points.order.4$ORD_STRA2[grepl("alt76",points.order.4$SurveyID)]<- 1
points.order.4$ORD_STRA2[grepl("alt69",points.order.4$SurveyID)]<- 1
points.order.4$ORD_STRA2[grepl("alt72",points.order.4$SurveyID)]<- 1
points.order.4$ORD_STRA2[grepl("alt61",points.order.4$SurveyID)]<- 1

write.table(points.order.4, file="points.22k.csv",sep = ",") # export
