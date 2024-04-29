# zone 35v

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
z35 <- utm.zones[utm.zones$ZONE == "35", ]
z35v <- z35[z35$ROW_ == "V",]
z35v_sf<-z35v %>% st_as_sf() 

#  clip the sites with the zone
points35v<- points.sf %>% sf::st_intersection(z35v_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro35v <- hydro.eu.sf %>% sf::st_intersection(z35v_sf) %>% 
  as("Spatial")

# convert to UTM 35
hydro35v.utm<- spTransform(hydro35v,CRS("+proj=utm +zone=35 +datum=WGS84 +ellps=WGS84"))
points35v.utm<- spTransform(points35v,CRS("+proj=utm +zone=35 +datum=WGS84 +ellps=WGS84"))

points35v.utm.sf <- points35v.utm %>% st_as_sf() 
hydro35v.utm.sf <- hydro35v.utm %>% st_as_sf() 

# check if points and hydrography are in the same projection:
st_crs(points35v.utm.sf)==st_crs(hydro35v.utm.sf) #TRUE

# visualize data in mapview:
mapview::mapview(hydro35v.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points35v.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap points to hydrography:
points35v.snap <- maptools::snapPointsToLines(points35v.utm, hydro35v.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points35v.utm@data) %>% 
  st_as_sf()  

#  check on mapviews:
mapview::mapview(hydro35v.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points35v.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points35v.snap, col.regions="green",layer.name="Snapped sites")

# buffer on hydrography
buffer <- st_buffer(hydro35v.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# intersection of points with the buffer to obtain Strahler order
points.order <- points35v.snap %>% sf::st_intersection(buffer.select.cols)

# select points that were more than 1000m of distance from the hydrography to 
# check if the assigned order makes sense 
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) 

# here we can look the points in the map to check the order
mapview::mapview(hydro35v.utm.sf,zcol="ORD_STRA", legend=TRUE, layer.name="Stream Order")+ 
  mapview::mapview(points35v.utm.sf,col.regions="red", legend=F,layer.name= "Sites") +
  mapview::mapview(distant,col.regions="green",layer.name="Snapped sites")

# create a new column to alter the order, if needed.
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist, 
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))
# remove geometry
points.order.4 <- st_drop_geometry(points.order.3)

# change the order of this sites in the dataframe:
points.order.4$ORD_STRA2[grepl("fin4591",points.order.4$SurveyID)]<- 2
points.order.4$ORD_STRA2[grepl("fin4910",points.order.4$SurveyID)]<- 1
points.order.4$ORD_STRA2[grepl("fin4594",points.order.4$SurveyID)]<- 2
points.order.4$ORD_STRA2[grepl("fin996",points.order.4$SurveyID)]<- 1
points.order.4$ORD_STRA2[grepl("fin995",points.order.4$SurveyID)]<- 1

write.table(points.order.4, file="points.35v.csv",sep = ",")  # export
