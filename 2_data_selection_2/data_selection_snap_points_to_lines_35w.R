# zone 35W

# Required packages:
library(rgdal)
library(tidyverse)
library(sf)
library(mapview)

# read the dataframe generated in the script "data_selection_1".
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
z35w <- z35[z35$ROW_ == "W",]
z35w_sf<-z35w %>% st_as_sf() 

# clip the sites with the zone
points35w<- points.sf %>% sf::st_intersection(z35w_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro35w <- hydro.eu.sf %>% sf::st_intersection(z35w_sf) %>% 
  as("Spatial")

# convert to UTM 35
hydro35w.utm<- spTransform(hydro35w,CRS("+proj=utm +zone=35 +datum=WGS84 +ellps=WGS84"))
points35w.utm<- spTransform(points35w,CRS("+proj=utm +zone=35 +datum=WGS84 +ellps=WGS84"))

points35w.utm.sf <- points35w.utm %>% st_as_sf() 
hydro35w.utm.sf <- hydro35w.utm %>% st_as_sf() 

# check if points and hydrography are in the same projection:
st_crs(points35w.utm.sf)==st_crs(hydro35w.utm.sf) #TRUE

# visualize data in mapview: 
mapview::mapview(hydro35w.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points35w.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap points to hydrography:
points35w.snap <- maptools::snapPointsToLines(points35w.utm, hydro35w.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points35w.utm@data) %>% 
  st_as_sf()  

# check on mapviews:
mapview::mapview(hydro35w.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points35w.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points35w.snap, col.regions="green",layer.name="Snapped sites")

# now, to hold the order information we need to intersect the hydrography with
# the snapped points, but for this work, we need to do a buffer in the hydrography.
buffer <- st_buffer(hydro35w.utm.sf, 10)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# intersection of points with the buffer to obtain Strahler order
points.order <- points35w.snap %>% sf::st_intersection(buffer.select.cols)

# select points that were more than 1000m of distance from the hydrography to 
# check if the assigned order makes sense 
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) 

# here we can look the points in the map to check the order
mapview::mapview(hydro35w.utm.sf,zcol="ORD_STRA", legend=TRUE, layer.name="Stream Order")+ 
  mapview::mapview(points35w.utm.sf,col.regions="red", legend=F,layer.name= "Sites") +
  mapview::mapview(distant,col.regions="green",layer.name="Snapped sites")

# create a new column to alter the order, if needed.
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist, 
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))
# remove geometry to save as a dataframe
points.order.4 <- st_drop_geometry(points.order.3)

# change the order of this sites in the dataframe:
points.order.4$ORD_STRA2[grepl("fin2948",points.order.4$SurveyID)]<- 4
points.order.4$ORD_STRA2[grepl("fin2734",points.order.4$SurveyID)]<- 2
points.order.4$ORD_STRA2[grepl("fin2992",points.order.4$SurveyID)]<- 2
points.order.4$ORD_STRA2[grepl("fin1854",points.order.4$SurveyID)]<- 4
points.order.4$ORD_STRA2[grepl("fin935",points.order.4$SurveyID)]<- 4

write.table(points.order.4, file="points.35w.csv",sep = ",")  # export
