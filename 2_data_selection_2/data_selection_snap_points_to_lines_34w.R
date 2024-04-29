# zone 34w

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
z34 <- utm.zones[utm.zones$ZONE == "34", ]
z34w <- z34[z34$ROW_ == "W",]
z34w_sf<-z34w %>% st_as_sf() 

# clip the sites with the zone
points34w<- points.sf %>% sf::st_intersection(z34w_sf)%>%  
  as("Spatial") 

# clip of HydroRIVERS with the zone
hydro34w <- hydro.eu.sf %>% sf::st_intersection(z34w_sf) %>% 
  as("Spatial")

# convert to UTM 34
hydro34w.utm<- spTransform(hydro34w,CRS("+proj=utm +zone=34 +datum=WGS84 +ellps=WGS84"))
points34w.utm<- spTransform(points34w,CRS("+proj=utm +zone=34 +datum=WGS84 +ellps=WGS84"))

points34w.utm.sf <- points34w.utm %>% st_as_sf() 
hydro34w.utm.sf <- hydro34w.utm %>% st_as_sf() 

# check if it are in the same projection:
st_crs(points34w.utm.sf)==st_crs(hydro34w.utm.sf) #TRUE

# view data
mapview::mapview(hydro34w.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points34w.utm.sf,col.regions="red", legend=F, layer.name="Sites")

# snap
points34w.snap <- maptools::snapPointsToLines(points34w.utm, hydro34w.utm, maxDist = NA, withAttrs = FALSE, idField = NA) %>% 
  cbind(points34w.utm@data) %>% 
  st_as_sf()  

# check in mapviews:
mapview::mapview(hydro34w.utm.sf, zcol="ORD_STRA",legend=TRUE, layer.name="Stream order")+
  mapview::mapview(points34w.utm.sf,col.regions="red", legend=F, layer.name="Sites")+
  mapview::mapview(points34w.snap, col.regions="green",layer.name="Snapped sites")

# buffer 
buffer <- st_buffer(hydro34w.utm.sf, 1)

# select column
buffer.select.cols <- subset(buffer, select = c(ORD_STRA))

# Intersection of the snapped points with the buffer to obtain the order
points.order <- points34w.snap %>% sf::st_intersection(buffer.select.cols)

# select snapped sites located initially more distant then 1000m from the hydrography
distant <- points.order %>% 
  dplyr::filter(snap_dist>=1000) 

# with mapview we can look if the snapped points were moved to a stream of appropriate order
mapview::mapview(hydro34w.utm.sf,zcol="ORD_STRA", legend=TRUE, layer.name="Stream Order")+ 
  mapview::mapview(points34w.utm.sf,col.regions="red", legend=F,layer.name= "Sites") +
  mapview::mapview(distant,col.regions="green",layer.name="Snapped sites")

# add a new column to change the order
points.order.2 <- points.order %>% dplyr::mutate(ORD_STRA2 = ORD_STRA)

# remove unnecessary columns:
points.order.3 <- subset(points.order.2, select = -c(nearest_line_id, snap_dist, 
                                                     max_val,n_sites,FID,WEST_VALUE:EAST_VALUE))

points.order.4 <- st_drop_geometry(points.order.3)

# change the order of this sites in the dataframe:
points.order.4$ORD_STRA2[grepl("ser19137",points.order.4$SurveyID)]<- 4
points.order.4$ORD_STRA2[grepl("ser14671",points.order.4$SurveyID)]<- 2
points.order.4$ORD_STRA2[grepl("ser14367",points.order.4$SurveyID)]<- 4
points.order.4$ORD_STRA2[grepl("ser10898",points.order.4$SurveyID)]<- 1
points.order.4$ORD_STRA2[grepl("ser10335",points.order.4$SurveyID)]<- 2
points.order.4$ORD_STRA2[grepl("ser9073",points.order.4$SurveyID)]<- 3
points.order.4$ORD_STRA2[grepl("ser8954",points.order.4$SurveyID)]<- 3
points.order.4$ORD_STRA2[grepl("ser8935",points.order.4$SurveyID)]<- 3
points.order.4$ORD_STRA2[grepl("ser8874",points.order.4$SurveyID)]<- 3
points.order.4$ORD_STRA2[grepl("ser10335",points.order.4$SurveyID)]<- 2
points.order.4$ORD_STRA2[grepl("ser10237",points.order.4$SurveyID)]<- 1
points.order.4$ORD_STRA2[grepl("ser9408",points.order.4$SurveyID)]<- 1
points.order.4$ORD_STRA2[grepl("ser9308",points.order.4$SurveyID)]<- 2
points.order.4$ORD_STRA2[grepl("ser8708",points.order.4$SurveyID)]<- 4
points.order.4$ORD_STRA2[grepl("ser8681",points.order.4$SurveyID)]<- 4
points.order.4$ORD_STRA2[grepl("ser8655",points.order.4$SurveyID)]<- 4
points.order.4$ORD_STRA2[grepl("ser8378",points.order.4$SurveyID)]<- 4

write.table(points.order.4, file="points.34w.csv",sep = ",") # export
