##############
# Code to attribute basin ID from Hydrosheds (https://www.hydrosheds.org/page/hydrobasins) 
# to the points in the RivFishTIME database (https://doi.org/10.1111/geb.13210)
# and to select basins whit at least 10 sites.

# Prepared by Cristina M. Jacobi
# April/2022

## Required packages
library(tidyverse)
library(rgdal)
library(raster)
library(sf)
library(sp)
library(spatialEco)
library(readr)

# read RivFishTIME data
timeseries <- read.csv("1873_2_RivFishTIME_TimeseriesTable.csv",header=T,sep=",")
surveytable <- read.csv("1873_2_RivFishTIME_SurveyTable.csv", header = T, sep = ",",stringsAsFactors=F)

sp.data.1 <- full_join(surveytable,timeseries) # 
sp.data.2 <- na.omit(sp.data.1) # remove NAs

# transform in a spatial object
sp.data.3 <- sp.data.2 %>% 
  dplyr::mutate(lat=Latitude, long=Longitude)

sp.data.4 <- st_as_sf(sp.data.3, 
                          coords = c(x = "Longitude", y = "Latitude"), 
                          crs = 4326) %>% 
  as_Spatial()

# read the HydroBasin (level 7) data for continents with data on RivFishTime
# (https://www.hydrosheds.org/page/hydrobasins)
hybas.na07 <- readOGR(dsn = ".", layer = "hybas_na_lev07_v1c") # North America
hybas.au07 <- readOGR(dsn = ".", layer = "hybas_au_lev07_v1c") # Australia
hybas.sa07 <- readOGR(dsn = ".", layer = "hybas_sa_lev07_v1c") # South America
hybas.eu07 <- readOGR(dsn = ".", layer = "hybas_eu_lev07_v1c") # Europe
hybas.as07 <- readOGR(dsn = ".", layer = "hybas_as_lev07_v1c") # Asia
hybas.af07 <- readOGR(dsn = ".", layer = "hybas_af_lev07_v1c") # Africa

hybas.07 <- bind(hybas.na07,hybas.au07,hybas.sa07,hybas.eu07,hybas.as07,hybas.af07)

hybas.07.sf<- st_as_sf(hybas.07) # transform in sf to validate the geometries
hybas.07.valid <- st_make_valid(hybas.07.sf)
hybas.07.sp <- as_Spatial(hybas.07.valid) # transform again in sp

# Now we intersect all points from RivFishTIME database with the Hydrobasin database
# to add hydrobasin ID (HYBAS_ID) to the points.
pts.poly.tot <- point.in.poly(sp.data.4, hybas.07.sp) 

sp.data.5 <- as.data.frame(pts.poly.tot) %>%  # transform in a dataframe and
  subset(select = -c(X,NEXT_DOWN:SORT)) %>%  # remove unnecessary columns
  dplyr::rename(latitude=coords.x2, longitude=coords.x1) # rename column
View(sp.data.5)
######################################     #####################################

# Now, we want to select basins with 10 or more sampled sites per year.

select.cols <- sp.data.5 %>% 
  dplyr::select(Year, SiteID, HYBAS_ID) %>%
  dplyr::distinct()

n.sites.1 <- select.cols %>% 
  dplyr::group_by(Year, HYBAS_ID) %>%
  dplyr::summarise(n_sites = n()) %>% # number of sites per basin and year
  dplyr::arrange(desc(n_sites)) %>%
  dplyr::ungroup() 

n.sites.2 <- n.sites.1 %>%
  dplyr::group_by(HYBAS_ID) %>%
  dplyr::summarise(max_val = max(n_sites)) # maximum values per basin but we lost year,
# than to obtain year again we can do a merge:
n.sites.3 <- left_join(n.sites.2, n.sites.1, by = "HYBAS_ID")

n.sites.4 <-  n.sites.3 %>% # filter n-sites that are equal to maximum value to obtain 
  dplyr::filter (n_sites == max_val) %>% # only the year in which each basin had more sites. 
  dplyr::filter (max_val >= 10) # select only basins with 10 or more sites

# Some basins had the same number of max_val in different years, than we selected 
# only the most recent year of these basins.
n.sites.5 <- n.sites.4 %>%
  dplyr::group_by(HYBAS_ID) %>%
  dplyr::summarise(Year = max(Year)) %>%
  dplyr::ungroup()

# now we can do a left_join with sp.data.5
points.year <- n.sites.5 %>%
  left_join(sp.data.5, by = c("HYBAS_ID", "Year")) # 199 basins

#### Some basins were sampled in different quarters of the year. So, in the same 
# year we can find the same site being sampled. As we are not using temporal data, 
# we want to select only the samples from one quarter of the year for each basin. 
# So, we will select the quarter of year with more sites sampled for each year.

quarter.1 <- points.year %>% 
  dplyr::select(Quarter,SiteID, HYBAS_ID) %>%
  dplyr::distinct()

quarter.2 <- quarter.1 %>% # count how many sites by quarter of year and basin
  dplyr::group_by(Quarter, HYBAS_ID) %>%
  dplyr::summarise(n_sites = n()) %>%
  dplyr::arrange(desc(n_sites)) %>%
  dplyr::ungroup()

quarter.3 <- quarter.2 %>%
  dplyr::group_by(HYBAS_ID) %>%
  dplyr::summarise(max_val = max(n_sites)) # here we can find the maximum values
# of sites per basyn but we lost the information of quarter of the year. Then, we
# can merge this information with the previous selection:
quarter.4 <- left_join(quarter.3, quarter.2, by = "HYBAS_ID")

# Now we can filter n-sites values that are equal the max_val and so we select 
# only the basins with higher number of sites by quarter of year.
quarter.5 <- quarter.4 %>%
  dplyr::filter (n_sites == max_val) %>%
  dplyr::filter (max_val >= 10) # select only basins with 10 or more sites

# Join with "points.year" 
quarter.final <- quarter.5 %>%
  left_join(points.year, by = c("Quarter","HYBAS_ID")) 

# save dataframe
write.table(quarter.final, file="quarter.final.csv", sep=",")
View(quarter.final)
