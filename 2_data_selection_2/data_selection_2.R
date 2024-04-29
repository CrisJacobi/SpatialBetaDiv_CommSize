########################
## Code to join snapped sites and filter only basins with 10 or more sampled 
# sites between orders 1, 2 or 3. Also, in this code we generate one dataframe for 
# count and other dataframe for density data, selecting only basins with at least 
# five species.

# Prepared by Cristina M. Jacobi
# April/2022

# Required packages:
library(plyr)

points.11t<- read.csv("points.11t.csv", header = T, sep = ",")
points.11u<- read.csv("points.11u.csv", header = T, sep = ",")
points.12t<- read.csv("points.12t.csv", header = T, sep = ",")
points.15t<- read.csv("points.15t.csv", header = T, sep = ",")
points.15u<- read.csv("points.15u.csv", header = T, sep = ",")
points.17s<- read.csv("points.17s.csv", header = T, sep = ",")
points.17t<- read.csv("points.17t.csv", header = T, sep = ",")
points.18s<- read.csv("points.18s.csv", header = T, sep = ",")
points.22j<- read.csv("points.22j.csv", header = T, sep = ",")
points.22k<- read.csv("points.22k.csv", header = T, sep = ",")
points.30u<- read.csv("points.30u.csv", header = T, sep = ",")
points.30t<- read.csv("points.30t.csv", header = T, sep = ",")
points.31t<- read.csv("points.31t.csv", header = T, sep = ",")
points.31u<- read.csv("points.31u.csv", header = T, sep = ",")
points.32t<- read.csv("points.32t.csv", header = T, sep = ",")
points.32u<- read.csv("points.32u.csv", header = T, sep = ",")
points.32v<- read.csv("points.32v.csv", header = T, sep = ",")
points.33t<- read.csv("points.33t.csv", header = T, sep = ",")
points.33u<- read.csv("points.33u.csv", header = T, sep = ",")
points.33v<- read.csv("points.33v.csv", header = T, sep = ",")
points.33w<- read.csv("points.33w.csv", header = T, sep = ",")
points.34v<- read.csv("points.34v.csv", header = T, sep = ",")
points.34w<- read.csv("points.34w.csv", header = T, sep = ",")
points.35v<- read.csv("points.35v.csv", header = T, sep = ",")
points.35w<- read.csv("points.35w.csv", header = T, sep = ",")
points.54t<- read.csv("points.54t.csv", header = T, sep = ",")
points.56j<- read.csv("points.56j.csv", header = T, sep = ",")

snapped.points <-rbind(points.11t,points.11u,points.12t,points.15t,points.15u,points.17s,
              points.17t,points.18s,points.22j,points.22k,points.30t,points.30u,
              points.31t,points.31u,points.32t,points.32u,points.32v,points.33t,
              points.33u,points.33v,points.33w,points.34v,points.34w,points.35v,
              points.35w,points.54t,points.56j)
View(snapped.points)

#
### 

# select only streams of orders 1, 2 and 3:
points123 <- subset(snapped.points, ORD_STRA2<=3)

table(points123$UnitAbundance) # types of unit of abundance

# subset according the unit of abundance, then select only basins with 10 or
# more sites and with at least five species.

library(tidyverse)

count.data.1 <- points123 %>% 
  dplyr::filter(UnitAbundance=="Count") %>% 
  dplyr::select(HYBAS_ID,Quarter, SiteID,UnitAbundance) %>%  
  dplyr::distinct() %>% as.data.frame() %>% 
  dplyr::group_by(HYBAS_ID,Quarter,UnitAbundance) %>% 
  dplyr::count() %>% 
  dplyr::filter(n>=10) %>%
  dplyr::ungroup()

count.data.2 <- left_join(count.data.1,points123) %>% 
  subset(select = -c(n))

count.data.3 <- count.data.2 %>%   
  dplyr::select(HYBAS_ID,Quarter,Species) %>% distinct() %>%    
  dplyr::group_by(HYBAS_ID,Quarter) %>% 
  dplyr::count() %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::filter(n>=5) 

count.data.4 <- left_join(count.data.3,count.data.2, by="HYBAS_ID","Quarter") %>%
  subset(select = -c(n))

count.data.4$HYBAS_ID %>% unique() # 32 metacommunities
view(count.data.4)
write.table(count.data.4,file="count.csv",sep = ",") # 

### 

dens.data.1 <- points123 %>% 
  dplyr::filter(UnitAbundance=="Ind.100m2") %>% 
  dplyr::select(HYBAS_ID,Quarter, SiteID,UnitAbundance) %>%  
  dplyr::distinct() %>% as.data.frame() %>% 
  dplyr::group_by(HYBAS_ID,Quarter,UnitAbundance) %>% 
  dplyr::count() %>%
  dplyr::filter(n>=10) %>% 
  dplyr::ungroup()

dens.data.2 <- left_join(dens.data.1,points123) %>% 
  subset(select = -c(n))

dens.data.3 <- dens.data.2 %>%    # at least five species in the basin
  dplyr::select(HYBAS_ID,Quarter,Species) %>% distinct() %>%    
  dplyr::group_by(HYBAS_ID,Quarter) %>% 
  dplyr::count() %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::filter(n>=5) 

dens.data.4 <- left_join(dens.data.3,dens.data.2, by="HYBAS_ID","Quarter") %>%
  subset(select = -c(n))

dens.data.4$HYBAS_ID %>% unique() # 81 metacommunities

write.table(dens.data.4,file="dens.csv",sep = ",") # 

###

### We opted for don't use CPUE data because of the small quantity of data
cpue.data.1 <- points123 %>% 
  dplyr::filter(UnitAbundance=="CPUE") %>% 
  dplyr::select(HYBAS_ID,Quarter, SiteID) %>%  
  dplyr::distinct() %>% as.data.frame() %>% 
  dplyr::group_by(HYBAS_ID,Quarter) %>% 
  dplyr::count() %>% 
  dplyr::filter(n>=10) %>% 
  dplyr::ungroup()

