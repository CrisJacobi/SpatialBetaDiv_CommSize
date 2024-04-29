##############
## Code to join the snapped sites and export the environmental dataframe of count
# and density data.

# Prepared by Cristina M. Jacobi
# April/2022

# Required packages:
library(plyr)

# read the snapped points:
points.11t<- read.csv("environment.11t.csv", header = T, sep = ",")
points.11u<- read.csv("environment.11u.csv", header = T, sep = ",")
points.12t<- read.csv("environment.12t.csv", header = T, sep = ",")
points.15t<- read.csv("environment.15t.csv", header = T, sep = ",")
points.15u<- read.csv("environment.15u.csv", header = T, sep = ",")
points.17s<- read.csv("environment.17s.csv", header = T, sep = ",")
points.17t<- read.csv("environment.17t.csv", header = T, sep = ",")
points.18s<- read.csv("environment.18s.csv", header = T, sep = ",")
points.22j<- read.csv("environment.22j.csv", header = T, sep = ",")
points.22k<- read.csv("environment.22k.csv", header = T, sep = ",")
points.30u<- read.csv("environment.30u.csv", header = T, sep = ",")
points.30t<- read.csv("environment.30t.csv", header = T, sep = ",")
points.31t<- read.csv("environment.31t.csv", header = T, sep = ",")
points.31u<- read.csv("environment.31u.csv", header = T, sep = ",")
points.32t<- read.csv("environment.32t.csv", header = T, sep = ",")
points.32u<- read.csv("environment.32u.csv", header = T, sep = ",")
points.32v<- read.csv("environment.32v.csv", header = T, sep = ",")
points.33t<- read.csv("environment.33t.csv", header = T, sep = ",")
points.33u<- read.csv("environment.33u.csv", header = T, sep = ",")
points.33v<- read.csv("environment.33v.csv", header = T, sep = ",")
points.33w<- read.csv("environment.33w.csv", header = T, sep = ",")
points.34v<- read.csv("environment.34v.csv", header = T, sep = ",")
points.34w<- read.csv("environment.34w.csv", header = T, sep = ",")
points.35v<- read.csv("environment.35v.csv", header = T, sep = ",")
points.35w<- read.csv("environment.35w.csv", header = T, sep = ",")
points.54t<- read.csv("environment.54t.csv", header = T, sep = ",")
points.56j<- read.csv("environment.56j.csv", header = T, sep = ",")

library(plyr)
snapped.points.environment <-rbind(points.11t,points.11u,points.12t,points.15t,points.15u,points.17s,
                       points.17t,points.18s,points.22j,points.22k,points.30t,points.30u,
                       points.31t,points.31u,points.32t,points.32u,points.32v,points.33t,
                       points.33u,points.33v,points.33w,points.34v,points.34w,points.35v,
                       points.35w,points.54t,points.56j)

# The Human Footprint (HF) was estimated for 1993 and 2009, however we have data between
# 1985 and 2017. We opted to use the data of 1993 from HF to all sites sampled 
# until 2000 and to use the data of 2009 from HF to all sites sampled after 2000. 

# So, now we need to select the appropriate year of HF for all sites:
data.before.2000 <-snapped.points.environment %>% 
  dplyr::filter(Year<=2000)

data.after.2000 <- snapped.points.environment %>% 
  dplyr::filter(Year >=2001)

# remove the year of HF that we don't need:
data.before.2000.2 <- subset(data.before.2000, select = -c(hft_ix_c09))  
data.after.2000.2 <- subset(data.after.2000, select = -c(hft_ix_c93))

# rename columns to stay with the same name:
data.before.2000.3 <-data.before.2000.2 %>% dplyr::rename(hft_ix_c=hft_ix_c93) 
data.after.2000.3 <- data.after.2000.2 %>% dplyr::rename(hft_ix_c=hft_ix_c09)

# joint the two dataframes again:
joined.data <- rbind(data.before.2000.3,data.after.2000.3)

# now we can separate in a dataframe of count and a dataframe of density data.
count.environment <- joined.data %>% 
  dplyr::filter(UnitAbundance=="Count") 

write.table(count.environment, file="count.environment.csv",sep = ",") # export

dens.environment <- joined.data %>% 
  dplyr::filter(UnitAbundance=="Ind.100m2")

write.table(dens.environment, file="dens.environment.csv",sep = ",") # export 


