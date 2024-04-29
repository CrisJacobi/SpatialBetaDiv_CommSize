# Code to measure the average geographic distance among communities.

# required packages
library(tidyverse)
library(data.table)
library(geosphere)
library(ggplot2)
library(QuantPsyc)

# Read data:
count <- read.csv("count.environment.csv", header = T, sep = ",")

count2 <- count %>% 
  dplyr::select(HYBAS_ID, long,lat) %>% 
  unique()

setDT(count2) # Convert to data.table:

# Find the centroid:
findCentroid <- function(lon, lat, ...){
  centroid(cbind(lon, lat), ...)
}

count2[, c("Cent_lon", "Cent_lat") := as.list(findCentroid(long, lat)), by = HYBAS_ID]
count2

# Measure the shortest distance between points and the centroid (meters):
count2$distance<-distHaversine(count2[,2:3], count2[,4:5]) 
count2

# Mean distance of each metacommunity:
count2.lists <- split(count2,f = count2$HYBAS_ID, drop = T)

c.mean <- lapply(count2.lists, function (x) {
  x %>%
    dplyr::select(distance) %>%
    colMeans() 
})

c.mean.tab <- do.call(rbind, c.mean) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "HYBAS_ID") %>% 
  dplyr::rename(mean=distance)

# Join the mean distance to the centroid with community size:
count.median.tab <- read.csv("count.median.tab.csv", header = T, sep = ",")
str(count.median.tab)

count.median.tab$HYBAS_ID <- count.median.tab$HYBAS_ID %>% as.character()

dist.size <- full_join(count.median.tab,c.mean.tab)

# Export dataframe
names(dist.size)[2] <- "comm.size"
names(dist.size)[3] <- "mean.distance"
write.table(dist.size, file="mean_distante.csv", sep=",")

# Plot
distance <- ggplot(dist.size, aes(x = comm.size, y = mean.distance)) +
  geom_point(alpha=0.4, col="black", fill="black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text=element_text(family = "Verdana"))+
  theme(text=element_text(size=11))+
  labs(y = "Mean geographic distance (m)", x= "Community size")

lm1 <- lm(mean.distance ~ comm.size, data = dist.size)
summary(lm1)
lm.beta(lm1) # standardized regression coefficient

ggsave("Figure_S.tiff", units="in", width=5, height=4, dpi=300, compression = 'lzw')
