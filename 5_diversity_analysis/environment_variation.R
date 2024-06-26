# Code to measure the variation of environmental data inside each metacommunity.
# Betadisper calculates the average distance of group members to the group
# centroid in multivariate space (generated by a distance matrix). 

# Prepared by Cristina M. Jacobi
# september/2022

# Required packages:
library(tidyverse)
library(vegan)
library(QuantPsyc)

# Read data:
count <- read.csv("count.environment.csv", header = T, sep = ",")

# Select columns of environmental data:
count_c <- count %>% 
  dplyr::select(HYBAS_ID, dis_m3_pyr, ele_mt_cav, tmp_dc_cyr, pre_mm_cyr,
                hft_ix_c, CSI,CSI_FF2) %>%  
  unique() 

# Metacommunities ID = groups
groups <- count_c$HYBAS_ID %>% as.factor()

# Calculate euclidian distances between samples.
dist_metas2 <- vegdist(count_c[2:8],method="euclidian")

# Now, we have to use the distance matrix to calculate the multivariate dispersions 
# (variances; average distance to centroids):
mod_metas2 <- betadisper(dist_metas2, groups)

mod_results <- mod_metas2$group.distances %>% as.data.frame()  
mod_results2 <- rownames_to_column(mod_results,var = "HYBAS_ID")

names(mod_results2)[2] <- "environment.variation"

# Export:
write.table(mod_results2, file = "environment_variation.csv", sep = ",")

# anova to test for groups differences:
anova(mod_metas2) 

# tukey to see where are the differences
TukeyHSD(mod_metas2) 

plot(mod_metas2, label = FALSE) 

boxplot(mod_metas2, xlab="HYBAS_ID", notch=TRUE)

# Plot with community size:
count_median_tab <- read.csv("count.median.tab.csv", header = T, sep = ",")

count_median_tab$HYBAS_ID <- count_median_tab$HYBAS_ID %>% as.factor()

disper_size <- full_join(count_median_tab,mod_results2) %>% 
  dplyr::rename(comm.size = median)
  
# Plot
betadisper <- ggplot(disper_size, aes(x = comm.size, y = environment.variation)) +
  geom_point(alpha=0.4, col="black", fill="black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text = element_text(family = "Verdana"))+
  theme(text = element_text(size=11))+
  labs(y = "Environment distance", x = "Community size")

lm1 <- lm(environment.variation ~ comm.size, data = disper_size)
summary(lm1)
lm.beta(lm1) # standardized regression coefficient
