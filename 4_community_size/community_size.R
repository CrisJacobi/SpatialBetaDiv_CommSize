# This code was used to calculate community median size for count data.

# Prepared by Cristina M. Jacobi
# April/2022

# Required packages:
library(tidyverse)

# Read data:
count <- read.csv("count.csv", header = T, sep = ",")

count.1 <- count %>% 
  dplyr::select(HYBAS_ID, SiteID, Species, Abundance) # select columns of interest

count.1.list <-split(count.1,f = count.1$HYBAS_ID, drop = T) # transform in a list 
# according to HYBAS_ID

# sum individuals in each site  
count.sum <-lapply(count.1.list, function (x) {
  x %>%
    dplyr::select(HYBAS_ID, SiteID, Species, Abundance) %>%
    pivot_wider(names_from = "Species", values_from = "Abundance", values_fn = sum) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
    dplyr::select(-HYBAS_ID, -SiteID) %>% 
    mutate(sum=rowSums(.)) 
})

# median value of individuals per basin
count.median <- lapply(count.sum, function (x) {
  x %>%
    dplyr::select(sum) %>%
    unlist() %>% 
    as.numeric %>% 
    median() 
})

count.median.tab <- do.call(rbind, count.median) %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "HYBAS_ID") %>% 
  dplyr::rename(median=V1)
  
write.table(count.median.tab, file="count.median.tab.csv",sep = ",") # save dataframe
