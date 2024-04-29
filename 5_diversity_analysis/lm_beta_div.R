# Code to do the linear models regressing beta-diversity metric (rank difference)
# against community size, environmental heterogeneity and spatial extent.

# Prepared by Cristina M. Jacobi
# september/2022

# Required packages:
library(tidyverse)
library(lm.beta)
library(performance)

# Read data:
rank_diff <- read.csv("rank_diff_count.csv", header = T, sep = ",")
environment_variation <- read.csv("environment_variation.csv", header = T, sep = ",")
distance <- read.csv("mean_distante.csv", header = T, sep = ",")

df1_rank <- full_join(rank_diff,environment_variation)
df2_rank <- full_join(df1_rank,distance)
head(df2_rank)

mdl_rank <- lm(rank_diff ~ comm.size + environment.variation + mean.distance,
                data = df2_rank) 
summary(mdl_rank)
lm.beta(mdl_rank)

check_model(mdl_rank) # Visual check of model various assumptions.

