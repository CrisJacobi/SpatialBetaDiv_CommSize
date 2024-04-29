# rodar td o modelo aq


# Required packages:
library(tidyverse)
library(vegan)
library(Hmsc)
library(fastDummies) 
library(QuantPsyc)

# HMSC ----------------------------------------------------

# Read count data:
count <- read.csv("count.environment.csv", header = T, sep = ",")

# Select the species 10% most abundant in each community:
select_cols <- count %>% 
  dplyr::select(HYBAS_ID,SiteID,Species,Abundance)

count_list <- split(select_cols, # split by sites
                    f = select_cols$SiteID, drop = T)

sps.percent <-lapply(count_list, function (x) {
  x %>%
    dplyr::arrange(desc(Abundance)) %>% 
    slice(1:round(nrow(.)/10,0)) 
})

sps_percent_tab <-do.call(rbind, sps.percent) %>% 
  as.data.frame() 

# Now, we need to join the 10% most abundant species with the environmental variables:
environment <- left_join(sps_percent_tab,count)

environment_list <- split(environment, # split by MCs
                          f = environment$HYBAS_ID, drop = T)

# y = species incidence matrix
y <- lapply(environment_list, function (x) {
  x %>%
    dplyr::select(HYBAS_ID, SiteID, Species, Abundance) %>%
    pivot_wider(names_from = "Species", values_from = "Abundance", values_fn = sum) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
    dplyr::select(-HYBAS_ID, -SiteID) %>%
    vegan::decostand(method = "pa") 
})

# X = environmental matrix
x <-  lapply(environment_list, function (x) {
  x %>%
    dplyr::select(HYBAS_ID, SiteID, Species, Abundance,dis_m3_pyr:CSI_FF2) %>%
    pivot_wider(names_from = "Species", values_from = "Abundance", values_fn = sum) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>% 
    dplyr::select(dis_m3_pyr:CSI_FF2) %>% 
    dplyr::mutate(dis_m3_pyr = decostand(dis_m3_pyr,method = "stand") ) %>% 
    dplyr::mutate(ele_mt_cav = decostand(ele_mt_cav,method = "stand") ) %>% 
    dplyr::mutate(tmp_dc_cyr = decostand(tmp_dc_cyr,method = "stand") ) %>% 
    dplyr::mutate(pre_mm_cyr = decostand(pre_mm_cyr,method = "stand") ) %>% 
    dplyr::mutate(hft_ix_c = decostand(hft_ix_c,method = "stand") ) %>%
    dplyr::mutate(CSI = decostand(CSI,method = "stand") ) %>%
    dummy_cols('CSI_FF2') %>% 
    dplyr::select(-CSI_FF2) %>% 
    mutate_all(.funs = as.numeric)
})

# HMSC model
m <- list()

nChains = 4  # number of independent MCMC chains to be run
thin = 100     # the number of MCMC steps between each recording of samples from the posterior
samples = 1000   # the number of MCMC samples to be obtained in each chain
transient = 500*thin   # the number of MCMC steps that are executed before starting recording posterior samples
verbose = 0   # the interval between MCMC steps printed to the console 

result <- matrix(NA, nrow = length(x), ncol = 9)

for (i in 1:length(x)) {
  m[[i]] <- Hmsc(Y = y[[i]], XData = x[[i]], XFormula = ~.,distr="probit") 
  m[[i]] <- sampleMcmc(m[[i]],thin = thin, samples = samples, transient = transient,
                       nChains = nChains, verbose = verbose )
  preds <- computePredictedValues(m[[i]])
  R2.hmsc <- evaluateModelFit(hM = m[[i]], predY = preds)
  mean <- mean(R2.hmsc$TjurR2)
  median <-median(R2.hmsc$TjurR2)
  deviation <- sd(R2.hmsc$TjurR2)
  quartile <- quantile(R2.hmsc$TjurR2)
  result[i,] <- as.numeric(c(mean,median,deviation,quartile,names(environment_list[i])))
}

saveRDS(m, file="hmsc_model.RData")
model_1 <- readRDS("hmsc_model.RData")

?computeVariancePartitioning

# Here you can compute the variance partitioning for each metacommunity.
# example with the first metacommunity:
VP_meta1 <- computeVariancePartitioning(model_1[[1]],  group = c(1,1,2,3,4,5,6,7), 
                                        groupnames = c("Dis","Ele","Tmp","Pre","hft","csi","csiff2"))

plotVariancePartitioning(model_1[[1]], VP = VP)
VP_meta1$vals

write.table(VP_meta1$vals, file='VP1.csv') 

