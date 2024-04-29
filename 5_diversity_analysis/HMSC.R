# Code to measure the Hierarchical Modelling of Species Communities (HMSC - Ovaskainen et al. 2017).

# Our data for HMSC-analyses includes a matrix of species occurrences and a matrix 
# of environmental covariates.

# Prepared by Cristina M. Jacobi
# september/2022

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

colnames(result) <- c("meanR2","medianR2","deviation","qua_0","qua_25","qua_50","qua_75","qua_100", "HYBAS_ID")

hmsc_count <- as_tibble(result)
hmsc_count %>% view()

# join with community size
count_median_tab <-read.csv("count.median.tab.csv",header = T, sep = ",")

hmsc_commsize <- full_join(count_median_tab,hmsc_count)

write.table(hmsc_commsize, file = "hmsc.count_probit.csv",sep = ",") # export dataframe

hmsc_commsize <- read.csv("hmsc.count_probit.csv", header = T, sep = ",")

hmsc_count <- ggplot(hmsc_commsize, aes(x= median, y= medianR2)) +
  geom_point(alpha = 0.6, col = "grey37", fill = "grey37", show.legend = FALSE,size = 2.2,shape=21,stroke=1)+
  theme_classic()+
  stat_smooth(method = "lm", col = "grey37",fill = "gray85",show.legend = FALSE)+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 10))+
  labs(y = expression("Tjur" ~R^2  *  - HMSC), x = "Median community size")+
labs(subtitle = "(b)") 

hmsc_count_probit <-lm(medianR2 ~ median, data = hmsc_commsize)
summary(hmsc_count_probit)

# measure MCMC convergence
for (i in 1:length(x)) {
  m[[i]] <- Hmsc(Y = y[[i]], XData = x[[i]], XFormula = ~.,distr="probit") 
  m[[i]] <- sampleMcmc(m[[i]],thin = thin, samples = samples, transient = transient,
                       nChains = nChains, verbose = verbose )
}

mpost = convertToCodaObject(m[[i]])
mpost
summary(mpost$Beta)

effectiveSize(mpost$Beta)

convergence <-gelman.diag(mpost$Beta,multivariate=FALSE)$psrf
mean_convergence <- mean(convergence) # mean convergence of the model = 1.001922
table(convergence) # from 0.999470132177748 to 1.01442441220301
median(convergence) # 1.001239

# The gelman.diag gives you the scale reduction factors for each parameter. A factor 
# of 1 means that between variance and within chain variance are equal, larger values 
# mean that there is still a notable difference between chains. Often, it is said that
# everything below 1.1 or 1.05 or so is OK. 
