## Code to calculate GLM and HMSC for simulated metacommunities with incidence
# data and considering the core species (10% most abundants).

# Prepared by Cristina M. Jacobi
# september/2022

# Required packages:
library(tidyverse)
library(vegan)
library(Hmsc)
library(fastDummies) 
library(QuantPsyc)
library(jtools)
library(broom)
library(gridExtra)
library("extrafont")
font_import()
extrafont::loadfonts()
windowsFonts("Arial" = windowsFont("Arial"))

# Simulate communities ----------------------------------------------------
n.communities=30
nspecies= 30
Env=seq(0,5,length.out=n.communities)#Equally spaced Env. values
Traits=runif(nspecies,0,5)#Niche optima
Tol=0.2#niche breadth
Rmax=runif(30,5,120)# Changing this parameter will change the size of metacommunities

# GLM ---------------------------------------------------------------------
Results_glm<-matrix(NA,nrow = length(Rmax),ncol = 2,dimnames = list(1:length(Rmax),c("ComSize","Pseudo_R2")))

for(i in 1:length(Rmax)){
  Abundance_matrix<-data.frame(sapply(1:nspecies,function(x)rpois(n.communities,Rmax[i] *
                                                                    exp(-(Env-Traits[x])^2/(2*Tol^2)))))
  tab1 <- Abundance_matrix %>%
    rownames_to_column()
  tab2 <- split(tab1,f = tab1$rowname, drop = T) 
  tab3 <- lapply(tab2, function (x) {
    x %>%
      pivot_longer(cols = X1:X30, names_to = "Species",values_to = "Abundance") %>% 
      dplyr::arrange(desc(Abundance)) %>% 
      slice(1:round(nrow(.)/10,0)) 
  })
  tab4 <- do.call(rbind, tab3) %>% 
    as.data.frame() %>% 
    pivot_wider(names_from = "Species", values_from = "Abundance", values_fn = sum) %>%
    replace(is.na(.), 0) 
  Incidence_matrix <- tab4 %>% dplyr::select(-rowname) %>% 
    vegan::decostand(method = "pa")  
  MCsize <- median(rowSums(Abundance_matrix)) 
  
  Dist.stacked <- as.vector(as.matrix(Abundance_matrix)) %>%  
    vegan::decostand(method = "pa") 
  Esim <- rep(1, nspecies) %x% scale(Env) 
  datasim<-data.frame(PA=Dist.stacked,Env=scale(Esim)) 
  model <- glm(PA ~poly(Env,2),"binomial",data=datasim) 
  Pseudo_R2<-broom::glance(summ(model))$pseudo.r.squared
  Results_glm[i,]<-c(MCsize,Pseudo_R2)
}

colnames(Results_glm) <- c("MCsize","Pseudo_R2")

Results_glm <- as.data.frame(Results_glm)

write.table(Results_glm, file = "glm_simulation.csv",sep = ",") # export dataframe

glm_sim_plot <- ggplot(Results_glm, aes(x= MCsize, y= Pseudo_R2))+
  geom_point(alpha = 0.4, col = "black", fill = "black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  stat_smooth(method = "lm", col = "grey37",fill = "snow3",show.legend = FALSE)+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size = 12))+
  labs(y= expression(Pseudo~R^2  *  - GLM[" simulated "]),x= "Community size")+
  labs(subtitle = "(a)") 

glm_lm <-lm(Pseudo_R2~MCsize, data = Results_glm)
summary(glm_lm)
lm.beta(glm_lm)

# HMSC --------------------------------------
Results_sim <- matrix(NA, nrow = length(Rmax), ncol = 2)
nChains = 4  
thin = 100     
samples = 1000
transient = 500*thin   
verbose = 0   

hmsc.sim <- list()

for(i in 1:length(Rmax)){
  Abundance_matrix <- data.frame(sapply(1:nspecies,function(x)rpois(n.communities,Rmax[i] *
                                                                      exp(-(Env-Traits[x])^2/(2*Tol^2)))))
  tab1 <- Abundance_matrix %>%
    rownames_to_column()
  
  tab2 <- split(tab1,f = tab1$rowname, drop = T) 
  tab3 <- lapply(tab2, function (x) {
    x %>%
      pivot_longer(cols = X1:X30, names_to = "Species",values_to = "Abundance") %>% 
      dplyr::arrange(desc(Abundance)) %>% 
      slice(1:round(nrow(.)/10,0)) 
  }) 
  tab4 <- do.call(rbind, tab3) %>% 
    as.data.frame() %>% 
    pivot_wider(names_from = "Species", values_from = "Abundance", values_fn = sum) %>%
    replace(is.na(.), 0) 
  Incidence_matrix <- tab4 %>% dplyr::select(-rowname) %>% 
    vegan::decostand(method = "pa")  
  MCsize <- median(rowSums(Abundance_matrix)) # MC size
  Esim <- rep(1, nspecies) %x% scale(Env) 
  env.df <- Esim[1:30,] %>% as.data.frame()
  hmsc.sim <- Hmsc(Y = Incidence_matrix, XData = env.df, XFormula = ~.,distr="probit")
  hmsc.sim <- sampleMcmc(hmsc.sim,thin = thin, samples = samples, transient = transient,
                         nChains = nChains, verbose = verbose )
  preds <- computePredictedValues(hmsc.sim)
  R2.hmsc <- evaluateModelFit(hM = hmsc.sim, predY = preds)
  median <-median(R2.hmsc$TjurR2)
  Results_sim[i,]<-c(MCsize,median)
}

colnames(Results_sim) <- c("MCsize","medianR2")

Results_sim_prediction <- as.data.frame(Results_sim)

write.table(Results_sim_prediction, file = "hmsc_simulation.csv",sep = ",") # export dataframe

hmsc_sim_plot <- ggplot(Results_sim_prediction, aes(x= MCsize, y= medianR2))+
  geom_point(alpha=0.4, col="black", fill="black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text=element_text(family = "Arial"))+
  theme(text=element_text(size=12))+
  labs(y= expression(R^2  *  - HMSC[" simulated "]),x= "Community size")+
  labs(subtitle = "(b)") 

hmsc_sim_probit <-lm(medianR2 ~ MCsize, data = Results_sim_prediction)
summary(hmsc_sim_probit)
lm.beta(hmsc_sim_probit)
