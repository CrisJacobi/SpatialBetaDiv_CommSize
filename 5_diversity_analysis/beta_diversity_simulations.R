# Code to measure beta-diversity of the simulated metacommunities using six 
# different metrics: Jaccard, C-score, corrected Shannon, Jaccard-Chao, 
# beta-c and rank difference.

# multiple-site dissimilarity - Baselga, A. 2009 (https://doi.org/10.1111/j.1466-8238.2009.00490.x) 
# corrected Shannon and Jaccard-Chao - Cao et al. 2021 (https://doi.org/10.1002/ecy.3448)
# beta-c - Engel et al. 2021 (doi: 10.1002/ecs2.3745)
# rank difference - Avolio et al. 2019 (https://doi.org/10.1002/ecs2.2881)

# Prepared by Cristina M. Jacobi
# december/2022

# Required packages
library(tidyverse)
library(vegan)
library(bipartite)
library(betapart)
library(devtools) 
library(codyn)
devtools::install_github("T-Engel/betaC")
library(betaC)
library(QuantPsyc)
devtools::install_github("MoBiodiv/mobsim", build_vignettes = TRUE)
library(mobsim)
library(entropart)
library(collapse)
library(gridExtra)
library(extrafont)
library(scales)
font_import()
extrafont::loadfonts()
windowsFonts("Arial" = windowsFont("Arial"))


# Simulate metacommunities: -----------------------------------------------

N_species<-20
Regional_abundance_vector<-sort(round(runif(30,756,15956)))# By increasing MC
# abundance we also increase community size.
N_samples<-20
Coordinates<-c(1:20) 
Simulated_MC_disp_lim<-list()

#Creating MCs with the same SAD, Sites and Richness, but with different Abundances
for(y in 1:length(Regional_abundance_vector)){
  lognormal_pool<-sim_sad(s_pool=N_species,n_sim=Regional_abundance_vector[y],sad_type="lnorm",
                          sad_coef = list("meanlog"=5,"sdlog"=0.8))# Create log-normal SADs for all MCs
  teste_matrix_dispersal_lim<-matrix(0,nrow = N_samples,ncol=N_species,
                                     dimnames = list(c(paste0("S",1:N_samples)),c(paste0("species",1:N_species))))
  Prob_dispersal_among_plots<-as.matrix(exp(-0.7*dist(Coordinates)))# Colonization
  # probablitiy decreases with distance from central site following a kernel
  diag(Prob_dispersal_among_plots)<-max(Prob_dispersal_among_plots)# Given a the highest 
  # value to the central site
  dimnames(Prob_dispersal_among_plots)<-list(rownames(teste_matrix_dispersal_lim),
                                             colnames(teste_matrix_dispersal_lim))
  #Populating x
  for(i in 1:length(lognormal_pool)){
    Distribution_first<- which(rownames(teste_matrix_dispersal_lim)==names(table(sample(rownames(teste_matrix_dispersal_lim),1,replace=T))))# The central site of a given species. This is random
    Distribution_rest<- (table(sample(rownames(teste_matrix_dispersal_lim),as.numeric(lognormal_pool)[i],
                                      prob=Prob_dispersal_among_plots[,Distribution_first],replace=T)))#Individuals of species i are
    # more likely to colonize sites closer to the central one
    Sites<-names(Distribution_rest)
    Local_abundances<-as.numeric(Distribution_rest)
    teste_matrix_dispersal_lim[Sites,i]<-Local_abundances
  }
  Simulated_MC_disp_lim[[y]]<-teste_matrix_dispersal_lim  
  
}
# save the simulated metacommunities:
saveRDS(Simulated_MC_disp_lim, file="simulated_metacommunities20.RData")
Simulated_MC_disp_lim <- readRDS("simulated_metacommunities20.RData")

#####
# You can redo the steps above to simulate metacommunities of different sizes.
# We did it for metacommunities simulated with 6, 10, 20, 30, 40 and 48 species
# and sites.
#####

# Beta-diversity metrics to measure for the simulated metacommunities-----------

# Function to measure multiple-site dissimilarity with presence-absence data
baselga.betadiv.function = function(x) 
{
  size_meta <- median(rowSums(x))
  presabs<- ifelse(x>0,1,0)
  beta <- beta.multi(presabs, index.family = "jaccard")
  bas_results <- c(size_meta,beta$beta.JAC)
  names(bas_results)<- c("comm.size","beta_multi")
  return(bas_results)
}

baselga_betadiv_simulated <- do.call(rbind, lapply(Simulated_MC_disp_lim, baselga.betadiv.function)) %>% 
  as.data.frame() 

# Plot
baselga_simm_plot <- ggplot(baselga_betadiv_simulated, aes(x= comm.size, y= beta_multi)) +
  geom_point(alpha=0.4, col="black", fill="black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  stat_smooth(method = "lm", col = "grey37",fill = "snow3",show.legend = FALSE)+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text=element_text(family = "Arial"))+
  theme(text=element_text(size=12))+
  labs(y = expression("Jaccard-Baselga"[" simulated"]), x= "Community size")+
  labs(subtitle = "(a)") +
  scale_y_continuous(labels = label_number(accuracy = 0.01))

baselga_lm <-lm(beta_multi ~ comm.size, data = baselga_betadiv_simulated)
summary(baselga_lm)
lm.beta(baselga_lm)

# Function to measure C-score
c.score.function = function(x) 
{
  size_meta <- median(rowSums(x))
  df <- x[which(rowSums(x)>0),which(colSums(x)>0)]
  c_sco <- C.score(df)
  C_sco_results <- c(size_meta,c_sco)
  names(C_sco_results)<- c("comm.size","c_sco")
  return(C_sco_results)
}

c_score_simulated <- do.call(rbind, lapply(Simulated_MC_disp_lim, c.score.function)) %>% 
  as.data.frame()

# Plot
c_score_simm_plot <- ggplot(c_score_simulated, aes(x= comm.size, y= c_sco)) +
  geom_point(alpha=0.4, col="black", fill="black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  stat_smooth(method = "lm", col = "grey37",fill = "snow3",show.legend = FALSE)+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text=element_text(family = "Arial"))+
  theme(text=element_text(size=12))+
  labs(y = expression("C-score"[" simulated"]), x= "Community size")+
  labs(subtitle = "(b)") 

c_score_lm <-lm(c_sco ~ comm.size, data = c_score_simulated)
summary(c_score_lm)
lm.beta(c_score_lm)

# Function to measure Jaccard-Chao 
jaccard.chao = function(sim.data)
{
  D = vegan::vegdist(sim.data,method="chao")
  SStotal <- sum(D^2)/dim(sim.data)[1]
  BDtotal <- SStotal/(dim(sim.data)[1]-1)
  return(BDtotal)
}

# Function to measure Jaccard-Chao and corrected Shannon
bd.function = function(x) 
{
  size_meta <- median(rowSums(x))
  df <- x[which(rowSums(x)>0),which(colSums(x)>0)]
  Metacom <- MetaCommunity(data.frame(t(df)))
  Shannon_BD<-DivPart(q=1,MC=Metacom)$TotalBetaDiversity
  Jaccard_chao_BD<-jaccard.chao(df)
  Results <- c(size_meta,Shannon_BD,Jaccard_chao_BD)
  names(Results)<- c("comm.size","shannon","jaccard")
  return(Results)
}

jac_shan_simulated <- do.call(rbind, lapply(Simulated_MC_disp_lim, bd.function)) %>% 
  as.data.frame()

# Plots
jaccard_chao_simm_plot <- ggplot(jac_shan_simulated, aes(x= comm.size, y= jaccard)) +
  geom_point(alpha=0.4, col="black", fill="black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  stat_smooth(method = "lm", col = "grey37",fill = "snow3",show.legend = FALSE)+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text=element_text(family = "Arial"))+
  theme(text=element_text(size=12))+
  labs(y = expression("Jaccard-Chao"[" simulated"]), x= "Community size")+
  labs(subtitle = "(c)") 

simm_jac_lm <-lm(jaccard ~ comm.size, data = jac_shan_simulated)
summary(simm_jac_lm)
lm.beta(simm_jac_lm)

shannon_simm_plot <- ggplot(jac_shan_simulated, aes(x= comm.size, y= shannon)) +
  geom_point(alpha=0.4, col="black", fill="black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  stat_smooth(method = "lm", col = "grey37",fill = "snow3",show.legend = FALSE)+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text=element_text(family = "Arial"))+
  theme(text=element_text(size=12))+
  labs(y = expression("Shannon"[" simulated"]), x= "Community size")+
  labs(subtitle = "(d)") 

simm_shan_lm <-lm(shannon ~ comm.size, data = jac_shan_simulated)
summary(simm_shan_lm)
lm.beta(simm_shan_lm)

#  Function to measure rank difference 

rank.diff.function = function(x) 
{
  size_meta <- median(rowSums(x))
  tab0 <- x %>% as.data.frame()
  tab1 <- tibble::rownames_to_column(tab0, "sites") 
  tab2 <- tab1 %>% pivot_longer(cols = -(sites), names_to = "Species",values_to = "Abundance")
  codyn.rank <- tab2 %>% codyn::RAC_difference(replicate.var = "sites",
                                               species.var = "Species",
                                               abundance.var = "Abundance") %>% 
    dplyr::select(rank_diff) %>% 
    unlist() %>% 
    median()
  Results <- c(size_meta,codyn.rank)
  names(Results)<- c("size_meta","rank")
  return(Results)
}

simmulated_rank <- do.call(rbind, lapply(Simulated_MC_disp_lim, rank.diff.function)) %>% 
  as.data.frame()

# Plot
rank_diff_simm_plot <- ggplot(simmulated_rank, aes(x= size_meta, y= rank)) +
  geom_point(alpha=0.4, col="black", fill="black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text=element_text(family = "Arial"))+
  theme(text=element_text(size=12))+
  labs(y = expression("Rank difference"[" simulated"]), x= "Median community size")+
  labs(subtitle = "(f)")+
  scale_y_continuous(limits = c(0.1, 0.4),labels = label_number(accuracy = 0.01))+
  scale_x_continuous(limits = c(100, 2000))  

simm_rank_lm <-lm(rank ~ size_meta, data = simmulated_rank)
summary(simm_rank_lm)
lm.beta(simm_rank_lm) # standardized regression coefficient

# Function to measure Beta-c

# Function to measure beta-c:
bc.function = function(x) 
{
  tab0 <- apply(x, 2, as.numeric) %>%  
    as.data.frame() %>% 
    group_nest(.key = "SAD") 
  tab1 <- tab0 %>%
    dplyr::mutate(n_samples= map_dbl(SAD,nrow),
                  n_min=map_dbl(SAD, function(x) min(rowSums(x))))
  tabmin <- tab1 %>% filter(n_samples==20, # here you need to chance according the 
                            #number of species/sites of the metacommunity
                            n_min > 5) 
  SizeMetacom <- tabmin %>% 
    dplyr::mutate(size_meta= map_dbl(SAD,function(x) median(rowSums(x))))
  
  tab2 <- SizeMetacom %>% 
    dplyr::mutate(beta_true= map_dbl(SAD, function(x) beta_true(x))) 
  tab3 <- tab2 %>% 
    dplyr::mutate(S_gamma= map_dbl(SAD, function(x) specnumber(colSums(x))))
  tab4 <- tab3 %>% 
    dplyr::mutate(C_target= map_dbl(SAD, function(x) C_target(x)))
  C_tar=fmin(tab4$C_target)
  bc <- tab4 %>% 
    dplyr::mutate(betaC= map_dbl(SAD,beta_C, C=C_tar))
  return(bc)
}

bc_simulated <- do.call(rbind, lapply(Simulated_MC_disp_lim, bc.function))

Resuls_Sim_bc <- bc_simulated %>% dplyr::select(size_meta,betaC)

# Plot:
bc_simm_plot <- ggplot(Resuls_Sim_bc,aes(x=size_meta, y= betaC))+
  geom_point(alpha=0.4, col="black", fill="black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  stat_smooth(method = "lm", col = "grey37",fill = "snow3",show.legend = FALSE)+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text=element_text(family = "Arial"))+
  theme(text=element_text(size=12))+
  labs(y = expression(beta["C simulated"]), x= "Median community size")+
  labs(subtitle = "(b)")+
  scale_y_continuous(limits = c(1.5, 3.5),labels = label_number(accuracy = 0.01))+
  scale_x_continuous(limits = c(100,600))

lm_betac <-lm(betaC~size_meta, data = Resuls_Sim_bc)
summary(lm_betac)
lm.beta(lm_betac)

#### NOTE: for metacommunities with 10 or less species/sites we added some lines
# to run the function. 
Simulated_MC_disp_lim <- readRDS("simulated_metacommunities10.RData")

bc.function = function(x) 
{tryCatch({
  tab0 <- apply(x, 2, as.numeric) %>%  
    as.data.frame() %>% 
    group_nest(.key = "SAD") 
  tab1 <- tab0 %>%
    dplyr::mutate(n_samples= map_dbl(SAD,nrow),
                  n_min=map_dbl(SAD, function(x) min(rowSums(x))))
  tabmin <- tab1 %>% filter(n_samples==10, 
                            n_min > 0) 
  SizeMetacom <- tabmin %>% 
    dplyr::mutate(size_meta= map_dbl(SAD,function(x) median(rowSums(x))))
  
  tab2 <- SizeMetacom %>% 
    dplyr::mutate(beta_true= map_dbl(SAD, function(x) beta_true(x))) 
  tab3 <- tab2 %>% 
    dplyr::mutate(S_gamma= map_dbl(SAD, function(x) specnumber(colSums(x))))
  tab4 <- tab3 %>% 
    dplyr::mutate(C_target= map_dbl(SAD, function(x) C_target(x)))
  C_tar=fmin(tab4$C_target)
  bc <- tab4 %>% 
    dplyr::mutate(betaC= map_dbl(SAD,beta_C, C=C_tar))
  print(x) # eu adicionei um print para identificar qual id de meta que nao funciona
  # print (c(ncol(x), nrow(x)))
  return(bc)},
  # if an error occurs, tell me the error
  error=function(e) {
    message('An Error Occurred')
    print(e)
  },
  #if a warning occurs, tell me the warning
  warning=function(w) {
    message('A Warning Occurred')
    print(w)
    return(NA)
  }
)
}

beta_c_10 <-lapply(Simulated_MC_disp_lim, bc.function) 

# values of size and beta-c obtained in the function above using:simulated_metacommunities10.RData
size_metas <- c(75, 129, 172, 367, 424, 444, 411, 638, 572, 480, 786, 714, 582,
                1028, 779, 826, 654, 1072, 1230, 1248, 1358, 1338, 1466, 1418, 1582)

betaCs <- c(1.22, 1.08, 1.14, 0.986, 1.03, 1.10, 1.05, 1.05, 1.04, 1.03, 1, 0.990,
            1.06, 1.03, 1.05, 0.976, 1.02, 1.04, 0.990, 1.04, 1, 1.01, 1.02, 1, 1.02)

tab_size_betac <- cbind(size_metas,betaCs) %>% 
  as.data.frame()
plot(size_metas,betaCs)

# Plot:
bc_simm_plot10 <- ggplot(tab_size_betac,aes(x=size_metas, y= betaCs))+
  geom_point(alpha=0.4, col="black", fill="black", show.legend = FALSE,size = 3.2,shape=21,stroke=1)+
  theme_classic()+
  stat_smooth(method = "lm", col = "grey37",fill = "snow3",show.legend = FALSE)+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text=element_text(family = "Arial"))+
  theme(text=element_text(size=12))+
  labs(y = expression(beta["C simulated"]), x= "Community size")+
  labs(subtitle = "(a)") #+
  scale_y_continuous(limits = c(1.5, 3.5))

lm_betac_10 <-lm(betaCs~size_metas, data = tab_size_betac)
summary(lm_betac_10)
lm.beta(lm_betac_10)

# Beta-c: 6 species/communities
Simulated_MC_disp_lim <- readRDS("simulated_metacommunities6.RData")

bc.function = function(x) 
{tryCatch({
  tab0 <- apply(x, 2, as.numeric) %>%  
    as.data.frame() %>% 
    group_nest(.key = "SAD") 
  tab1 <- tab0 %>%
    dplyr::mutate(n_samples= map_dbl(SAD,nrow),
                  n_min=map_dbl(SAD, function(x) min(rowSums(x))))
  tabmin <- tab1 %>% filter(n_samples==6, 
                            n_min > 0) 
  SizeMetacom <- tabmin %>% 
    dplyr::mutate(size_meta= map_dbl(SAD,function(x) median(rowSums(x))))
  
  tab2 <- SizeMetacom %>% 
    dplyr::mutate(beta_true= map_dbl(SAD, function(x) beta_true(x))) 
  tab3 <- tab2 %>% 
    dplyr::mutate(S_gamma= map_dbl(SAD, function(x) specnumber(colSums(x))))
  tab4 <- tab3 %>% 
    dplyr::mutate(C_target= map_dbl(SAD, function(x) C_target(x)))
  C_tar=fmin(tab4$C_target)
  bc <- tab4 %>% 
    dplyr::mutate(betaC= map_dbl(SAD,beta_C, C=C_tar))
  print(x) # eu adicionei um print para identificar qual id de meta que nao funciona
  # print (c(ncol(x), nrow(x)))
  return(bc)},
  # if an error occurs, tell me the error
  error=function(e) {
    message('An Error Occurred')
    print(e)
  },
  #if a warning occurs, tell me the warning
  warning=function(w) {
    message('A Warning Occurred')
    print(w)
    return(NA)
  }
)
}

betac_6 <-lapply(Simulated_MC_disp_lim, bc.function) # many metacommunities with error

size_6 <- c(178, 253, 292, 554, 1090)
betaCs_6 <- c(0.980, 1.01, 1, 1, 1)

tab6 <- cbind(size_6,betaCs_6) %>% 
  as.data.frame()
plot(size_6,betaCs_6)
