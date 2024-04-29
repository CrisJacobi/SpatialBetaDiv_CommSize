# Script to measure rank difference for riverine
# fish counts obtained from the RivFishTIME database (Comte et al. 2020).

# Prepared by Cristina M. Jacobi
# december/2022

library(tidyverse)
library(codyn)
library(lm.beta)

# Read count data:
count <- read.csv("count.environment.csv", header = T, sep = ",")

select_cols <- count %>% 
  dplyr::select(HYBAS_ID, SiteID, Species, Abundance) 

count_list <- split(select_cols,
                    f = select_cols$HYBAS_ID, drop = T)

# Function to measure rank difference.
rank.count <- lapply(count_list, function (x) {
  x %>%
    pivot_wider(names_from = "Species", values_from = "Abundance", values_fn = sum) %>%
    replace(is.na(.), 0) %>% 
    pivot_longer(cols = -c(HYBAS_ID,SiteID),names_to = "Species",values_to = "Abundance") %>%  
    codyn::RAC_difference(replicate.var = "SiteID",
                          species.var = "Species",
                          abundance.var = "Abundance") %>% 
    dplyr::select(rank_diff) %>% 
    mutate_all(.funs = as.numeric) 
})

# Measure the median rank difference by metacommunity:
rank.diff <- lapply(rank.count, function (x) {
  x %>%
    dplyr::select(rank_diff) %>%
    unlist() %>% 
    median()
})

rank_diff_tab <-do.call(rbind, rank.diff) %>% 
  as.data.frame() %>% 
  tibble::rownames_to_column(var = "HYBAS_ID") %>% 
  dplyr::rename(rank_diff=V1)

# join with community size
com_size <- read.csv("count.median.tab.csv", header = T, sep = ",")
com_size$HYBAS_ID <- com_size$HYBAS_ID %>% as.factor()
rank_diff_tab2 <- full_join(rank_diff_tab,com_size)

write.table(rank_diff_tab2, file = "rank_diff_count.csv",sep = ",") # export dataframe

rank_diff_tab2 <- read.csv("rank_diff_count.csv", header = T, sep = ",")

# plot: 
rank_diff_count <- ggplot(rank_diff_tab2, aes(x= median, y= rank_diff)) +
  geom_point(alpha = 0.6, col = "grey37", fill = "grey37", show.legend = FALSE,size = 2.2,shape=21,stroke=1)+
  theme_classic()+
  stat_smooth(method = "lm", col = "grey37",fill = "gray85",show.legend = FALSE)+
  theme(axis.line = element_line(size = 0.5, colour = "gray93"))+
  theme(text = element_text(family = "Arial"))+
  theme(text = element_text(size =10))+
  labs(y = "Rank difference", x = "Median community size")+
  labs(subtitle = "(a)") 

rank_lm <-lm(rank_diff~median, data = rank_diff_tab2)
summary(rank_lm)
lm.beta(rank_lm) # standardized regression coefficient
