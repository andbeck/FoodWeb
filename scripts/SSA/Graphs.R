library(tidyverse)
library(VIM)

# set enviro
rm(list=ls())
set.seed(12)

# Read
bsq <- read.csv("data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv")
csm <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")
epb <- read.csv("data/interactionwebdb/Carpinteria/EPBweb_Nodes.csv")

mega <- bind_rows(bsq, csm, epb)
mega %>% count(Phylum)


mega_wrk <- 
  mega %>% # dplyr::select and rename some variables
  dplyr::select(WorkingName, FunctionalGroup = ConsumerStrategy.stage., Resolution, BodySize = BodySize.g., 
                Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha., 
                Kingdom:SpecificEpithet) %>% 
  mutate(BodySize = BodySize / 1000) # change BodySize from g to Kg

aggr(mega_wrk[,4:6], col=c('navyblue','red'), 
     numbers=TRUE, sortVars=TRUE, labels=names(mega_wrk[, 4:6]), 
     cex.axis=.7, gap=3, ylab=c("Histogram of missing data for all nodes original data","Pattern"))


bsq_wrk <- 
  bsq %>% # dplyr::select and rename some variables
  dplyr::select(WorkingName, FunctionalGroup = ConsumerStrategy.stage., Resolution, BodySize = BodySize.g., 
                Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha., 
                Kingdom:SpecificEpithet) %>% 
  mutate(BodySize = BodySize / 1000) # change BodySize from g to Kg

aggr(bsq_wrk[,4:6], col=c('navyblue','red'), 
     numbers=TRUE, sortVars=TRUE, labels=names(bsq_wrk[, 4:6]), 
     cex.axis=.7, gap=3, ylab=c("Histogram of missing data for BSQ nodes original data","Pattern"))



