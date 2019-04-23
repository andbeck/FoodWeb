# Script for Parasite list
library(tidyverse)
library(missForest)
library(mice)
library(VIM)
rm(list=ls())
set.seed(12)

# Read
bsq <- read.csv("data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv")
csm <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")
epb <- read.csv("data/interactionwebdb/Carpinteria/EPBweb_Nodes.csv")

bsq_wrk <- 
  bsq %>% # select and rename some variables
  select(WorkingName, Lifestyle.stage., Resolution, BodySize = BodySize.g., 
         Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha., 
           Kingdom:SpecificEpithet) %>% 
  mutate(BodySize = BodySize / 1000) # change BodySize from g to Kg

## only 50% of data is complete for impute...
bsq_plot <- aggr(bsq_wrk[,4:6], col=c('navyblue','red'), 
                 numbers=TRUE, sortVars=TRUE, labels=names(bsq_wrk[, 4:6]), 
                 cex.axis=.7, gap=3, ylab=c("Histogram of missing data for BSQ nodes original data","Pattern"))

bsq_new <- 
  bsq_wrk %>% 
## if BodySize = NA & if Biomass & Abundance != NA then BodySize = Biomass/Abundance - else do nothing
mutate(BodySizeNew = case_when(!is.na(Abundance & Biomass) ~ Biomass / Abundance),
## if Abundance = NA and BodySize & Biomass != NA then Abundance = Biomass/BodySize - else do nothing
      AbundanceNew = case_when(!is.na(BodySize & Biomass) ~ Biomass / BodySize),
## if Biomass = NA and BodySize & Abundance != NA then Biomass = Abundance * BodySize - else do nothing
      BiomassNew = case_when(!is.na(Abundance & BodySize) ~ Abundance * BodySize)) %>% 
## join the columns of new and old together use coalesce -- Biomass + BiomassNew...
## this selects the original value first so only values that are missing from original dataset and then computed are selected  
mutate(BodySize_Work = coalesce(BodySize, BodySizeNew),
       Biomass_Work = coalesce(Biomass, BiomassNew),
       Abundance_Work = coalesce(Abundance, AbundanceNew)) %>% 
## now select and rename the new working colums to replace the old incomplete data set
select(WorkingName, Lifestyle.stage., Resolution, BodySize = BodySize_Work, 
       Biomass = Biomass_Work, Abundance = Abundance_Work, 
       Kingdom:SpecificEpithet)

## 75% of data is now complete for imputation...
bsq_new_plot <- aggr(bsq_new[,4:6], col=c('navyblue','red'),
                 numbers=TRUE, sortVars=TRUE, labels=names(bsq_new[, 4:6]),
                 cex.axis=.7, gap=3, ylab=c("Histogram of missing data for BSQ nodes new data","Pattern"))


