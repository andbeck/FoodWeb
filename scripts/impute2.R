# Script for Parasite list
library(tidyverse)
library(VIM)
library(gdata)

# set enviro
rm(list=ls())
set.seed(12)

# Read
bsq <- read.csv("data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv")
csm <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")
epb <- read.csv("data/interactionwebdb/Carpinteria/EPBweb_Nodes.csv")

bsq_wrk <- 
  bsq %>% # dplyr::select and rename some variables
  dplyr::select(WorkingName, FunctionalGroup = ConsumerStrategy.stage., Resolution, BodySize = BodySize.g., 
         Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha., 
           Kingdom:SpecificEpithet) %>% 
  mutate(BodySize = BodySize / 1000) # change BodySize from g to Kg

# ## only 50% of data is complete for impute...
# bsq_plot <- aggr(bsq_wrk[,4:6], col=c('navyblue','red'), 
#                  numbers=TRUE, sortVars=TRUE, labels=names(bsq_wrk[, 4:6]), 
#                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data for BSQ nodes original data","Pattern"))

bsq_new <- 
  bsq_wrk %>% 
## if BodySize = NA & if Biomass & Abundance != NA then BodySize = Biomass/Abundance - else do nothing
mutate(BodySizeNew = case_when(!is.na(Abundance & Biomass) ~ Biomass / Abundance),
## if Abundance = NA and BodySize & Biomass != NA then Abundance = Biomass/BodySize - else do nothing
      AbundanceNew = case_when(!is.na(BodySize & Biomass) ~ Biomass / BodySize),
## if Biomass = NA and BodySize & Abundance != NA then Biomass = Abundance * BodySize - else do nothing
      BiomassNew = case_when(!is.na(Abundance & BodySize) ~ Abundance * BodySize)) %>% 
## join the columns of new and old together use coalesce() -- Biomass + BiomassNew...
## this selects the original value first so only values that are missing from original dataset and then computed are selected  
mutate(BodySize_Work = coalesce(BodySize, BodySizeNew),
       Biomass_Work = coalesce(Biomass, BiomassNew),
       Abundance_Work = coalesce(Abundance, AbundanceNew)) %>% 
## now dplyr::select and rename the new working colums to replace the old incomplete data set
dplyr::select(WorkingName, FunctionalGroup, Resolution, BodySize = BodySize_Work, 
       Biomass = Biomass_Work, Abundance = Abundance_Work, 
       Kingdom:SpecificEpithet)

# ## 75% of data is now complete for imputation...
# bsq_new_plot <- aggr(bsq_new[,4:6], col=c('navyblue','red'),
#                  numbers=TRUE, sortVars=TRUE, labels=names(bsq_new[, 4:6]),
#                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data for BSQ nodes new data","Pattern"))

###################

csm_wrk <- 
  csm %>% # dplyr::select and rename some variables
  dplyr::select(WorkingName, FunctionalGroup = ConsumerStrategy.stage., Resolution, BodySize = BodySize.g., 
         Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha., 
         Kingdom:SpecificEpithet) %>% 
  mutate(BodySize = BodySize / 1000) # change BodySize from g to Kg

# ## only 49% of data is complete for impute...
# csm_plot <- aggr(csm_wrk[,4:6], col=c('navyblue','red'), 
#                  numbers=TRUE, sortVars=TRUE, labels=names(csm_wrk[, 4:6]), 
#                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data for CSM nodes original data","Pattern"))

csm_new <- 
  csm_wrk %>% 
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
  ## now dplyr::select and rename the new working colums to replace the old incomplete data set
  dplyr::select(WorkingName, FunctionalGroup, Resolution, BodySize = BodySize_Work, 
         Biomass = Biomass_Work, Abundance = Abundance_Work, 
         Kingdom:SpecificEpithet)

# ## 74% of data is now complete for imputation...
# csm_new_plot <- aggr(csm_new[,4:6], col=c('navyblue','red'),
#                      numbers=TRUE, sortVars=TRUE, labels=names(csm_new[, 4:6]),
#                      cex.axis=.7, gap=3, ylab=c("Histogram of missing data for CSM nodes new data","Pattern"))

###################

epb_wrk <- 
  epb %>% # dplyr::select and rename some variables
  dplyr::select(WorkingName, FunctionalGroup = ConsumerStrategy.stage., Resolution, BodySize = BodySize.g., 
         Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha., 
         Kingdom:SpecificEpithet) %>% 
  mutate(BodySize = BodySize / 1000) # change BodySize from g to Kg

# ## only 50% of data is complete for impute...
# epb_plot <- aggr(epb_wrk[,4:6], col=c('navyblue','red'), 
#                  numbers=TRUE, sortVars=TRUE, labels=names(epb_wrk[, 4:6]), 
#                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data for EPB nodes original data","Pattern"))

epb_new <- 
  epb_wrk %>% 
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
  ## now dplyr::select and rename the new working colums to replace the old incomplete data set
  dplyr::select(WorkingName, FunctionalGroup, Resolution, BodySize = BodySize_Work, 
         Biomass = Biomass_Work, Abundance = Abundance_Work, 
         Kingdom:SpecificEpithet)

# ## 75% of data is now complete for imputation...
# epb_new_plot <- aggr(epb_new[,4:6], col=c('navyblue','red'),
#                      numbers=TRUE, sortVars=TRUE, labels=names(epb_new[, 4:6]),
#                      cex.axis=.7, gap=3, ylab=c("Histogram of missing data for EPB nodes new data","Pattern"))

library(mice)
## non phylo imputation using logged values of imputeation variables to reduce colinearity

## non-phylo imputation epb
epb_imp <- epb_new %>% dplyr::select(BodySize, Biomass, Abundance) %>% 
  mutate(BodySize = log(BodySize + 1), Biomass = log(Biomass + 1), Abundance = log(Abundance + 1))
epb_imp <- mice(epb_imp, m = 5, method = "pmm")
xyplot(epb_imp, BodySize ~ Abundance + Biomass)
densityplot(epb_imp, BodySize ~ Abundance + Biomass)
stripplot(epb_imp, pch = 20, cex = 1.2)
epb_imp <- complete(epb_imp)

## non-phylo imputation csm
csm_imp <- csm_new %>% dplyr::select(BodySize, Biomass, Abundance) %>% 
  mutate(BodySize = log(BodySize + 1), Biomass = log(Biomass + 1), Abundance = log(Abundance + 1))
csm_imp <- mice(csm_imp, m = 5, method = "pmm")
xyplot(csm_imp, BodySize ~ Abundance + Biomass)
densityplot(csm_imp, BodySize ~ Abundance + Biomass)
stripplot(csm_imp, pch = 20, cex = 1.2)
csm_imp <- complete(csm_imp)

## non-phylo imputation bsq
bsq_imp <- bsq_new %>% dplyr::select(BodySize, Biomass, Abundance) %>% 
  mutate(BodySize = log(BodySize + 1), Biomass = log(Biomass + 1), Abundance = log(Abundance + 1))
bsq_imp <- mice(bsq_imp, m = 5, method = "pmm")
xyplot(bsq_imp, BodySize ~ Abundance + Biomass)
densityplot(bsq_imp, BodySize ~ Abundance + Biomass)
stripplot(bsq_imp, pch = 20, cex = 1.2)
bsq_imp <- complete(bsq_imp)

# transform back
epb_imp <- epb_new %>% dplyr::select(BodySize, Biomass, Abundance) %>% 
  mutate(BodySize = exp(BodySize - 1), Biomass = exp(Biomass - 1), Abundance = exp(Abundance - 1))

csm_imp <- csm_new %>% dplyr::select(BodySize, Biomass, Abundance) %>% 
  mutate(BodySize = exp(BodySize - 1), Biomass = exp(Biomass - 1), Abundance = exp(Abundance - 1))

bsq_imp <- bsq_new %>% dplyr::select(BodySize, Biomass, Abundance) %>% 
  mutate(BodySize = exp(BodySize - 1), Biomass = exp(Biomass - 1), Abundance = exp(Abundance - 1))


keep(bsq_imp, csm_imp, epb_imp,
     bsq, csm, epb, sure = T)


## gplot functional space values
# ## psuedo replication?
# ggplot(NULL, aes(x = log10(BodySize), y = log10(Abundance), colour = FunctionalGroup)) +
#   geom_point(data = csm_wrk, shape = 16) +
#   geom_point(data = bsq_wrk, shape = 17) +
#   geom_point(data = epb_wrk, shape = 23) + 
#   facet_wrap(~ FunctionalGroup)
# 
# 

