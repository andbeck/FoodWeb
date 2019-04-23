# Script for Parasite list
library(tidyverse)

library(missForest)
library(mice)
rm(list=ls())
set.seed(12)

# Read
bsq <- read.csv("data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv")
csm <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")
epb <- read.csv("data/interactionwebdb/Carpinteria/EPBweb_Nodes.csv")
fle <- read.csv("data/interactionwebdb/Flensburg/Flensburg_Data_Nodes.csv")
ota <- read.csv("data/interactionwebdb/Otago/Otago_Data_Nodes.csv")
syl <- read.csv("data/interactionwebdb/Sylt/Sylt_Data_Nodes.csv")

mega <- bind_rows(bsq, csm, epb, fle, ota, syl)
glimpse(mega)

# remove columns with all NA values with this function
not_all_na <- function(x) any(!is.na(x))
para <- mega %>% # select if with NA all function
  select_if(not_all_na)

# select useful columns into parasite file
para <- mega %>%  
  select(WorkingName, Lifestyle.stage., Resolution, BodySize.g., 
         Biomass.kg.ha., Abundance.no..ha., Kingdom:SpecificEpithet) %>% 
  distinct()

# parasites with body mass
para_NA <- para %>% 
  drop_na(BodySize.g.)

# split into list by phyla
phylum <- split(para, para$Phylum)

# extract data.frame from list
platy <- phylum[["Platyhelminthes"]]
summary(platy)

# make new data frame with whatever variables
platy_log <- platy %>%
  select(BodySize.g., Biomass.kg.ha. , Abundance.no..ha.) %>% 
  rename(BodySize = BodySize.g., Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha.) %>% 
  mutate(Abundance = Biomass / BodySize) %>% 
  # log the values to reduce errors with colinearity of variables
  mutate(BodySize = log(BodySize + 1), Biomass = log(Biomass + 1), Abundance = log(Abundance + 1))

# # understanding the patterns in the missing data using mice
# md.pattern(platy_log)
# 
# # easy visualisation of missing data
# library(VIM)
# platy_plot <- aggr(platy_log, col=c('navyblue','red'), 
#                   numbers=TRUE, sortVars=TRUE, labels=names(platy_log), 
#                   cex.axis=.7, gap=3, ylab=c("Histogram of missing data for Platyhelminths","Pattern"))
# 
# 
# tempData <- mice(platy_log, m = 50, method = "pmm")
# xyplot(tempData,BodySize ~ Abundance + Biomass)
# densityplot(tempData)
# stripplot(tempData, pch = 20, cex = 1.2)
# modelFit1 <- with(tempData, lm(Abundance ~ BodySize + Biomass))
# summary(pool(modelFit1)) # does imputed data differ signif?
# 
# # without using any phylogenetic information and missForest
# platy_imp <- missForest(platy_log)
# summary(platy_log)
# summary(platy_imp$ximp)

# using phylogenetic values? need to use eignevector regression to extract distances between species? Is this needed?
# Could controlling and splitting by Genus/Family provide enough reliable data anyway?


## All values - messing around...
all_log <- mega %>% 
  select(BodySize.g., Biomass.kg.ha. , Abundance.no..ha.) %>% 
  rename(BodySize = BodySize.g., Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha.) %>% 
  # log the values to reduce errors with colinearity of variables
  mutate(BodySize = log(BodySize + 1), Biomass = log(Biomass + 1), Abundance = log(Abundance + 1))

library(VIM)
all_plot <- aggr(all_log, col=c('navyblue','red'), 
                  numbers=TRUE, sortVars=TRUE, labels=names(all_log), 
                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data for all nodes","Pattern"))

allData <- mice(all_log, m = 50, method = "pmm")
xyplot(allData,BodySize ~ Abundance + Biomass)
densityplot(allData)
stripplot(allData, pch = 20, cex = 1.2)

# fitting a model to all the impouted data sets
modelfit1 <- with(data = allData, exp = lm(BodySize ~ Biomass + Abundance))
combine <- pool(modelfit1) # combine the data
summary(combine)

# ## AS ABOVE BUT NEMATODES
# nem <- phylum[["Nematoda"]]
# nem_log <- nem %>% 
#   select(BodySize.g., Biomass.kg.ha. , Abundance.no..ha.) %>% 
#   rename(BodySize = BodySize.g., Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha.) %>% 
#   # log the values to reduce errors with colinearity of variables
#   mutate(BodySize = log(BodySize + 1), Biomass = log(Biomass + 1), Abundance = log(Abundance + 1))
# nem_plot <- aggr(nem_log, col=c('navyblue','red'), 
#                  numbers=TRUE, sortVars=TRUE, labels=names(nem_log), 
#                  cex.axis=.7, gap=3, ylab=c("Histogram of missing data for Nematoda","Pattern"))
# 
# nemData <- mice(nem_log, m = 50, method = "pmm")
# xyplot(nemData,BodySize ~ Abundance + Biomass)
# densityplot(nemData)
# stripplot(nemData, pch = 20, cex = 1.2)

# one option is to split by genus as there are no missing values...
# this might make impuatation more accurate / better


# trying to get a phylogenetic tree
library(phytools)
phy <- para %>% 
  select(WorkingName, Resolution, Kingdom, Phylum, 
         Class, Order, Family, Genus, SpecificEpithet) %>% 
  rename(Species = SpecificEpithet) %>% 
  distinct()
# get rid of blank strings
phy[phy == ""] <- NA
# use dplyr to change to factors
phy <- mutate_if(phy, is.character, as.factor)

# create a phylo object
phy1 <- as.phylo.formula(~Kingdom/Phylum/Order,
                         data = phy)

#-------
# create carp matrix by binding rows of subsets
carp <- bind_rows(csm, bsq, epb) 
carp <- carp %>% select(WorkingName, BodySize.g., Biomass.kg.ha. , Abundance.no..ha.) %>% 
  rename(BodySize = BodySize.g., Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha.) %>% 
  mutate(BodySize = BodySize / 1000) 
  # log the values to reduce errors with colinearity of variables

# summarised N/M/B values in the carpinteria matrix
sum_carp <- carp %>% 
# Doesnt work because multiplying my NA values - Need to impute from RAW, maybe can fill one column in? -- Abundance = Biomass / BodySize
  # mutate(Abundance = Biomass / BodySize, 
  #        Biomass = Abundance * BodySize,
  #        BodySize = Biomass / Abundance) %>% 
  group_by(WorkingName) %>% 
  summarise(BodySize = sum(BodySize),
            Abundance = sum(Abundance),
            Biomass = sum(Biomass))
  
