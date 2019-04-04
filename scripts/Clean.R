set.seed(12)
rm(list=ls())

# Cleanup Carp Raw Data ---------------------------------------------------

library(tidyverse)
library(data.table)

# Load in Data ------------------------------------------------------------

bsq_nodes <- read.csv("data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv")
bsq_links <- read.csv("data/interactionwebdb/Carpinteria/BSQweb_Links.csv")

csm_nodes <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")
csm_links <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Links.csv")

epb_nodes <- read.csv("data/interactionwebdb/Carpinteria/EPBweb_Nodes.csv")
epb_links <- read.csv("data/interactionwebdb/Carpinteria/EPBweb_Links.csv")

# Select Only Nodes with N/M Data -----------------------------------------

# 1) BSQ...

bsq_n <- 
  bsq_nodes %>% 
  drop_na(Biomass.kg.ha., BodySize.g.)

# create clone of nodes file and rename Species.StageID to ConsumerSpecies.StageID

bsq_n_clone <- 
  bsq_n %>% 
  rename(ConsumerSpecies.StageID = SpeciesID.StageID)


bsq_l <- 
  bsq_links %>% 
  semi_join(bsq_n_clone, by = "ConsumerSpecies.StageID")

bsq_l <- # remove links that are no longer nodes
  bsq_n[!bsq_n$SpeciesID.StageID %in% bsq_links$ConsumerSpecies.StageID,]


