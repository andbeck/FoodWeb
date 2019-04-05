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

bsq_na <- 
  bsq_nodes %>% 
  drop_na(Biomass.kg.ha., BodySize.g.)

bsq_n <- 
  bsq_na %>% 
  select(NodeID, WorkingName, Abundance.no..ha., 
         BodySize.g., ConsumerStrategy.stage., Biomass.kg.ha.) %>%
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"),
         BodySize.kg = BodySize.g. * 1000)

is.na(bsq_n$Abundance.no..ha.)

