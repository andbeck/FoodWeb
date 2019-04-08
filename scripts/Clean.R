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
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"),
         BodySize = BodySize.g. * 1000,
         Abundance = BodySize * Biomass.kg.ha.) %>% 
  rename(Biomass = Biomass.kg.ha., ConsumerType = ConsumerStrategy.stage.) %>% 
  select(NodeID, WorkingName, Abundance, BodySize, ConsumerType, Biomass)

# Select only links that are present in node data -------------------------

bsq_l <- 
  bsq_links %>% 
  select(ConsumerNodeID, ResourceNodeID)

## use the %in% function? 

## semi-Join?
# bsq_wrk_con <- 
#   bsq_l %>%  
#   mutate(NodeID = ConsumerNodeID) %>% 
#   anti_join(bsq_n, by = "NodeID")
# 
# bsq_wrk_res <- 
#   bsq_l %>% 
#   mutate(NodeID = ResourceNodeID) %>% 
#   anti_join(bsq_n, by = "NodeID")

## anti-join?
# bsq_wrk_con <- 
#   bsq_l %>%  
#   mutate(NodeID = ConsumerNodeID) %>% 
#   anti_join(bsq_n, by = "NodeID")
# 
# bsq_wrk_res <- 
#   bsq_l %>% 
#   mutate(NodeID = ResourceNodeID) %>% 
#   anti_join(bsq_n, by = "NodeID")

bsq_wrk <- 
  rbind(bsq_wrk_con, bsq_wrk_res)
  





