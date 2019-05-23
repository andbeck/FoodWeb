# Start -------------------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(ggforce)
library(igraph)
library(fluxweb)
library(cheddar)
library(ggfortify)
library(missForest)

# Load in the edges and nodes ------------------------------------------------------

nodes_CSM <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_nodes.csv")
links_CSM <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_links.csv")

# rough imputation ----

nodes_imputation <- 
  nodes_CSM %>% 
  select(M = BodySize.g.,
         N = Abundance.no..ha.,
         Biomass = Biomass.kg.ha.,
         Phylum,
         Class,
         Order) %>% 
  mutate(M = M / 1000) %>% 
  missForest()

nodes_imputation <- nodes_imputation$ximp

nodes_CSM <- 
  bind_cols(nodes_CSM,
            nodes_imputation)

nodes_wrk <- 
  nodes_CSM %>% 
  select(NodeID, 
         WorkingName, 
         N, 
         M, 
         Group = ConsumerStrategy.stage.,) %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))

# limit low values to 0.01
nodes_wrk$N[nodes_wrk$N == 0] <- 0.01

# trophic.links ----

# 1. get links:
trophic.links <- links_CSM %>% select(ConsumerNodeID, ResourceNodeID)

# 2. isolate each column and rename to match nodes NodeID column name
CNtemp <- trophic.links %>% select(ConsumerNodeID) %>%
  rename(NodeID = ConsumerNodeID)

RNtemp <- trophic.links %>% select(ResourceNodeID) %>%
  rename(NodeID = ResourceNodeID)

# 3. use left_join to replace with names
CN_name <- left_join(CNtemp, nodes_wrk)
RN_name <- left_join(RNtemp, nodes_wrk)

# head(left_join(CNtemp, nodes_wrk))
# head(CNtemp) 

# 4. rebuild trophic.links
trophic.links <- data.frame(ConsumerNodeID = CN_name$WorkingName,
                            ResourceNodeID = RN_name$WorkingName)
head(trophic.links)

trophic.links <- trophic.links %>% 
  rename(consumer = ConsumerNodeID, resource = ResourceNodeID)

# change node names
nodes <- 
  nodes_wrk %>%
  select(node = WorkingName, 
         M, 
         N, 
         Group)

# properties
properties <-  list(title = "CSM", M.units = "g", N.units = "no.ha")
properties

# make the community
CSM <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)

PlotWebByLevel(CSM, highlight.links = ResourceLargerThanConsumer(CSM))
tail(NPS(CSM, c('Log10MNBiomass', Deg='Degree', Top='IsTopLevelNode', TS='TrophicSpecies')))

# export



