
# Start -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(fluxweb)
library(cheddar)

rm(list = ls())
set.seed(1)

# Load in the edges and nodes ------------------------------------------------------

nodes_EPB <- read.csv("./data/interactionwebdb/Carpinteria/EPBweb_nodes.csv")
links_EPB <- read.csv("./data/interactionwebdb/Carpinteria/EPBweb_links.csv")

nodes_BSQ <- read.csv("./data/interactionwebdb/Carpinteria/BSQweb_nodes.csv")
links_BSQ <- read.csv("./data/interactionwebdb/Carpinteria/BSQweb_links.csv")

nodes_CSM <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_nodes.csv")
links_CSM <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_links.csv")

# Let’s try to make sense -------------------------------------------------
# need to make a community file for cheddar and put them in a list

##### EPB #####

# nodes
glimpse(nodes_EPB)
nodes_EPB <- nodes_EPB %>% select(NodeID, ConsumerStrategy.stage.)
nodes <- nodes_EPB %>% rename(node = NodeID, functional.group = ConsumerStrategy.stage.)
glimpse(nodes)

# trophic.links
trophic.links <- links_EPB %>% select(ConsumerNodeID, ResourceNodeID) %>% 
  rename(consumer = ConsumerNodeID, resource = ResourceNodeID)
glimpse(trophic.links)

# properties
properties <-  list(title = "EPB")
properties

# put in a list
community_EPB <- list(nodes = nodes, trophic.links = trophic.links, properties = properties)
glimpse(community_EPB)

# cleanup before next step
rm(properties)
rm(nodes)
rm(trophic.links)

##### CSM ######

# nodes
glimpse(nodes_CSM)
nodes_CSM <- nodes_CSM %>% select(NodeID, ConsumerStrategy.stage.)
nodes <- nodes_CSM %>% rename(node = NodeID, functional.group = ConsumerStrategy.stage.)
glimpse(nodes)

# trophic.links
trophic.links <- links_CSM %>% select(ConsumerNodeID, ResourceNodeID) %>% 
  rename(consumer = ConsumerNodeID, resource = ResourceNodeID)
glimpse(trophic.links)

# properties
properties <-  list(title = "CSM")
properties

# put in a list
community_CSM <- list(nodes = nodes, trophic.links = trophic.links, properties = properties)
glimpse(community_CSM)

# cleanup before next step
rm(properties)
rm(nodes)
rm(trophic.links)

##### BSQ ######

# nodes
glimpse(nodes_BSQ)
nodes_BSQ <- nodes_BSQ %>% select(NodeID, ConsumerStrategy.stage.)
nodes <- nodes_BSQ %>% rename(node = NodeID, functional.group = ConsumerStrategy.stage.)
glimpse(nodes)

# trophic.links
trophic.links <- links_BSQ %>% select(ConsumerNodeID, ResourceNodeID) %>% 
  rename(consumer = ConsumerNodeID, resource = ResourceNodeID)
glimpse(trophic.links)

# properties
properties <-  list(title = "BSQ")
properties

# put in a list
community_BSQ <- list(nodes = nodes, trophic.links = trophic.links, properties = properties)
glimpse(community_BSQ)

PlotPredationMatrix(community_BSQ)
# cleanup before next step
rm(properties)
rm(nodes)
rm(trophic.links)

