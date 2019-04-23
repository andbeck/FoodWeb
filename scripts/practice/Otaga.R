
# Start -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(fluxweb)
library(cheddar)

rm(list = ls())
set.seed(1)

# Load in the edges and nodes ------------------------------------------------------

nodes_Otago <- read_csv("./data/interactionwebdb/Otago/Otago_Data_Nodes.csv")
links_Otago <- read_csv("./data/interactionwebdb/Otago/Otago_Data_Links.csv")

glimpse(nodes_Otago)
glimpse(links_Otago)

# Let’s try to make sense -------------------------------------------------
# need to make a community file for cheddar and put them in a list

# nodes
glimpse(nodes_Otago)
nodes_otagocheddar <- nodes_Otago %>% select(NodeID)
nodes <- nodes_otagocheddar %>% rename(node = NodeID)
glimpse(nodes)
unique(nodes)
write.csv(nodes, file = "./data/cheddar/Otago/nodes.csv")

# trophic.links
trophic.links <- links_Otago %>% select(ConsumerNodeID, ResourceNodeID) %>% 
rename(consumer = ConsumerNodeID, resource = ResourceNodeID)
glimpse(trophic.links)
write.csv(trophic.links, file = "./data/cheddar/Otago/trophic.links.csv")

# properties
properties <-  list(title = "Otago")
properties
write.csv(properties, file = "./data/cheddar/Otago/properties.csv")

# put in a list
# community_Otago <- list(nodes = nodes, trophic.links = trophic.links, properties = properties)
# glimpse(community_Otago)

Otago <- LoadCommunity("./data/cheddar/Otago") # this should work but doesnt


