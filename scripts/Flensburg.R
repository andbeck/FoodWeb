# Start -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(fluxweb)
library(cheddar)

rm(list = ls())
set.seed(1)


# Flensburg ---------------------------------------------------------------

nodes_flensburg <-  read_csv("./data/interactionwebdb/Flensburg/Flensburg_Data_Nodes.csv")
links_flensburg <- read_csv("./data/interactionwebdb/Flensburg/Flensburg_Data_Links.csv")

glimpse(nodes_flensburg)
glimpse(links_flensburg)


# iGraph ------------------------------------------------------------------

web_flensburg <- graph_from_data_frame(d = links_flensburg, vertices = nodes_flensburg, directed = T)
web_flensburg

layout_circle <- layout.circle(web_flensburg)
plot(web_flensburg, layout = layout_circle) # That's a mess

# Let’s try to make sense -------------------------------------------------
# need to make a community file for cheddar and put them in a list
  
# nodes
glimpse(nodes_flensburg)
nodes_flensburg <- nodes_flensburg %>% select(`Node ID`, ConsumerStrategyStage)
nodes <- nodes_flensburg %>% rename(node = `NodeID`, functional.group = ConsumerStrategyStage)
glimpse(nodes)

# trophic.links
trophic.links <- links_flensburg %>% select(ConsumerNodeID, ResourceNodeID) %>% 
rename(consumer = ConsumerNodeID, resource = ResourceNodeID)
glimpse(trophic.links)

# properties
properties <-  properties <-  list(title = "Flensburg")
properties

# put in a list
community_Flensburg <- list(nodes = nodes, trophic.links = trophic.links, properties = properties)
glimpse(community_Flensburg)

# load community from list? ARGHHH

