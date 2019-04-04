
# Start -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(fluxweb)
library(cheddar)

rm(list = ls())
set.seed(1)

# Load in the edges and nodes ------------------------------------------------------

nodes_Sylt <- read.csv("./data/interactionwebdb/Sylt/Sylt_Data_Nodes.csv")
links_Sylt <- read.csv("./data/interactionwebdb/Sylt/Sylt_Data_Links.csv")


# Let’s try to make sense -------------------------------------------------
# need to make a community file for cheddar and put them in a list

# nodes
glimpse(nodes_Sylt)
nodes_wrk <- nodes_Sylt %>% select(NodeID, WorkingName) %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))
glimpse(nodes_wrk)

# trophic.links ----

# 1. get links:
trophic.links <- links_Sylt %>% select(ConsumerNodeID, ResourceNodeID)

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
nodes <- nodes_wrk %>% rename(node = WorkingName) %>% 
  select(node)
head(nodes)

# some missing node names?
nodes$node[231] <- NA
nodes <- na.omit(nodes)

# properties
properties <-  list(title = "Sylt")
properties

# make the community
Sylt_web <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)

# plot
plot(Sylt_web)

