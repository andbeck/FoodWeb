
# Start -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(fluxweb)
library(cheddar)

rm(list = ls())
set.seed(1)

# Load in the edges and nodes ------------------------------------------------------

nodes_Otago <- read.csv("./data/interactionwebdb/Otago/Otago_Data_Nodes.csv")
links_Otago <- read.csv("./data/interactionwebdb/Otago/Otago_Data_Links.csv")

glimpse(nodes_Otago)
glimpse(links_Otago)

# Let’s try to make sense -------------------------------------------------
# need to make a community file for cheddar and put them in a list

# nodes ANDREW
# you MUST 
# a) NOT use numbers for nodes; 
# b) create unique identifiers for node names; then 
# c) map these to the trophic links to reconstruct the trophic.links

# get the detail on node names ----

# this is a data frame with the NodeID and the WorkingName, with working names unique
# we can talk through the choice here.
nodes_wrk <- nodes_Otago %>% select(NodeID,WorkingName) %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))

# adjust the trophic links now.... ----
# replace all NodeID with appropriate Working Name

# step 1, get links
trophic.links <- links_Otago %>% select(ConsumerNodeID, ResourceNodeID)

# step 2: isolate each column and rename to match nodes NodeID column name
CNtemp <- trophic.links %>% select(ConsumerNodeID) %>% 
  rename(NodeID = ConsumerNodeID)

RNtemp <- trophic.links %>% select(ResourceNodeID) %>% 
  rename(NodeID = ResourceNodeID)

# step 3: use left_join to replace with names
# this is a giant look up table exercise
# the NodeID column is common to each data frame
# the unique name info in nodes_wrk is mapped onto the shared NodesID column
CN_name <- left_join(CNtemp, nodes_wrk)
RN_name <- left_join(RNtemp, nodes_wrk)

# # proof - check and see that the sequence of numbers (the shared bit) matches....
# head(left_join(CNtemp, nodes))
# head(CNtemp)

# step 4: rebuild trophic.links
trophic.links <- data.frame(ConsumerNodeID = CN_name$WorkingName,
                            ResourceNodeID = RN_name$WorkingName)
head(trophic.links)

trophic.links <- trophic.links %>% 
  rename(consumer = ConsumerNodeID, resource = ResourceNodeID)

# change nodes names
nodes <- nodes_wrk %>% rename(node = WorkingName) %>% 
  select(node)
head(nodes)

# properties
properties <-  list(title = "Otago")
properties

# make community
otago_web <- Community(nodes = nodes, properties = properties, trophic.links = trophic.links)

# view it.
plot(otago_web)
