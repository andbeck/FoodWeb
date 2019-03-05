
# Start -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(fluxweb)
library(cheddar)

rm(list = ls())
set.seed(1)

# Load in the edges and nodes ------------------------------------------------------

nodes_CSM <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_nodes.csv")
links_CSM <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_links.csv")

nodes_BSQ <- read.csv("./data/interactionwebdb/Carpinteria/BSQweb_nodes.csv")
links_BSQ <- read.csv("./data/interactionwebdb/Carpinteria/BSQweb_links.csv")

nodes_EPB <- read.csv("./data/interactionwebdb/Carpinteria/EPBweb_nodes.csv")
links_EPB <- read.csv("./data/interactionwebdb/Carpinteria/EPBweb_links.csv")

# Let’s try to make sense -------------------------------------------------
# need to make a community file for cheddar and put them in a list

##### CSM #####

# nodes
glimpse(nodes_CSM)
nodes_wrk <- nodes_CSM %>% select(NodeID, WorkingName, Abundance.no..ha., BodySize.g.) %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))
glimpse(nodes_wrk)

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
nodes <- nodes_wrk %>% rename(node = WorkingName, M = BodySize.g., N = Abundance.no..ha.) %>% 
  select(node, M, N)
head(nodes)

#----- Trying to make all N/M values not blank values
# sum(is.na(nodes$N))
# nodes$N[is.na(nodes$N)] <- 0 # This has to be 0<N<inf
# sum(is.na(nodes$N))
# 
# sum(is.na(nodes$M))
# nodes$M[!is.na(nodes$M)] <- 0 # This has to be 0<M<inf
# sum(is.na(nodes$M))


# properties
properties <-  list(title = "CSM", M.units = "g", N.units = "no.ha")
properties

# make the community
CSM_web <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)

# plot
plot(CSM_web)

# cleanup before next step
rm(properties)
rm(nodes)
rm(trophic.links)

##### EPB ######

# nodes
glimpse(nodes_EPB)
nodes_wrk <- nodes_EPB %>% select(NodeID, WorkingName) %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))
glimpse(nodes_wrk)

# trophic.links ----

# 1. get links:
trophic.links <- links_EPB %>% select(ConsumerNodeID, ResourceNodeID)

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

# properties
properties <-  list(title = "EPB")
properties

# make the community
EPB_web <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)

# plot
plot(EPB_web)

# cleanup before next step
rm(properties)
rm(nodes)
rm(trophic.links)

##### BSQ ######

# nodes
glimpse(nodes_BSQ)
nodes_wrk <- nodes_BSQ %>% select(NodeID, WorkingName) %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))
glimpse(nodes_wrk)
dim(nodes_wrk)
unique(nodes_wrk)
# trophic.links ----

# 1. get links:
trophic.links <- links_BSQ %>% select(ConsumerNodeID, ResourceNodeID)

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
nodes

# properties
properties <-  list(title = "BSQ")
properties

# make the community
BSQ_web <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)

# plot
plot(BSQ_web)

# cleanup before next step
rm(properties)
rm(nodes)
rm(trophic.links)

