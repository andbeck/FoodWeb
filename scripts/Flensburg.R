# Start -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(fluxweb)
library(cheddar)

rm(list = ls())
set.seed(1)


# Flensburg ---------------------------------------------------------------

nodes_flensburg <-  read.csv("./data/interactionwebdb/Flensburg/Flensburg_Data_Nodes.csv")
links_flensburg <- read.csv("./data/interactionwebdb/Flensburg/Flensburg_Data_Links.csv")

glimpse(nodes_flensburg)
glimpse(links_flensburg)

# nodes
glimpse(nodes_flensburg)
nodes_wrk <- nodes_flensburg %>% select(Node.ID, WorkingName) %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))
glimpse(nodes_wrk)

# trophic.links ----

# 1. get links:
trophic.links <- links_flensburg %>% select(ConsumerNodeID, ResourceNodeID)

# 2. isolate each column and rename to match nodes Node.ID column name
CNtemp <- trophic.links %>% select(ConsumerNodeID) %>%
  rename(Node.ID = ConsumerNodeID)

RNtemp <- trophic.links %>% select(ResourceNodeID) %>%
  rename(Node.ID = ResourceNodeID)

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
properties <-  list(title = "Flensburg")
properties

# make the community
flensburg_web <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)

# plot
plot(flensburg_web)

# cleanup before next step
rm(properties)
rm(nodes)
rm(trophic.links)
