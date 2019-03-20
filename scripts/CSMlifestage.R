
library(tidyverse)
library(ggforce)
library(igraph)
library(fluxweb)
library(cheddar)
library(ggfortify)


# Load in the edges and nodes ------------------------------------------------------

nodes_CSM <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_nodes.csv")
links_CSM <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_links.csv")

# Let’s try to make sense -------------------------------------------------
# need to make a community file for cheddar and put them in a list

##### CSM #####

# nodes
glimpse(nodes_CSM)
nodes_wrk <- nodes_CSM %>% select(SpeciesID.StageID, WorkingName, Abundance.no..ha., BodySize.g., ConsumerStrategy.stage.) %>% 
  # mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_")) %>% 
  rename(NodeID = SpeciesID.StageID)
glimpse(nodes_wrk)

# trophic.links ----

# 1. get links:
trophic.links <- links_CSM %>% select(ConsumerSpeciesID.StageID, ResourceSpeciesID.StageID)

# 2. isolate each column and rename to match nodes NodeID column name
CNtemp <- trophic.links %>% select(ConsumerSpeciesID.StageID) %>%
  rename(NodeID = ConsumerSpeciesID.StageID)

RNtemp <- trophic.links %>% select(ResourceSpeciesID.StageID) %>%
  rename(NodeID = ResourceSpeciesID.StageID)

# 3. use left_join to replace with names
CN_name <- left_join(CNtemp, nodes_wrk) # semi_join needed to drop observations that are not included in df2
RN_name <- left_join(RNtemp, nodes_wrk)


# head(left_join(CNtemp, nodes_wrk))
# head(CNtemp) 

# 4. rebuild trophic.links
trophic.links <- data.frame(ConsumerSpeciesID.StageID = CN_name$WorkingName,
                            ResourceSpeciesID.StageID = RN_name$WorkingName)
head(trophic.links)

trophic.links <- trophic.links %>% 
  rename(consumer = ConsumerSpeciesID.StageID, resource = ResourceSpeciesID.StageID)

# change node names
nodes <- nodes_wrk %>% rename(node = WorkingName, 
                              M = BodySize.g., 
                              N = Abundance.no..ha.,
                              functional.group = ConsumerStrategy.stage.) %>% 
  select(node, M, N, functional.group)
head(nodes)

nodes <- nodes %>% mutate(node = make.unique(as.character(node), sep = "_")) 

# duplicated trophic links
nrow(trophic.links) - nrow(unique(trophic.links)) # 262 duplicated links
trophic.links <- trophic.links %>% distinct() # gets rid of duplicated rows in a dataframe

#----- Trying to make all node values not blank values
# functional.group
levels(nodes$functional.group)[levels(nodes$functional.group == "")] <- "NA"

# N values
sum(is.na(nodes$N))
nodes$N[is.na(nodes$N)] <- NA # This has to be 0<N<inf or empty 
sum(is.na(nodes$N))
nodes$N

# there are still some 0 values that need to be changed
nodes$N[nodes$N == 0] <- NA

# M values
sum(is.na(nodes$M))
nodes$M[is.na(nodes$M)] <- NA # This has to be 0<M<inf or empty
sum(is.na(nodes$M))
nodes$M

# properties
properties <-  list(title = "CSM", M.units = "g", N.units = "no.ha")
properties


# make the community
CSM <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)

PlotWebByLevel(RemoveIsolatedNodes(CSM))
RemoveIsolatedNodes(CSM)
PlotNvM(RemoveIsolatedNodes(CSM), PreyAveragedTrophicLevel(RemoveIsolatedNodes(CSM)))
        
