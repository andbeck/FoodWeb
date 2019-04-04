
# Start -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(fluxweb)
library(cheddar)
library(cowplot)


# Load in the edges and nodes ------------------------------------------------------

nodes_EPB <- read.csv("./data/interactionwebdb/Carpinteria/EPBweb_nodes.csv")
links_EPB <- read.csv("./data/interactionwebdb/Carpinteria/EPBweb_links.csv")

# Let’s try to make sense -------------------------------------------------
# need to make a community file for cheddar and put them in a list

##### EPB #####

# nodes
glimpse(nodes_EPB)
nodes_wrk <- nodes_EPB %>% select(NodeID, WorkingName, Abundance.no..ha., BodySize.g., ConsumerStrategy.stage.) %>% 
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
nodes <- nodes_wrk %>% rename(node = WorkingName, 
                              M = BodySize.g., 
                              N = Abundance.no..ha.,
                              functional.group = ConsumerStrategy.stage.) %>% 
  select(node, M, N, functional.group)
head(nodes)

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
properties <-  list(title = "EPB", M.units = "g", N.units = "no.ha")
properties

# make the community
EPB <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)

### PLOTTING WITH CHEDDAR ###
# # symbols
# symbol.spec = c("autotroph" = 15, "detritivore" = 16, "detritus" = 17, "macroparasite" = 15,
#                 "micropredator" = 16, "nonfeeding" = 17, "parasitic castrator" = 15,
#                 "pathogen" = 16, "predator" = 17, "trophically transmitted parasite" = 15, "parasitoid" = 16)
# colour.spec = c("autotroph" = "green", "detritivore" = "grey", "detritus" = "brown", "macroparasite" = "red",
#                 "micropredator" = "blue", "nonfeeding" = "darkgreen", "parasitic castrator" = "orange",
#                 "pathogen" = "coral", "predator" = "black", "trophically transmitted parasite" = "purple", "parasitoid" = "steelblue")
# 
# 
# # plot
# par(mfrow=c(1,2))
# plot(EPB, show.na = FALSE, symbol.by = 'functional.group', 
#      symbol.spec = symbol.spec, colour.by = 'functional.group',
#      colour.spec = colour.spec, show.web = FALSE, cex = 1.5)
# legend("topright", legend = names(colour.spec), pch = symbol.spec,
#        col = colour.spec, pt.bg = colour.spec, cex = .5, pt.cex = 1,
#        text.width = 3)
# # now with NA's
# plot(EPB, show.na = TRUE, symbol.by = 'functional.group', 
#      symbol.spec = symbol.spec, colour.by = 'functional.group',
#      colour.spec = colour.spec, show.web = FALSE, cex = 1.5)
# legend("topright", legend = names(colour.spec), pch = symbol.spec,
#        col = colour.spec, pt.bg = colour.spec, cex = .5, pt.cex = 1,
#        text.width = 3)
# par(mfrow=c(1,1))
# 
# # plot with missing N/M's
# plot(EPB, show.na = TRUE)
# 
# PlotWebByLevel(EPB)
# 
# par(mfrow=c(1,2))
# PlotNvM(EPB, highlight.links = ResourceLargerThanConsumer(EPB))
# PlotNvM(EPB, highlight.links = ResourceLargerThanConsumer(EPB),
#         show.na = TRUE)
# par(mfrow=c(1,1))

# removing N/M nodes with NA values ---------------------------------------

### NOT NEEDED ###
# # cant get rid of Basal nodes? Figure out which ones have N/M values:
# EPBbasal <- BasalNodes(EPB)
# EPBbasal #list of basal node names
# EPBbasal <- subset(nodes, node %in% EPBbasal) # now have all row values for the basal nodes in R
# complete.cases(EPBbasal) # none of the basal nodes are complete cases

### Not Needed ###
# # get a list of node names that need removing?
# nodeNA_EPB <- nodes %>% filter(!complete.cases(.)) %>% 
#   select(node)
# nodeNA_EPB <- nodeNA_EPB %>% as.list # should work

# now with NA nodes
EPBlite <- RemoveNodes(EPB, !complete.cases(nodes), method = "direct") # changing this to secondary/cascade results in NA nodes
EPBlite

plot(EPBlite, show.nodes.as = "labels", node.labels = "node", cex = .5, show.na = T)
PlotWebByLevel(EPBlite, show.na = FALSE, show.nodes.as = "labels", node.labels = "node", cex = .5,
               highlight.links = ResourceLargerThanConsumer(EPBlite))

# now any isolated nodes
EPBlite <- RemoveIsolatedNodes(EPBlite) #there are 0 anyway:
IsolatedNodes(EPBlite)



