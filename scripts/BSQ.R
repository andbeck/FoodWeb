
# Start -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(fluxweb)
library(cheddar)
library(cowplot)

# Load in the edges and nodes ------------------------------------------------------

nodes_BSQ <- read.csv("./data/interactionwebdb/Carpinteria/BSQweb_nodes.csv")
links_BSQ <- read.csv("./data/interactionwebdb/Carpinteria/BSQweb_links.csv")

# Let’s try to make sense -------------------------------------------------
# need to make a community file for cheddar and put them in a list

##### BSQ #####

# nodes
glimpse(nodes_BSQ)
nodes_wrk <- nodes_BSQ %>% select(NodeID, WorkingName, Abundance.no..ha., BodySize.g., ConsumerStrategy.stage.) %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))
glimpse(nodes_wrk)

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
properties <-  list(title = "BSQ", M.units = "g", N.units = "no.ha")
properties

# make the community
BSQ <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)

# # symbols -----
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
# plot(BSQ_web, show.na = FALSE, symbol.by = 'functional.group', 
#      symbol.spec = symbol.spec, colour.by = 'functional.group',
#      colour.spec = colour.spec, show.web = FALSE, cex = 1.5)
# legend("topright", legend = names(colour.spec), pch = symbol.spec,
#        col = colour.spec, pt.bg = colour.spec, cex = .5, pt.cex = 1,
#        text.width = 3)
# # now with NA's
# plot(BSQ_web, show.na = TRUE, symbol.by = 'functional.group', 
#      symbol.spec = symbol.spec, colour.by = 'functional.group',
#      colour.spec = colour.spec, show.web = FALSE, cex = 1.5)
# legend("topright", legend = names(colour.spec), pch = symbol.spec,
#        col = colour.spec, pt.bg = colour.spec, cex = .5, pt.cex = 1,
#        text.width = 3)
# par(mfrow=c(1,1))
# 
# # plot with missing N/M's
# plot(BSQ_web, show.na = TRUE)
# 
# PlotWebByLevel(BSQ_web)
# 
# par(mfrow=c(1,2))
# PlotNvM(BSQ_web, highlight.links = ResourceLargerThanConsumer(BSQ_web))
# PlotNvM(BSQ_web, highlight.links = ResourceLargerThanConsumer(BSQ_web),
#         show.na = TRUE)
# par(mfrow=c(1,1)) This is cheddar plotting - not needed so far...

#### ---- simplify network
BSQlite <- RemoveNodes(BSQ, !complete.cases(nodes), method = "direct") # changing this to secondary/cascade results in NA nodes
BSQlite

plot(BSQlite, show.nodes.as = "labels", node.labels = "node", cex = .5, show.na = T)
PlotWebByLevel(BSQlite, show.na = FALSE, show.nodes.as = "labels", node.labels = "node", cex = .5,
               highlight.links = ResourceLargerThanConsumer(BSQlite))

# now any isolated nodes
BSQlite <- RemoveIsolatedNodes(BSQlite) #there are 0 anyway:
IsolatedNodes(BSQlite)

plot(BSQlite, ChainAveragedTrophicLevel(BSQlite))

