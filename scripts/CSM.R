
# Start -------------------------------------------------------------------

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
nodes_wrk <- nodes_CSM %>% select(NodeID, WorkingName, Abundance.no..ha., BodySize.g., ConsumerStrategy.stage.) %>% 
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
properties <-  list(title = "CSM", M.units = "g", N.units = "no.ha")
properties

# make the community
CSM <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)


# Lumped Trophic Species --------------------------------------------------

# CSM_lump <- LumpTrophicSpecies(CSM, include.isolated = FALSE)
# 
# par(mfrow = c(1,2))
# plot(CSM)
# plot(CSM_lump, show.na = TRUE)
# par(mfrow = c(1,1))


# removing N/M nodes with NA values ---------------------------------------
# cant get rid of Basal nodes? Figure out which ones have N/M values:
CSMbasal <- BasalNodes(CSM)
CSMbasal #list of basal node names
CSMbasal <- subset(nodes, node %in% CSMbasal) # now have all row values for the basal nodes in R
complete.cases(CSMbasal) # none of the basal nodes are complete cases


# get a list of node names that need removing?
nodeNA <- nodes %>% filter(!complete.cases(.)) %>% 
  select(node)
nodeNA <- nodeNA %>% as.list # should work

# now with NA nodes
CSMlite <- RemoveNodes(CSM, !complete.cases(nodes), method = "direct") # changing this to secondary/cascade results in NA nodes
plot(CSMlite, show.nodes.as = "labels", node.labels = "node", cex = .5, show.na = T)
PlotWebByLevel(CSMlite, show.na = FALSE, show.nodes.as = "labels", node.labels = "node", cex = .5,
               highlight.links = ResourceLargerThanConsumer(CSMlite))

# now any isolated nodes
CSMlite <- RemoveIsolatedNodes(CSMlite) #there are 0 anyway:
IsolatedNodes(CSMlite)




# exploratory plot --------------------------------------------------------

# symbols for plot NvM
symbol.spec = c("autotroph" = 15, "detritivore" = 16, "detritus" = 17, "macroparasite" = 15,
                "micropredator" = 16, "nonfeeding" = 17, "parasitic castrator" = 15,
                "pathogen" = 16, "predator" = 17, "trophically transmitted parasite" = 15)
colour.spec = c("autotroph" = "green", "detritivore" = "grey", "detritus" = "brown", "macroparasite" = "red",
                "micropredator" = "blue", "nonfeeding" = "darkgreen", "parasitic castrator" = "orange",
                "pathogen" = "coral", "predator" = "black", "trophically transmitted parasite" = "purple")
# plot NvM
plot(CSMlite, show.na = FALSE, symbol.by = 'functional.group',
     symbol.spec = symbol.spec, colour.by = 'functional.group',
     colour.spec = colour.spec, show.web = FALSE, cex = 1.5)
legend("topright", legend = names(colour.spec), pch = symbol.spec,
       col = colour.spec, pt.bg = colour.spec, cex = .4, pt.cex = 1,
       text.width = 3)

# add models by functional.group? - cant get to plot
models <- NvMLinearRegressions(CSMlite, class = 'functional.group')
colours <- PlotLinearModels(models, colour.spec = colour.spec)

