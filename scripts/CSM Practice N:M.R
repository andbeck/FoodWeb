
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
CSM_web <- Community(nodes = nodes, trophic.links = trophic.links, properties = properties)

# symbols
symbol.spec = c("autotroph" = 15, "detritivore" = 16, "detritus" = 17, "macroparasite" = 15,
                "micropredator" = 16, "nonfeeding" = 17, "parasitic castrator" = 15,
                "pathogen" = 16, "predator" = 17, "trophically transmitted parasite" = 15)
colour.spec = c("autotroph" = "green", "detritivore" = "grey", "detritus" = "brown", "macroparasite" = "red",
                "micropredator" = "blue", "nonfeeding" = "darkgreen", "parasitic castrator" = "orange",
                "pathogen" = "coral", "predator" = "black", "trophically transmitted parasite" = "purple")


# plot
plot(CSM_web, show.na = FALSE, symbol.by = 'functional.group', 
     symbol.spec = symbol.spec, colour.by = 'functional.group',
     colour.spec = colour.spec, show.web = FALSE, cex = 1.5)


legend("topright", legend = names(colour.spec), pch = symbol.spec,
       col = colour.spec, pt.bg = colour.spec, cex = .4, pt.cex = 1,
       text.width = 3)

# plot with missing N/M's
plot(CSM_web, show.na = TRUE)


# exporting to iGraph? ----------------------------------------------------


#### The ToIGraph function:
ToIgraph <- function(community, weight=NULL)
{
  if(is.null(TLPS(community)))
  {
    stop("The community has no trophic links")
  }
  else
  {
    tlps <- TLPS(community, link.properties = weight)
    if(!is.null(weight))
    {
      tlps$weight <- tlps[,weight]
    }
    return(graph.data.frame(tlps,
                            vertices = NPS(community),
                            directed = TRUE))
  }
}

### Delete isolated nodes function
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, isolates)
}

# convert to Igraph object
CSM.ig <- ToIgraph(CSM_web)

# removing the isolated nodes using igraph
LO = layout_nicely(CSM.ig)
Isolated = which(degree(CSM.ig)==0)
CSMnew = delete.vertices(CSM.ig, Isolated)
LO2 = LO[-Isolated,]
plot(CSMnew, layout=LO2, vertex.size = 1, edge.size = 1)

# layout the new network and plot
lgrid <- layout_on_sphere(CSMnew)
# ceb <- cluster_edge_betweenness(G2)
# dendPlot(ceb, mode="hclust") # this is clustering detection for communities
plot(CSMnew, layout = lgrid, label = NA) # horrible layout
# .... Looks shite


# tidygraph ---------------------------------------------------------------
library(tidygraph)
library(ggraph)

# convert to tidygraph object
graph <- as_tbl_graph(CSMnew)
graph

# test with previously made degree function plotting
degree <- mutate(graph, centrality = centrality_degree()) %>% 
  ggraph(layout = "kk") +
  geom_edge_link(colour = "lightgray") +
  scale_edge_width(range = c(1,6)) +
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  scale_color_continuous(guide = 'legend') +
  labs(colour = "Degree", size = "Degree") +
  theme_graph(base_size = 15)
plot(degree)

# playing around
ggraph(graph, layout = "linear") +
  geom_edge_arc() +
  scale_edge_width(range = c(.2, 10)) + 
  geom_node_text(aes(label = functional.group)) +
  theme_graph()


# visNetwork --------------------------------------------------------------
library(visNetwork)

# converting from igraph object to vis network
network = visIgraph(CSM.ig, layout = "layout_on_grid") %>%
  visLayout(hierarchical = F,
            improvedLayout = T)
network # cool beans
visSave(network, file = "CSM.html") # saves network as interactive HTML
layout
# glimpse(network$x)
