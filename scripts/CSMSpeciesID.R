
# using netInd and Igraph to generate trophic level layout ----------------
library(igraph)
library(NetIndices)
library(tidyverse)

# read in data
CSM_links <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Links.csv")
CSM_nodes <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")

# colnames
colnames(CSM_links)
colnames(CSM_nodes)

# get the edgelist and turn into graph object
CSM_graph <- graph.edgelist(as.matrix(CSM_links[,7:8]))

# do the same but jsut for predator prey - filter first
CSM_graph_pp <- CSM_links %>% # these linktypeID's are in metadata
  filter(!str_detect(LinkTypeID, '4|5|6|8|9|12|14|15|16|19'))
CSM_graph_pp <- graph.edgelist(as.matrix(CSM_graph_pp[,7:8]))

clean.graph <- function(graph){ # function to remove multiple links and isolated nodes
  # need to clean up some isolated nodes
  Isolated <-  which(degree(graph)==0)
  graph <- delete.vertices(graph, Isolated)
  # simplify
  graph <- simplify(graph)
}

# cleanup
CSM_graph <- clean.graph(CSM_graph)
CSM_graph_pp <- clean.graph(CSM_graph_pp)

# get the webs into a matrix form
CSM_adjmatrix <- get.adjacency(clean.graph(CSM_graph), sparse = F)
CSM_adjmatrix_pp <- get.adjacency(clean.graph(CSM_graph_pp), sparse = F)

# get basic network indices from the matrix with GenID()
ind_CSM <- GenInd(CSM_adjmatrix)
ind_CSM_pp <- GenInd(CSM_adjmatrix_pp)

# plot these two webs to see whats going on 
plot(CSM_graph)
plot(CSM_graph_pp)

troph_CSM <- TrophInd(CSM_adjmatrix)
troph_CSM_pp <- TrophInd(CSM_adjmatrix_pp)


l = layout_with_lgl(clean.graph(CSM_graph))
plot(clean.graph(CSM_graph), vertex.label = V(CSM_graph)$WorkingName, layout = layout.matrix.1)
