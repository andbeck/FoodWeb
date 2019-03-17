library(tidyverse)
library(data.table)
library(igraph)

# iGraph for CSM ----------------------------------------------------------
CSM_vertices <- read.csv(file = "data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv") 
CSM_edges <- read.csv(file = "data/interactionwebdb/Carpinteria/CSMweb_Links.csv")

# Clean up the edges
colnames(vertices)
colnames(edges)

vertices <- # remove all columns full of NA values
  CSM_vertices[,colSums(is.na(CSM_vertices)) < nrow(CSM_vertices)] %>% # removes columns with NA values
  select(NodeID, SpeciesID, WorkingName, OrganismalGroup,  # select useful bits
         ConsumerStrategy.stage.) %>% 
  rename(FunctionalGroup = ConsumerStrategy.stage.)
 
  
edges <- # as above but selecting only Resource/ConsumerNodeID and LinkType
  CSM_edges[,colSums(is.na(CSM_edges)) < nrow(CSM_edges)] %>% 
  select(ResourceNodeID, ConsumerNodeID, LinkType)

graph <- # creating the igraph object from the data frames
  graph_from_data_frame(d = edges, vertices = vertices, directed = TRUE)

# view the plot 
l = layout_with_lgl(graph)
plot(graph, edge.arrow.size = .3, vertex.label = V(graph)$NodeID, layout = NULL)

# need to clean up some isolated nodes
Isolated <-  which(degree(graph)==0)
graph <- delete.vertices(graph, Isolated)

# simplify
graph <- simplify(graph, remove.multiple = T)

# add colours depending on FunctionalGroup
library(RColorBrewer)

colrs <- c("green", "black", "olivedrab", "yellow", "cyan4", "orange", "red", "white", "purple", "blue")
V(graph)$color <- V(graph)$FunctionalGroup
# V(graph)$color <- colrs[V(graph)$FunctionalGroup] # Trying to assign all colours at once doesnt work...
V(graph)$color = gsub("autotroph", "green", V(graph)$color)
V(graph)$color = gsub("detritus", "black", V(graph)$color)
V(graph)$color = gsub("detritivore", "olivedrab", V(graph)$color)
V(graph)$color = gsub("micropredator", "yellow", V(graph)$color)
V(graph)$color = gsub("predator", "cyan4", V(graph)$color)
V(graph)$color = gsub("macroparasite", "orange", V(graph)$color)
V(graph)$color = gsub("parasitic castrator", "red", V(graph)$color)
V(graph)$color = gsub("nonfeeding", "white", V(graph)$color)
V(graph)$color = gsub("pathogen", "purple", V(graph)$color)
V(graph)$color <- gsub("trophically transmitted parasite", "blue", V(graph)$color)
V(graph)$color

# set vertex size based on degree
deg <- degree(graph, mode = "all")
V(graph)$size <- log(deg, 10) * 3 + 2


#plot it
l <- layout_with_lgl(graph)
plot(graph, layout = l, edge.curved = .3, vertex.label.family = "Arial Black", 
     vertex.label.color = NA, edge.arrow.size = .3, vertex.label = NA)
legend(x= -1.4, y= -1 ,c("Autotrophs","Detritus", "Detritivore", "Micropredator", 
                       "Predator", "Macroparasite", "Parasitic Castrator", "Nonfeeding",
                       "Pathogen", "Trophic Parasite"),
       pch=21, col="#777777", pt.bg=colrs, pt.cex=2, 
       cex=.8, bty="n", ncol = 4)

# analysis: is connected?
is.connected(graph) # TRUE
no.clusters(graph) # 1 cluster
plot(degree.distribution(graph, mode = "in"), log = "xy") # in degree distribution
plot(degree.distribution(graph, mode = "out"), log = "xy") # out degree distribution

g <- erdos.renyi.game(vcount(graph), ecount(graph), type="gnm")# Random graph of same size

max(degree(graph, mode="in")) # 92
max(degree(graph, mode="out")) # 42 # all these values are 42 for random graph
max(degree(graph, mode="all")) # 119

graph.density(graph) # 0.05
graph.density(g) # 0.1

transitivity(graph) # 0.152
transitivity(g) # 0.1 

dyad.census(graph)
plot(triad.census(graph))

q <-  communities(graph)


layout.matrix.1<-matrix(
  nrow=length(V(graph)),  # Rows equal to the number of vertices
  ncol=2
)
layout.matrix.1[,1]<-runif(length(V(otago.graph))) # randomly assign along x-axis
layout.matrix.1[,2]<-troph.otago$TL # y-axis value based on trophic level

layout.matrix.1p<-matrix(
  nrow=length(V(otago.graph.p)),  # Rows equal to the number of vertices
  ncol=2
)
layout.matrix.1p[,1]<-runif(length(V(otago.graph.p)))
layout.matrix.1p[,2]<-troph.otago.p$TL