rm(list=ls())
set.seed(12)

# packages
library(tidyverse)
library(igraph)

# load data from source ---------------------------------------------------
source("scripts/Clean.R")


# set up graphs -----------------------------------------------------------
bsq <- graph_from_data_frame(d = bsq_l, vertices = bsq_n)
csm <- graph_from_data_frame(d = csm_l, vertices = csm_n)
epb <- graph_from_data_frame(d = epb_l, vertices = epb_n)
bsq_free <- graph_from_data_frame(d = bsq_lfree, vertices = bsq_nfree)
csm_free <- graph_from_data_frame(d = csm_lfree, vertices = csm_nfree)
epb_free <- graph_from_data_frame(d = epb_lfree, vertices = epb_nfree)

# clean up isolated nodes
bsq <- delete_vertices(bsq, which(degree(bsq) == 0)) # delete all vertices whose degree values are 0
csm <- delete_vertices(csm, which(degree(csm) == 0)) # delete all vertices whose degree values are 0
epb <- delete_vertices(epb, which(degree(epb) == 0)) # delete all vertices whose degree values are 0
bsq_free <- delete_vertices(bsq_free, which(degree(bsq_free) == 0)) # delete all vertices whose degree values are 0
csm_free <- delete_vertices(csm_free, which(degree(csm_free) == 0)) # delete all vertices whose degree values are 0
epb_free <- delete_vertices(epb_free, which(degree(epb_free) == 0)) # delete all vertices whose degree values are 0



# BSQ ---------------------------------------------------------------------

# set sizes of nodes
V(bsq)$size <- 5

# edge colours by LinkTypes
unique(E(bsq)$LinkTypeID) # LinkType shows the type of link between consumer and resource

E(bsq)$color[E(bsq)$LinkTypeID == 1] <- 'grey' # predation
E(bsq)$color[E(bsq)$LinkTypeID == 14] <- 'grey' # concurrent predation on symbionts
E(bsq)$color[E(bsq)$LinkTypeID == 15] <- 'grey' # trophic transmission
E(bsq)$color[E(bsq)$LinkTypeID == 16] <- 'grey' # predation on free-living non-feeding stage
E(bsq)$color[E(bsq)$LinkTypeID == 6] <- 'grey' # macroparasitism
E(bsq)$color[E(bsq)$LinkTypeID == 4] <- 'grey' # parasitic castration
E(bsq)$color[E(bsq)$LinkTypeID == 19] <- 'grey' # parasite intraguild antagonism
E(bsq)$color[E(bsq)$LinkTypeID == 12] <- 'grey' # trophically transmitted parasitism

# plot
plot(bsq, layout = layout_with_lgl(bsq), edge.arrow.size = .3)

# NetIndicies

library(NetIndices)

# get adjacency
bsq_matrix <- get.adjacency(bsq, sparse=F)

# trophic indicies
bsq_troph <- TrophInd(bsq_matrix)

# layout using trophic indices
layout.matrix.1<-matrix(
  nrow=length(V(bsq)),  # Rows equal to the number of vertices
  ncol=2
)
layout.matrix.1[,1]<-runif(length(V(bsq))) # randomly assign along x-axis
layout.matrix.1[,2]<-bsq_troph$TL 

# plot 
plot(bsq, layout = layout.matrix.1, edge.arrow.size = .3)



# CSM ---------------------------------------------------------------------

# set sizes of nodes
V(csm)$size <- 5

# edge colours by LinkTypes
unique(E(csm)$LinkTypeID) # LinkType shows the type of link between consumer and resource

E(csm)$color[E(csm)$LinkTypeID == 1] <- 'grey' # predation
E(csm)$color[E(csm)$LinkTypeID == 14] <- 'grey' # concurrent predation on symbionts
E(csm)$color[E(csm)$LinkTypeID == 15] <- 'grey' # trophic transmission
E(csm)$color[E(csm)$LinkTypeID == 16] <- 'grey' # predation on free-living non-feeding stage
E(csm)$color[E(csm)$LinkTypeID == 6] <- 'grey' # macroparasitism
E(csm)$color[E(csm)$LinkTypeID == 4] <- 'grey' # parasitic castration
E(csm)$color[E(csm)$LinkTypeID == 19] <- 'grey' # parasite intraguild antagonism
E(csm)$color[E(csm)$LinkTypeID == 12] <- 'grey' # trophically transmitted parasitism

# plot
plot(csm, layout = layout_with_lgl(csm), edge.arrow.size = .3)

# NetIndicies
library(NetIndices)

# get adjacency
csm_matrix <- get.adjacency(csm, sparse=F)

# trophic indicies
csm_troph <- TrophInd(csm_matrix)

# layout using trophic indices
layout.matrix.1<-matrix(
  nrow=length(V(csm)),  # Rows equal to the number of vertices
  ncol=2
)
layout.matrix.1[,1]<-runif(length(V(csm))) # randomly assign along x-axis
layout.matrix.1[,2]<-csm_troph$TL 

# plot 
plot(csm, layout = layout.matrix.1, edge.arrow.size = .3)



# EPB ---------------------------------------------------------------------

# set sizes of nodes
V(epb)$size <- 5

# edge colours by LinkTypes
unique(E(epb)$LinkTypeID) # LinkType shows the type of link between consumer and resource

E(epb)$color[E(epb)$LinkTypeID == 1] <- 'grey' # predation
E(epb)$color[E(epb)$LinkTypeID == 14] <- 'grey' # concurrent predation on symbionts
E(epb)$color[E(epb)$LinkTypeID == 15] <- 'grey' # trophic transmission
E(epb)$color[E(epb)$LinkTypeID == 16] <- 'grey' # predation on free-living non-feeding stage
E(epb)$color[E(epb)$LinkTypeID == 6] <- 'red' # macroparasitism
E(epb)$color[E(epb)$LinkTypeID == 4] <- 'grey' # parasitic castration
E(epb)$color[E(epb)$LinkTypeID == 19] <- 'grey' # parasite intraguild antagonism
E(epb)$color[E(epb)$LinkTypeID == 12] <- 'yellow' # trophically transmitted parasitism

# plot
plot(epb, layout = layout_with_lgl(epb), edge.arrow.size = .3)

# NetIndicies
library(NetIndices)

# get adjacency
epb_matrix <- get.adjacency(epb, sparse=F)

# trophic indicies
epb_troph <- TrophInd(epb_matrix)

# layout using trophic indices
layout.matrix.1<-matrix(
  nrow=length(V(epb)),  # Rows equal to the number of vertices
  ncol=2
)
layout.matrix.1[,1]<-runif(length(V(epb))) # randomly assign along x-axis
layout.matrix.1[,2]<-epb_troph$TL 

# plot 
plot(epb, layout = layout.matrix.1, edge.arrow.size = .3)

# Network Stats -----------------------------------------------------------
webs <- list(bsq = "bsq", csm = "csm", epb ="epb")


# Motif Analysis ----------------------------------------------------------



