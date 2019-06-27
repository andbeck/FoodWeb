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
# need to make functions to apply to these


# Motif Analysis ----------------------------------------------------------

# Here are the adjacency matrices for each of the 13 subgraphs again
s1<-matrix(c(0,1,0,0,0,1,0,0,0),nrow=3,ncol=3)
s2<-matrix(c(0,1,1,0,0,1,0,0,0),nrow=3,ncol=3)
s3<-matrix(c(0,1,0,0,0,1,1,0,0),nrow=3,ncol=3)
s4<-matrix(c(0,0,1,0,0,1,0,0,0),nrow=3,ncol=3)
s5<-matrix(c(0,1,1,0,0,0,0,0,0),nrow=3,ncol=3)
d1<-matrix(c(0,1,1,0,0,1,0,1,0),nrow=3,ncol=3)
d2<-matrix(c(0,1,1,1,0,1,0,0,0),nrow=3,ncol=3)
d3<-matrix(c(0,0,1,1,0,0,1,0,0),nrow=3,ncol=3)
d4<-matrix(c(0,0,0,1,0,1,0,1,0),nrow=3,ncol=3)
d5<-matrix(c(0,1,1,0,0,1,1,0,0),nrow=3,ncol=3)
d6<-matrix(c(0,1,1,1,0,1,1,1,0),nrow=3,ncol=3)
d7<-matrix(c(0,1,1,1,0,1,1,0,0),nrow=3,ncol=3)
d8<-matrix(c(0,1,1,1,0,0,1,0,0),nrow=3,ncol=3)

# Turn them into a convenient list
subgraph3.mat<-list(s1,s2,s3,s4,s5,d1,d2,d3,d4,d5,d6,d7,d8)
# And then into a list of graph objects
subgraph3.graph<-lapply(subgraph3.mat,graph.adjacency)

# Count the number of the 13 different 3-node subgraphs in the two webs
motif_csm_free<-c()
motif_csm_para<-c()
motif_bsq_free<-c()
motif_bsq_para<-c()
motif_epb_free<-c()
motif_epb_para<-c()

for(i in 1:13){
  motif_csm_free [i]<-
    graph.count.subisomorphisms.vf2(csm_free,subgraph3.graph[[i]])
  motif_csm_para[i]<-
    graph.count.subisomorphisms.vf2(csm,subgraph3.graph[[i]])
  motif_bsq_free [i]<-
    graph.count.subisomorphisms.vf2(bsq_free,subgraph3.graph[[i]])
  motif_bsq_para[i]<-
    graph.count.subisomorphisms.vf2(bsq,subgraph3.graph[[i]])
  motif_epb_free [i]<-
    graph.count.subisomorphisms.vf2(epb_free,subgraph3.graph[[i]])
  motif_epb_para[i]<-
    graph.count.subisomorphisms.vf2(epb,subgraph3.graph[[i]])
}

## plot subgraph frequencies
par(mfrow=c(1,3))
plot(motif_csm_para,type="o",lty=3, xlab="Subgraph",ylab="Frequency", main = "CSM Subgraphs")
points(motif_csm_free,type="o",lty=2)
plot(motif_bsq_para,type="o",lty=3, xlab="Subgraph",ylab="Frequency", main = "BSQ Subgraphs")
points(motif_bsq_free,type="o",lty=2)
plot(motif_epb_para,type="o",lty=3, xlab="Subgraph",ylab="Frequency", main = "EPB Subgraphs")
points(motif_epb_free,type="o",lty=2)
par(mfrow=c(1,1))



## Table of motif frequency differences
motifs <- data.frame(S1 = integer(),
                 S2 = integer(),
                 S3 = integer(),
                 S4 = integer(),
                 S5 = integer(),
                 D1 = integer(),
                 D2 = integer(),
                 D3 = integer(),
                 D4 = integer(),
                 D5 = integer(),
                 D6 = integer(),
                 D7 = integer(),
                 D8 = integer(),
                 stringsAsFactors = FALSE)

list_motif_results <- list("CSMFree" = motif_csm_free,
                           "CSMPara" = motif_csm_para,
                           "BSQFree" = motif_bsq_free,
                           "BSQPara" = motif_bsq_para,
                           "EPBFree" = motif_epb_free,
                           "EPBPara" = motif_epb_para)

motifs <- as.data.frame(do.call(rbind, list_motif_results))
motifs <- motifs %>% 
  rename(S1 = "V1", S2 = "V2", S3 = "V3", S4 = "V4", S5 = "V5",
         D1 = "V6", D2 = "V7", D3 = "V8", D4 = "V9", D5 = "V10",
         D6 = "V11", D7 = "V12", D8 = "V13")

# give rownames new column and set rownames to null after
motifs <- cbind(Web = rownames(motifs), motifs)
rownames(motifs) <- NULL



# do motifs in ggplot -- rownames need new column
ggplot(motifs, aes())




