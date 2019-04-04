##### Complete Workflow for Food Web Analysis #####
##### Specifically for Carpinteria Food Webs ######

library(readxl)
library(cheddar)
library(fluxweb)
library(igraph)
library(NetIndices)
library(tidyverse)

set.seed(12)
rm(list=ls())

source("scripts/FoodWebFunctions.R")
# Step 1 - Data Reading and Cleaning --------------

nodes <- 
  read_xlsx("data/interactionwebdb/Carpinteria/carp_nodes.xlsx")
edges <- 
  read_xlsx("data/interactionwebdb/Carpinteria/carp_links.xlsx")

# Step 2 - Cleaning Data --------------------------

# remove any columns that contain just NA vlaues
nodes <-  
  nodes[,colSums(is.na(nodes)) < nrow(nodes)]
edges <- 
  edges[,colSums(is.na(edges)) < nrow(edges)]

# Step 3 - Cheddar and Cleaning Networks ----------



# Step 4 - iGraph and Network Stats ---------------

# get the edgelist and turn into graph object
carp <- distinct(edges[, 7:8]) # only take the non-duplicated edges
carp <- graph_from_edgelist(as.matrix(edges[, 7:8]))
plot(carp)
centr_degree(carp)

# do the same but jsut for predator prey - filter first
carp_pp <- edges %>% # these linktypeID's are in metadata
  filter(!str_detect(LinkTypeID, '4|5|6|8|9|12|14|15|16|19'))
carp_pp <- graph.edgelist(as.matrix(carp_pp[, 7:8]))
