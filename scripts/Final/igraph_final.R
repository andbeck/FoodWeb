### Network Setup
library(igraph)
library(tidyverse)

# library(NetIndices) # masks select

# Load --------------------------------------------------------------------

load("data/flux_data.RDS")

# Graphs ------------------------------------------------------------------

csm.ig <- 
  links_csm %>%
  mutate(resource = as.character(resource_species_id_stage_id),
         consumer = as.character(consumer_species_id_stage_id)) %>% 
  select(resource, consumer) %>% 
  graph_from_data_frame(directed = T, vertices = NULL)

epb.ig <- 
  links_epb %>%
  mutate(resource = as.character(resource_species_id_stage_id),
         consumer = as.character(consumer_species_id_stage_id)) %>% 
  select(resource, consumer) %>%
  graph_from_data_frame(directed = T, vertices = NULL)

bsq.ig <- 
  links_bsq %>%
  mutate(resource = as.character(resource_species_id_stage_id),
         consumer = as.character(consumer_species_id_stage_id)) %>% 
  select(resource, consumer) %>% 
  graph_from_data_frame(directed = T, vertices = NULL)


# List --------------------------------------------------------------------

graphs <- list(csm.ig, epb.ig, bsq.ig)

# Indices -----------------------------------------------------------------

mats <- # matrix form
  lapply(graphs, get.adjacency) %>% # dcgClass matrix
  lapply(as.matrix) # traditional matrix form


GenInd <- lapply(mats, NetIndices::GenInd) # network stats
TrophInd <- lapply(mats, NetIndices::TrophInd) # trophic levels of nodes

# Stats -------------------------------------------------------------------

Stats_matrix <- function(mat){
  S = nrow(mat)
  L = sum(mat)
  
  basal = sum(colSums(mat) == 0)/S
  top   = sum(colSums(t(mat)) == 0)/S
  int   = 1 - basal - top
  gen   = mean(colSums(mat))
  vun   = mean(rowSums(mat))
  gensd = sd(colSums(mat)/(L/S))
  vunsd = sd(rowSums(mat)/(L/S))
  
  return(c(S,L,basal,int,top,gen,gensd,vun,vunsd))
}

simple_stats <- lapply(mats, Stats_matrix)

source("scripts/practice/FoodWebFunctions.R")

detail_stats <- Get.web.stats(mats)

# Trophic Level -----------------------------------------------------------


## off by one - missing a node from troph ind?

csm_troph <- TrophInd[[1]]
epb_troph <- TrophInd[[2]]
bsq_troph <- TrophInd[[3]]

# csm_troph losing a node

TL_csm <- cbind(csm_troph, c_list_select[[1]])
TL_epb <- cbind(epb_troph, e_list_select[[2]])
TL_bsq <- cbind(bsq_troph, b_list_select[[3]])

TL_csm %>% 
  ggplot(aes(x = log(body_size))) +
  geom_histogram(binwidth = 1)


save.image("data/igraph_data.RDS")
