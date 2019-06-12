### Network Setup
library(igraph)
library(tidyverse)

# Load --------------------------------------------------------------------

load("data/workspace_data.RDS")

# Graphs ------------------------------------------------------------------

csm.ig <- 
  c_links_wrk %>%
  mutate(resource = as.character(resource_species_id_stage_id),
         consumer = as.character(consumer_species_id_stage_id)) %>% 
  select(resource, consumer) %>% 
  graph_from_data_frame(directed = T, vertices = NULL)

epb.ig <- 
  e_links_wrk %>%
  mutate(resource = as.character(resource_species_id_stage_id),
         consumer = as.character(consumer_species_id_stage_id)) %>% 
  select(resource, consumer) %>% 
  graph_from_data_frame(directed = T, vertices = NULL)

bsq.ig <- 
  b_links_wrk %>%
  mutate(resource = as.character(resource_species_id_stage_id),
         consumer = as.character(consumer_species_id_stage_id)) %>% 
  select(resource, consumer) %>% 
  graph_from_data_frame(directed = T, vertices = NULL)
