# Filter LinkType - Then grab nodes from that file

library(tidyverse)
library(janitor)

# filter links
links_csm <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Links.csv")

predpara_links <- 
  links_csm %>% 
  remove_empty("cols") %>% 
#  filter(LinkType == "predation" | # get only pred and ttp links
#         LinkType == "trophically transmitted parasite") %>% 
  clean_names() # for my sanity...

# filter nodes by new links
nodes_csm <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")

filterthese <- pull(predpara_links, consumer_species_id) # vectors of consumer ID's we need
filterthese2 <- pull(predpara_links, resource_species_id) # vectors of resource species ID's we need

predpara_nodes <- 
  nodes_csm %>% 
  remove_empty("cols") %>% # cleanup
  filter(NodeID %in% c(filterthese, filterthese2)) %>% # filter if present in these two vectors
  filter(Kingdom == "Animalia" | Kingdom == "Plantae") %>% # just in case any slip through
  clean_names() # for my sanity...

# cheddar
library(cheddar)
library(missForest)

# simple and rough imputation - replace this with the mice one soon
nodes_imputation <- 
  predpara_nodes %>% 
  select(M = body_size_g,
         N = abundance_no_ha,
         B = biomass_kg_ha) %>% 
  mutate(M = M / 1000) %>% 
  missForest()

nodes_imputation <- nodes_imputation$ximp

# nodes for cheddar
nodes_cheddar <- bind_cols(predpara_nodes, nodes_imputation)
nodes_cheddar <- 
  nodes_cheddar %>% 
  mutate(working_name = make.unique(as.character(working_name))) %>% 
  select(node_id,
         working_name,
         N,
         M,
         B)

# links for cheddar
trophic_links <- 
  predpara_links %>% 
  select(consumer_species_id, resource_species_id)

consumer_temp <- 
  trophic_links %>% 
  select(node_id = consumer_species_id)

resource_temp <- 
  trophic_links %>% 
  select(node_id = resource_species_id)

consumer_names <- inner_join(nodes_cheddar, consumer_temp)

resource_names <- inner_join(nodes_cheddar, resource_temp)




