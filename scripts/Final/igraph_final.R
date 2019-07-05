### Network Setup
library(igraph)
library(tidyverse)
library(parallel)
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
  mclapply(graphs, get.adjacency, mc.cores = 10) %>% # dcgClass matrix
  lapply(as.matrix) # traditional matrix form


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
GenInd <- mclapply(mats, NetIndices::GenInd, mc.cores = 10) # network stats
TrophInd <- mclapply(mats, NetIndices::TrophInd, mc.cores = 10) # trophic levels of nodes

csm_troph <- TrophInd[[1]]
csm_troph$species_id_stage_id <- as.numeric(rownames(csm_troph))

epb_troph <- TrophInd[[2]]
epb_troph$species_id_stage_id <- as.numeric(rownames(epb_troph))

bsq_troph <- TrophInd[[3]]
bsq_troph$species_id_stage_id <- as.numeric(rownames(bsq_troph))


# Join levels with imputed M/N --------------------------------------------
troph_csm <- 
  flux_csm %>% 
  map(left_join, csm_troph) %>% 
  map(cbind, 
      org_type = org_type_csm,
      con_type = con_type_csm) %>% 
  bind_rows()

troph_bsq <- 
  flux_bsq %>% 
  map(left_join, bsq_troph) %>% 
  map(cbind, 
      org_type = org_type_bsq,
      con_type = con_type_bsq) %>%
  bind_rows()

troph_epb <- 
  flux_epb %>% 
  map(left_join, epb_troph) %>% 
  map(cbind, org_type = org_type_epb,
      con_type = con_type_epb) %>%
  bind_rows()

distribution <- 
  bind_rows(troph_bsq, troph_csm, troph_epb) %>% 
  drop_na()

distribution %>% 
  ggplot(aes(x = con_type, y = TL, col = org_type)) +
  geom_boxplot()

distribution %>% 
  group_by(con_type) %>% 
  summarise(
    medianTL = median(TL),
    meanTL = mean(TL),
    maxTL = max(TL),
    minTL = min(TL)
  )

# save --------------------------------------------------------------------

save.image("data/igraph_data.RDS")
