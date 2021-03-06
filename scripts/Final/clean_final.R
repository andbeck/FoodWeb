### Clean Data
library(tidyverse)
library(janitor)


# -----------------------------------------------------------------------


### Load Nodes
csm_nodes <- 
  read_csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")
epb_nodes <- 
  read_csv("data/interactionwebdb/Carpinteria/EPBweb_Nodes.csv") %>% 
  clean_names %>% 
  remove_empty("cols")
bsq_nodes <- 
  read_csv("data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")

### Clean Nodes
c_nodes_wrk <- 
  csm_nodes %>% 
  # rename values and sort out SI units
  mutate(body_size_kg = body_size_g / 1000,
         working_name = make.unique(working_name, sep = "_")) %>% 
  # select the useful columns
  select(system,
         node_id,
         species_id,
         species_id_stage_id,
         working_name,
         organismal_group,
         node_type,
         lifestyle_stage,
         consumer_strategy_stage,
         body_size_kg,
         abundance_no_ha,
         biomass_kg_ha) %>%
  # change characters to lower
  mutate_if(is.character, str_to_lower)

e_nodes_wrk <- 
  epb_nodes %>% 
  # rename values and sort out SI units
  mutate(body_size_kg = body_size_g / 1000,
         working_name = make.unique(working_name, sep = "_")) %>% 
  # select the useful columns
  select(system,
         node_id,
         species_id,
         species_id_stage_id,
         working_name,
         organismal_group,
         node_type,
         lifestyle_stage,
         consumer_strategy_stage,
         body_size_kg,
         abundance_no_ha,
         biomass_kg_ha) %>% 
  # change characters to lower
  mutate_if(is.character, str_to_lower)

b_nodes_wrk <- 
  bsq_nodes %>% 
  # rename values and sort out SI units
  mutate(body_size_kg = body_size_g / 1000,
         working_name = make.unique(working_name, sep = "_")) %>% 
  # select the useful columns
  select(system,
         node_id,
         species_id,
         species_id_stage_id,
         working_name,
         organismal_group,
         node_type,
         lifestyle_stage,
         consumer_strategy_stage,
         body_size_kg,
         abundance_no_ha,
         biomass_kg_ha) %>% 
  # change characters to lower
  mutate_if(is.character, str_to_lower)


# ---------------------------------------------------------------------


### Fill N * M = B values
c_nodes_fill <-
  c_nodes_wrk %>% 
  # mutate the values
  mutate(
    # M = B / N
    body_size_new = case_when(!is.na(abundance_no_ha & biomass_kg_ha) ~ biomass_kg_ha / abundance_no_ha),
    # N = B / M 
    abundance_no_ha_new = case_when(!is.na(body_size_kg & biomass_kg_ha) ~ biomass_kg_ha / body_size_kg),
    # B = N * M
    biomass_kg_ha_new = case_when(!is.na(body_size_kg & abundance_no_ha) ~ abundance_no_ha * body_size_kg)) %>% 
  # select the original values first then the calculated values in one column
  mutate(
    body_size_wrk = coalesce(body_size_kg, body_size_new),
    abundance_no_ha_wrk = coalesce(abundance_no_ha, abundance_no_ha_new),
    biomass_kg_ha_wrk = coalesce(biomass_kg_ha, biomass_kg_ha_new))

e_nodes_fill <-
  e_nodes_wrk %>% 
  # mutate the values
  mutate(
    # M = B / N
    body_size_new = case_when(!is.na(abundance_no_ha & biomass_kg_ha) ~ biomass_kg_ha / abundance_no_ha),
    # N = B / M 
    abundance_no_ha_new = case_when(!is.na(body_size_kg & biomass_kg_ha) ~ biomass_kg_ha / body_size_kg),
    # B = N * M
    biomass_kg_ha_new = case_when(!is.na(body_size_kg & abundance_no_ha) ~ abundance_no_ha * body_size_kg)) %>% 
  # select the original values first then the calculated values in one column
  mutate(
    body_size_wrk = coalesce(body_size_kg, body_size_new),
    abundance_no_ha_wrk = coalesce(abundance_no_ha, abundance_no_ha_new),
    biomass_kg_ha_wrk = coalesce(biomass_kg_ha, biomass_kg_ha_new))

b_nodes_fill <-
  b_nodes_wrk %>% 
  # mutate the values
  mutate(
    # M = B / N
    body_size_new = case_when(!is.na(abundance_no_ha & biomass_kg_ha) ~ biomass_kg_ha / abundance_no_ha),
    # N = B / M 
    abundance_no_ha_new = case_when(!is.na(body_size_kg & biomass_kg_ha) ~ biomass_kg_ha / body_size_kg),
    # B = N * M
    biomass_kg_ha_new = case_when(!is.na(body_size_kg & abundance_no_ha) ~ abundance_no_ha * body_size_kg)) %>% 
  # select the original values first then the calculated values in one column
  mutate(
    body_size_wrk = coalesce(body_size_kg, body_size_new),
    abundance_no_ha_wrk = coalesce(abundance_no_ha, abundance_no_ha_new),
    biomass_kg_ha_wrk = coalesce(biomass_kg_ha, biomass_kg_ha_new))


# -----------------------------------------------------------------------

### Clean Links

# load the links
csm_links <- 
  read_csv("data/interactionwebdb/Carpinteria/CSMweb_Links.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")

epb_links <- 
  read_csv("data/interactionwebdb/Carpinteria/EPBweb_Links.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")

bsq_links <- 
  read_csv("data/interactionwebdb/Carpinteria/BSQweb_Links.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")

# - -----------------------------------------------------------------------

library(mice)

# - -----------------------------------------------------------------------

imp_csm <- 
  c_nodes_fill %>% 
  mutate(log_body_size = log(body_size_wrk),
         log_biomass = log(biomass_kg_ha_wrk),
         log_abundance = case_when(abundance_no_ha_wrk > 0 ~ log(abundance_no_ha_wrk))) %>%  
  select(log_body_size, log_abundance, log_biomass)

imp_epb <- 
  e_nodes_fill %>% 
  mutate(log_body_size = log(body_size_wrk),
         log_biomass = log(biomass_kg_ha_wrk),
         log_abundance = case_when(abundance_no_ha_wrk > 0 ~ log(abundance_no_ha_wrk))) %>% 
  select(log_body_size, log_abundance, log_biomass)

imp_bsq <- 
  b_nodes_fill %>% 
  mutate(log_body_size = log(body_size_wrk),
         log_biomass = log(biomass_kg_ha_wrk),
         log_abundance = case_when(abundance_no_ha_wrk > 0 ~ log(abundance_no_ha_wrk))) %>%  
  select(log_body_size, log_abundance, log_biomass)

# mice setup --------------------------------------------------------------

meth <- make.method(imp_csm)

pred <- make.predictorMatrix(imp_csm)

meth["log_body_size"] <- "pmm"
meth["log_abundance"] <- "pmm"
meth["log_biomass"] <- "~I(log_body_size + log_abundance)" # addition of logs is the same as multiplying exponent of log
pred[c("log_body_size", "log_abundance"), "log_biomass"] <-  0

# imputation --------------------------------------------------------------

c_imputed <- 
  mice(imp_csm, meth = meth, predictorMatrix = pred, m = 50, maxit = 100, printFlag = F)

e_imputed <-
  mice(imp_epb, meth = meth, predictorMatrix = pred, m = 50, maxit = 100, printFlag = F)

b_imputed <-
  mice(imp_bsq, meth = meth, predictorMatrix = pred, m = 50, maxit = 100, printFlag = F)


# pool --------------------------------------------------------------------

c_fit <- with(data = c_imputed, expr = lm(log_body_size ~ log_abundance))
c_sum <- as.data.frame(summary(pool(c_fit)))

e_fit <- with(data = e_imputed, expr = lm(log_body_size ~ log_abundance))
e_sum <- as.data.frame(summary(pool(e_fit)))

b_fit <- with(data = b_imputed, expr = lm(log_body_size ~ log_abundance))
b_sum <- as.data.frame(summary(pool(b_fit)))

# complete ----------------------------------------------------------------

c_complete <- mice::complete(c_imputed, 'long')
c_list <- split(c_complete, f = c_complete$.imp)

e_complete <- mice::complete(e_imputed, 'long')
e_list <- split(e_complete, f = e_complete$.imp)

b_complete <- mice::complete(b_imputed, 'long')
b_list <- split(b_complete, f = b_complete$.imp)

# colnames wanted ---------------------------------------------------------

df_nameswant <- c("species_id_stage_id",
                  "body_size",
                  "abundance",
                  "node_type",
                  "working_name",
                  "organismal_group",
                  "consumer_strategy_stage")



# map() ---------------------------------------------------------------

### csm

c_list_bound <- map(c_list, cbind, c_nodes_fill)

c_list_mutate <- map(c_list_bound, mutate,
                     # name the mutate variables
                     body_size = exp(log_body_size), 
                     abundance = exp(log_abundance))

c_list_select <- map(c_list_mutate, select, df_nameswant)

### epb

e_list_bound <- map(e_list, cbind, e_nodes_fill)

e_list_mutate <- map(e_list_bound, mutate,
                     # name the mutate variables
                     body_size = exp(log_body_size), 
                     abundance = exp(log_abundance))

e_list_select <- map(e_list_mutate, select, df_nameswant)

### bsq

b_list_bound <- map(b_list, cbind, b_nodes_fill)

b_list_mutate <- map(b_list_bound, mutate,
                     # name the mutate variables
                     body_size = exp(log_body_size), 
                     abundance = exp(log_abundance))

b_list_select <- map(b_list_mutate, select, df_nameswant)

### Network Setup
library(igraph)
library(tidyverse)

# library(NetIndices) # masks select

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

### Fluxing

library(fluxweb)


# Fluxweb Prep ------------------------------------------------------------

# matrix
mat_csm <- mats[[1]]

# bodymasses - list
bodymasses_csm <- 
  c_list_select %>% 
  lapply(pull, body_size)

# abundance - list
abundance_csm <- 
  c_list_select %>% 
  lapply(pull, abundance)

# biomass - list
biomasses_csm <- # multiply to get biomass
  map2(bodymasses_csm, abundance_csm, ~ .x * .y) 


# Organism Type -----------------------------------------------------------

# vector with org types for efficiencies calculating
org_type_csm <- 
  c_list_select %>% 
  lapply(pull, organismal_group) %>% 
  lapply(as.factor)

# recode factor 
org_type_csm <- 
  org_type_csm %>% 
  lapply(fct_recode, 
         # recode the factors of list
         # plants
         plant = "vascular plant",
         plant = "microphytobenthos",
         plant = "macroalgae",
         # animals
         animal = "protist",
         animal = "annelid",
         animal = "leech",
         animal = "nemertean",
         animal = "bivalve",
         animal = "snail",
         animal = "mosquito",
         animal = "branchiuran",
         animal = "amphipod",
         animal = "copepod",
         animal = "dipteran",
         animal = "isopod",
         animal = "ostracod",
         animal = "spider",
         animal = "water boatman",
         animal = "burrowing shrimp",
         animal = "crab",
         animal = "fish",
         animal = "elasmobranch",
         animal = "bird",
         animal = "mammal",
         animal = "trematode",
         animal = "cestode",
         animal = "nematode",
         animal = "acanthocephalan")

# Feeding Type ------------------------------------------------------------

# vector
feed_type_csm <- 
  c_list_select %>% 
  lapply(pull, consumer_strategy_stage) %>% 
  lapply(as.factor)

# recode factor 
feed_type_csm <- 
  feed_type_csm %>% 
  lapply(fct_recode, 
         # recode the factors of list
         # basal 
         plant = "autotroph",
         detritus = "detritus",
         # detritivore
         detritivore = "detritivore",
         # predators
         predator = "micropredator",
         predator = "predator",
         # parasite
         para = "macroparasite",
         para = "parasitic castrator",
         para = "trophically transmitted parasite"
  )


# Metabolic Type ----------------------------------------------------------

met_types <-  c("ectothermic_vert", "endothermic_vert", "invert")

# losses
losses_csm <- 
  lapply(bodymasses_csm, function(bodymasses){
    losses = rep(NA, length(bodymasses))
    ecto.vert = met_types == "ectothermic_vert"
    endo.vert = met_types == "endothermic_vert"
    inv = met_types == "invert"
    losses[ecto.vert] = 18.18 * bodymasses[ecto.vert] ^ (-0.29)
    losses[endo.vert] = 19.5 * bodymasses[endo.vert] ^ (-0.29)
    losses[inv] = 18.18 * bodymasses[inv] ^ (-0.29)
    losses
  })

# calculate efficiencies
efficiencies_csm <- rep(NA, length(bodymasses_csm[[1]]))
efficiencies_csm[org_type_csm[[1]] == "animal"] <- 0.906
efficiencies_csm[org_type_csm[[1]] == "plant"] <- 0.545 
efficiencies_csm[org_type_csm[[1]] == "detritus"] <- 0.158

# metabolic rates
met_rates <- 
  bodymasses_csm %>% 
  lapply(function(x) 0.71* x ^-0.25)

# fluxing - lapply
mat.fluxes <- 
  fluxing(mats[[1]],
          biomasses = biomasses_csm[[1]],
          losses = losses_csm[[1]],
          efficiencies =  efficiencies_csm)






