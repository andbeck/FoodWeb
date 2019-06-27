library(tidyverse)
library(igraph)
library(fluxweb)
library(mice)
library(janitor)

# load --------------------------------------------------------------------

nodes_bsq <- read.csv("./data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv") %>% 
  clean_names()
links_bsq <- read.csv("./data/interactionwebdb/Carpinteria/BSQweb_Links.csv") %>% 
  clean_names()

nodes_csm <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_nodes.csv") %>% 
  clean_names()
links_csm <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_links.csv") %>% 
  clean_names()

nodes_epb <- read.csv("./data/interactionwebdb/Carpinteria/EPBweb_Nodes.csv") %>% 
  clean_names()
links_epb <- read.csv("./data/interactionwebdb/Carpinteria/EPBweb_Links.csv") %>% 
  clean_names()

# list --------------------------------------------------------------------

nodes <- 
  list(nodes_bsq, nodes_csm, nodes_epb)

links <- 
  list(links_bsq, links_csm, links_epb)

# clean -------------------------------------------------------------------

foo <-
  nodes %>%
  bind_rows() %>% 
  group_by(system) %>% 
  select(M = body_size_g,
         N = abundance_no_ha,
         B = biomass_kg_ha,
         phylum,
         class,
         order,
         organismal_group,
         consumer_strategy_stage,
         species_id_stage_id,
         node_id) %>% 
  mutate(M = M / 1000)

# impute prep -------------------------------------------------------------

impute_me <- 
  foo %>% 
  mutate(logM = log(M),
         logN = case_when(N > 0 ~ log(N)),
         logB = log(B)) %>% 
  select(logM, logN, logB) %>% 
  group_split() %>% 
  lapply(select, -system)


# predictor matrix  -------------------------------------------------------

meth <- make.method(impute_me[[1]])

pred <- make.predictorMatrix(impute_me[[1]])

meth["logM"] <- "pmm"
meth["logN"] <- "pmm"
meth["logB"] <- "~I(logM + logN)" 

pred[c("logM", "logN"), "logB"] <-  0


# imputation --------------------------------------------------------------

imputed_bsq <-
  impute_me[[1]] %>% 
  mice(m = 50, maxit = 100, printFlag = FALSE)

imputed_csm <-
  impute_me[[2]] %>% 
  mice(m = 50, maxit = 100, printFlag = FALSE)

imputed_epb <-
  impute_me[[3]] %>% 
  mice(m = 50, maxit = 100, printFlag = FALSE)

# completion --------------------------------------------------------------

complete_bsq <- 
  complete(imputed_bsq, "long") %>% 
  group_split(.imp)

complete_csm <- 
  complete(imputed_csm, "long") %>% 
  group_split(.imp)

complete_epb <- 
  complete(imputed_epb, "long") %>% 
  group_split(.imp)

# easy select -------------------------------------------------------------

df_nameswant <- c("species_id_stage_id",
                  "M",
                  "N",
                  "node_type",
                  "working_name",
                  "organismal_group",
                  "consumer_strategy_stage")

# final clean -------------------------------------------------------------

flux_bsq <-
  complete_bsq %>% 
  map(cbind, nodes_bsq) %>% 
  map(mutate,
      # name the mutate variables
      M = exp(logM), 
      N = exp(logN)) %>%
  map(select, df_nameswant)

flux_csm <-
  complete_csm %>% 
  map(cbind, nodes_csm) %>% 
  map(mutate,
      # name the mutate variables
      M = exp(logM), 
      N = exp(logN)) %>%
  map(select, df_nameswant)

flux_epb <-
  complete_epb %>% 
  map(cbind, nodes_epb) %>% 
  map(mutate,
      # name the mutate variables
      M = exp(logM), 
      N = exp(logN)) %>%
  map(select, df_nameswant)

# org types ---------------------------------------------------------------

org_type_csm <- 
  flux_csm[[1]] %>% 
  pull(organismal_group) %>% 
  as.factor() %>%
  fct_recode(
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
    animal = "myxozoan",
    animal = "monogenean",
    animal = "cestode",
    animal = "nematode",
    animal = "acanthocephalan",
    # animal = "anthozoan",
    # animal = "holothurian",
    # animal = "phoronid",
    # animal = "turbellarian",
    # viruslabel as detritus for now,
    detritus = "virus",
    para = "trematode"
  )

org_type_bsq <- 
  flux_bsq[[1]] %>% 
  pull(organismal_group) %>% 
  as.factor() %>%
  fct_recode(
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
    animal = "anthozoan",
    animal = "amphipod",
    animal = "copepod",
    animal = "dipteran",
    animal = "isopod",
    animal = "ostracod",
    animal = "cumacean",
    animal = "water boatman",
    animal = "burrowing shrimp",
    animal = "crab",
    animal = "fish",
    animal = "elasmobranch",
    animal = "bird",
    animal = "leptostracan",
    animal = "ophiuroid",
    animal = "monogenean",
    animal = "cestode",
    animal = "nematode",
    animal = "acanthocephalan",
    animal = "tanaidacean",
    animal = "beetle",
    # animal = "phoronid",
    # animal = "turbellarian",
    # viruslabel as detritus for now,
    # detritus = "virus",
    para = "trematode"
  )

org_type_epb <- 
  flux_epb[[1]] %>% 
  pull(organismal_group) %>% 
  as.factor() %>%
  fct_recode(
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
    # animal = "spider",
    animal = "water boatman",
    animal = "burrowing shrimp",
    animal = "crab",
    animal = "fish",
    animal = "elasmobranch",
    animal = "bird",
    # animal = "mammal",
    # animal = "myxozoan",
    animal = "monogenean",
    animal = "cestode",
    animal = "nematode",
    animal = "acanthocephalan",
    animal = "anthozoan",
    animal = "holothurian",
    animal = "phoronid",
    animal = "turbellarian",
    # viruslabel as detritus for now,
    detritus = "virus",
    para = "trematode"
  )

# matrices ----------------------------------------------------------------

mat_bsq <- 
  links_bsq %>% 
  mutate(resource_id = as.character(resource_species_id_stage_id),
         consumer_id = as.character(consumer_species_id_stage_id)) %>% 
  select(resource_id, consumer_id) %>% 
  graph_from_data_frame(directed = T, vertices = flux_bsq[[1]]$species_id_stage_id) %>% 
  as_adj() %>% 
  as.matrix()

mat_csm <- 
  links_csm %>% 
  mutate(resource_id = as.character(resource_species_id_stage_id),
         consumer_id = as.character(consumer_species_id_stage_id)) %>% 
  select(resource_id, consumer_id) %>% 
  graph_from_data_frame(directed = T, vertices = flux_csm[[1]]$species_id_stage_id) %>% 
  as_adj() %>% 
  as.matrix()

mat_epb <- 
  links_epb %>% 
  mutate(resource_id = as.character(resource_species_id_stage_id),
         consumer_id = as.character(consumer_species_id_stage_id)) %>% 
  select(resource_id, consumer_id) %>% 
  graph_from_data_frame(directed = T, vertices = flux_epb[[1]]$species_id_stage_id) %>% 
  as_adj() %>% 
  as.matrix()

# vectors -----------------------------------------------------------------

M_bsq <- 
  flux_bsq %>% 
  lapply(pull, M)

N_bsq <- 
  flux_bsq %>% 
  lapply(pull, N)

B_bsq <- 
  flux_bsq %>% 
  map(mutate,
      B = M * N) %>% 
  map(pull, B)

M_csm <- 
  flux_csm %>% 
  lapply(pull, M)

N_csm <- 
  flux_csm %>% 
  lapply(pull, N)

B_csm <- 
  flux_csm %>% 
  map(mutate,
      B = M * N) %>% 
  map(pull, B)

M_epb <- 
  flux_epb %>% 
  lapply(pull, M)

N_epb <- 
  flux_epb %>% 
  lapply(pull, N)

B_epb <- 
  flux_epb %>% 
  map(mutate,
      B = M * N) %>% 
  map(pull, B)

# met types ---------------------------------------------------------------

met_types <-  c("ecto_vert", "endo_vert", "invert")

# losses ------------------------------------------------------------------

losses_bsq <- 
  lapply(M_bsq, function(x){
    losses = rep(NA, length(x))
    ecto.vert = met_types == "ecto_vert"
    endo.vert = met_types == "endo_vert"
    inv = met_types == "invert"
    losses[ecto.vert] = 18.18 * x[ecto.vert] ^ (-0.29)
    losses[endo.vert] = 19.5 * x[endo.vert] ^ (-0.29)
    losses[inv] = 18.18 * x[inv] ^ (-0.29)
    losses
  })

losses_csm <- 
  lapply(M_csm, function(x){
    losses = rep(NA, length(x))
    ecto.vert = met_types == "ecto_vert"
    endo.vert = met_types == "endo_vert"
    inv = met_types == "invert"
    losses[ecto.vert] = 18.18 * x[ecto.vert] ^ (-0.29)
    losses[endo.vert] = 19.5 * x[endo.vert] ^ (-0.29)
    losses[inv] = 18.18 * x[inv] ^ (-0.29)
    losses
  })

losses_epb <- 
  lapply(M_epb, function(x){
    losses = rep(NA, length(x))
    ecto.vert = met_types == "ecto_vert"
    endo.vert = met_types == "endo_vert"
    inv = met_types == "invert"
    losses[ecto.vert] = 18.18 * x[ecto.vert] ^ (-0.29)
    losses[endo.vert] = 19.5 * x[endo.vert] ^ (-0.29)
    losses[inv] = 18.18 * x[inv] ^ (-0.29)
    losses
  })


# efficiencies -----------------------------------------------------------

efficiencies_bsq <- 
  lapply(M_bsq, function(x){
    efficiencies_bsq = rep(NA, length(x[[1]]))
    efficiencies_bsq[org_type_bsq == "animal"] = 0.906
    efficiencies_bsq[org_type_bsq == "para"] = 0.906
    efficiencies_bsq[org_type_bsq == "plant"] = 0.545 
    efficiencies_bsq[org_type_bsq == "detritus"] = 0.158
    efficiencies_bsq
  })

efficiencies_csm <- 
  lapply(M_csm, function(x){
    efficiencies_csm = rep(NA, length(x[[1]]))
    efficiencies_csm[org_type_csm == "animal"] = 0.906
    efficiencies_csm[org_type_csm == "para"] = 0.906
    efficiencies_csm[org_type_csm == "plant"] = 0.545 
    efficiencies_csm[org_type_csm == "detritus"] = 0.158
    efficiencies_csm
  })

efficiencies_epb <- 
  lapply(M_epb, function(x){
    efficiencies_epb = rep(NA, length(x[[1]]))
    efficiencies_epb[org_type_epb == "animal"] = 0.906
    efficiencies_epb[org_type_epb == "para"] = 0.906
    efficiencies_epb[org_type_epb == "plant"] = 0.545 
    efficiencies_epb[org_type_epb == "detritus"] = 0.158
    efficiencies_epb
  })


# fluxweb -----------------------------------------------------------------

bsq_fluxes <- 
  lapply(1:length(N_bsq), function(x) {
    fluxing(
      mat = mat_bsq,
      biomasses = B_bsq[[x]],
      losses = losses_bsq[[x]],
      efficiencies = efficiencies_bsq[[x]]
    )
  })

csm_fluxes <- 
  lapply(1:length(N_csm), function(x) {
    fluxing(
      mat = mat_csm,
      biomasses = B_csm[[x]],
      losses = losses_csm[[x]],
      efficiencies = efficiencies_csm[[x]]
    )
  })

epb_fluxes <- 
  lapply(1:length(N_epb), function(x) {
    fluxing(
      mat = mat_epb,
      biomasses = B_epb[[x]],
      losses = losses_epb[[x]],
      efficiencies = efficiencies_epb[[x]]
    )
  })

#### END ####



