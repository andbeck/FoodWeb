### Fluxing
library(tidyverse)
library(fluxweb)

load("igraph_data.RDS")


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


