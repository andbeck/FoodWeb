# Start -------------------------------------------------------------------
rm(list=ls())

library(tidyverse)
library(ggforce)
library(igraph)
library(fluxweb)
library(cheddar)
library(ggfortify)
library(missForest)

# Load in the edges and nodes ------------------------------------------------------

nodes_CSM <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_nodes.csv")
links_CSM <- read.csv("./data/interactionwebdb/Carpinteria/CSMweb_links.csv")

# rough imputation ----

nodes_imputation <- 
  nodes_CSM %>% 
  select(M = BodySize.g.,
         N = Abundance.no..ha.,
         Biomass = Biomass.kg.ha.,
         Phylum,
         Class,
         Order,
         OrganismalGroup) %>% 
  mutate(M = M / 1000) %>% 
  missForest()

nodes_imputation <- nodes_imputation$ximp

nodes_CSM <- 
  bind_cols(nodes_CSM,
            nodes_imputation)

nodes_wrk <-
  nodes_CSM %>%
  mutate(SpeciesID.StageID = as.character(SpeciesID.StageID)) %>% 
  select(NodeID,
         StageID = SpeciesID.StageID,
         WorkingName,
         N,
         M,
         ConsumerType = ConsumerStrategy.stage.,
         OrganismalGroup) %>%
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))

# limit low values to 0.01
nodes_wrk$N[nodes_wrk$N == 0] <- 0.01

# rename species stage Id to not include decimals ---- Works when creating edgelist
as.factor(nodes_CSM$SpeciesID.StageID) # as factor may work?
as.character(nodes_CSM$SpeciesID.StageID) # or as character...

# 1. links for igraph and matrix ----

links_wrk <-
  links_CSM %>% 
  mutate(ResStageID = as.character(ResourceSpeciesID.StageID),
         ConStageID = as.character(ConsumerSpeciesID.StageID)) %>% 
  select(ResStageID, ConStageID)

graph_csm <- graph_from_data_frame(links_wrk, directed = T, vertices = nodes_wrk$StageID )

mat <- as_adj(graph_csm) %>% 
  as.matrix()

# 2. bodymasses
bodymasses <- pull(nodes_wrk, M) # pull creates vectors

# 3. biomasses
biomasses <- bodymasses * pull(nodes_wrk, N)

# 4. a vector with organism types
org.type <-  pull(nodes_wrk, OrganismalGroup)
unique(org.type)

# recode factor levels
org.type <- 
  fct_recode(org.type,
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
             animal = "myxozoan",
             animal = "monogenean",
             para = "trematode",
             animal = "cestode",
             animal = "nematode",
             animal = "acanthocephalan",
             # animal = "anthozoan",
             # animal = "holothurian",
             # animal = "phoronid",
             # animal = "turbellarian",
             # virus? - should probabaly remove this just label as detritus for now,
             detritus = "virus")

# 5. a vector of the metabolic types
met.types = c("Ectothermic vertebrates", "Endothermic vertebrates", "Invertebrates")

# Calculate losses with X = aM^b
# Values and sources listed in fluxweb package

# a = 0.71 | b = -0.25
losses = rep(NA, length(bodymasses))
ecto.vert = met.types == "Ectothermic vertebrates"
endo.vert = met.types == "Endothermic vertebrates"
inv = met.types == "Invertebrates"
losses[ecto.vert] = 18.18 * bodymasses[ecto.vert] ^ (-0.29)
losses[endo.vert] = 19.5 * bodymasses[endo.vert] ^ (-0.29)
losses[inv] = 18.18 * bodymasses[inv] ^ (-0.29)

# Calculate efficiencies - values and sources listed in fluxweb package
efficiencies <-  rep(NA, length(bodymasses))
efficiencies[org.type == "animal"] <- 0.906
efficiencies[org.type == "plant"] <- 0.545
efficiencies[org.type == "detritus"] <- 0.158
efficiencies[org.type == "para"] <- 0.906

# Fluxweb ----
library(fluxweb)
mat.fluxes <- fluxing(mat, biomasses, losses, efficiencies)

# Basal Species
basal = colSums(mat.fluxes) == 0

# Plants
plants = basal

# Herbivory
herbivory = sum(rowSums(mat.fluxes[plants,]))

# Carnivory 
carnivory = sum(rowSums(mat.fluxes[!basal,]))

# Detritivory
detritivory = sum(rowSums(mat.fluxes[(mat.fluxes) == "21.1",]))
#need a way to extract all fluxes that are detritus labelled SpeciesID's

# -- Wrong shouldnt have to list all of them can we match to list?
# mapping may work here? need to get parasites out of matrix depending upon name
# parasitism = sum(mat.fluxes[names == list of parasite StageIDs])
# do this as a tibble or DF instead? Probably best

# Total
total = sum(mat.fluxes)

fluxes <- c(herbivory, carnivory, detritivory, total)
feedingtype <- c("herbivory", "carnivory", "detritivory", "total")
flux <- data.frame(fluxes, feedingtype)

ggplot(flux, aes(x = feedingtype, y = log(fluxes), fill = feedingtype)) +
  geom_bar(stat = "identity") +
  ggtitle("Fluxes through the CSM Subweb")

