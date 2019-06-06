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
         OrganismalGroup,
         ConsumerType = ConsumerStrategy.stage.) %>% 
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
         OrganismalGroup,
         Phylum) %>%
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))

# vis
nodes_wrk$Organismalgroup <- fct_recode(nodes_wrk$OrganismalGroup,
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

nodes_wrk %>% 
  ggplot(aes(x = log(M), y = log(N), group = Organismalgroup, col = Organismalgroup)) +
  geom_smooth(method = lm, se = FALSE)


# limit low values to lowest other values present
nodes_wrk$N[nodes_wrk$N == 0] <- min(nodes_wrk$N)
nodes_wrk$M[nodes_wrk$M == 0] <- min(nodes_wrk$M)


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

met.rates = 0.71 * bodymasses ^ -0.25
mat.fluxes2 <- fluxing(mat, biomasses, met.rates, efficiencies)

# Basal Species
basal = colSums(mat.fluxes) == 0

# Plants
plants = basal

# Herbivory -- should be true
Herbivory = sum(rowSums(mat.fluxes[plants,]))

# Carnivory -- wrong becase this includes carnivores and detritivores
Carnivory = sum(rowSums(mat.fluxes[org.type == "animal" ,]))

# Detritivory -- need a way to identify detritivores
Detritivory = sum(rowSums(mat.fluxes[org.type == "detritus",]))

# Parasitism
Parasitism = sum(rowSums(mat.fluxes[org.type == "para",]))

# Total
Total = sum(mat.fluxes)

fluxes <- c(Herbivory, Carnivory, Detritivory, Parasitism, Total)
feedingtype <- c("Herbivory", "Carnivory", "Detritivory", "Parasitsm", "Total")
flux <- data.frame(fluxes, feedingtype)

p1 <- 
  ggplot(flux, aes(x = reorder(feedingtype, fluxes), y = fluxes)) +
  geom_bar(stat = "identity") +
  ggtitle("Fluxes through the CSM Subweb") +
  xlab("Feeding Type") +
  ylab("Energy J.yr^-1") +
  theme(plot.title = element_text(hjust = 0.5))
p1

# Sum using consumer strategy stages instead?
consumer.type <-  pull(nodes_wrk, ConsumerType)
unique(consumer.type)

Autotrophs = sum(rowSums(mat.fluxes[consumer.type == "autotroph",]))
Predators = sum(rowSums(mat.fluxes[consumer.type == "predator" ,]))
MicroPred = sum(rowSums(mat.fluxes[consumer.type == "micropredator",]))
Detritivores = sum(rowSums(mat.fluxes[consumer.type == "detritivore",]))
MacroParasites = sum(rowSums(mat.fluxes[consumer.type == "macroparasite",]))
NonFeeding = sum(rowSums(mat.fluxes[consumer.type == "nonfeeding",]))
ParaCastrators = sum(rowSums(mat.fluxes[consumer.type == "parasitic castrator",]))
TTPara = sum(rowSums(mat.fluxes[consumer.type == "trophically transmitted parasite",]))

# Total
Total = sum(mat.fluxes)

fluxes.group <- c(Autotrophs, Predators, MicroPred, Detritivores, MacroParasites, 
            NonFeeding, ParaCastrators, TTPara, 
            Total)
org.group <- c("Autotrophs", "Predators", "MicroPred", "Detritivores", "MacroParasites", 
                 "NonFeeding", "ParaCastrators", "TTPara",
                 "Total")
flux.group <- data.frame(fluxes.group, org.group)

p2 <- 
  ggplot(flux.group, aes(x = reorder(org.group, fluxes.group), y = log(fluxes.group))) +
  geom_bar(stat = "identity") +
  xlab("Organism Group") +
  ylab(expression(log("Energy J.yr^-1"))) +
  labs(caption = "Values all depend on imputation...") +
  theme(plot.title = element_text(hjust = 0.5))
 p2 # honestly cant be right... predators almost double the flux of herbivores

 
library(gridExtra)
grid.arrange(p1 + theme_classic(),
             p2 + theme_classic()) 

