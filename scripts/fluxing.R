### Impute Workflow ###

source("scripts/impute2.R")
library(janitor)

# combine imputed data with original data frame --
csm_complete <- 
  bind_cols(csm, csm_imp) %>% 
  remove_empty("cols") %>% 
 # filter(StageID == 1) %>% 
  select(NodeID:ConsumerStrategy.stage.,
         BodySize:Abundance) %>% # beware - the imputation process is wrong
  mutate(BodySize = exp(BodySize - 1), Biomass = exp(Biomass - 1), Abundance = exp(Abundance - 1))

bsq_complete <- 
  bind_cols(bsq, bsq_imp) %>% 
  remove_empty("cols") %>% 
  select(NodeID:ConsumerStrategy.stage.,
         BodySize:Abundance) %>% # beware - the imputation process is wrong
  mutate(BodySize = exp(BodySize - 1), Biomass = exp(Biomass - 1), Abundance = exp(Abundance - 1))

epb_complete <- 
  bind_cols(epb, epb_imp) %>% 
  remove_empty("cols") %>% 
  select(NodeID:ConsumerStrategy.stage.,
         BodySize:Abundance) %>% # beware - the imputation process is wrong
  mutate(BodySize = exp(BodySize - 1), Biomass = exp(Biomass - 1), Abundance = exp(Abundance - 1))

# links --
csm_links <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Links.csv")
bsq_links <- read.csv("data/interactionwebdb/Carpinteria/BSQweb_Links.csv")
epb_links <- read.csv("data/interactionwebdb/Carpinteria/EPBweb_Links.csv")

# library --
library(igraph) 

# 1. matrix from edgelist - need to use species.stage.ID instead but this will do for now
csm_edge1 <- 
  csm_links %>% 
  select(ResourceSpeciesID, ConsumerSpeciesID) %>% 
  distinct()

csm_edge <- 
  graph_from_edgelist(as.matrix(csm_edge1))

csmedge <- delete.vertices(simplify(csm_edge), degree(csm_edge)==0)

mat <- as.matrix(as_adjacency_matrix(csm_edge))

# 2. bodymasses
bodymasses <- pull(csm_complete, BodySize) # pull creates vectors

# 3. biomasses
biomasses <- bodymasses * pull(csm_complete, Abundance)

# 4. a vector with organism types
org.type <-  pull(csm_complete, OrganismalGroup)
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
           animal = "trematode",
           animal = "cestode",
           animal = "nematode",
           animal = "acanthocephalan",
           animal = "anthozoan",
           animal = "holothurian",
           animal = "phoronid",
           animal = "turbellarian",
           # virus? - should probabaly remove this just label as detritus for now,
           detritus = "virus")

# 5. a vector of the metabolic types
met.types = c("Ectothermic vertebrates", "Endothermic vertebrates", "Invertebrates")

# Calculate losses with X = aM^b
# a = 0.71 | b = -0.25
losses = rep(NA, length(bodymasses))
ecto.vert = met.types == "Ectothermic vertebrates"
endo.vert = met.types == "Endothermic vertebrates"
inv = met.types == "Invertebrates"
losses[ecto.vert] = 18.18 * bodymasses[ecto.vert] ^ (-0.29)
losses[endo.vert] = 19.5 * bodymasses[endo.vert] ^ (-0.29)
losses[inv] = 18.18 * bodymasses[inv] ^ (-0.29)

# Calculate efficiencies
efficiencies <-  rep(NA, length(bodymasses))
efficiencies[org.type == "animal"] <- 0.906
efficiencies[org.type == "plant"] <- 0.545
efficiencies[org.type == "detritus"] <- 0.158

library(fluxweb)

# unsure why matrix is a different size... 
# maybe have to make matrix from scratch... 
# rather than edgelist in igraph...

dim(mat) # 314 for some reason...
mat.fluxes <- fluxing(mat, biomasses, losses, efficiencies)


matrixplot(mat)
dim(mat)

plot(csm_edge)

