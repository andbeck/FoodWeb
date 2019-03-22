
# Start - Fluxweb ---------------------------------------------------------
library(fluxweb)
library(cheddar)
library(igraph)
library(tidyverse)

# Need 5 things
# 1. A matrix defining the food web interactions - food.web
# 2. Vector with average bodymasses of species - bodymasses
# 3. Vector with total biomass of each population - biomasses
# 4. A vector with organism type - org.type
# 5. Metabolic types vector
# these are all used to calculate further parameters


# # 1. Creating food web matrix for species - using the niche model? --------
# source("scripts/FoodWebFunctions.R")
# 
# food.web <- Niche.model(S = 122, L = 1284, N = 1) # number of links and nodes in Ythan Estuary with concomitant links
# 
# 
# # 2. Generating body mass estimations for the species by trophic level --------
# library(NetIndices)
# 
# # find trophic levels
# tl <- TrophInd(food.web)
# tl # calculated the trophic level of all species (1-122)



# Using YthanEstuary data in R --------------------------------------------
data("YthanEstuary")

# 1. Food web matrix
adj_list <- YthanEstuary$trophic.links
# adj_list <- slice(adj_list, "POM (detritus)")
graph <- graph_from_edgelist(as.matrix(adj_list))
food.web <- as.matrix(as_adjacency_matrix(graph))

# 2. generate body mass from these trophic levels
bodymasses <- as.vector(YthanEstuary$nodes$M)
bodymasses[is.na(bodymasses)] <- 1000 # give NA for detritus a value?.. what value?

# 3. generate biomass for each pop (N*M)
biomasses <- bodymasses * as.vector(YthanEstuary$nodes$N)
biomasses[is.na(biomasses)] <-1000 

# 4. a vector with organism types
org.type <-  as.vector(YthanEstuary$nodes$category)
unique(org.type)
org.type[org.type == ""] <- "detritus"
org.type[org.type == "vert.ecto"] <- "animal"
org.type[org.type == "vert.endo"] <- "animal"
org.type[org.type == "invertebrate"] <- "animal"
org.type[org.type == "producer"] <- "plant"


# 5. a vector of the metabolic types
met.types = c("Ectothermic vertebrates", "Endothermic vertebrates", "Invertebrates")

# Calculate losses | X = aM^b
# a = 0.71 | b = -0.25
losses = rep(NA, 92)
ecto.vert = met.types == "Ectothermic vertebrates"
endo.vert = met.types == "Endothermic vertebrates"
inv = met.types == "Invertebrates"
losses[ecto.vert] = 18.18 * bodymasses[ecto.vert] ^ (-0.29)
losses[endo.vert] = 19.5 * bodymasses[endo.vert] ^ (-0.29)
losses[inv] = 18.18 * bodymasses[inv] ^ (-0.29)
# losses[c(65, 79, 91, 92)] <-  0 # set basal nodes to 0 losses?

# Calculate efficiencies
efficiencies <-  rep(NA, 92)
efficiencies[org.type == "animal"] <- 0.906
efficiencies[org.type == "plant"] <- 0.545
efficiencies[org.type == "detritus"] <- 0.158

# Calculate fluxes - creates a matrix with flux exchanges for each unique interaction
mat.fluxes <- fluxing(food.web, biomasses, losses, efficiencies)

# Basal Species
basal = colSums(mat.fluxes) == 0
sum(basal) # 4 basal species

# Plants
plantspp #65, 79, 91, 92
plants = basal
plants[92] <-  FALSE

# Herbivory
herbivory = sum(rowSums(mat.fluxes[plants,]))
# Carnivory 
carnivory = sum(rowSums(mat.fluxes[!basal,]))
# Detritivory
detritivory = sum(mat.fluxes[92,])
# Total
total = sum(herbivory, carnivory, detritivory)

fluxes <- c(herbivory, carnivory, detritivory, total)
feedingtype <- c("herbivory", "carnivory", "detritivory", "total")
flux <- data.frame(fluxes, feedingtype)

ggplot(flux, aes(x = feedingtype, y = fluxes, fill = feedingtype)) +
  geom_bar(stat = "identity")

     