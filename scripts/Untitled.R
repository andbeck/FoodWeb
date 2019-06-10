# Fluxweb but with mice() ----------
rm(list=ls())

### Using MICE to obtain good imputation values for Mass and Abundance of organisms ###
library(tidyverse)
library(mice)
library(fluxweb)
library(igraph)

### Read in data
csm_nodes <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv", as.is = T)

### For filtering in future
filter_virus_proto <- 
  csm_nodes %>% 
  filter(Kingdom == "Virus" | Kingdom == "Protozoa") %>% 
  pull(SpeciesID.StageID)

### Clean - remove protozoan kingdoms and virus
csm_nodes <- 
  csm_nodes %>% 
  filter(Kingdom != "Virus",
         Kingdom != "Protozoa")

### Select variables we want for imputation
work <- 
  csm_nodes %>% 
  # Turn body size into SI unts (Kg)
  mutate(BodySize = BodySize.g. / 1000,
         Abundance = Abundance.no..ha.,
         Biomass = Biomass.kg.ha.) %>% 
  # Fill in missing values using M * N = B
  mutate(BodySizeNew = case_when(!is.na(Abundance & Biomass) ~ Biomass / Abundance),
         AbundanceNew = case_when(!is.na(BodySize & Biomass) ~ Biomass / BodySize),
         BiomassNew = case_when(!is.na(Abundance & BodySize) ~ Abundance * BodySize)) %>% 
  # Take values from calculations
  mutate(BodySize_Work = coalesce(BodySize, BodySizeNew),
         Biomass_Work = coalesce(Biomass, BiomassNew),
         Abundance_Work = coalesce(Abundance, AbundanceNew)) %>% 
  # Select the variables
  select(BodySize = BodySize_Work,
         Abundance = Abundance_Work,
         Biomass = Biomass_Work,
         Kingdom,
         Phylum,
         Class,
         Order,
         Family,
         Genus) %>% 
  # Log the variables you want to impute
  mutate(LogBodySize = log(BodySize),
         LogBiomass = log(Biomass),
         # Only log values above 0 otherwise leads to inf values
         LogAbundance = case_when(Abundance > 0 ~ log(Abundance)))

### Visualise and prep for imputation
work %>% 
  ggplot(aes(x = LogBodySize, y = LogAbundance)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

### Impute data
dat <- 
  work %>% 
  # filter(Abundance != 0) %>% # remove abundance values = 0
  select(LogBodySize, LogAbundance, LogBiomass)

### Setup
ini <- 
  mice(dat, maxit = 0)
meth <- 
  ini$method
pred <- 
  ini$predictorMatrix

### Methods 
meth["LogBodySize"] <- "pmm"
meth["LogAbundance"] <- "pmm"
meth["LogBiomass"] <- "~I(LogBodySize + LogAbundance)" # addition of logs is the same as multiplying exponent of log
pred[c("LogBodySize", "LogAbundance"), "LogBiomass"] <-  0

### Impute
imp <- mice(dat, meth = meth, predictorMatrix = pred, printFlag = FALSE, m = 20, maxit = 100)
complete <- complete(imp)

### Check
densityplot(imp, layout = c(3,1)) # densities look good
stripplot(imp) # distribution looks good
bwplot(imp) # bw plot looks within original data range
xyplot(imp, LogBodySize ~ LogAbundance| as.factor(.imp), type = c("p", "r"), pch = c(1, 20)) # respects allometry
plot(imp) # good convergance

### Add completed data
nodes_wrk <- 
  bind_cols(csm_nodes, complete) %>% 
  janitor::clean_names()

nodes_wrk <- 
  nodes_wrk %>% 
  mutate(body_size = exp(log_body_size),
         abundance = exp(log_abundance),
         biomass = exp(log_biomass),
         species_id_stage_id = as.character(species_id_stage_id),
         working_name = make.unique(as.character(working_name))) %>% 
  select(node_id, species_id_stage_id,
         body_size, abundance, biomass,
         log_body_size, log_abundance, log_biomass,
         working_name, consumer_strategy_stage, organismal_group)

nodes_wrk$organismal_group <- fct_recode(nodes_wrk$organismal_group,
                                        # plants
                                        plant = "vascular plant",
                                        plant = "microphytobenthos",
                                        plant = "macroalgae",
                                        # animals
                                        # animal = "protist",
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
                                        animal = "acanthocephalan")
                                        # animal = "anthozoan",
                                        # animal = "holothurian",
                                        # animal = "phoronid",
                                        # animal = "turbellarian",
                                        # detritus = "virus")
nodes_wrk %>% 
  ggplot(aes(x = log_body_size, y = log_abundance, 
             group = organismal_group, col = organismal_group)) +
  geom_smooth(method = lm, se = FALSE)

# matrix from igraph
links_csm <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Links.csv") %>% 
  janitor::clean_names() %>% 
  janitor::remove_empty("cols")

# remove virus links and protozoa links
links_to_cut <- 
  links_csm %>% 
  mutate(resource_species_id_stage_id = as.character(resource_species_id_stage_id),
         consumer_species_id_stage_id = as.character(consumer_species_id_stage_id)) %>%
  filter(consumer_species_id_stage_id %in% filter_virus_proto | resource_species_id_stage_id %in% filter_virus_proto)

# get links for mat
links_wrk <- 
  links_csm %>% 
  mutate(resource_species_id_stage_id = as.character(resource_species_id_stage_id),
         consumer_species_id_stage_id = as.character(consumer_species_id_stage_id)) %>%
  anti_join(links_to_cut) %>% 
  select(resource_species_id_stage_id, consumer_species_id_stage_id)

# make graph 
graph_csm <- 
  graph_from_data_frame(links_wrk, directed = T, vertices = nodes_wrk$species_id_stage_id)

# get mat from graph
mat <- as_adj(graph_csm) %>% 
  as.matrix()

# 2. bodymasses
bodymasses <- pull(nodes_wrk, body_size) # pull creates vectors

# 3. biomasses
biomasses <- bodymasses * pull(nodes_wrk, abundance)

# 4. a vector with organism types
org.type <-  pull(nodes_wrk, organismal_group)
unique(org.type)

# recode factor levels
org.type <- 
  fct_recode(org.type,
             # plants
             plant = "vascular plant",
             plant = "microphytobenthos",
             plant = "macroalgae",
             # animals
             # animal = "protist",
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
             animal = "acanthocephalan"
             # animal = "anthozoan",
             # animal = "holothurian",
             # animal = "phoronid",
             # animal = "turbellarian",
             # virus? - should probabaly remove this just label as detritus for now,
             #detritus = "virus"
             )


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
consumer.type <-  pull(nodes_wrk, consumer_strategy_stage)
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

