#### Creating Taxanomic Trees From Species Names ####

set.seed(12)
rm(list = ls())

# install.packages("rotl")
library(rotl)
library(ape)
library(tidyverse)
library(janitor)

# library of all taxa names
taxa <-  read.csv("data/parasiteimpute.csv")

# take taxo data out
dat <-
  taxa %>% 
  filter(Lifestyle.stage. != "non-living") %>% 
  select(WorkingName,
         Kingdom:SpecificEpithet) %>% 
  rename(Species = SpecificEpithet) %>% 
  droplevels() %>% 
  remove_empty("cols")

# remove empty cols using janitor
dat <- 
  dat %>% 
  remove_empty("cols")

# factors and levels
str(dat)

### SPECIES ### 
# get all unique species names
species <- 
  unique(dat$Species)

# cleanup
species <- 
  species[-1] %>% # remove blank factor level
  droplevels() # drop blank factor level

### GENUS ### 
# get all unique genus names
genus <- 
  unique(dat$Genus)

# cleanup
genus <- 
  genus[-1] %>% # remove blank factor level
  droplevels() # drop blank factor level

### FAMILY ###
# get all unique family names
family <- 
  unique(dat$Family)

# cleanup
family <- 
  family[-1] %>% # remove blank factor level
  droplevels() # drop blank factor level

### ORDER ###
# get all unique order names
order <- 
  unique(dat$Order)

# cleanup
order <- 
  order[-1] %>% # remove blank factor level
  droplevels() # drop blank factor level

### PHYLUM ###
# get all unique phylum names
phylum <- 
  unique(dat$Phylum)

# cleanup
phylum <- 
  phylum[-1] %>% # remove blank factor level
  droplevels() # drop blank factor level

# matching
order_match <- tnrs_match_names(as.character(order))

order_filter <- 
  order_match %>% 
  filter(flags == "")

ord_tree <- tol_induced_subtree(ott_ids = order_filter$ott_id)
plot(ord_tree, cex = 0.7)

family_match <- tnrs_match_names(as.character(family))



