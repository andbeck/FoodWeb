#### Creating Taxanomic Trees From Species Names ####

set.seed(12)
rm(list = ls())

# install.packages("rotl")
library(rotl)
library(ape)
library(tidyverse)
library(janitor)

# data 
csm <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")

# take taxo data out
dat <-
  csm %>% 
  select(WorkingName,
        Kingdom:SpecificEpithet) %>% 
  rename(Species = SpecificEpithet)

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

# tnrs match to Open Tree of Life
# species_match <- tnrs_match_names(as.character(species))
# genus_match <- tnrs_match_names(as.character(genus))
family_match <- tnrs_match_names(as.character(family))

familyfilter_match <- family_match %>% 
  filter(search_string != "trichodinidae")
         
  
  
order_match <- tnrs_match_names(as.character(order))
phylum_match <- tnrs_match_names(as.character(phylum))

# Getting the tree for taxa
# sp_tree <- tol_induced_subtree(ott_ids = species_match$ott_id)
# gen_tree <- tol_induced_subtree(ott_ids = genus_match$ott_id) # Can't find ott391774
fam_tree <- tol_induced_subtree(ott_ids = familyfilter_match$ott_id)
ord_tree <- tol_induced_subtree(ott_ids = order_match$ott_id)
phy_tree <- tol_induced_subtree(ott_ids = phylum_match$ott_id)


plot(fam_tree)
plot(ord_tree)
plot(phy_tree)

# try to remove ott391774 and other missing data - seems to be "BARREN" individuals
genfiltermatch <-  genus_match %>% 
  filter(search_string != "clevelandia",
         search_string != "monanthocloe")


gen_tree <- tol_induced_subtree(ott_ids = genfiltermatch$ott_id) # Can't find ott391774
plot(gen_tree, show.tip.label=FALSE)

mono_id <- tnrs_match_names(mono_tree$tip.label)
mono_tree <- tol_subtree(ott_id = ott_id(mono_id))
plot(mono_tree, show.tip.label = T)

# Bind these columns/matches together? ---------------------------------------------
bound <- bind_rows(genfiltermatch, order_match, phylum_match, familyfilter_match)
all <- plot(tol_induced_subtree(ott_ids = bound$ott_id), show.tip.label = T, cex = .5) # is this my whole tree?
tiplabels()
# Try to unite the columns ------------------------------------------------

fam_gen <- 
  csm %>% 
  select(WorkingName,
         Kingdom:SpecificEpithet) %>% 
  rename(Species = SpecificEpithet) %>% 
  unite(TaxaSearch, Family, Genus, sep = " ")

fam_gen_dat <- unique(fam_gen$TaxaSearch) 
fam_gen_dat <- fam_gen_dat[-1] # drop blank factor level

fam_gen_match <- tnrs_match_names(as.character(fam_gen_dat))

