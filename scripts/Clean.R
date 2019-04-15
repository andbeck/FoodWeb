set.seed(12)

# Cleanup Carp Raw Data ---------------------------------------------------

library(tidyverse)
library(data.table)
library(gdata)
library(forcats)

# Load in Data ------------------------------------------------------------

bsq_nodes <- read.csv("data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv")
bsq_links <- read.csv("data/interactionwebdb/Carpinteria/BSQweb_Links.csv")

csm_nodes <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")
csm_links <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Links.csv")

epb_nodes <- read.csv("data/interactionwebdb/Carpinteria/EPBweb_Nodes.csv")
epb_links <- read.csv("data/interactionwebdb/Carpinteria/EPBweb_Links.csv")

###### BSQ ######

# 1) Nodes

# drop na values
bsq_na <- 
  bsq_nodes %>% 
  drop_na(Biomass.kg.ha., BodySize.g.)

# select and rename useful columns from NA free file
bsq_n <- 
  bsq_na %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"),
         BodySize = BodySize.g. * 1000,
         Abundance = BodySize * Biomass.kg.ha.) %>% 
  rename(Biomass = Biomass.kg.ha., ConsumerType = ConsumerStrategy.stage.) %>% 
  dplyr::select(NodeID, WorkingName, Abundance, BodySize, ConsumerType, Biomass)

# drop unused factor levels
bsq_n$ConsumerType <- 
  bsq_n$ConsumerType %>% 
  fct_drop()

# levels(bsq_n$ConsumerType)

# select only factor levels detritivore and predator -- only ones left that arent para
bsq_nfree <- bsq_n %>% 
  filter(ConsumerType == "detritivore" | ConsumerType == "predator")
  

# 2) Links
bsq_l <- 
  bsq_links %>% 
  dplyr::select(ConsumerNodeID, ResourceNodeID, LinkTypeID)

# all links
bsq_l <-
  bsq_l %>%
  mutate(NodeID = ConsumerNodeID) %>% # rename so semi_join has something to compare against
  semi_join(bsq_n, by = "NodeID") %>%  # join
  dplyr::select(-NodeID) %>% # remove added column
  mutate(NodeID = ResourceNodeID) %>% # rename so semi_join has another thing to compare against
  semi_join(bsq_n, by = "NodeID") %>% # join
  dplyr::select(-NodeID) # again remove NodeID column

# parasite free links
bsq_lfree <-
  bsq_l %>%
  mutate(NodeID = ConsumerNodeID) %>% # rename so semi_join has something to compare against
  semi_join(bsq_nfree, by = "NodeID") %>%  # join
  dplyr::select(-NodeID) %>% # remove added column
  mutate(NodeID = ResourceNodeID) %>% # rename so semi_join has another thing to compare against
  semi_join(bsq_nfree, by = "NodeID") %>% # join
  dplyr::select(-NodeID) # again remove NodeID column

####### CSM... ######

# 1) Nodes
csm_na <- 
  csm_nodes %>% 
  drop_na(Biomass.kg.ha., BodySize.g.)

csm_n <- 
  csm_na %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"),
         BodySize = BodySize.g. * 1000,
         Abundance = BodySize * Biomass.kg.ha.) %>% 
  rename(Biomass = Biomass.kg.ha., ConsumerType = ConsumerStrategy.stage.) %>% 
  dplyr::select(NodeID, WorkingName, Abundance, BodySize, ConsumerType, Biomass)

# drop unused factor levels
csm_n$ConsumerType <- 
  csm_n$ConsumerType %>% 
  fct_drop()

# levels(csm_n$ConsumerType)

# select only factor levels detritivore and predator -- only ones left that arent para
csm_nfree <- csm_n %>% 
  filter(ConsumerType == "detritivore" | ConsumerType == "predator")

# 2) Links
csm_l <- 
  csm_links %>% 
  dplyr::select(ConsumerNodeID, ResourceNodeID, LinkTypeID)

# all links
csm_l <-
  csm_l %>%
  mutate(NodeID = ConsumerNodeID) %>% # rename so semi_join has something to compare against
  semi_join(csm_n, by = "NodeID") %>%  # join
  dplyr::select(-NodeID) %>% # remove added column
  mutate(NodeID = ResourceNodeID) %>% # rename so semi_join has another thing to compare against
  semi_join(csm_n, by = "NodeID") %>% # join
  dplyr::select(-NodeID) # again remove NodeID column

# parasite free links
csm_lfree <-
  csm_l %>%
  mutate(NodeID = ConsumerNodeID) %>% # rename so semi_join has something to compare against
  semi_join(csm_nfree, by = "NodeID") %>%  # join
  dplyr::select(-NodeID) %>% # remove added column
  mutate(NodeID = ResourceNodeID) %>% # rename so semi_join has another thing to compare against
  semi_join(csm_nfree, by = "NodeID") %>% # join
  dplyr::select(-NodeID) # again remove NodeID column


####### EPB... ######

# 1) Nodes
epb_na <- 
  epb_nodes %>% 
  drop_na(Biomass.kg.ha., BodySize.g.)

epb_n <- 
  epb_na %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"),
         BodySize = BodySize.g. * 1000,
         Abundance = BodySize * Biomass.kg.ha.) %>% 
  rename(Biomass = Biomass.kg.ha., ConsumerType = ConsumerStrategy.stage.) %>% 
  dplyr::select(NodeID, WorkingName, Abundance, BodySize, ConsumerType, Biomass)

# drop unused factor levels
epb_n$ConsumerType <- 
  epb_n$ConsumerType %>% 
  fct_drop()

# levels(epb_n$ConsumerType)

# select only factor levels detritivore and predator -- only ones left that arent para
epb_nfree <- epb_n %>% 
  filter(ConsumerType == "detritivore" | ConsumerType == "predator")

# 2) Links

# selecting useful columns
epb_l <- 
  epb_links %>% 
  dplyr::select(ConsumerNodeID, ResourceNodeID, LinkTypeID)

# all links
epb_l <-
  epb_l %>%
  mutate(NodeID = ConsumerNodeID) %>% # rename so semi_join has something to compare against
  semi_join(epb_n, by = "NodeID") %>%  # join
  dplyr::select(-NodeID) %>% # remove added column
  mutate(NodeID = ResourceNodeID) %>% # rename so semi_join has another thing to compare against
  semi_join(epb_n, by = "NodeID") %>% # join
  dplyr::select(-NodeID) # again remove NodeID column

# parasite free links
epb_lfree <-
  epb_l %>%
  mutate(NodeID = ConsumerNodeID) %>% # rename so semi_join has something to compare against
  semi_join(epb_nfree, by = "NodeID") %>%  # join
  dplyr::select(-NodeID) %>% # remove added column
  mutate(NodeID = ResourceNodeID) %>% # rename so semi_join has another thing to compare against
  semi_join(epb_nfree, by = "NodeID") %>% # join
  dplyr::select(-NodeID) # again remove NodeID column

# keep only the cleaned up files in the environment -----------------------

keep(bsq_n, bsq_l, bsq_nfree, bsq_lfree,
     csm_n, csm_l, csm_nfree, csm_lfree,
     epb_n, epb_l, epb_nfree, epb_lfree,
     sure = TRUE)

