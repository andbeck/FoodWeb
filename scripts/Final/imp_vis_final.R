### Using MICE to obtain good imputation values for Mass and Abundance of organisms ###
library(tidyverse)
library(mice)
rm(list = ls())
### Read in data
csm_nodes <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv", as.is = T)
glimpse(csm_nodes)
summary(csm_nodes$Kingdom)

### Clean - remove protozoan kingdoms and virus
csm_nodes <- 
  csm_nodes

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

dat <- 
  work %>% 
  # filter(Abundance != 0) %>% # remove abundance values = 0
  select(LogBodySize, LogAbundance, LogBiomass)

ini <- 
  mice(dat, maxit = 0)

meth <- 
  ini$method

pred <- 
  ini$predictorMatrix

meth["LogBodySize"] <- "pmm"
meth["LogAbundance"] <- "pmm"
meth["LogBiomass"] <- "~I(LogBodySize + LogAbundance)" # addition of logs is the same as multiplying exponent of log
pred[c("LogBodySize", "LogAbundance"), "LogBiomass"] <-  0


imp <- parlmice(dat, meth = meth, predictorMatrix = pred, printFlag = T, m = 5, maxit = 100)
complete <- complete(imp)

densityplot(imp, layout = c(3,1)) # densities look good
stripplot(imp) # distribution looks good
bwplot(imp) # bw plot looks within original data range
xyplot(imp, LogBodySize ~ LogAbundance| as.factor(.imp), type = c("p", "r"), pch = c(20, 20)) # respects allometry
plot(imp) # good convergance
