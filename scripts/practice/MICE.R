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

lm1 <- lm(LogBodySize ~ Phylum, data = work)
summary(lm1)


### Imputation - With unlogged data
dat <- 
  work %>% 
  select(BodySize, Biomass, Abundance,
         LogBodySize, LogBiomass, LogAbundance) 

summary(dat)

ini <- # initialise imputation with 0 iterations
  mice(dat, maxit = 0)

meth <- 
  ini$method

pred <- 
  dat %>%
  quickpred()

# highlights potentially good predictors
fluxplot(work)

### Preserving transformations ----
# need to somehow keep the rlationship between bodysize & abundance

# methods --
# relationships between transformed variables
meth["LogBodySize"] <- "~log(BodySize)"
meth["LogBiomass"] <- "~log(Biomass)"
meth["LogAbundance"] <- "~log(Abundance)"
# dont impute non-transformed variables but use them as predictors
meth["BodySize"] <- "pmm"
meth["Biomass"] <- "pmm"
meth["Abundance"] <- "pmm"


# predictor matrix
pred["BodySize", "LogBodySize"] <- 0
pred["Biomass", "LogBiomass"] <- 0
pred["Abundance", "LogAbundance"] <- 0

### Passive imputation
# meth["Biomass"] <- "~I(BodySize * Abundance)"
# meth["BodySize"] <- "~I(Biomass / Abundance)"
# meth["Abundance"] <- "~I(Biomass * BodySize)"


# impute
imp <- mice(dat, meth = meth, predictorMatrix = pred, maxit = 50, m = 2)
stripplot(imp)
densityplot(imp)

# convergence
plot(imp)

### Pool
fit <- with(imp, lm(BodySize ~ Abundance * Biomass))
pool(fit)

### Complete
imp.complete <- 
  complete(imp)

xyplot(imp, LogAbundance ~ LogBodySize)



### compare vis
ggplot(work) +
  geom_point(aes(x = LogBodySize, y = LogAbundance), col = "red") +
  geom_point(data = imp.complete, aes(x = LogBodySize, y = LogAbundance), col = "blue")

summary(imp.complete)  
summary(work)
