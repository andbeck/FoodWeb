### Visualising Missing Data for SSA
library(tidyverse)
library(missForest)
library(VIM)
library(naniar)
library(visdat)

# set enviro
rm(list=ls())
set.seed(12)

# Read
bsq <- read.csv("data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv")

bsq_wrk <- 
  bsq %>% # dplyr::select and rename some variables
  dplyr::select(FunctionalGroup = ConsumerStrategy.stage., BodySize = BodySize.g., 
                Biomass = Biomass.kg.ha., Abundance = Abundance.no..ha.) %>% 
  mutate(BodySize = BodySize / 1000) # change BodySize from g to Kg

bsq_new <- 
  bsq_wrk %>% 
  ## if BodySize = NA & if Biomass & Abundance != NA then BodySize = Biomass/Abundance - else do nothing
  mutate(BodySizeNew = case_when(!is.na(Abundance & Biomass) ~ Biomass / Abundance),
         ## if Abundance = NA and BodySize & Biomass != NA then Abundance = Biomass/BodySize - else do nothing
         AbundanceNew = case_when(!is.na(BodySize & Biomass) ~ Biomass / BodySize),
         ## if Biomass = NA and BodySize & Abundance != NA then Biomass = Abundance * BodySize - else do nothing
         BiomassNew = case_when(!is.na(Abundance & BodySize) ~ Abundance * BodySize)) %>% 
  ## join the columns of new and old together use coalesce -- Biomass + BiomassNew...
  ## this selects the original value first so only values that are missing from original dataset and then computed are selected  
  mutate(BodySize_Work = coalesce(BodySize, BodySizeNew),
         Biomass_Work = coalesce(Biomass, BiomassNew),
         Abundance_Work = coalesce(Abundance, AbundanceNew)) %>% 
  ## now dplyr::select and rename the new working colums to replace the old incomplete data set
  dplyr::select(FunctionalGroup, BodySize = BodySize_Work,
                Biomass = Biomass_Work, Abundance = Abundance_Work)

# several useful ways to visualise this missing data:
# missing by functional group
plot.missing <- 
  gg_miss_fct(x = bsq_new, fct = FunctionalGroup) + 
  labs(title = "% NA Values by Functional Group for BSQ")
ggsave("plot.missing.pdf", plot.missing)

# overall data structure and missing values
plot.vis <- 
  vis_dat(bsq_new) +
  labs(title = "Structure and NA values of BSQ data")
ggsave("plot.vis.pdf", plot.vis)

# a more detailed VIM plot of missing values
plot.vim <- aggr(bsq_new[,2:4], col=c('navyblue','red'),
     numbers=TRUE, sortVars=TRUE, labels=names(bsq_new[, 2:4]),
     cex.axis=.7, gap=3, ylab=c("Histogram of missing data for BSQ","Pattern of missing data for BSQ"))

# phylo count plot
plot.freq <- 
  bsq %>%
  # count factor levels and frequency
  count(Phylum) %>% 
  # rename blank factor varables to other
  mutate(Phylum = fct_recode(Phylum, "Other" = "")) %>% 
  # plot
  ggplot(aes(x = reorder(Phylum, -n), y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  ggtitle("Frequency plot of Phylum in BSQ") +
  xlab("Phylum") +
  ylab("Count") +
  coord_flip()
ggsave("plot.freq.pdf", plot.freq)  

## Does MCAR or MAR 
# install.packages("BaylorEdPsych")
library(BaylorEdPsych) # for littles test of MCAR 
bsq_little <- select(bsq_new, -FunctionalGroup)
bsq_little[!is.na(bsq_little)] <- 1
bsq_little[is.na(bsq_little)] <- 0
LittleMCAR(bsq_little)

## modelling missingness
bsq_new %>% 
  add_prop_miss() %>% 
  head

bsq_new %>%
  add_prop_miss() %>%
  rpart(prop_miss_all ~ ., data = .) %>%
  prp(type = 4, extra = 101, prefix = "Prop. Miss = ")
