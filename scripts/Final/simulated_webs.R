source("scripts/practice/FoodWebFunctions.R")
load("data/igraph_data.RDS")

library(igraph)

# models for BSQ
random_bsq <- Random.model(S = gorder(bsq.ig), L = gsize(bsq.ig), N = 100)
niche_bsq <- Niche.model(S = gorder(bsq.ig), L = gsize(bsq.ig), N = 100)

# models for CSM
random_csm <- Random.model(S = gorder(csm.ig), L = gsize(csm.ig), N = 100)
niche_csm <- Niche.model(S = gorder(csm.ig), L = gsize(csm.ig), N = 100)

# models for EPB
random_epb <- Random.model(S = gorder(epb.ig), L = gsize(epb.ig), N = 100)
niche_epb <- Niche.model(S = gorder(epb.ig), L = gsize(epb.ig), N = 100)

# parallel
library(parallel)
niche_bsq_TL <- mclapply(niche_bsq, NetIndices::TrophInd, mc.cores = 12)
niche_csm_TL <- mclapply(niche_csm, NetIndices::TrophInd, mc.cores = 12)
niche_epb_TL <- mclapply(niche_epb, NetIndices::TrophInd, mc.cores = 12)

test <- bind_rows(niche_bsq_TL)
ggplot(test, aes(x = TL)) +
  geom_density()

