
# Simulating Webs ---------------------------------------------------------
source("scripts/practice/FoodWebFunctions.R")
load("data/igraph_data.RDS")
load("data/flux_data.RDS")

# library
library(igraph)
library(tidyverse)
library(parallel)

# Niche and Random Models --------------------------------------------------

# models for BSQ
# random_bsq <- Random.model(S = gorder(bsq.ig) + 1, L = gsize(bsq.ig), N = 10) # +1 for roots node which gets removed because its isolated
niche_bsq <- Niche.model(S = gorder(bsq.ig) + 1, L = gsize(bsq.ig), N = 50) # ^ as above

# models for CSM
# random_csm <- Random.model(S = gorder(csm.ig), L = gsize(csm.ig), N = 10)
niche_csm <- Niche.model(S = gorder(csm.ig), L = gsize(csm.ig), N = 50)

# models for EPB
# random_epb <- Random.model(S = gorder(epb.ig), L = gsize(epb.ig), N = 10)
niche_epb <- Niche.model(S = gorder(epb.ig), L = gsize(epb.ig), N = 50)


# Get TL of Simulated Webs ------------------------------------------------

niche_bsq_TL <- mclapply(niche_bsq, NetIndices::TrophInd, mc.cores = 12)
niche_csm_TL <- mclapply(niche_csm, NetIndices::TrophInd, mc.cores = 12)
niche_epb_TL <- mclapply(niche_epb, NetIndices::TrophInd, mc.cores = 12)

# Empirical TL Distribution -----------------------------------------------

emp_TL <- 
  list(troph_bsq, troph_csm, troph_epb) %>% 
  # drop NA from Roots isolated node
  lapply(drop_na) %>% 
  # bin TL into .5 trophic level intervals
  lapply(mutate, 
         # arguments for mutate
         TL_cut = cut_width(TL, .5))

# BSQ only web ------------------------------------------------------------

bsq_TL <- 
  troph_bsq %>% 
  # select useful cols
  select(org_type, TL) %>% 
  # drop NA TL value from isolated node in web
  drop_na() %>% 
  # bin TL into .5 trophic level intervals
  mutate( 
    # arguments for mutate
    TL_cut = cut_width(TL, .5)) %>%
  # split into trophic cut groups
  group_split(TL_cut) %>% 
  # pull vector of org_type for each group
  lapply(pull, org_type)

# need to drop animals and non-feeding stage parasites from the first trophic level
bsq_TL[[1]] <- 
  bsq_TL[[1]] %>% 
  enframe() %>% 
  subset(value != "para") %>% 
  subset(value != "animal") %>% 
  droplevels() %>% 
  pull(value)
  
  
bsq_M <- 
  troph_bsq %>% 
  # remove parasite abundances from Null Model
  filter(org_type != "para") %>%  
  # select useful cols
  select(M, TL) %>% 
  # drop NA TL value from isolated node in web
  drop_na() %>% 
  # bin TL into .5 trophic level intervals
  mutate( 
    # arguments for mutate
    TL_cut = cut_width(TL, .5)) %>%
  # split into trophic cut groups
  group_split(TL_cut) %>% 
  # pull vector of org_type for each group
  lapply(pull, M)

bsq_N <- 
  troph_bsq %>% 
  # remove parasite abundances from Null Model
  filter(org_type != "para") %>%  
  # select useful cols
  select(N, TL) %>% 
  # drop NA TL value from isolated node in web
  drop_na() %>% 
  # bin TL into .5 trophic level intervals
  mutate( 
    # arguments for mutate
    TL_cut = cut_width(TL, .5)) %>%
  # split into trophic cut groups
  group_split(TL_cut) %>% 
  # pull vector of org_type for each group
  lapply(pull, N)

bsq_sim_TL <- 
  niche_bsq_TL %>% 
  # add organism column, and cut TL's into bins of 0.5
  map(mutate,
      TL_cut = cut_width(TL, .5),
      org_type = NA,
      M = NA,
      N = NA)

# pre allocate output
out_bsq <- list()

# loop -  replace with sampled animal types for appropriate TL
for(i in seq(bsq_sim_TL)){
  x <- bsq_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, org_type)
  for(j in 1:length(z)){
    z[[j]] <- sample(bsq_TL[[j]], replace = TRUE, length(z[[j]]))
  }
  out_bsq$TL[[i]] <- z
}

# loop -  replace with sampled animal types for appropriate M
for(i in seq(bsq_sim_TL)){
  x <- bsq_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, M)
  for(j in 1:length(z)){
    if (length(z) > length(bsq_M)){
      z[[j]] <- sample(bsq_N[[length(bsq_M)]], replace = TRUE, length(z[[j]]))
    }
    else
      z[[j]] <- sample(bsq_M[[j]], replace = TRUE, length(z[[j]]))
  }
  out_bsq$M[[i]] <- z
}

# loop -  replace with sampled animal types for appropriate N
for(i in seq(bsq_sim_TL)){
  x <- bsq_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, N)
  for(j in 1:length(z)){
    if (length(z) > length(bsq_N)){
      z[[j]] <- sample(bsq_N[[length(bsq_N)]], replace = TRUE, length(z[[j]]))
    }
    else
    z[[j]] <- sample(bsq_N[[j]], replace = TRUE, length(z[[j]]))
  }
  out_bsq$N[[i]] <- z
}

# CSM only web ------------------------------------------------------------

csm_TL <- 
  troph_csm %>% 
  # select useful cols
  select(org_type, TL) %>% 
  # drop NA TL value from isolated node in web
  drop_na() %>% 
  # bin TL into .5 trophic level intervals
  mutate( 
    # arguments for mutate
    TL_cut = cut_width(TL, .5)) %>%
  # split into trophic cut groups
  group_split(TL_cut) %>% 
  # pull vector of org_type for each group
  lapply(pull, org_type)

# need to drop animals and non-feeding stage parasites from the first trophic level
csm_TL[[1]] <- 
  csm_TL[[1]] %>% 
  enframe() %>% 
  subset(value != "para") %>% 
  subset(value != "animal") %>% 
  droplevels() %>% 
  pull(value)

csm_M <- 
  troph_csm %>% 
  # remove parasite abundances from Null Model
  filter(org_type != "para") %>%  
  # select useful cols
  select(M, TL) %>% 
  # drop NA TL value from isolated node in web
  drop_na() %>% 
  # bin TL into .5 trophic level intervals
  mutate( 
    # arguments for mutate
    TL_cut = cut_width(TL, .5)) %>%
  # split into trophic cut groups
  group_split(TL_cut) %>% 
  # pull vector of org_type for each group
  lapply(pull, M)

csm_N <- 
  troph_csm %>% 
  # remove parasite abundances from Null Model
  filter(org_type != "para") %>% 
  # select useful cols
  select(N, TL) %>% 
  # drop NA TL value from isolated node in web
  drop_na() %>% 
  # bin TL into .5 trophic level intervals
  mutate( 
    # arguments for mutate
    TL_cut = cut_width(TL, .5)) %>%
  # split into trophic cut groups
  group_split(TL_cut) %>% 
  # pull vector of org_type for each group
  lapply(pull, N)

csm_sim_TL <- 
  niche_csm_TL %>% 
  # add organism column, and cut TL's into bins of 0.5
  map(mutate,
      TL_cut = cut_width(TL, .5),
      org_type = NA,
      M = NA,
      N = NA)

# pre allocate output
out_csm <- list()

# loop -  replace with sampled animal types for appropriate TL
for(i in seq(csm_sim_TL)){
  x <- csm_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, org_type)
  for(j in 1:length(z)){
    z[[j]] <- sample(csm_TL[[j]], replace = TRUE, length(z[[j]]))
  }
  out_csm$TL[[i]] <- z
}

# loop -  replace with sampled animal types for appropriate M
for(i in seq(csm_sim_TL)){
  x <- csm_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, M)
  for(j in 1:length(z)){
    z[[j]] <- sample(csm_M[[j]], replace = TRUE, length(z[[j]]))
  }
  out_csm$M[[i]] <- z
}

# loop -  replace with sampled animal types for appropriate N
for(i in seq(csm_sim_TL)){
  x <- csm_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, N)
  for(j in 1:length(z)){
    z[[j]] <- sample(csm_N[[j]], replace = TRUE, length(z[[j]]))
  }
  out_csm$N[[i]] <- z
}

# EPB only web ------------------------------------------------------------

epb_TL <- 
  troph_epb %>% 
  # select useful cols
  select(org_type, TL) %>% 
  # drop NA TL value from isolated node in web
  drop_na() %>% 
  # bin TL into .5 trophic level intervals
  mutate( 
    # arguments for mutate
    TL_cut = cut_width(TL, .5)) %>%
  # split into trophic cut groups
  group_split(TL_cut) %>% 
  # pull vector of org_type for each group
  lapply(pull, org_type)

# need to drop animals and non-feeding stage parasites from the first trophic level
epb_TL[[1]] <- 
  epb_TL[[1]] %>% 
  enframe() %>% 
  subset(value != "para") %>% 
  subset(value != "animal") %>% 
  droplevels() %>% 
  pull(value)

epb_M <- 
  troph_epb %>% 
  # remove parasite abundances from Null Model
  filter(org_type != "para") %>%  
  # select useful cols
  select(M, TL) %>% 
  # drop NA TL value from isolated node in web
  drop_na() %>% 
  # bin TL into .5 trophic level intervals
  mutate( 
    # arguments for mutate
    TL_cut = cut_width(TL, .5)) %>%
  # split into trophic cut groups
  group_split(TL_cut) %>% 
  # pull vector of org_type for each group
  lapply(pull, M)

epb_N <- 
  troph_epb %>% 
  # remove parasite abundances from Null Model
  filter(org_type != "para") %>%  
  # select useful cols
  select(N, TL) %>% 
  # drop NA TL value from isolated node in web
  drop_na() %>% 
  # bin TL into .5 trophic level intervals
  mutate( 
    # arguments for mutate
    TL_cut = cut_width(TL, .5)) %>%
  # split into trophic cut groups
  group_split(TL_cut) %>% 
  # pull vector of org_type for each group
  lapply(pull, N)

epb_sim_TL <- 
  niche_epb_TL %>% 
  # add organism column, and cut TL's into bins of 0.5
  map(mutate,
      TL_cut = cut_width(TL, .5),
      org_type = NA,
      M = NA,
      N = NA)

# pre allocate output
out_epb <- list()

# loop -  replace with sampled animal types for appropriate TL
for(i in seq(epb_sim_TL)){
  x <- epb_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, org_type)
  for(j in 1:length(z)){
    z[[j]] <- sample(epb_TL[[j]], replace = TRUE, length(z[[j]]))
  }
  out_epb$TL[[i]] <- z
}

# loop -  replace with sampled animal types for appropriate M
for(i in seq(epb_sim_TL)){
  x <- epb_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, M)
  for(j in 1:length(z)){
    z[[j]] <- sample(epb_M[[j]], replace = TRUE, length(z[[j]]))
  }
  out_epb$M[[i]] <- z
}

# loop -  replace with sampled animal types for appropriate N
for(i in seq(epb_sim_TL)){
  x <- epb_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, N)
  for(j in 1:length(z)){
    z[[j]] <- sample(epb_N[[j]], replace = TRUE, length(z[[j]]))
  }
  out_epb$N[[i]] <- z
}

# Get Biomass -------------------------------------------------------------

out_bsq$B <- out_bsq$M
for(i in seq(out_bsq$B)){
  for(j in seq(out_bsq$B[[i]])){
    out_bsq$B[[i]][[j]] <- out_bsq$M[[i]][[j]] * out_bsq$N[[i]][[j]]
  }
}

out_csm$B <- out_csm$M
for(i in seq(out_csm$B)){
  for(j in seq(out_csm$B[[i]])){
    out_csm$B[[i]][[j]] <- out_csm$M[[i]][[j]] * out_csm$N[[i]][[j]]
  }
}

out_epb$B <- out_epb$M
for(i in seq(out_epb$B)){
  for(j in seq(out_epb$B[[i]])){
    out_epb$B[[i]][[j]] <- out_epb$M[[i]][[j]] * out_epb$N[[i]][[j]]
  }
}


# Unlist Data -------------------------------------------------------------

out_bsq$TL <- lapply(out_bsq$TL, unlist)
out_bsq$M <- lapply(out_bsq$M, unlist)
out_bsq$N <- lapply(out_bsq$N, unlist)
out_bsq$B <- lapply(out_bsq$B, unlist)

out_csm$TL <- lapply(out_csm$TL, unlist)
out_csm$M <- lapply(out_csm$M, unlist)
out_csm$N <- lapply(out_csm$N, unlist)
out_csm$B <- lapply(out_csm$B, unlist)

out_epb$TL <- lapply(out_epb$TL, unlist)
out_epb$M <- lapply(out_epb$M, unlist)
out_epb$N <- lapply(out_epb$N, unlist)
out_epb$B <- lapply(out_epb$B, unlist)


# Run through fluxweb -----------------------------------------------------


# Met Types ---------------------------------------------------------------

met_types <-  c("ecto_vert", "endo_vert", "invert")

# losses ------------------------------------------------------------------
losses_bsq_sim <- 
  lapply(out_bsq$M, function(x){
    losses = rep(NA, length(x))
    ecto.vert = met_types == "ecto_vert"
    endo.vert = met_types == "endo_vert"
    inv = met_types == "invert"
    losses[ecto.vert] = 18.18 * x[ecto.vert] ^ (-0.29)
    losses[endo.vert] = 19.5 * x[endo.vert] ^ (-0.29)
    losses[inv] = 18.18 * x[inv] ^ (-0.29)
    losses
  })

losses_csm_sim <- 
  lapply(out_csm$M, function(x){
    losses = rep(NA, length(x))
    ecto.vert = met_types == "ecto_vert"
    endo.vert = met_types == "endo_vert"
    inv = met_types == "invert"
    losses[ecto.vert] = 18.18 * x[ecto.vert] ^ (-0.29)
    losses[endo.vert] = 19.5 * x[endo.vert] ^ (-0.29)
    losses[inv] = 18.18 * x[inv] ^ (-0.29)
    losses
  })

losses_epb_sim <- 
  lapply(out_epb$M, function(x){
    losses = rep(NA, length(x))
    ecto.vert = met_types == "ecto_vert"
    endo.vert = met_types == "endo_vert"
    inv = met_types == "invert"
    losses[ecto.vert] = 18.18 * x[ecto.vert] ^ (-0.29)
    losses[endo.vert] = 19.5 * x[endo.vert] ^ (-0.29)
    losses[inv] = 18.18 * x[inv] ^ (-0.29)
    losses
  })

# Efficiencies ------------------------------------------------------------

efficiencies_bsq_sim <- 
  lapply(seq(out_bsq$TL), function(x) {
    efficiencies_bsq_sim = rep(NA, length(out_bsq$TL[[x]]))
    efficiencies_bsq_sim[out_bsq$TL[[x]] == "animal"] = 0.906
    efficiencies_bsq_sim[out_bsq$TL[[x]] == "para"] = 0.906
    efficiencies_bsq_sim[out_bsq$TL[[x]] == "plant"] = 0.545
    efficiencies_bsq_sim[out_bsq$TL[[x]] == "detritus"] = 0.906
    efficiencies_bsq_sim
  })

efficiencies_csm_sim <- 
  lapply(seq(out_csm$TL), function(x) {
    efficiencies_csm_sim = rep(NA, length(out_csm$TL[[x]]))
    efficiencies_csm_sim[out_csm$TL[[x]] == "animal"] = 0.906
    efficiencies_csm_sim[out_csm$TL[[x]] == "para"] = 0.906
    efficiencies_csm_sim[out_csm$TL[[x]] == "plant"] = 0.545
    efficiencies_csm_sim[out_csm$TL[[x]] == "detritus"] = 0.906
    efficiencies_csm_sim
  })

efficiencies_epb_sim <- 
  lapply(seq(out_epb$TL), function(x) {
    efficiencies_epb_sim = rep(NA, length(out_epb$TL[[x]]))
    efficiencies_epb_sim[out_epb$TL[[x]] == "animal"] = 0.906
    efficiencies_epb_sim[out_epb$TL[[x]] == "para"] = 0.906
    efficiencies_epb_sim[out_epb$TL[[x]] == "plant"] = 0.545
    efficiencies_epb_sim[out_epb$TL[[x]] == "detritus"] = 0.906
    efficiencies_epb_sim
  })


# Fluxing -----------------------------------------------------------------

library(fluxweb)

bsq_fluxes_sim <- 
  lapply(1:length(niche_bsq), function(x){
    fluxing(
      mat = niche_bsq[[x]],
      biomasses = out_bsq$B[[x]],
      losses = losses_bsq_sim[[x]],
      efficiencies = efficiencies_bsq_sim[[x]]
    )
  })

csm_fluxes_sim <- 
  lapply(seq(niche_csm), function(x){
    fluxing(
      mat = niche_csm[[x]],
      biomasses = out_csm$B[[x]],
      losses = losses_csm_sim[[x]],
      efficiencies = efficiencies_csm_sim[[x]]
    )
  })

epb_fluxes_sim <- 
  lapply(seq(niche_epb), function(x){
    fluxing(
      mat = niche_epb[[x]],
      biomasses = out_epb$B[[x]],
      losses = losses_epb_sim[[x]],
      efficiencies = efficiencies_epb_sim[[x]]
    )
  })

