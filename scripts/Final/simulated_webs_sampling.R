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

Plot.matrix(niche_bsq[[1]])

mclapply(niche_bsq[[1]], Plot.matrix)

# models for CSM
# random_csm <- Random.model(S = gorder(csm.ig), L = gsize(csm.ig), N = 10)
niche_csm <- Niche.model(S = gorder(csm.ig), L = gsize(csm.ig), N = 50)

# models for EPB
# random_epb <- Random.model(S = gorder(epb.ig), L = gsize(epb.ig), N = 10)
niche_epb <- Niche.model(S = gorder(epb.ig), L = gsize(epb.ig), N = 50)


# Get TL of Simulated Webs ------------------------------------------------

niche_bsq_TL <- mclapply(niche_bsq, NetIndices::TrophInd, mc.cores = 4)
niche_csm_TL <- mclapply(niche_csm, NetIndices::TrophInd, mc.cores = 4)
niche_epb_TL <- mclapply(niche_epb, NetIndices::TrophInd, mc.cores = 4)

# Empirical TL Distribution -----------------------------------------------

emp_TL <- 
  list(troph_bsq, troph_csm, troph_epb) %>%
  # distinct rows
  lapply(distinct) %>% 
  # drop NA from Roots isolated node
  lapply(drop_na) %>% 
  # bin TL into .5 trophic level intervals
  lapply(mutate, 
         # arguments for mutate
         TL_cut = cut_width(TL, .5))

# BSQ only web ------------------------------------------------------------

bsq_TL <- 
  troph_bsq %>% 
  # distinct
  distinct() %>%  
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
  # distinct
  distinct() %>%  
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
  # distinct
  distinct() %>%  
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

# CSM only web ------------------------------------------------------------

csm_TL <- 
  troph_csm %>% 
  # distinct
  distinct() %>%  
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
  # distinct
  distinct() %>%  
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
  # distinct
  distinct() %>%  
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

# EPB only web ------------------------------------------------------------

epb_TL <- 
  troph_epb %>%
  # distinct
  distinct() %>% 
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
  # distinct
  distinct() %>% 
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
  # distinct
  distinct() %>%  
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



# Sampling and matching ---------------------------------------------------

out_bsq <- list()
out_csm <- list()
out_epb <- list()

for (i in 1:length(bsq_sim_TL)) {
  # gets the list structure of the food web we want to sample into
  x <- bsq_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, org_type)
  # create M list of structure z
  out_bsq$m[[i]] <- z
  # create N list of structure z
  out_bsq$n[[i]] <- z
  # create org list of structure z
  out_bsq$org[[i]] <- z
  # sample the M data
  for (j in 1:length(z)) {
    if (j < length(bsq_N)) {
      out_bsq$m[[i]][[j]] <- sample(bsq_M[[j]], replace = TRUE, length(z[[j]]))
      # now find the index numbers of those sampled
      a <- match(x = out_bsq$m[[i]][[j]], table = bsq_M[[j]])
      # now take these index numbers and take their respective values for N
      out_bsq$n[[i]][[j]] <- bsq_N[[j]][a]
      # now give them the matching animal type
      out_bsq$org[[i]][[j]] <- bsq_TL[[j]][a]
    } else {
      out_bsq$m[[i]][[j]] <- sample(bsq_M[[length(bsq_M)]], replace = TRUE, length(z[[j]]))
      # now find the index numbers of those sampled
      a <- match(x = out_bsq$m[[i]][[j]], table = bsq_M[[length(bsq_M)]])
      # now take these index numbers and take their respective values for N
      out_bsq$n[[i]][[j]] <- bsq_N[[length(bsq_M)]][a]
      # now give them the matching animal type
      out_bsq$org[[i]][[j]] <- bsq_TL[[length(bsq_M)]][a]
    }
  }
}

for (i in 1:length(csm_sim_TL)) {
  # gets the list structure of the food web we want to sample into
  x <- csm_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, org_type)
  # create M list of structure z
  out_csm$m[[i]] <- z
  # create N list of structure z
  out_csm$n[[i]] <- z
  # create org list of structure z
  out_csm$org[[i]] <- z
  # sample the M data
  for (j in 1:length(z)) {
    if (j < length(csm_N)) {
      out_csm$m[[i]][[j]] <- sample(csm_M[[j]], replace = TRUE, length(z[[j]]))
      # now find the index numbers of those sampled
      a <- match(x = out_csm$m[[i]][[j]], table = csm_M[[j]])
      # now take these index numbers and take their respective values for N
      out_csm$n[[i]][[j]] <- csm_N[[j]][a]
      # now give them the matching animal type
      out_csm$org[[i]][[j]] <- csm_TL[[j]][a]
    } else {
      out_csm$m[[i]][[j]] <- sample(csm_M[[length(csm_M)]], replace = TRUE, length(z[[j]]))
      # now find the index numbers of those sampled
      a <- match(x = out_csm$m[[i]][[j]], table = csm_M[[length(csm_M)]])
      # now take these index numbers and take their respective values for N
      out_csm$n[[i]][[j]] <- csm_N[[length(csm_M)]][a]
      # now give them the matching animal type
      out_csm$org[[i]][[j]] <- csm_TL[[length(csm_M)]][a]
    }
  }
}

for (i in 1:length(epb_sim_TL)) {
  # gets the list structure of the food web we want to sample into
  x <- epb_sim_TL[[i]]
  y <- group_split(x, TL_cut)
  z <- map(y, pull, org_type)
  # create M list of structure z
  out_epb$m[[i]] <- z
  # create N list of structure z
  out_epb$n[[i]] <- z
  # create org list of structure z
  out_epb$org[[i]] <- z
  # sample the M data
  for (j in 1:length(z)) {
    if (j < length(epb_N)) {
      out_epb$m[[i]][[j]] <- sample(epb_M[[j]], replace = TRUE, length(z[[j]]))
      # now find the index numbers of those sampled
      a <- match(x = out_epb$m[[i]][[j]], table = epb_M[[j]])
      # now take these index numbers and take their respective values for N
      out_epb$n[[i]][[j]] <- epb_N[[j]][a]
      # now give them the matching animal type
      out_epb$org[[i]][[j]] <- epb_TL[[j]][a]
    } else {
      out_epb$m[[i]][[j]] <- sample(epb_M[[length(epb_M)]], replace = TRUE, length(z[[j]]))
      # now find the index numbers of those sampled
      a <- match(x = out_epb$m[[i]][[j]], table = epb_M[[length(epb_M)]])
      # now take these index numbers and take their respective values for N
      out_epb$n[[i]][[j]] <- epb_N[[length(epb_M)]][a]
      # now give them the matching animal type
      out_epb$org[[i]][[j]] <- epb_TL[[length(epb_M)]][a]
    }
  }
}

# Compact this ------------------------------------------------------------

out_bsq$b <- out_bsq$m
for(i in seq(out_bsq$b)){
  for(j in seq(out_bsq$b[[i]])){
    out_bsq$b[[i]][[j]] <- out_bsq$m[[i]][[j]] * out_bsq$n[[i]][[j]]
  }
}

out_csm$b <- out_csm$m
for(i in seq(out_csm$b)){
  for(j in seq(out_csm$b[[i]])){
    out_csm$b[[i]][[j]] <- out_csm$m[[i]][[j]] * out_csm$n[[i]][[j]]
  }
}

out_epb$b <- out_epb$m
for(i in seq(out_epb$b)){
  for(j in seq(out_epb$b[[i]])){
    out_epb$b[[i]][[j]] <- out_epb$m[[i]][[j]] * out_epb$n[[i]][[j]]
  }
}

# # works but returns them unnamed so not useful with current workflow
# get_biomass <- function(web){
#   web$b <- web$m
#   for(i in seq(web$b)){
#     for(j in seq(web$b[[i]])){
#       web$b[[i]][[j]] <- web$m[[i]][[j]] * web$n[[i]][[j]]
#     }
#   }
#   web$b
# }
# 
# foo <- lapply(list(out_bsq, out_csm, out_epb), get_biomass)

# unlist ------------------------------------------------------------------

out_bsq$org <- lapply(out_bsq$org, unlist)
out_bsq$m <- lapply(out_bsq$m, unlist)
out_bsq$n <- lapply(out_bsq$n, unlist)
out_bsq$b <- lapply(out_bsq$b, unlist)

out_csm$org <- lapply(out_csm$org, unlist)
out_csm$m <- lapply(out_csm$m, unlist)
out_csm$n <- lapply(out_csm$n, unlist)
out_csm$b <- lapply(out_csm$b, unlist)

out_epb$org <- lapply(out_epb$org, unlist)
out_epb$m <- lapply(out_epb$m, unlist)
out_epb$n <- lapply(out_epb$n, unlist)
out_epb$b <- lapply(out_epb$b, unlist)


# losses ------------------------------------------------------------------

losses_bsq_sim <- 
  lapply(out_bsq$m, function(x){
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
  lapply(out_csm$m, function(x){
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
  lapply(out_epb$m, function(x){
    losses = rep(NA, length(x))
    ecto.vert = met_types == "ecto_vert"
    endo.vert = met_types == "endo_vert"
    inv = met_types == "invert"
    losses[ecto.vert] = 18.18 * x[ecto.vert] ^ (-0.29)
    losses[endo.vert] = 19.5 * x[endo.vert] ^ (-0.29)
    losses[inv] = 18.18 * x[inv] ^ (-0.29)
    losses
  })


# efficiencies ------------------------------------------------------------

efficiencies_bsq_sim <- 
  lapply(seq(out_bsq$org), function(x) {
    efficiencies_bsq_sim = rep(NA, length(out_bsq$org[[x]]))
    efficiencies_bsq_sim[out_bsq$org[[x]] == "animal"] = 0.906
    efficiencies_bsq_sim[out_bsq$org[[x]] == "para"] = 0.906
    efficiencies_bsq_sim[out_bsq$org[[x]] == "plant"] = 0.545
    efficiencies_bsq_sim[out_bsq$org[[x]] == "detritus"] = 0.906
    efficiencies_bsq_sim
  })

efficiencies_csm_sim <- 
  lapply(seq(out_csm$org), function(x) {
    efficiencies_csm_sim = rep(NA, length(out_csm$org[[x]]))
    efficiencies_csm_sim[out_csm$org[[x]] == "animal"] = 0.906
    efficiencies_csm_sim[out_csm$org[[x]] == "para"] = 0.906
    efficiencies_csm_sim[out_csm$org[[x]] == "plant"] = 0.545
    efficiencies_csm_sim[out_csm$org[[x]] == "detritus"] = 0.906
    efficiencies_csm_sim
  })

efficiencies_epb_sim <- 
  lapply(seq(out_epb$org), function(x) {
    efficiencies_epb_sim = rep(NA, length(out_epb$org[[x]]))
    efficiencies_epb_sim[out_epb$org[[x]] == "animal"] = 0.906
    efficiencies_epb_sim[out_epb$org[[x]] == "para"] = 0.906
    efficiencies_epb_sim[out_epb$org[[x]] == "plant"] = 0.545
    efficiencies_epb_sim[out_epb$org[[x]] == "detritus"] = 0.906
    efficiencies_epb_sim
  })


# flux --------------------------------------------------------------------
sl <- list(
  mat = niche_bsq[[42]],
  biomasses = out_bsq$b[[42]],
  losses = losses_bsq_sim[[42]],
  efficiencies = efficiencies_bsq_sim[[42]]
)

vec.in = as.vector(t(sl$mat) %*% sl$efficiencies)
vec.1p = rep(0, dim(sl$mat)[1])
vec.1p[colSums(sl$mat) == 0] = 1
F = solve(diag(vec.in + vec.1p) - sl$mat) %*% sl$losses

any(F < 0)

#losses <- 0.1*out_bsq$m[[1]]^(-0.25)

fluxing(
  sl$mat, 
  sl$biomasses,
  sl$losses,
  sl$efficiencies,
  bioms.prefs = TRUE,
  ef.level = "prey"
)


bsq_fluxes_sim <- 
  lapply(seq(niche_bsq), function(x){
    try(fluxing(
      mat = niche_bsq[[x]],
      biomasses = out_bsq$b[[x]],
      losses = losses_bsq_sim[[x]],
      efficiencies = efficiencies_bsq_sim[[x]]
    ))
  })

winners <- Filter(is.numeric, bsq_fluxes_sim)
length(winners) # only 11 are without a negative flux value

csm_fluxes_sim <- 
  lapply(seq(niche_csm), function(x){
    fluxing(
      mat = niche_csm[[x]],
      biomasses = out_csm$b[[x]],
      losses = losses_csm_sim[[x]],
      efficiencies = efficiencies_csm_sim[[x]]
    )
  })

epb_fluxes_sim <- 
  lapply(seq(niche_epb), function(x){
    fluxing(
      mat = niche_epb[[x]],
      biomasses = out_epb$b[[x]],
      losses = losses_epb_sim[[x]],
      efficiencies = efficiencies_epb_sim[[x]]
    )
  })

