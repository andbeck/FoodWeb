### Fluxing
library(tidyverse)
library(fluxweb)

load("igraph_data.RDS")


# Fluxweb Prep ------------------------------------------------------------

# matrix
mat_csm <- mats[[1]]

# bodymasses - list
bodymasses_csm <- 
  c_list_select %>% 
  lapply(pull, body_size)

# abundance - list
abundance_csm <- 
  c_list_select %>% 
  lapply(pull, abundance)

# biomass - list
biomasses_csm <- # multiply to get biomass
  map2(bodymasses_csm, abundance_csm, ~ .x * .y) 


# Organism Type -----------------------------------------------------------

# vector with org types for efficiencies calculating
org_type_csm <- 
  c_list_select %>% 
  lapply(pull, organismal_group) %>% 
  lapply(as.factor)

# recode factor 
org_type_csm <- 
  org_type_csm %>% 
  lapply(fct_recode, 
         # recode the factors of list
         # plants
         plant = "vascular plant",
         plant = "microphytobenthos",
         plant = "macroalgae",
         # animals
         animal = "protist",
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
         animal = "trematode",
         animal = "cestode",
         animal = "nematode",
         animal = "acanthocephalan")

# Feeding Type ------------------------------------------------------------

# vector
feed_type_csm <- 
  c_list_select %>% 
  lapply(pull, consumer_strategy_stage) %>% 
  lapply(as.factor)

# recode factor 
feed_type_csm <- 
  feed_type_csm %>% 
  lapply(fct_recode, 
         # recode the factors of list
         # basal 
         plant = "autotroph",
         detritus = "detritus",
         # detritivore
         detritivore = "detritivore",
         # predators
         predator = "micropredator",
         predator = "predator",
         # parasite
         para = "macroparasite",
         para = "parasitic castrator",
         para = "trophically transmitted parasite"
         )


# Metabolic Type ----------------------------------------------------------

met_types <-  c("ectothermic_vert", "endothermic_vert", "invert")

# losses
losses_csm <- 
lapply(bodymasses_csm, function(bodymasses){
  losses = rep(NA, length(bodymasses))
  ecto.vert = met_types == "ectothermic_vert"
  endo.vert = met_types == "endothermic_vert"
  inv = met_types == "invert"
  losses[ecto.vert] = 18.18 * bodymasses[ecto.vert] ^ (-0.29)
  losses[endo.vert] = 19.5 * bodymasses[endo.vert] ^ (-0.29)
  losses[inv] = 18.18 * bodymasses[inv] ^ (-0.29)
  losses
})

# calculate efficiencies
efficiencies_csm <- rep(NA, length(bodymasses_csm[[1]]))
efficiencies_csm[org_type_csm[[1]] == "animal"] <- 0.906
efficiencies_csm[org_type_csm[[1]] == "plant"] <- 0.545 
efficiencies_csm[org_type_csm[[1]] == "detritus"] <- 0.158

# metabolic rates
met_rates <- 
  bodymasses_csm %>% 
  lapply(function(x) 0.71* x ^-0.25)

# fluxing - lapply

# mat.fluxes <- fluxing(mats[[1]], 
#                       biomasses = biomasses_csm[[1]],
#                       losses = losses_csm[[1]], 
#                       efficiencies =  efficiencies_csm)
# 
# 
# lapply(1:length(biomasses_csm), function(x){
#   fluxing(mat = mat_csm, biomasses = NULL, losses = losses_csm[[x]], efficiencies = efficiencies_csm)
# })


# does the function work when its bad?
badflux <- # has the stop removed that is stopping the function completing
  function (mat, biomasses = NULL, losses, efficiencies, bioms.prefs = TRUE, 
          bioms.losses = TRUE, ef.level = "prey") 
{
  if (!is.numeric(mat)) {
    stop("'mat' must be numeric")
  }
  if (dim(mat)[1] != dim(mat)[2]) {
    stop("mat should be a square matrix")
  }
  if (!is.null(biomasses)) {
    if (!is.vector(biomasses)) {
      stop("biomasses should be a vector")
    }
    else {
      if (length(biomasses) != dim(mat)[1]) {
        stop("length of biomasses vector should equal to dimensions of mat")
      }
    }
    if (!is.numeric(biomasses)) {
      stop("'biomasses' must be numeric")
    }
    else if (any(biomasses < 0)) {
      stop("'biomasses' must be all >=0")
    }
  }
  else if (bioms.prefs) {
    stop("bioms.prefs set to TRUE but no biomasses provided")
  }
  if (!is.numeric(losses)) {
    stop("'losses' should be numeric")
  }
  else if (any(losses < 0)) {
    stop("'losses' contain negative value(s)")
  }
  if (!is.numeric(efficiencies)) {
    stop("'efficiencies' must be numeric")
  }
  if (ef.level == "pred") {
    colsums = colSums(mat)
    if (sum(is.na(efficiencies[colsums == 0])) > 0) {
      efficiencies[colsums == 0] = 1
    }
  }
  if (ef.level == "prey") {
    rowsums = rowSums(mat)
    if (sum(is.na(efficiencies[rowsums == 0])) > 0) {
      efficiencies[rowsums == 0] = 1
    }
  }
  if (ef.level == "link.specific") {
    if (sum(efficiencies[mat == 0]) > 0) {
      warning("Efficiencies of some non existing links are not 0")
    }
  }
  if (!(ef.level %in% c("prey", "pred", "link.specific"))) {
    stop("ef.level should be set to 'pred', 'prey' or 'link.specific'")
  }
  if (ef.level == "prey" && is.matrix(efficiencies)) {
    warning("'ef.level' is set to 'prey' and expect a vector of efficiencies but get a matrix instead.\n ef.level was then set to 'link.specific'")
    ef.level = "link.specific"
  }
  if (any(efficiencies < 0) || any(efficiencies > 1)) {
    stop("'efficiencies' must all be in interval [0,1]")
  }
  if (is.vector(efficiencies)) {
    if (ef.level == "link.specific") {
      stop("'efficiencies' should be a matrix not a vector when efficiencies are link specific")
    }
    if (length(efficiencies) != dim(mat)[1]) {
      stop("'efficiencies' vector length sould be equal to number of species (dimension of mat)")
    }
  }
  else if (dim(efficiencies != dim(mat))) {
    stop("'efficiencies' matrix dimension different from 'mat'")
  }
  column.sum = colSums(mat)
  if (bioms.prefs) {
    mat[, column.sum > 0] = apply(as.matrix(mat[, column.sum > 
                                                  0]), 2, function(vec) vec * biomasses/sum(vec * 
                                                                                              biomasses))
  }
  else {
    mat[, column.sum > 0] = sweep(as.matrix(mat[, column.sum > 
                                                  0]), 2, column.sum[column.sum > 0], "/")
  }
  if (!is.vector(losses)) {
    losses = rowSums(losses)
  }
  if (bioms.losses == T) {
    losses = losses * biomasses
  }
  if (ef.level == "pred") {
    F = solve(diag(efficiencies) - mat) %*% losses
  }
  if (ef.level == "prey") {
    vec.in = as.vector(t(mat) %*% efficiencies)
    vec.1p = rep(0, dim(mat)[1])
    vec.1p[colSums(mat) == 0] = 1
    F = solve(diag(vec.in + vec.1p) - mat) %*% losses
  }
  if (ef.level == "link.specific") {
    U = mat * efficiencies
    vec.one = rep(1, dim(efficiencies)[1])
    vec.1p = rep(0, dim(mat)[1])
    vec.1p[colSums(mat) == 0] = 1
    vec.in = as.vector(t(U) %*% vec.one + vec.1p)
    F = solve(diag(vec.in) - mat) %*% losses
  }
  flux.mat = sweep(mat, 2, F, "*")
  return(flux.mat)
  }

badmat <- 
  badflux(mat = mat_csm, biomasses = biomasses_csm[[4]], losses = losses_csm[[4]], efficiencies = efficiencies_csm)

orgtype <- org_type_csm[[1]]
feedtype <- feed_type_csm[[1]]

# gather - feedtype
plant = sum(rowSums(badmat[feedtype == "plant",]))
detritivore = sum(rowSums(badmat[feedtype == "detritivore",]))
detritus = sum(rowSums(badmat[feedtype == "detritus",]))
nonfeeding = sum(rowSums(badmat[feedtype == "nonfeeding",]))
predator = sum(rowSums(badmat[feedtype == "predator",]))
para = sum(rowSums(badmat[feedtype == "para",]))

baddat <- data.frame(fluxes = c(plant, detritivore, detritus, nonfeeding, predator, para),
                     feedtype = c("plant", "detritivore", "detritus", 
                                  "nonfeeding", "predator", "para"))
baddat %>% # this is awful...
  ggplot(aes(x = reorder(feedtype, fluxes), y = fluxes)) +
  geom_bar(stat = "identity")


