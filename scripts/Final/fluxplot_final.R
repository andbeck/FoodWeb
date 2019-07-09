library(tidyverse)
library(ggthemes)

load("data/flux_data.RDS")

herb_csm = rep(NA, length(csm_fluxes))
carn_csm = rep(NA, length(csm_fluxes))
detr_csm = rep(NA, length(csm_fluxes))
para_csm = rep(NA, length(csm_fluxes))
total_csm = rep(NA, length(csm_fluxes))

for (x in 1:length(csm_fluxes)) {
  # Basal Species
  basal = colSums(csm_fluxes[[x]]) == 0
  # Plants
  plants = basal - sum(rowSums(csm_fluxes[[x]][feed_type_csm == "detritus",]))
  # herb_csm -- should be true
  herb_csm[[x]] = sum(rowSums(csm_fluxes[[x]][feed_type_csm == "autotroph",]))
  # carn_csm -- wrong becase this includes carnivores and detritivores
  carn_csm[[x]] = sum(rowSums(csm_fluxes[[x]][feed_type_csm == "carnivore",]))
  # detr_csm -- need a way to identify detritivores
  detr_csm[[x]] = sum(rowSums(csm_fluxes[[x]][feed_type_csm == "detritus",]))
  # para_csm
  para_csm[[x]] = sum(rowSums(csm_fluxes[[x]][feed_type_csm == "para",]))
  # total_csm
  total_csm[[x]] = sum(csm_fluxes[[x]])
}

csm_dat <- 
  cbind(herb_csm, carn_csm, detr_csm, para_csm, total_csm) %>% 
  as_tibble %>% 
  gather(key = interaction, value = flux) %>% 
  mutate(system = "CSM")


csm <- 
  csm_dat %>% 
  ggplot(aes(x = reorder(interaction, flux), y = log(flux))) +
  geom_boxplot(aes(fill = interaction)) +
  ggtitle("Fluxes through the CSM Subweb") +
  xlab("Feeding Type") +
  ylab("Energy J.yr^-1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  ylim(0, 20)
csm



# bsq ---------------------------------------------------------------------

herb_bsq = rep(NA, length(bsq_fluxes))
carn_bsq = rep(NA, length(bsq_fluxes))
detr_bsq = rep(NA, length(bsq_fluxes))
para_bsq = rep(NA, length(bsq_fluxes))
total_bsq = rep(NA, length(bsq_fluxes))

for (x in 1:length(bsq_fluxes)) {
  # Basal Species
  basal = colSums(bsq_fluxes[[x]]) == 0
  # Plants
  plants = basal - sum(rowSums(bsq_fluxes[[x]][feed_type_bsq == "detritus",]))
  # herb_bsq -- should be true
  herb_bsq[[x]] = sum(rowSums(bsq_fluxes[[x]][feed_type_bsq == "autotroph",]))
  # carn_bsq -- wrong becase this includes carnivores and detritivores
  carn_bsq[[x]] = sum(rowSums(bsq_fluxes[[x]][feed_type_bsq == "carnivore",]))
  # detr_bsq -- need a way to identify detritivores
  detr_bsq[[x]] = sum(rowSums(bsq_fluxes[[x]][feed_type_bsq == "detritus",]))
  # para_bsq
  para_bsq[[x]] = sum(rowSums(bsq_fluxes[[x]][feed_type_bsq == "para",]))
  # total_bsq
  total_bsq[[x]] = sum(bsq_fluxes[[x]])
}

bsq_dat <- 
  cbind(herb_bsq, carn_bsq, detr_bsq, para_bsq, total_bsq) %>% 
  as_tibble %>% 
  gather(key = interaction, value = flux) %>% 
  mutate(system = "BSQ")

bsq <- 
  bsq_dat %>% 
  ggplot(aes(x = reorder(interaction, flux), y = log(flux))) +
  geom_boxplot(aes(fill = interaction)) +
  ggtitle("Fluxes through the BSQ Subweb") +
  xlab("Feeding Type") +
  ylab("Energy J.yr^-1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  ylim(0, 20)


# epb ---------------------------------------------------------------------

herb_epb = rep(NA, length(epb_fluxes))
carn_epb = rep(NA, length(epb_fluxes))
detr_epb = rep(NA, length(epb_fluxes))
para_epb = rep(NA, length(epb_fluxes))
total_epb = rep(NA, length(epb_fluxes))

for (x in 1:length(epb_fluxes)) {
  # Basal Species
  basal = colSums(epb_fluxes[[x]]) == 0
  # Plants
  plants = basal - sum(rowSums(epb_fluxes[[x]][feed_type_epb == "detritus",]))
  # herb_epb -- should be true
  herb_epb[[x]] = sum(rowSums(epb_fluxes[[x]][feed_type_epb == "autotroph",]))
  # carn_epb -- wrong becase this includes carnivores and detritivores
  carn_epb[[x]] = sum(rowSums(epb_fluxes[[x]][feed_type_epb == "carnivore",]))
  # detr_epb -- need a way to identify detritivores
  detr_epb[[x]] = sum(rowSums(epb_fluxes[[x]][feed_type_epb == "detritivore",]))
  # para_epb
  para_epb[[x]] = sum(rowSums(epb_fluxes[[x]][feed_type_epb == "para",]))
  # total_epb
  total_epb[[x]] = sum(epb_fluxes[[x]])
}

epb_dat <- 
  cbind(herb_epb, carn_epb, detr_epb, para_epb, total_epb) %>% 
  as_tibble %>% 
  gather(key = interaction, value = flux) %>% 
  mutate(system = "EPB")

epb <- 
  epb_dat %>% 
  ggplot(aes(x = reorder(interaction, flux), y = log(flux))) +
  geom_boxplot(aes(fill = interaction)) +
  ggtitle("Fluxes through the EPB Subweb") +
  xlab("Feeding Type") +
  ylab("Energy J.yr^-1") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  ylim(0, 20)

library(cowplot)
theme_set(theme_grey())
plot_grid(bsq, csm, epb)

fluxingdat <- 
  bind_rows(bsq_dat, csm_dat, epb_dat) %>% 
  mutate(interaction = unlist(lapply(strsplit(interaction, '_',fixed = TRUE), '[', 1))) %>% 
  mutate(interaction = case_when(interaction == "para" ~ "Parasitism",
                                 interaction == "carn" ~ "Carnivory",
                                 interaction == "herb" ~ "Herbivory",
                                 interaction == "total" ~ "Total",
                                 interaction == "detr" ~ "Detritivory")) %>% 
  mutate(interaction = as.factor(interaction))

fluxingdat %>% 
  ggplot(aes(x = reorder(interaction, flux), y = log(flux))) +
  geom_boxplot() +
  facet_wrap(~ system) +
  xlab("Feeding Type") +
  ylab(expression(log("Energy J.yr^-1")))

# org type ----------------------------------------------------------------

con_type_bsq <- 
  flux_bsq[[1]] %>% 
  pull(consumer_strategy_stage) %>% 
  as.factor()

autotroph = rep(NA, length(bsq_fluxes))
detritivore = rep(NA, length(bsq_fluxes))
predator = rep(NA, length(bsq_fluxes))
detritus = rep(NA, length(bsq_fluxes))
macroparasite = rep(NA, length(bsq_fluxes))
ttp = rep(NA, length(bsq_fluxes))
pathogen = rep(NA, length(bsq_fluxes))
micropredator = rep(NA, length(bsq_fluxes))
paracastrator = rep(NA, length(bsq_fluxes))
nonfeeding = rep(NA, length(bsq_fluxes))
parasitoid = rep(NA, length(bsq_fluxes))



for (x in 1:length(bsq_fluxes)) {
  autotroph[[x]] = sum(rowSums(bsq_fluxes[[x]][con_type_bsq == "autotroph" ,]))
  detritivore[[x]] = sum(rowSums(bsq_fluxes[[x]][con_type_bsq == "detritivore" ,]))
  predator[[x]] = sum(rowSums(bsq_fluxes[[x]][con_type_bsq == "predator" ,]))
  detritus[[x]] = sum(rowSums(bsq_fluxes[[x]][con_type_bsq == "detritus" ,]))
  macroparasite[[x]] = sum(rowSums(bsq_fluxes[[x]][con_type_bsq == "macroparasite" ,]))
  ttp[[x]] = sum(rowSums(bsq_fluxes[[x]][con_type_bsq == "trophically transmitted parasite" ,]))
  pathogen[[x]] = sum(rowSums(bsq_fluxes[[x]][con_type_bsq == "pathogen" ,]))
  micropredator[[x]] = sum(rowSums(bsq_fluxes[[x]][con_type_bsq == "micropredator" ,]))
  paracastrator[[x]] = sum(rowSums(bsq_fluxes[[x]][con_type_bsq == "parasitic castrator" ,]))
  nonfeeding[[x]] = sum(rowSums(bsq_fluxes[[x]][con_type_bsq == "nonfeeding" ,]))
  parasitoid[[x]] = NA
}

con_bsq <- 
  cbind(detritus, autotroph, detritivore, predator, macroparasite,
        nonfeeding, pathogen, parasitoid, paracastrator, ttp) %>% 
  as_tibble %>% 
  gather(key = consumer, value = flux) %>% 
  mutate(system = "BSQ")

####

autotroph = rep(NA, length(csm_fluxes))
detritivore = rep(NA, length(csm_fluxes))
predator = rep(NA, length(csm_fluxes))
detritus = rep(NA, length(csm_fluxes))
macroparasite = rep(NA, length(csm_fluxes))
ttp = rep(NA, length(csm_fluxes))
pathogen = rep(NA, length(csm_fluxes))
micropredator = rep(NA, length(csm_fluxes))
paracastrator = rep(NA, length(csm_fluxes))
nonfeeding = rep(NA, length(csm_fluxes))
parasitoid = rep(NA, length(csm_fluxes))

con_type_csm <- 
  flux_csm[[1]] %>% 
  pull(consumer_strategy_stage) %>% 
  as.factor()

for (x in 1:length(csm_fluxes)) {
  autotroph[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "autotroph" ,]))
  detritivore[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "detritivore" ,]))
  predator[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "predator" ,]))
  detritus[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "detritus" ,]))
  macroparasite[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "macroparasite" ,]))
  ttp[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "trophically transmitted parasite" ,]))
  pathogen[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "pathogen" ,]))
  micropredator[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "micropredator" ,]))
  paracastrator[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "parasitic castrator" ,]))
  nonfeeding[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "nonfeeding" ,]))
  parasitoid[[x]] = sum(rowSums(csm_fluxes[[x]][con_type_csm == "parasitoid" ,]))
}

con_csm <- 
  cbind(detritus, autotroph, detritivore, predator, macroparasite,
        nonfeeding, pathogen, parasitoid, paracastrator, ttp) %>% 
  as_tibble %>% 
  gather(key = consumer, value = flux) %>% 
  mutate(system = "CSM")

####

con_type_epb <- 
  flux_epb[[1]] %>% 
  pull(consumer_strategy_stage) %>% 
  as.factor()

autotroph = rep(NA, length(epb_fluxes))
detritivore = rep(NA, length(epb_fluxes))
predator = rep(NA, length(epb_fluxes))
detritus = rep(NA, length(epb_fluxes))
macroparasite = rep(NA, length(epb_fluxes))
ttp = rep(NA, length(epb_fluxes))
pathogen = rep(NA, length(epb_fluxes))
micropredator = rep(NA, length(epb_fluxes))
paracastrator = rep(NA, length(epb_fluxes))
nonfeeding = rep(NA, length(epb_fluxes))
parasitoid = rep(NA, length(epb_fluxes))

for (x in 1:length(epb_fluxes)) {
  autotroph[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "autotroph" ,]))
  detritivore[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "detritivore" ,]))
  predator[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "predator" ,]))
  detritus[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "detritus" ,]))
  macroparasite[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "macroparasite" ,]))
  ttp[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "trophically transmitted parasite" ,]))
  pathogen[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "pathogen" ,]))
  micropredator[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "micropredator" ,]))
  paracastrator[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "parasitic castrator" ,]))
  nonfeeding[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "nonfeeding" ,]))
  parasitoid[[x]] = sum(rowSums(epb_fluxes[[x]][con_type_epb == "parasitoid" ,]))
}

parasitism_epb = c(nonfeeding, paracastrator, ttp, macroparasite)

con_epb <- 
  cbind(detritus, autotroph, detritivore, predator, macroparasite,
        nonfeeding, pathogen, parasitoid, paracastrator, ttp) %>% 
  as_tibble %>% 
  gather(key = consumer, value = flux) %>% 
  mutate(system = "EPB")

####

fluxingdat_con <- bind_rows(con_bsq,con_csm,con_epb)

fluxingdat_con %>% 
  filter(consumer == c("autotroph", "detritivore", "predator", "ttp")) %>% 
  ggplot(aes(x = reorder(consumer, flux), y = log(flux))) +
  geom_boxplot() +
  facet_wrap(~ system)

fluxingdat %>% 
  ggplot(aes(x = reorder(interaction, flux), y = log(flux))) +
  geom_boxplot() +
  facet_wrap(~ system)

save.image("fluxing_graph_data.RDS")

