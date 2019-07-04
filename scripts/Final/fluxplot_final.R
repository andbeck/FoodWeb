library(tidyverse)
library(ggthemes)

herb_csm = rep(NA, length(csm_fluxes))
carn_csm = rep(NA, length(csm_fluxes))
detr_csm = rep(NA, length(csm_fluxes))
para_csm = rep(NA, length(csm_fluxes))
total_csm = rep(NA, length(csm_fluxes))

for (x in 1:length(csm_fluxes)) {
  # Basal Species
  basal = colSums(csm_fluxes[[x]]) == 0
  # Plants
  plants = basal
  # herb_csm -- should be true
  herb_csm[[x]] = sum(rowSums(csm_fluxes[[x]][plants,]))
  # carn_csm -- wrong becase this includes carnivores and detritivores
  carn_csm[[x]] = sum(rowSums(csm_fluxes[[x]][org_type_csm == "animal" ,]))
  # detr_csm -- need a way to identify detritivores
  detr_csm[[x]] = sum(rowSums(csm_fluxes[[x]][org_type_csm == "detritus",]))
  # para_csm
  para_csm[[x]] = sum(rowSums(csm_fluxes[[x]][org_type_csm == "para",]))
  # total_csm
  total_csm[[x]] = sum(csm_fluxes[[x]])
}

csm_dat <- 
  cbind(herb_csm, carn_csm, detr_csm, para_csm, total_csm) %>% 
  as_tibble %>% 
  gather(key = interaction, value = flux)

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
  plants = basal
  # herb_bsq -- should be true
  herb_bsq[[x]] = sum(rowSums(bsq_fluxes[[x]][plants,]))
  # carn_bsq -- wrong becase this includes carnivores and detritivores
  carn_bsq[[x]] = sum(rowSums(bsq_fluxes[[x]][org_type_bsq == "animal" ,]))
  # detr_bsq -- need a way to identify detritivores
  detr_bsq[[x]] = sum(rowSums(bsq_fluxes[[x]][org_type_bsq == "detritus",]))
  # para_bsq
  para_bsq[[x]] = sum(rowSums(bsq_fluxes[[x]][org_type_bsq == "para",]))
  # total_bsq
  total_bsq[[x]] = sum(bsq_fluxes[[x]])
}

bsq_dat <- 
  cbind(herb_bsq, carn_bsq, detr_bsq, para_bsq, total_bsq) %>% 
  as_tibble %>% 
  gather(key = interaction, value = flux)

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
  plants = basal
  # herb_epb -- should be true
  herb_epb[[x]] = sum(rowSums(epb_fluxes[[x]][plants,]))
  # carn_epb -- wrong becase this includes carnivores and detritivores
  carn_epb[[x]] = sum(rowSums(epb_fluxes[[x]][org_type_epb == "animal" ,]))
  # detr_epb -- need a way to identify detritivores
  detr_epb[[x]] = sum(rowSums(epb_fluxes[[x]][org_type_epb == "detritus",]))
  # para_epb
  para_epb[[x]] = sum(rowSums(epb_fluxes[[x]][org_type_epb == "para",]))
  # total_epb
  total_epb[[x]] = sum(epb_fluxes[[x]])
}

epb_dat <- 
  cbind(herb_epb, carn_epb, detr_epb, para_epb, total_epb) %>% 
  as_tibble %>% 
  gather(key = interaction, value = flux)

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
plot_grid(bsq, csm, epb, ncol = 1)

