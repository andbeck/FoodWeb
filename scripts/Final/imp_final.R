### Imputation
library(tidyverse)
library(mice)

source("scripts/Final/clean_final.R")


# - -----------------------------------------------------------------------

imp_csm <- 
  c_nodes_fill %>% 
  mutate(log_body_size = log(body_size_wrk),
         log_biomass = log(biomass_kg_ha_wrk),
         log_abundance = case_when(abundance_no_ha_wrk > 0 ~ log(abundance_no_ha_wrk))) %>%  
  select(log_body_size, log_abundance, log_biomass)

imp_epb <- 
  e_nodes_fill %>% 
  mutate(log_body_size = log(body_size_wrk),
         log_biomass = log(biomass_kg_ha_wrk),
         log_abundance = case_when(abundance_no_ha_wrk > 0 ~ log(abundance_no_ha_wrk))) %>% 
  select(log_body_size, log_abundance, log_biomass)

imp_bsq <- 
  b_nodes_fill %>% 
  mutate(log_body_size = log(body_size_wrk),
         log_biomass = log(biomass_kg_ha_wrk),
         log_abundance = case_when(abundance_no_ha_wrk > 0 ~ log(abundance_no_ha_wrk))) %>%  
  select(log_body_size, log_abundance, log_biomass)

# mice setup --------------------------------------------------------------

meth <- make.method(imp_csm)

pred <- make.predictorMatrix(imp_csm)

meth["log_body_size"] <- "pmm"
meth["log_abundance"] <- "pmm"
meth["log_biomass"] <- "~I(log_body_size + log_abundance)" # addition of logs is the same as multiplying exponent of log
pred[c("log_body_size", "log_abundance"), "log_biomass"] <-  0

# imputation --------------------------------------------------------------

c_imputed <- 
  mice(imp_csm, meth = meth, predictorMatrix = pred, m = 50, maxit = 100, printFlag = F)

e_imputed <-
  mice(imp_epb, meth = meth, predictorMatrix = pred, m = 50, maxit = 100, printFlag = F)

b_imputed <-
  mice(imp_bsq, meth = meth, predictorMatrix = pred, m = 50, maxit = 100, printFlag = F)


# pool --------------------------------------------------------------------

c_fit <- with(data = c_imputed, expr = lm(log_body_size ~ log_abundance))
c_sum <- as.data.frame(summary(pool(c_fit)))

e_fit <- with(data = e_imputed, expr = lm(log_body_size ~ log_abundance))
e_sum <- as.data.frame(summary(pool(e_fit)))

b_fit <- with(data = b_imputed, expr = lm(log_body_size ~ log_abundance))
b_sum <- as.data.frame(summary(pool(b_fit)))

# complete ----------------------------------------------------------------

c_complete <- mice::complete(c_imputed, 'long')
c_list <- split(c_complete, f = c_complete$.imp)

e_complete <- mice::complete(e_imputed, 'long')
e_list <- split(e_complete, f = e_complete$.imp)

b_complete <- mice::complete(b_imputed, 'long')
b_list <- split(b_complete, f = b_complete$.imp)

# colnames wanted ---------------------------------------------------------

df_nameswant <- c("species_id_stage_id",
                  "body_size",
                  "abundance",
                  "node_type",
                  "working_name",
                  "organismal_group",
                  "consumer_strategy_stage")



# map() ---------------------------------------------------------------

### csm

c_list_bound <- map(c_list, cbind, c_nodes_fill)

c_list_mutate <- map(c_list_bound, mutate,
                     # name the mutate variables
                     body_size = exp(log_body_size), 
                     abundance = exp(log_abundance))

c_list_select <- map(c_list_mutate, select, df_nameswant)

### epb

e_list_bound <- map(e_list, cbind, e_nodes_fill)

e_list_mutate <- map(e_list_bound, mutate,
                     # name the mutate variables
                     body_size = exp(log_body_size), 
                     abundance = exp(log_abundance))

e_list_select <- map(e_list_mutate, select, df_nameswant)

### bsq

b_list_bound <- map(b_list, cbind, b_nodes_fill)

b_list_mutate <- map(b_list_bound, mutate,
                     # name the mutate variables
                     body_size = exp(log_body_size), 
                     abundance = exp(log_abundance))

b_list_select <- map(b_list_mutate, select, df_nameswant)

# keep --------------------------------------------------------------------

gdata::keep(c_list_select, e_list_select, b_list_select, # complete list of 50 data sets combined
            c_links_wrk, e_links_wrk, b_links_wrk, # links needed for matrix
            sure = TRUE)
