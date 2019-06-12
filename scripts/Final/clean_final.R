### Clean Data
library(tidyverse)
library(janitor)


# -----------------------------------------------------------------------


### Load Nodes
csm_nodes <- 
  read_csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")
epb_nodes <- 
  read_csv("data/interactionwebdb/Carpinteria/EPBweb_Nodes.csv") %>% 
  clean_names %>% 
  remove_empty("cols")
bsq_nodes <- 
  read_csv("data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")

### Clean Nodes
c_nodes_wrk <- 
  csm_nodes %>% 
  # rename values and sort out SI units
  mutate(body_size_kg = body_size_g / 1000,
         working_name = make.unique(working_name, sep = "_")) %>% 
  # select the useful columns
  select(system,
         node_id,
         species_id,
         species_id_stage_id,
         working_name,
         organismal_group,
         node_type,
         lifestyle_stage,
         consumer_strategy_stage,
         body_size_kg,
         abundance_no_ha,
         biomass_kg_ha) %>% 
  # filter out pathogenic nodes
  filter(consumer_strategy_stage != "pathogen",
         consumer_strategy_stage != "parasitoid") %>% 
  # change characters to lower
  mutate_if(is.character, str_to_lower)

e_nodes_wrk <- 
  epb_nodes %>% 
  # rename values and sort out SI units
  mutate(body_size_kg = body_size_g / 1000,
         working_name = make.unique(working_name, sep = "_")) %>% 
  # select the useful columns
  select(system,
         node_id,
         species_id,
         species_id_stage_id,
         working_name,
         organismal_group,
         node_type,
         lifestyle_stage,
         consumer_strategy_stage,
         body_size_kg,
         abundance_no_ha,
         biomass_kg_ha) %>% 
  # filter out pathogenic and parasitoid nodes
  filter(consumer_strategy_stage != "pathogen",
         consumer_strategy_stage != "parasitoid") %>% 
  # change characters to lower
  mutate_if(is.character, str_to_lower)

b_nodes_wrk <- 
  bsq_nodes %>% 
  # rename values and sort out SI units
  mutate(body_size_kg = body_size_g / 1000,
         working_name = make.unique(working_name, sep = "_")) %>% 
  # select the useful columns
  select(system,
         node_id,
         species_id,
         species_id_stage_id,
         working_name,
         organismal_group,
         node_type,
         lifestyle_stage,
         consumer_strategy_stage,
         body_size_kg,
         abundance_no_ha,
         biomass_kg_ha) %>% 
  # filter out pathogenic and parasitoid nodes
  filter(consumer_strategy_stage != "pathogen",
         consumer_strategy_stage != "parasitoid") %>% 
  # change characters to lower
  mutate_if(is.character, str_to_lower)


# ---------------------------------------------------------------------


### Fill N * M = B values
c_nodes_fill <-
  c_nodes_wrk %>% 
  # mutate the values
  mutate(
    # M = B / N
    body_size_new = case_when(!is.na(abundance_no_ha & biomass_kg_ha) ~ biomass_kg_ha / abundance_no_ha),
    # N = B / M 
    abundance_no_ha_new = case_when(!is.na(body_size_kg & biomass_kg_ha) ~ biomass_kg_ha / body_size_kg),
    # B = N * M
    biomass_kg_ha_new = case_when(!is.na(body_size_kg & abundance_no_ha) ~ abundance_no_ha * body_size_kg)) %>% 
  # select the original values first then the calculated values in one column
  mutate(
    body_size_wrk = coalesce(body_size_kg, body_size_new),
    abundance_no_ha_wrk = coalesce(abundance_no_ha, abundance_no_ha_new),
    biomass_kg_ha_wrk = coalesce(biomass_kg_ha, biomass_kg_ha_new))

e_nodes_fill <-
  e_nodes_wrk %>% 
  # mutate the values
  mutate(
    # M = B / N
    body_size_new = case_when(!is.na(abundance_no_ha & biomass_kg_ha) ~ biomass_kg_ha / abundance_no_ha),
    # N = B / M 
    abundance_no_ha_new = case_when(!is.na(body_size_kg & biomass_kg_ha) ~ biomass_kg_ha / body_size_kg),
    # B = N * M
    biomass_kg_ha_new = case_when(!is.na(body_size_kg & abundance_no_ha) ~ abundance_no_ha * body_size_kg)) %>% 
  # select the original values first then the calculated values in one column
  mutate(
    body_size_wrk = coalesce(body_size_kg, body_size_new),
    abundance_no_ha_wrk = coalesce(abundance_no_ha, abundance_no_ha_new),
    biomass_kg_ha_wrk = coalesce(biomass_kg_ha, biomass_kg_ha_new))

b_nodes_fill <-
  b_nodes_wrk %>% 
  # mutate the values
  mutate(
    # M = B / N
    body_size_new = case_when(!is.na(abundance_no_ha & biomass_kg_ha) ~ biomass_kg_ha / abundance_no_ha),
    # N = B / M 
    abundance_no_ha_new = case_when(!is.na(body_size_kg & biomass_kg_ha) ~ biomass_kg_ha / body_size_kg),
    # B = N * M
    biomass_kg_ha_new = case_when(!is.na(body_size_kg & abundance_no_ha) ~ abundance_no_ha * body_size_kg)) %>% 
  # select the original values first then the calculated values in one column
  mutate(
    body_size_wrk = coalesce(body_size_kg, body_size_new),
    abundance_no_ha_wrk = coalesce(abundance_no_ha, abundance_no_ha_new),
    biomass_kg_ha_wrk = coalesce(biomass_kg_ha, biomass_kg_ha_new))


# -----------------------------------------------------------------------

### Clean Links

filtered_links <- 
  # bind rows
  bind_rows(
    # rows cut in csm
  filter(c_nodes_wrk,
         consumer_strategy_stage != "pathogen",
         consumer_strategy_stage != "parasitoid"),
    # rows cut in epb
  filter(e_nodes_wrk,
         consumer_strategy_stage != "pathogen",
         consumer_strategy_stage != "parasitoid"),
    # rows cut in bsq
  filter(b_nodes_wrk,
         consumer_strategy_stage != "pathogen",
         consumer_strategy_stage != "parasitoid"))

# filter
species_id_filter <- 
  filtered_links %>% 
  pull(species_id_stage_id)

# load the links
csm_links <- 
  read_csv("data/interactionwebdb/Carpinteria/CSMweb_Links.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")

epb_links <- 
  read_csv("data/interactionwebdb/Carpinteria/EPBweb_Links.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")

bsq_links <- 
  read_csv("data/interactionwebdb/Carpinteria/BSQweb_Links.csv") %>% 
  clean_names() %>% 
  remove_empty("cols")

# filter the links
c_links_wrk <- 
  csm_links %>% 
  filter(consumer_species_id_stage_id %in% species_id_filter)

e_links_wrk <- 
  epb_links %>% 
  filter(consumer_species_id_stage_id %in% species_id_filter)

b_links_wrk <- 
  bsq_links %>% 
  filter(consumer_species_id_stage_id %in% species_id_filter)


# - -----------------------------------------------------------------------


gdata::keep(c_nodes_fill, e_nodes_fill, b_nodes_fill,
            c_links_wrk, e_links_wrk, b_links_wrk,
            sure = TRUE)

  
