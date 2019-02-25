library(cheddar)
library(tidyverse)


rm(list=ls())
set.seed(1)


##### Ythan Est ---------------------------------------------------------------

data("YthanEstuary")
glimpse(YthanEstuary) # list of 3: nodes, properties and trophiclinks
# all data needs to be in Cheddar community format

##### Bring in Carpinteria data ------------------------------------------------

nodes_CSM <- read_csv(file="./data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")
links_CSM <- read_csv(file="./data/interactionwebdb/Carpinteria/CSMweb_Links.csv")

nodes_BSQ <- read_csv(file="./data/interactionwebdb/Carpinteria/BSQweb_Nodes.csv")
links_BSQ <- read_csv(file="./data/interactionwebdb/Carpinteria/BSQweb_Links.csv")

nodes_EPB <- read_csv(file="./data/interactionwebdb/Carpinteria/EPBweb_Nodes.csv")
links_EPB <- read_csv(file="./data/interactionwebdb/Carpinteria/EPBweb_Links.csv")


##### Bring in Otago data ------------------------------------------------------

nodes_Otago <- read_csv(file="./data/interactionwebdb/Otago/Otago_Data_Nodes.csv")
links_Otago <- read_csv(file="./data/interactionwebdb/Otago/Otago_Data_Links.csv")

glimpse(links_Otago)
glimpse(nodes_Otago)

###### Bring in Sylt data ------------------------------------------------------

nodes_Sylt <- read_csv(file="./data/interactionwebdb/Sylt/Sylt_Data_Nodes.csv")
links_Sylt <- read_csv(file="./data/interactionwebdb/Sylt/Sylt_Data_Links.csv")

###### Bring in Flensburg data -------------------------------------------------

nodes_Flen <- read_csv(file="./data/interactionwebdb/Flensburg/Flensburg_Data_Nodes.csv")
links_Flen <- read_csv(file="./data/interactionwebdb/Flensburg/Flensburg_Data_Links.csv")


