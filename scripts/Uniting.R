library(tidyverse)
library(cheddar)

rm(list=ls())
set.seed(1)

#-----------
# try and load in the Otago data set and clean it up
nodes_Otago <- read.csv("./data/interactionwebdb/Otago/Otago_Data_Nodes.csv")
colnames(nodes_Otago)

# remove all columns with only NA values using select_if()
nodes_Otago <- nodes_Otago %>% select_if(~sum(!is.na(.)) > 0)
colnames(nodes_Otago)

# we need to consolidate species names so we can then 
# input mass and abundance data for the right species

FullName <- nodes_Otago %>% unite("FullName", c("Kingdom","Phylum",
                                    "Class","Order",
                                    "Family","Genus")) %>% 
  select(FullName, WorkingName)

FullName
print(FullName)
