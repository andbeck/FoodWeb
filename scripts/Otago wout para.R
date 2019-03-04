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

# slice only predator - prey links
nodes_Otago_pp <- filter(nodes_Otago, nodes_Otago$Lifestyle.species. == "Free-Living")
nodes_Otago_pp

#-----------
# try to remove links with parasites in
links_Otago <- read.csv("./data/interactionwebdb/Otago/Otago_Data_Links.csv")
colnames(nodes_Otago)

# remove all columns with only NA values
links_Otago <- links_Otago %>% select_if(~sum(!is.na(.)) > 0)
colnames(links_Otago)

# slice away any parasite links (nodes ≥ 123)
links_Otago_pp <- filter(links_Otago, links_Otago$ConsumerNodeID < 123 & links_Otago$ResourceNodeID < 123)
dim(links_Otago)
dim(links_Otago_pp)

# now try to create two communities - one with and one without parasites

nodes_wrk <- nodes_Otago %>% select(NodeID,WorkingName) %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))

# adjust the trophic links now.... ----
# replace all NodeID with appropriate Working Name

# step 1, get links
trophic.links <- links_Otago %>% select(ConsumerNodeID, ResourceNodeID)

# step 2: isolate each column and rename to match nodes NodeID column name
CNtemp <- trophic.links %>% select(ConsumerNodeID) %>% 
  rename(NodeID = ConsumerNodeID)

RNtemp <- trophic.links %>% select(ResourceNodeID) %>% 
  rename(NodeID = ResourceNodeID)

# step 3: use left_join to replace with names
# this is a giant look up table exercise
# the NodeID column is common to each data frame
# the unique name info in nodes_wrk is mapped onto the shared NodesID column
CN_name <- left_join(CNtemp, nodes_wrk)
RN_name <- left_join(RNtemp, nodes_wrk)

# # proof - check and see that the sequence of numbers (the shared bit) matches....
# head(left_join(CNtemp, nodes))
# head(CNtemp)

# step 4: rebuild trophic.links
trophic.links <- data.frame(ConsumerNodeID = CN_name$WorkingName,
                            ResourceNodeID = RN_name$WorkingName)
head(trophic.links)

trophic.links <- trophic.links %>% 
  rename(consumer = ConsumerNodeID, resource = ResourceNodeID)

# change nodes names
nodes <- nodes_wrk %>% rename(node = WorkingName) %>% 
  select(node)
head(nodes)

# properties
properties <-  list(title = "Otago with Parasites")
properties

# make community
otago_web_para <- Community(nodes = nodes, properties = properties, trophic.links = trophic.links)

# view it.
plot(otago_web_para)

#----------
# Now we need to do the same for the web without parasites

nodes_wrk <- nodes_Otago_pp %>% select(NodeID,WorkingName) %>% 
  mutate(WorkingName = make.unique(as.character(WorkingName), sep = "_"))

# adjust the trophic links now.... ----
# replace all NodeID with appropriate Working Name

# step 1, get links
trophic.links <- links_Otago_pp %>% select(ConsumerNodeID, ResourceNodeID)

# step 2: isolate each column and rename to match nodes NodeID column name
CNtemp <- trophic.links %>% select(ConsumerNodeID) %>% 
  rename(NodeID = ConsumerNodeID)

RNtemp <- trophic.links %>% select(ResourceNodeID) %>% 
  rename(NodeID = ResourceNodeID)

# step 3: use left_join to replace with names
# this is a giant look up table exercise
# the NodeID column is common to each data frame
# the unique name info in nodes_wrk is mapped onto the shared NodesID column
CN_name <- left_join(CNtemp, nodes_wrk)
RN_name <- left_join(RNtemp, nodes_wrk)

# # proof - check and see that the sequence of numbers (the shared bit) matches....
# head(left_join(CNtemp, nodes))
# head(CNtemp)

# step 4: rebuild trophic.links
trophic.links <- data.frame(ConsumerNodeID = CN_name$WorkingName,
                            ResourceNodeID = RN_name$WorkingName)
head(trophic.links)

trophic.links <- trophic.links %>% 
  rename(consumer = ConsumerNodeID, resource = ResourceNodeID)

# change nodes names
nodes <- nodes_wrk %>% rename(node = WorkingName) %>% 
  select(node)
head(nodes)

# properties
properties <-  list(title = "Otago no Parasites")
properties

# make community
otago_web_pp <- Community(nodes = nodes, properties = properties, trophic.links = trophic.links)

# view it.
plot(otago_web_pp)

# view side by side
par(mfrow=c(1,2))
plot(otago_web_para)
plot(otago_web_pp)
