
# Start -------------------------------------------------------------------

library(tidyverse)
library(tidygraph)
library(igraph)
library(cheddar)

# need to laod the scripts that convert data files to cheddar graphs
source(file = "scripts/CSM.R")
source(file = "scripts/BSQ.R")
source(file = "scripts/EPB.R")

# contains functions to delete isolate nodes and turn cheddar to igraph
source(file = "scripts/ExportIgraph.R")

# igrpah objects of the cheddar graphs containing only complete cases
CSM.ig <- ToIgraph(CSMlite)
BSQ.ig <- ToIgraph(BSQlite)
EPB.ig <- ToIgraph(EPBlite)

# put into a list
carp <- as.list(CSM.ig, BSQ.ig, EPB.ig)

# motif counting
motifs(CSM.ig, size = 3)
count_motifs(CSM.ig, size = 3)
sample_motifs(CSM.ig, size = 3)

# converting to long format dataframe
CSM.ig.df <-  as_long_data_frame(CSM.ig)
EPB.ig.df <-  as_long_data_frame(EPB.ig)
BSQ.ig.df <-  as_long_data_frame(BSQ.ig)

# quick plot
l <- layout_on_sphere(CSM.ig)
simplify(CSM.ig) %>% 
  plot(layout = l, rescale = FALSE)

tkplot(CSM.ig)


  


