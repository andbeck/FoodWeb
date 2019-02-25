
# Start -------------------------------------------------------------------

library(tidyverse)
library(igraph)
library(fluxweb)
library(cheddar)

rm(list = ls())
set.seed(1)


# Ythan Estuary  ----------------------------------------------------------

data("YthanEstuary")

par(mfrow = c(1,2))
PlotMvB(YthanEstuary)
PlotMvN(YthanEstuary)
par(mfrow=c(1,1))

#NodePropertyNames(YthanEstuary)
#colour.cat <- 
#PlotRankNPS(YthanEstuary, 'Log10N', rank.by = 'M', show.nodes.as = 'labels',
#            node.labels = 'category', symbol.by = 'category')
#legend("topright", legend = names(colour.spec), pch = 19, col = colour.spec)

PlotNPS(YthanEstuary, 'Log10M', 'Log10N', highlight.nodes = ResourceLargerThanConsumer(YthanEstuary))

###### Trying to plot nodes by 

YthanEstuary$nodes$category
symbol.cat = c(vert.endo = 21, vert.ecto = 22, invertebrate = 23,
                producer = 24)
colour.cat = c(vert.endo = 'blue', vert.ecto = 'green', invertebrate = 'orange',
               producer = 'red')

PlotMRvMC(YthanEstuary,
          symbol.by = 'category', symbol.cat = symbol.cat, 
          bg.by = 'category', colour.cat = colour.cat)
legend("topright", legend = names(colour.cat), pch = symbol.cat,
       col = colour.cat, pt.bg = colour.cat) # disaster :(


par(mfrow=c(3,1))
PlotNDistribution(YthanEstuary)
PlotMDistribution(YthanEstuary)
PlotBDistribution(YthanEstuary)
par(mfrow=c(1,1))

par(mfrow=c(2,2))
PlotPredationMatrix(YthanEstuary)
PlotMRvMC(YthanEstuary)
PlotNCvNR(YthanEstuary)
PlotBRvBC(YthanEstuary)
par(mfrow=c(1,1))

biomass_by_class <- SumBiomassByClass(YthanEstuary, na.rm = TRUE)
class(biomass_by_class)


