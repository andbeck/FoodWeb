# Start -------------------------------------------------------------------
rm(list=ls())
library(tidyverse)
library(ggforce)
library(igraph)
library(fluxweb)
library(cheddar)
library(ggfortify)
library(ggthemes)
library(ggsci)


source(file = "scripts/CSM.R")
source(file = "scripts/BSQ.R")
source(file = "scripts/EPB.R")

# ggplot ------------------------------------------------------------------

ggplot(NULL, aes(x = log10(M), y = log10(N), colour = functional.group,
                 fill = functional.group, group = functional.group)) +
  geom_point(data = CSMlite$nodes, shape = 16) +
  geom_point(data = EPBlite$nodes, shape = 17) +
  geom_point(data = CSMlite$nodes, shape = 23) +
  facet_wrap(~ functional.group) + 
  theme_solarized()
 
# create site variable
CSMplot <- CSMlite$nodes
CSMplot$site <- "CSM"

BSQplot <- BSQlite$nodes
BSQplot$site <- "BSQ"

EPBplot <- EPBlite$nodes
EPBplot$site <- "EPB"

carpinteria <-  rbind.data.frame(CSMplot, EPBplot, BSQplot)

# sites plotted together
p1 <- ggplot(carpinteria, aes(x = log10(M), y = log10(N), colour = functional.group,
                        fill = functional.group, shape = site)) +
  geom_point(cex = 3, alpha = .7) +
  geom_smooth(method = "lm" , se = FALSE, cex = 1, aes(group = functional.group)) +
  # facet_wrap(~ site) +
  theme_minimal() +
  # labs(title = "Carpinteria Nodes")
p1
  # scale_color_ucscgb()
  

# faceted ~ sites
p2 <- ggplot(carpinteria, aes(x = log10(M), y = log10(N), colour = functional.group,
                              fill = functional.group), alpha = .5) +
  geom_point(cex = 3, alpha = .7) +
  # geom_smooth(method = "lm" , se = FALSE, cex = 1, aes(group = functional.group)) +
  facet_wrap(~ site, nrow = 2, ncol = 2) +
  theme_minimal() +
  theme(legend.position="none") +
  stat_ellipse()
p2

plot_grid(p1, p2, ncol = 1) 

ggplot(carpinteria, aes(x = log10(M), y = log10(N), colour = functional.group, fill = functional.group)) +
  # geom_point(cex = 3, alpha = .7) +
  stat_ellipse(geom = "polygon", alpha = .5)

