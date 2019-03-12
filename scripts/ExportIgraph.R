# exporting to iGraph? ----------------------------------------------------

library(igraph)
library(tidygraph)
library(visNetwork)

#### The ToIGraph function:
ToIgraph <- function(community, weight=NULL)
{
  if(is.null(TLPS(community)))
  {
    stop("The community has no trophic links")
  }
  else
  {
    tlps <- TLPS(community, link.properties = weight)
    if(!is.null(weight))
    {
      tlps$weight <- tlps[,weight]
    }
    return(graph.data.frame(tlps,
                            vertices = NPS(community),
                            directed = TRUE))
  }
}

### Delete isolated nodes function
delete.isolates <- function(graph, mode = 'all') {
  isolates <- which(degree(graph, mode = mode) == 0) - 1
  delete.vertices(graph, isolates)
}

# convert to Igraph object
CSM.ig <- ToIgraph(CSM_web)

# removing the isolated nodes using igraph
LO = layout_nicely(CSM.ig)
Isolated = which(degree(CSM.ig)==0)
CSMnew = delete.vertices(CSM.ig, Isolated)
LO2 = LO[-Isolated,]
plot(CSMnew, layout=LO2, vertex.size = 1, edge.size = 1)

# layout the new network and plot
lgrid <- layout_on_sphere(CSMnew)
# ceb <- cluster_edge_betweenness(G2)
# dendPlot(ceb, mode="hclust") # this is clustering detection for communities
plot(CSMnew, layout = lgrid, label = NA) # horrible layout



# tidygraph ---------------------------------------------------------------
library(tidygraph)
library(ggraph)

# convert to tidygraph object
graph <- as_tbl_graph(CSMnew)
graph

# test with previously made degree function plotting
degree <- mutate(graph, centrality = centrality_degree()) %>% 
  ggraph(layout = "kk") +
  geom_edge_link(colour = "lightgray") +
  scale_edge_width(range = c(1,6)) +
  geom_node_point(aes(size = centrality, colour = centrality)) + 
  scale_color_continuous(guide = 'legend') +
  labs(colour = "Degree", size = "Degree") +
  ggtitle("CSM Web Degree Centrality") +
  theme_graph(base_size = 15)
plot(degree)

ggraph(graph, layout = "lgl") +
  geom_edge_fan(alpha = 0.1) +
  theme_graph()

# playing around

V(graph)$functional.group <- as.factor(V(graph)$functional.group)
V(graph)$functional.group

# visNetwork --------------------------------------------------------------
library(visNetwork)

# converting from igraph object to vis network
network = visIgraph(CSM.ig, layout = "layout_on_sphere") %>%
  visLayout(hierarchical = F,
            improvedLayout = T)
network # cool beans
visSave(network, file = "CSM.html") # saves network as interactive HTML
layout
# glimpse(network$x)