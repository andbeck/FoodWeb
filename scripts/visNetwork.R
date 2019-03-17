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