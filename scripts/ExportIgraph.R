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


