
# using netInd and Igraph to generate trophic level layout ----------------
library(igraph)
library(NetIndices)


# read in data
CSM_links <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Links.csv")
CSM_nodes <- read.csv("data/interactionwebdb/Carpinteria/CSMweb_Nodes.csv")

# colnames
colnames(CSM_links)
colnames(CSM_nodes)

# get the edgelist and turn into graph object
CSM_graph <- graph.edgelist(as.matrix(CSM_links[,5:6]))

# do the same but jsut for predator prey - filter first
CSM_graph_pp <- CSM_links %>% # these linktypeID's are in metadata
  filter(!str_detect(LinkTypeID, '4|5|6|8|9|12|14|15|16|19'))
CSM_graph_pp <- graph.edgelist(as.matrix(CSM_graph_pp[,5:6]))

# get the webs into a matrix form
CSM_adjmatrix <- get.adjacency(CSM_graph, sparse = F)
CSM_adjmatrix_pp <- get.adjacency(CSM_graph_pp, sparse = F)

# get basic network indices from the matrix with GenID()
ind_CSM <- GenInd(CSM_adjmatrix)
ind_CSM_pp <- GenInd(CSM_adjmatrix_pp)

# plot these two webs to see whats going on 
plot(CSM_graph)
plot(CSM_graph_pp)

####### need to remove the lifestage links present | or include them? 
####### Unsure ---

# netInd can help assign trophic levels
troph_CSM <- TrophInd(CSM_adjmatrix)
troph_CSM_pp <- TrophInd(CSM_adjmatrix_pp)


# now we can make a matrix that gives x and y values for each node
layout.matrix.1<-matrix(
  nrow=length(V(CSM_graph)),  # Rows equal to the number of vertices
  ncol=2
)
layout.matrix.1[,1]<-runif(length(V(CSM_graph))) # randomly assign along x-axis
layout.matrix.1[,2]<-troph_CSM$TL # y-axis value based on trophic level

layout.matrix.1p<-matrix(
  nrow=length(V(CSM_graph_pp)),  # Rows equal to the number of vertices
  ncol=2
)
layout.matrix.1p[,1]<-runif(length(V(CSM_graph_pp)))
layout.matrix.1p[,2]<-troph_CSM_pp

# VIEW!!!!
par(mfrow=c(1,2))
plot.igraph(CSM_graph,
            vertex.label.cex=.35,
            vertex.size=3,
            edge.arrow.size=.25,
            layout=layout.matrix.1)
plot.igraph(CSM_graph_pp,
            vertex.label.cex=.35,
            vertex.size=3,
            edge.arrow.size=.25,
            layout=layout.matrix.1)
par(mfrow=c(1,1))
# kinda works.....


# lets look at how community membership changes..
wtc_CSM <- walktrap.community(CSM_graph)
wtc_CSM_pp <- walktrap.community(CSM_graph_pp)

par(mfrow=c(1,2))
plot.igraph(CSM_graph,
            vertex.label.cex=.35,
            vertex.size=3,
            edge.arrow.size=.25,
            layout=layout.matrix.1,
            mark.groups = wtc_CSM$membership,
            mark.col =  "green")
plot.igraph(CSM_graph_pp,
            vertex.label.cex=.35,
            vertex.size=3,
            edge.arrow.size=.25,
            layout=layout.matrix.1,
            mark.groups = wtc_CSM_pp$membership,
            mark.col = "red")
par(mfrow=c(1,1))
# membership for both these webs are insanely high and memebrship doest work

# lets look at degree distribution
deg_CSM <- degree(CSM_graph)
deg_CSM_pp <- degree(CSM_graph_pp)

# using degree distribution gives us a better way to visualise changes
# in degree tells us the generality of consumer diets
indeg_CSM <- degree(CSM_graph, mode = "in")
indeg_CSM_pp <- degree(CSM_graph_pp, mode = "in")


# out degree is a measure of vulnrability of organisms
outdeg_CSM <- degree(CSM_graph, mode = "out")
outdeg_CSM_pp <- degree(CSM_graph_pp, mode = "out")

# "all" degree shows us the connectedness of a network 
alldeg_CSM <- degree(CSM_graph, mode = "all")
alldeg_CSM_pp <- degree(CSM_graph_pp, mode = "all")

# plot out/in deg for para vs no para
par(mfrow=c(2,2))
plot(indeg_CSM,xlim=c(0,80))
plot(outdeg_CSM,xlim=c(0,80))
plot(indeg_CSM_pp,xlim=c(0,80))
plot(outdeg_CSM_pp,xlim=c(0,80))

power.fit<-power.law.fit(deg_CSM)
power.fit.p<-power.law.fit(deg_CSM_pp)

par(mfrow=c(1,2))
plot(CSM_graph,log="xy")
lines(1:180,10*(1:180)^((-power.fit$alpha)+1))

plot(CSM_graph_pp,log="xy")
lines(1:100,10*(1:100)^((-power.fit.p$alpha)+1))

# motif analysis
# Here are the adjacency matrices for each of the 13 subgraphs again
s1<-matrix(c(0,1,0,0,0,1,0,0,0),nrow=3,ncol=3)
s2<-matrix(c(0,1,1,0,0,1,0,0,0),nrow=3,ncol=3)
s3<-matrix(c(0,1,0,0,0,1,1,0,0),nrow=3,ncol=3)
s4<-matrix(c(0,0,1,0,0,1,0,0,0),nrow=3,ncol=3)
s5<-matrix(c(0,1,1,0,0,0,0,0,0),nrow=3,ncol=3)
d2<-matrix(c(0,1,1,1,0,1,0,0,0),nrow=3,ncol=3)
d1<-matrix(c(0,1,1,0,0,1,0,1,0),nrow=3,ncol=3)
d3<-matrix(c(0,0,1,1,0,0,1,0,0),nrow=3,ncol=3)
d4<-matrix(c(0,0,0,1,0,1,0,1,0),nrow=3,ncol=3)
d5<-matrix(c(0,1,1,0,0,1,1,0,0),nrow=3,ncol=3)
d6<-matrix(c(0,1,1,1,0,1,1,1,0),nrow=3,ncol=3)
d7<-matrix(c(0,1,1,1,0,1,1,0,0),nrow=3,ncol=3)
d8<-matrix(c(0,1,1,1,0,0,1,0,0),nrow=3,ncol=3)

# Turn them into a convenient list
subgraph3.mat<-list(s1,s2,s3,s4,s5,d1,d2,d3,d4,d5,d6,d7,d8)
# And then into a list of graph objects
subgraph3.graph<-lapply(subgraph3.mat,graph.adjacency)

# Count the number of the 13 different 3-node subgraphs in the two webs
subgraph_freq_CSM<-c()
subgraph_freq_CSM_pp<-c()
for(i in 1:13){
  subgraph_freq_CSM [i]<-
    graph.count.subisomorphisms.vf2(CSM_graph,subgraph3.graph[[i]])
  subgraph_freq_CSM_pp[i]<-
    graph.count.subisomorphisms.vf2(CSM_graph_pp,subgraph3.graph[[i]])
}

par(mfrow=c(1,1))
plot(subgraph_freq_CSM,type="o",lty=3, xlab="Subgraph",ylab="Frequency")
points(subgraph_freq_CSM_pp,type="o",lty=2)

plot(subgraph_freq_CSM~subgraph_freq_CSM_pp)
abline(a=0,b=1)

