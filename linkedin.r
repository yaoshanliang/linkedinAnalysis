
update.packages()

if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")
if (!require(networkR)) install.packages("networkR")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.10")

source("http://bioconductor.org/biocLite.R") 
biocLite("graph")


library(igraph)
library(ggplot2)
library(dplyr)
library(networkR)
library(graph)


print(getwd())

# Read files
allPositions <- read.csv("data/sna_positions_all.csv", header = T)
positions <- read.csv("data/sna_positions.csv", header = T)
relations <- read.csv("data/sna_edges.csv", header = T)

# Basic info of dataset
str(positions)
head(positions)
attributes(positions)
ncol(positions)
nrow(positions)
summary(positions)

# Bar chart
# png("1.png", width=1000, height=500)
# # plot(allPositions$jobTitle)
# # plot(allPositions$industry)
# plot(allPositions$seniorityLevel)
# dev.off()

# Plot the network
nodes <- data.frame(positions[, 1])
edges <- data.frame(from = relations[, 1], to = relations[, 2])
net <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
# plot(net)
plot.igraph(net, vertex.size = 1, vertex.label.cex = 0.6, main = "SNA")


# =============================Structural Analysis==============================
# Node degree
deg <- degree(net, mode="all")
deg_dataframe <- as.data.frame(deg)
deg_dataframe$node <- row.names(deg_dataframe)
top10_nodes_degree <- deg_dataframe %>% select(node, deg) %>% arrange(desc(deg)) %>% head(10)
top10_nodes_degree

# Plot node degree distribution
ggplot(data = top10_nodes_degree, aes(x = reorder(node, deg), y = deg)) + geom_histogram(stat = "identity") + coord_flip()

# Degree centrality
edge_density(net)
centr_degree(net, mode="all", normalized=T)

# Closeness centrality
closeness(net, mode="all", weights=NA) 
centr_clo = centr_clo(net, mode="all", normalized=T)

closeness_dataframe <- as.data.frame(centr_clo)
closeness_dataframe$position <- nodes
top10_nodes_closeness <- closeness_dataframe %>%
  select(position, res) %>%
  arrange(desc(res)) %>%
  head(10)
top10_nodes_closeness

# Eigenvector centrality
eigen_centrality = eigen_centrality(net, directed=F, weights=NA) 
centr_eigen(net, directed=F, normalized=T)

eigen_centrality_dataframe<-data.frame(eigen_centrality)
eigen_centrality_dataframe$position<-row.names(eigen_centrality_dataframe)
top10_nodes_eigen_centrality <- eigen_centrality_dataframe %>%
  select(position, vector, value) %>%
  arrange(desc(vector)) %>%
  head(10)
top10_nodes_eigen_centrality

# Betweenness centrality
betweenness(net, directed=T, weights=NA) 
edge_betweenness(net, directed=T, weights=NA) 
centr_betw(net, directed=T, normalized=T)


deg =degree(net, mode="all") 
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")
deg.dist <- degree_distribution(net, cumulative=T, mode="all") 
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")



# =============================Community Detection==============================
clv <- cluster_louvain(net)
class(clv)
length(clv) # number of communities
membership(clv) # community membership for each node
modularity(clv) # how modular the graph partitioning is
crossing(clv, net)
plot(clv, net,vertex.label.cex=0.5, vertex.size=10, main="CLV")


# =============================Link Analysis===================================
hub_score(net, weights=NA)$vector
as <- authority_score(net, weights=NA)$vector
par(mfrow=c(1,2)) 
plot(net, vertex.size=hs*50, main="Hubs",edge.arrow.size=.1)
plot(net, vertex.size=as*30, main="Authorities",edge.arrow.size=.1)
#get the PageRank
page.rank(net)
#get the HITS
source("HITS.R")
HITS(net,2)
cliques(net) # list of cliques
sapply(cliques(net), length) # clique sizes
largest_cliques(net) #find the largest clique
net2 <- as.undirected(net, mode= "collapse")
vcol <- rep("grey80", vcount(net2)) 
vcol[unlist(largest_cliques(net2))] <- "gold" 
plot(as.undirected(net2), vertex.color=vcol)


# =============================Proximity Mearsure==============================
adj<-as.matrix(get.adjacency(net))
g<-graph.adjacency(adj)
plot(g,edge.arrow.size=.1)
source("SNN_GRAPH.R") 
SNN_output<-SNN_GRAPH(adj,2)
SNN_output
SNN_output_ <- as.data.frame(SNN_output[[2]])


# =============================Graph Cluster Analysis==============================
# Graph cluster by HCS
source("HCSClustering.R")
HCSClustering(net)

# Graph cluster by SNN
source("SNN_Clustering.R")
SNN_Clustering(net, 2)

# Graph cluster by maximalCliqueEnumerator
source("maximalCliqueEnumerator.R")
maximalCliqueEnumerator(net)

# Graph cluster by k-means
lapKern = laplacedot(sigma = 1)
K = kernelMatrix(lapKern,adj)
kmeans(K, 3)

