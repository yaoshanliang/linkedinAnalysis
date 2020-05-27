
update.packages()

if (!require(igraph)) install.packages("igraph")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(dplyr)) install.packages("dplyr")

install.packages("sna")
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.11")
BiocManager::install("graph")
BiocManager::install("RBGL")

source("http://bioconductor.org/biocLite.R") 
biocLite("graph")

library(igraph)
library(ggplot2)
library(dplyr)
library(graph)
library(networkR)
library(sna)
library(RBGL)

print(getwd())


library(igraph)

# Read files
positions <- read.csv("data/sna_positions.csv", header = T)
relations <- read.csv("data/sna_edges.csv", header = T)

# Construct the network
nodes <- data.frame(positions[, 1])
edges <- data.frame(from = relations[, 1], to = relations[, 2])
net <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
plot(net, vertex.label.cex = 0.5, vertex.size = 10,
    main="Network visualization of the positions")
    

plot.igraph(net, vertex.label.cex = 0.5, vertex.size = 10)

# Basic info of dataset
str(positions)
head(positions)
attributes(positions)
ncol(positions)
nrow(positions)
summary(positions)

# =============================Structural Analysis==============================
library(igraph)
positions <- read.csv("data/sna_positions.csv", header = T)
relations <- read.csv("data/sna_edges.csv", header = T)

# Construct the network
nodes <- data.frame(positions[, 1])
edges <- data.frame(from = relations[, 1], to = relations[, 2])
net <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

# Degree, closeness, betweenness
degree <- degree(net)
closeness <- closeness(net)
betweenness <- betweenness(net)


# Node degree
deg <- degree(net)
deg_dataframe <- as.data.frame(deg)
deg_dataframe$node <- row.names(deg_dataframe)
top10_nodes_degree <- deg_dataframe %>% select(node, deg) %>%
    arrange(desc(deg)) %>% head(10)
top10_nodes_degree

# Plot node degree distribution
ggplot(data = top10_nodes_degree, aes(x = reorder(node, deg), y = deg)) +
    geom_histogram(stat = "identity") + coord_flip()

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

# =============================Link Analysis===================================
library(igraph)
library(sna)
positions <- read.csv("data/sna_positions.csv", header = T)
relations <- read.csv("data/sna_edges.csv", header = T)

# Construct the network
nodes <- data.frame(positions[, 1])
edges <- data.frame(from = relations[, 1], to = relations[, 2])
net <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

# Density, connectedness
# edge_density(net)
gden(as.matrix(get.adjacency(net)))
connectedness(as.matrix(get.adjacency(net)))

ego95 = ego.extract(as.matrix(get.adjacency(net)), 95)
ego43 = ego.extract(as.matrix(get.adjacency(net)), 43)
ego42 = ego.extract(as.matrix(get.adjacency(net)), 42)
ego81 = ego.extract(as.matrix(get.adjacency(net)), 81)
ego74 = ego.extract(as.matrix(get.adjacency(net)), 74)
gden(ego95)
gden(ego43)
gden(ego42)
gden(ego81)
gden(ego74)
connectedness(ego95)
connectedness(ego43)
connectedness(ego42)
connectedness(ego81)
connectedness(ego74)

# =============================Community Detection==============================
# 1）Louvain method for community detection
clv <- cluster_louvain(net)
crossing(clv, net)
plot(clv, net, vertex.label.cex = 0.5, vertex.size = 10,
    main="Louvain method for community detection")

# 2）Community detection based on edge betweenness
ceb <- cluster_edge_betweenness(net) 
plot(ceb, net, vertex.label.cex = 0.5, vertex.size = 10,
    main="Community detection based on edge betweenness")

# 3）Community detection based on spread labels
clp <- cluster_label_prop(net)
plot(clp, net, vertex.label.cex = 0.5, vertex.size = 10,
    main="Community detection based on spread labels")


# =============================Graph Cluster Analysis==============================
library(igraph)
library(RBGL)

positions <- read.csv("data/sna_positions.csv", header = T)
relations <- read.csv("data/sna_edges.csv", header = T)

# Construct the network
nodes <- data.frame(positions[, 1])
edges <- data.frame(from = relations[, 1], to = relations[, 2])
net <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

# Graph cluster by HCS
source("algorithm/HCSClustering.R")
HCSClustering(net, kappa = 2)

# Graph cluster by k-means
library("kernlab")
lapKern = laplacedot(sigma = 1)
adj <- as.matrix(get.adjacency(net))
K = kernelMatrix(lapKern, adj)
kmeans(K, 3)


class(clv)
length(clv) # number of communities
membership(clv) # community membership for each node
modularity(clv) # how modular the graph partitioning is
crossing(clv, net)

# Graph cluster by SNN
source("algorithm/SNN_Clustering.R")
SNN_Clustering(net, 2)

# Graph cluster by maximalCliqueEnumerator
source("algorithm/maximalCliqueEnumerator.R")
maximalCliqueEnumerator(net, 3)



# Graph cluster by HCS
source("HCSClustering.R")
HCSClustering(net, kappa=2)

# =============================Frequent Skills==============================
library(wordcloud2)
library(dplyr)
library(textfeatures)

# Read the position description
description = readLines('data/position_description.txt')
head(description)

txt = description[description != ""]
txt = tolower(txt)
txtList = lapply(txt, strsplit, " ")
txtChar = unlist(txtList)
# clean symbol(.,!:;?)
txtChar = gsub("\\.|,|\\!|:|;|\\?", "", txtChar)
txtChar = txtChar[txtChar != ""]
data = as.data.frame(table(txtChar))
colnames(data) = c("Word", "freq")
ordFreq = data[order(data$freq,decreasing=T),]

# Filter the stopwords
df = read.csv('data/stopwords.csv', header = T)
Word = select(df,Word)
antiWord = data.frame(Word, stringsAsFactors = F)
# ordFreq - antiWord
result = anti_join(ordFreq, antiWord, by = "Word") %>% arrange(desc(freq)) 

result = result[1:50,]
head(result, 20)

# Draw graph
wordcloud2(data=result, size=1)

# =============================All Data From database==============================
if (!require(RMySQL)) install.packages("RMySQL")
library("RMySQL");

# Create a connection Object to MySQL database.
mysqlconnection = dbConnect(MySQL(), user = 'root', password = '', dbname = 'linkedin', host = 'iat.net.cn')
result = dbSendQuery(mysqlconnection, "select `id` from social_positions")

allPositions = fetch(result, -1)
nrow(allPositions)

