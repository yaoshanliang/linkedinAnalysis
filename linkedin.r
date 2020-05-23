
update.packages()
if (!require(ggplot2)) install.packages("ggplot2")

library(igraph)
library(ggplot2)

print(getwd())

# Read files
jobs <- read.csv("data/sna_positions.csv", header = T)
relations <- read.csv("data/sna_edges.csv", header = T)

# Basic info of dataset
str(jobs)
head(jobs)
attributes(jobs)
ncol(jobs)
nrow(jobs)
summary(jobs)


nodes <- data.frame(jobs[,1])
edges <- data.frame(from=relations[,1],to=relations[,2])
net <- graph_from_data_frame(edges, vertices=nodes, directed=FALSE)
plot(net)

#Strucal Analysis
deg =degree(net, mode="all") 
plot(net, vertex.size=deg*3,edge.arrow.size=.1)
hist(deg, breaks=1:vcount(net)-1, main="Histogram of node degree")
deg.dist <- degree_distribution(net, cumulative=T, mode="all") 
plot( x=0:max(deg), y=1-deg.dist, pch=19, cex=1.2, col="orange", 
      xlab="Degree", ylab="Cumulative Frequency")

#get degree centrality
edge_density(net)
centr_degree(net, mode="all", normalized=T)
#closseness centrality
closeness(net, mode="all", weights=NA) 
centr_clo(net, mode="all", normalized=T)
#eigenvector centrality
eigen_centrality(net, directed=T, weights=NA) 
centr_eigen(net, directed=T, normalized=T)
#betweenness centrality
betweenness(net, directed=T, weights=NA) 
edge_betweenness(net, directed=T, weights=NA) 
centr_betw(net, directed=T, normalized=T)

# net <- cbind(relations[,1], relations[,2])
# one_mode = projecting_tm(net)

# 柱状图
# png("1.png", width=1000, height=500)
# # plot(jobs$jobTitle)
# # plot(jobs$industry)
# plot(jobs$seniorityLevel)
# dev.off()
