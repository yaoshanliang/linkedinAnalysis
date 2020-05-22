
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
g <- graph_from_data_frame(edges, vertices=nodes, directed=FALSE)
plot(g)

# 柱状图
# png("1.png", width=1000, height=500)
# # plot(jobs$jobTitle)
# # plot(jobs$industry)
# plot(jobs$seniorityLevel)
# dev.off()
