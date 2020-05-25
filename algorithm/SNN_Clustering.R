SNN_Clustering <-
function(igraph,tau=2,weighted=FALSE,phi=0.0,k=2){

if(weighted==TRUE)
{
adj_matrix = get.adjacency(igraph,attr="weight")
adj_matrix[adj_matrix<phi]=0
for(i in 1:nrow(adj_matrix)){
#print(i)
curr_node_row = adj_matrix[i,]
find_top_k = sort(curr_node_row,index.return=TRUE,decreasing=TRUE)
non_zero_values = length(which(find_top_k$x!=0))
find_top_k_non_zero = find_top_k$ix[1:non_zero_values] 
if(non_zero_values>=k)
{
top_k=k
}else
{
top_k=non_zero_values

}
adj_matrix[i,find_top_k_non_zero[1:top_k]]=1
adj_matrix[i,find_top_k_non_zero[top_k+1:nrow(adj_matrix)]]=0

}
}
else
{
adj_matrix = get.adjacency(igraph)
#print(adj_matrix)
}

SharedNearestGraph_WithThresholdApplied = SNN_GRAPH(adj_matrix,tau)
SNN_Clusters = clusters(SharedNearestGraph_WithThresholdApplied[[1]])
cluslist <- data.frame(1:length(V(igraph)), rep(0,length(V(igraph)))) 

names(cluslist) <- c('VertexID','ClusterID');
for(i in 1:length(V(igraph))) {
     cluslist[ i, ]$ClusterID <-SNN_Clusters$membership[i] 

}
cluslist$VertexID = cluslist$VertexID-1
SharedNearestGraph_WithThresholdApplied[[3]][,1]=SharedNearestGraph_WithThresholdApplied[[3]][,1]-1
SharedNearestGraph_WithThresholdApplied[[3]][,2]=SharedNearestGraph_WithThresholdApplied[[3]][,2]-1
SharedNearestGraph_WithThresholdApplied[[2]][,1]=SharedNearestGraph_WithThresholdApplied[[2]][,1]-1
SharedNearestGraph_WithThresholdApplied[[2]][,2]=SharedNearestGraph_WithThresholdApplied[[2]][,2]-1





return(list(Clusters=cluslist,"Shared Nearest Neighbor (SNN) Edge List" = SharedNearestGraph_WithThresholdApplied[[3]],"Edges from SNN that satisfy tau"=SharedNearestGraph_WithThresholdApplied[[2]]))
}

