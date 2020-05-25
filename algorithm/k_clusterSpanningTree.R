k_clusterSpanningTree <-
function(igraphObj,k=2)
{
 
if(class(igraphObj)=="list") {

igraphObjNew = graph.edgelist(t(igraphObj$edgeList),directed=FALSE)
E(igraphObjNew)$weight = igraphObj$weights
            igraphObj = igraphObjNew

}

lay <- layout.reingold.tilford(igraphObj, mode="all")
plot(igraphObj, layout=lay,vertex.label=V(igraphObj)$name,edge.label=paste(E(igraphObj)$weight),edge.label.cex=3,vertex.label.cex=1,main="Original Spanning Tree")

J = sort(E(igraphObj)$weight, index.return=TRUE,decreasing=TRUE)
Index_edges = J$ix[1:k-1]
      Index_edges=Index_edges-1
Edges_removed = E(MST_PRIM)[Index_edges]
ouputGraph = delete.edges(igraphObj,Edges_removed)
lay <- layout.reingold.tilford(ouputGraph, mode="all")
plot(ouputGraph, layout=lay,vertex.label=V(ouputGraph)$name,edge.label=paste(E(ouputGraph)$weight),edge.label.cex=1,vertex.label.cex=2,main="k Clusters of Spanning Tree")
Clusters = clusters(ouputGraph)
cluslist <- data.frame(1:length(V(ouputGraph)), rep(0,length(V(ouputGraph)))) 
names(cluslist) <- c('VertexID','ClusterID');
for(i in 1:length(V(ouputGraph))) {
     cluslist[ i, ]$ClusterID <-Clusters$membership[i] 

}

return(list(cluslist,ouputGraph))
#return(ouputGraph)

}

