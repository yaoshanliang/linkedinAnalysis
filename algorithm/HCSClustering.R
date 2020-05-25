HCSClustering <-
function(G,weighted=FALSE,kappa=3,gamma=0.2)
{
if(class(G)[1]=="igraph")
{
K = igraph.to.graphNEL(G)
G = K
}

if(weighted==TRUE)
{
edgeM = edgeMatrix(G)
edgeW =eWV(G,eM=edgeM)
Index = which(edgeW==edgeW[edgeW<gamma])
for(i in Index){
#print (i)
from = paste(edgeM[1,i])
to = paste(edgeM[2,i])
G = removeEdge(from,to,G)
}

}
GWithOutSelfLoops = removeSelfLoops(G)
HCSClusters = highlyConnSG(GWithOutSelfLoops,sat=kappa)
return(HCSClusters)

}

