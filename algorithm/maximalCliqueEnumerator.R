maximalCliqueEnumerator <-
function(G) {

if(class(G)[1]=="igraph")
{
K = igraph.to.graphNEL(G)
G = K
}
maximalCliques = maxClique(G)
return(maximalCliques)
}

