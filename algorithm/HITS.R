HITS=function(g,k){
  #Get the adjacency matrix of the input grpah g 
  adj<-as.matrix(get.adjacency(g))
  
  #Get number of nodes(rows) in adjacency matrix 
  nodes<-dim(adj)[1]
  
  #Initialize authority and hub vector to 1 for each node 
  auth<-c(rep(1,nodes))
  hub<-c(rep(1,nodes))
  
  #Iterate for k number of iteration 
  for (i in 1:k) {
    
    #Get transpose of adjacency matrix 
    t_adj<-t(adj)
    
    #Authority and Hub scores are calculated using HITS mathematical definition 
    auth<-t_adj%*%hub
    hub<-adj%*%auth
    
    #Summation of squares of authority and hub scores are calculated to normalize Authority and Hub scores 
    sum_sq_auth<-sum(auth*auth)
    sum_sq_hub<-sum(hub*hub)
    
    #Normalize Hub and Authority scores 
    auth<-auth/sqrt(sum_sq_auth) 
    hub<-hub/sqrt(sum_sq_hub)
  }
  #Authority and Hub scores are combined into 1 vector and returned 
  result<-c(auth,hub)
  return (result)
}