SNN_GRAPH <-
function(adj_mat,K) 
{
flag = TRUE
flagA=TRUE
flagNoEdgesInSNN= TRUE
# Get number of nodes in adjacency matrix
       no_of_nodes=ncol(adj_mat);
# Create square matrix of size same as adjacency matrix and initialize it to zero
       KNN_ADJ=matrix(c(0),nrow=no_of_nodes,ncol=no_of_nodes);
# Loop through each row of adjacency matrix
       for ( rows in 1:(no_of_nodes) )
       {
# Loop through all next rows of current row till we reach last row
               for (next_rows in (rows+1):(no_of_nodes))
               {


                       if((next_rows <= no_of_nodes) && (rows != next_rows))
                       {
# Counter is used to measure number of shared neighbours
                               counter<-c(0);
# Loop through each column of adjacency matrix
                               for(no_cols in 1:(no_of_nodes))
                               {
#Common neighbours between two rows ia found and counter is incremented
                                       if((adj_mat[rows,no_cols]==1) && (adj_mat[next_rows,no_cols]==1))
                                       {
                                       counter<-(counter+1);
                                      }
                              }
# If common neighbours between two rows of matrix are more than or equal to K
# then make its entry in output adjacency matrix by putting 1 in corresponding position
if((adj_mat[rows,next_rows]==1))
{ 
#counter=counter-1
                              if (( counter >= K ))
                               {
flagNoEdgesInSNN= FALSE
                                       KNN_ADJ[rows,next_rows]<-c(1);        
                                     
   KNN_ADJ[next_rows,rows]<-c(1);
  #KNN_ADJ[rows,next_rows]<-counter;        
                                      
#KNN_ADJ[next_rows,rows]<-counter;
if(flag==TRUE)
{
print("am here")

EdgeMatrix = rbind(c(rows,next_rows,counter))
flag=FALSE

}
else
{

print(EdgeMatrix)
print(rows)
 EdgeMatrix = rbind(EdgeMatrix,c(rows,next_rows,counter))
}

                               }
if(flagA==TRUE)
{
EdgeMatrixALL = rbind(c(rows,next_rows,counter))
flagA=FALSE

}
else
{
 EdgeMatrixALL = rbind(EdgeMatrixALL,c(rows,next_rows,counter))
}


}


                       }
               }
       } 


if(flagNoEdgesInSNN==TRUE)
{
EdgeMatrix=matrix(numeric(0), 0,3)
}
#Convert adjacency matrix to undirected,unweighted graph and return it
G1<-graph.adjacency(KNN_ADJ , mode=c("undirected"), weighted=NULL);

colnames(EdgeMatrix)<-c("Vertex","Vertex","Number of Shared Neighbors")
colnames(EdgeMatrixALL)<-c("Vertex","Vertex","Number of Shared Neighbors")
return (list(G1,EdgeMatrix,EdgeMatrixALL))
#return(EdgeMatrix)
#return(KNN_ADJ)
}

