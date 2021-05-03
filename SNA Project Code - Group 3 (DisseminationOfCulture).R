library(igraph)
library(igraph)
library(NetData)

setwd('C:/Users/cchahine/OneDrive - IESEG/01- Attachements/IESEG/01- Courses/2nd Semester/08- Social Network Analysis/Group Project')

# Importing the tables
friendship_data_frame <- read.table('Krack-High-Tec-edgelist-Friendship.txt')
attributes <- read.csv('Krack-High-Tec-Attributes.csv', header=T)

# Changing the columns names
colnames(friendship_data_frame) <- c('ego', 'alter', 'friendship_tie')

friendship_data_frame = friendship_data_frame[friendship_data_frame$friendship_tie >0,]

# Creating graph object
g <- graph.data.frame(friendship_data_frame)
plot(g)

n <- max(friendship_data_frame) # number of individuals
F <- 10                         # cultural features
q <- 5                          # number of levels of the cultural features
A <- as.matrix(as_adjacency_matrix(g))

E <- matrix(round(runif(n*F, 0,q)),n,F) # initial cultural state of nodes


# Suppose that we have two cultural features:
#   1) professional attitude (driven by DEPT, TENURE, LEVEL)
#   2) personal attitude (driven by AGE, DEGREE)


G1 = attributes[,c(2, 3, 4)]
G1[,1] = ifelse(G1[,1] <= quantile(G1[,1],probs = c(0.3)),0,
                ifelse(G1[,1] <= quantile(G1[,1],probs = c(0.6)) & (G1[,1] > quantile(G1[,1],probs = c(0.3))),1,
                       ifelse((G1[,1] <= quantile(G1[,1],probs = c(0.9))) & (G1[,1] > quantile(G1[,1],probs = c(0.6))),2,3)))

E1 = rowSums(G1)

G2 = attributes[,1]

E21 = ifelse(G2 <= quantile(G2,probs = c(0.3)),0,
             ifelse(G2 <= quantile(G2,probs = c(0.6)) & (G2 > quantile(G2,probs = c(0.3))),1,
                    ifelse((G2 <= quantile(G2,probs = c(0.9))) & (G2 > quantile(G2,probs = c(0.6))),2,3)))

E22 = ifelse(rowSums(A) <= quantile(rowSums(A),probs = c(0.3)),0,
             ifelse(rowSums(A) <= quantile(rowSums(A),probs = c(0.6)) & (rowSums(A) > quantile(rowSums(A),probs = c(0.3))),1,
                    ifelse((rowSums(A) <= quantile(rowSums(A),probs = c(0.9))) & (rowSums(A) > quantile(rowSums(A),probs = c(0.6))),2,3)))
E2 = E21* 0.25  + E22 * 10

E2

F = 2
E = cbind(E1, E2)


Vertex.col <- rowSums(E)
V(g)$color <- Vertex.col

Axeldod.model <- function(EE,AA){
  
  gg = graph_from_adjacency_matrix(AA)
  
  
  M <- 100
  SIMIL <- NULL
  for(k in 1:M){
    
    for(i in 1:n){
      
      for(j in which(AA[i,]>= 1) ){
        
        common.features <- which(abs(EE[i,] - EE[j,]) == 0)
        numer <- length(common.features)
        prob <- AA[i,j]*(numer/F)
        
        coin <- runif(1,0,1)
        
        if(coin < prob & prob < 1){
          
          different.features <- which(abs(EE[i,] - EE[j,]) != 0)
          diff <- length(different.features)
          hh <- different.features[round(runif(1,1,diff))]
          
          EE[j,hh] <- EE[i,hh]
          
        }
        
      }
      
    }
    Vertex.col <- EE%*%(1:F)
    V(gg)$color <- Vertex.col
    SIMIL <- c(SIMIL, mean(cor(t(EE))[cor(t(EE)) > 0])) 
    
    par(mfrow = c(1, 2))
    plot(1:k, SIMIL, type = 'o', ylim = c(-1,1))
    plot(gg)
    
    Sys.sleep(1)	
  }
  
  
  return(EE)
}

EEE <- Axeldod.model(E,as.matrix(A))

