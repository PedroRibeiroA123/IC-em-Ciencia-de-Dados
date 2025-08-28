{ 
  library(tidyverse)
  library(reshape2) 
  library(reshape)
  
  filmes <- read.csv("FILMES.csv",header=T,sep=',')
  
  knn <- function(i, distances, k){
    return(order(distances[i, ])[2:(k + 1)])
  }
  
  #Recomenda filme para usuario baseado em filmes
  
  filme.prob1 <- function(usu, matriz, distances, k){
    filmes <- which(matriz[usu,]==0)
    prox <- c()
    for(filme in filmes){
    prox <- c(prox,sum(matriz[usu,knn(filme, distances, k)]))
    }
    return(filmes[order(prox,decreasing = T)])
  }
  
  #Recomenda filme para usuario baseado em usuarios
  
  filme.prob2 <- function(usu, matriz2, distances2, k){
    neighbors <- knn(usu, distances2, k)
    candidatos <- c()
    for(i in neighbors){
      temp <- matriz2[matriz2[,usu]==0 & matriz2[,i]!=0,i]
      temp2 <- which(matriz2[,usu]==0 & matriz2[,i]!=0)
      candidatos <- c(candidatos,temp2[order(temp, decreasing = T)])
    }
    filmes <- as.integer(names(table(candidatos)))
    notas <-c()
    resul <-c()
    for(j in filmes){
    notas <- c(notas,sum(matriz2[j,neighbors]))
    }
    return(filmes[order(notas,decreasing = T)])
  }

}

#Semelhança entre filmes baseado em usuários

matriz <- cast(filmes, Nome.de.usuário ~ Filme, value = 'Nota')
row.names(matriz) <- matriz[, 1]
matriz <- matriz[, -1]
matriz[is.na(matriz)] <- 0
matriz[1,28] <-0

similarities <- cor(matriz)

distances <- -log((similarities / 2) + 0.5)

lista <- filme.prob1(1,matriz,distances,10)
colnames(matriz)[lista[1:10]]

#Semelhança entre usuários baseado em filmes

matriz2 <- cast(filmes, Filme ~ Nome.de.usuário, value = 'Nota')
row.names(matriz2) <- matriz2[, 1]
matriz2 <- matriz2[, -1]
matriz2[is.na(matriz2)] <- 0

similarities2 <- cor(matriz2)

distances2 <- -log((similarities2 / 2) + 0.5)

preferidos <- rownames(matriz2[matriz2[,1]==5,])

matriz2[,1] <- 0

lista <- filme.prob2(1,matriz2,distances2,10)
recs <-rownames(matriz2)[lista[1:length(preferidos)]]

print(paste0(sum(preferidos %in% recs),"/",length(preferidos)))