{ 
  install.packages("randomForest")
  
  library(tidyverse)
  library(class)
  library(MASS) 
  library(reshape2) 
  library(reshape)
  library(corrplot)
  library(rpart)
  library(rpart.plot)
  library(randomForest)

  dados1 <- read.csv(file = "abalone.csv", header = TRUE)
  
  Idade <- c()
  for(i in 1:nrow(dados1)){
    if(dados1[i,9]<=5)
      Idade[i] <- "joven"
    else if(dados1[i,9]<=13)
      Idade[i] <- "adulto"
    else
      Idade[i] <- "velho"
  }
  
  dados1 <- data.frame(dados1[,-9],Idade)
  dados1 <- dados1[sample(1:4177),]
  dados1$Idade <- as.factor(dados1$Idade)
  
  distancia <- function(x,y){
    return(sqrt(sum((x-y)**2)))
  }
  nor <-function(x) {
    (x -min(x))/(max(x)-min(x))
  }
  accuracy <- function(x){
    sum(diag(x)/(sum(rowSums(x)))) * 100
  }
}

#==================================================================

corMatMy <- cor(dados1[,2:8])
corrplot(corMatMy, order = "hclust", tl.cex = 0.7)

#==================================================================

gini <- function(treino){
  resuls<-c()
  resulsGinis<-c()
  for(j in 1:7){
    ginis <- c()
    vet <- sort(treino[,j])
    
    for(i in 1:(nrow(treino)-1)){
      med <- (vet[i]+vet[i+1])/2
      a <-sum(treino[treino[,j]<med,]$Idade=="velho")
      b <-sum(treino[treino[,j]<med,]$Idade=="adulto")
      h <-sum(treino[treino[,j]<med,]$Idade=="joven")
      c <- a+b+h
      d <-sum(treino[treino[,j]>=med,]$Idade=="velho")
      e <-sum(treino[treino[,j]>=med,]$Idade=="adulto")
      k <-sum(treino[treino[,j]>=med,]$Idade=="joven")
      f <- d+e+k
      g <- c+f
      
      if(c!=0){gi1 <- c/g * (1 - (a/c)**2 - (b/c)**2 - (h/c)**2)
      }else{gi1 <- 0}
      
      if(f!=0){gi2 <- f/g * (1 - (d/f)**2 - (e/f)**2 - (k/f)**2)
      }else{gi2 <- 0}
      
      ginis[i] <- gi1 + gi2
    }
    resulsGinis[j] <- min(ginis)
    resuls[j] <- vet[which(min(ginis) == ginis)]
  }
  return(resulsGinis)}

g <- gini(dados1[,c(2:9)])
which(min(g) == g)

#==================================================================

ct2 <-1
ct3 <-40
vet2<-c()
dados0 <- data.frame(lapply(dados1[,2:8], nor))
dados <- dados0[c(1:3700),]
dadosZ <- dados1[c(1:3700),]

for(l in 1:3){
  
  for(j in ct2:ct3){
    
    ran<-sample(1:nrow(dados))
    
    dados <- dados[ran,]
    dadosZ <- dadosZ[ran,]
    
    k <- 10
    ct1 <- 1
    vet <- c()
    
    for(i in 1:k){
      treino <- dados[-(ct1:(ct1+(nrow(dados)/k))-1),] 
      teste <- dados[ct1:(ct1+(nrow(dados)/k)-1),]
      treinoC <- dadosZ[-(ct1:(ct1+(nrow(dados)/k))-1),9]
      testeC <- dadosZ[ct1:(ct1+(nrow(dados)/k)-1),9]
      ct1 <- ct1 + nrow(dados)/k
      pr <- knn(treino,teste,cl=treinoC,k=j-ct2+1)
      tab <- table(pr,testeC)
      vet[i] <- accuracy(tab)
      remove(treino)
      remove(teste)
    }
    
    vet2[j] <- mean(vet)
  }
  ct2 <- ct2 + 40
  ct3 <- ct3 + 40
}

v <-  rep(c(1:40),3)
v3 <- rep(c("iteração 1","iteração 2","iteração 3"),each =40)
grafico <- data.frame(vet2,v,v3)

ggplot(data=grafico,aes(x=v,y=vet2, group = v3))+
  geom_line(aes(color=v3))+
  geom_point()+
  labs(x="K",y="Taxa de acerto")

#==================================================================

dados0 <- data.frame(lapply(dados1[,2:8], nor))

vet <-c()

treino <- dados0[1:3700,]
teste <- dados0[3701:4177,]
treinoC <- dados1[1:3700,9]
testeC <- dados1[3701:4177,9]

for(i in 1:40){
  
  pr <- knn(treino,teste,cl=treinoC,k=i)
  tab <- table(pr,testeC)
  vet[i] <- accuracy(tab)/100
  
}

v <- 1:40
grafico <- data.frame(vet,v)

ggplot(data=grafico,aes(x=v,y=vet, group = 1))+
  geom_line()+
  geom_point()+
  labs(x="K",y="Taxa de acerto")

#==================================================================

dados0 <- data.frame(lapply(dados1[,2:8], nor))
dados <- dados0[c(1:3700),]
dadosZ <- dados1[c(1:3700),]
vet2 <- c()

for(j in 1:40){
  
  ran<-sample(1:nrow(dados))
  
  dados <- dados[ran,]
  dadosZ <- dadosZ[ran,]
  
  k <- 10
  ct1 <- 1
  vet <- c()
  
  for(i in 1:k){
    treino <- dados[-(ct1:(ct1+(nrow(dados)/k))-1),] 
    teste <- dados[ct1:(ct1+(nrow(dados)/k)-1),]
    treinoC <- dadosZ[-(ct1:(ct1+(nrow(dados)/k))-1),9]
    testeC <- dadosZ[ct1:(ct1+(nrow(dados)/k)-1),9]
    ct1 <- ct1 + nrow(dados)/k
    pr <- knn(treino,teste,cl=treinoC,k=j)
    tab <- table(pr,testeC)
    vet[i] <- accuracy(tab)
    remove(treino)
    remove(teste)
  }
  
  vet2[j] <- mean(vet)
}

v <- 1:40
grafico <- data.frame(vet2,v)

ggplot(data=grafico,aes(x=v,y=vet2, group = 1))+
  geom_line()+
  geom_point()+
  labs(x="K",y="Taxa de acerto")

#==================================================================

treino <- dados1[1:3700,]
teste <- dados1[3701:4177,]

modelo2 <- rpart(Idade ~ .,data = treino,
                 method="class",
                 control=rpart.control(cp=0.0071))
sum(predict(modelo2,teste,"class")==teste[,9])/(nrow(teste))

plotcp(modelo2)

rpart.plot(modelo2,extra = 101)

#==================================================================

modelo3 <- randomForest(Idade ~.,
                        data=treino,
                        proximity=TRUE)
sum(predict(modelo3,teste,"class")==teste[,9])/(nrow(teste))
