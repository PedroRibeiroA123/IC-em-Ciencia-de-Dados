{ 
  library(tidyverse)
  library(rpart)
  library(rpart.plot)
  
  dados1 <- read.csv(file = "diabetes.csv", header = TRUE)
}

#==================================================================

ct1 <-1000
ct2 <-1
ct3 <-20
vet <- c()

for(j in 1:3){
  
dados <- dados1[c(1:ct1),]

for(i in ct2:ct3){
  ran <- sample(1:nrow(dados), 0.75 * nrow(dados))
  treino <- dados[ran,] 
  teste <- dados[-ran,]
  modelo <- rpart(diabetes ~ .,data = treino,method="class",
                  control=rpart.control(cp=0))
  vet[i] <- sum(predict(modelo,teste,"class")==teste[,9])/(nrow(teste))
}

ct1 <- ct1*10
ct2 <- ct2 + 20
ct3 <- ct3 + 20

}

  v <- rep(c(1:20),3)
  v2 <- rep(c("mil","dez mil","cem mil"),each =20)
  grafico <- data.frame(vet,v,v2)
  grafico$v2 <- as.factor(v2)

ggplot(data=grafico,aes(x=v,y=vet, group = v2))+
  geom_line(aes(color=v2))+
  geom_point()+
  labs(x="",y="Taxa de acerto")

#==================================================================

dados <- dados1[c(1:10000),]
acc <- 0

for(i in 1:nrow(dados)){
  treino <- dados[-i,] 
  teste <- dados[i,]
  modelo <- rpart(diabetes ~ .,data = treino,method="class",
                  control=rpart.control(cp=0))
  acc <- acc + sum(predict(modelo,teste,"class")==teste[,9])
}

acc/nrow(dados)

modelo <- rpart(diabetes ~ .,data = dados,
                method="class",control=rpart.control(cp=0))
sum(predict(modelo,dados,"class")==dados[,9])

#==================================================================

ct0 <-1000
ct2 <-1
ct3 <-20

vet2 <- c()

for(l in 1:3){

dados <- dados1[c(1:ct0),]

for(j in ct2:ct3){

dados <- dados[sample(1:nrow(dados)),]

k <- 10
ct1 <- 1
vet <- c()

for(i in 1:k){
  treino <- dados[-(ct1:(ct1+(nrow(dados)/k))-1),] 
  teste <- dados[ct1:(ct1+(nrow(dados)/k)-1),]
  ct1 <- ct1 + nrow(dados)/k
  modelo <- rpart(diabetes ~ .,data = treino,
                  method="class",control=rpart.control(cp=0))
  vet[i] <- sum(predict(modelo,teste,"class")==teste[,9])/(nrow(teste))
  remove(treino)
  remove(teste)
}

vet2[j] <- mean(vet)

}

ct0 <- ct0*10
ct2 <- ct2 + 20
ct3 <- ct3 + 20

}

v <- rep(c(1:20),3)
v2 <- rep(c("mil","dez mil","cem mil"),each =20)
grafico <- data.frame(vet2,v,v2)
grafico$v2 <- as.factor(v2)

ggplot(data=grafico,aes(x=v,y=vet2, group = v2))+
  geom_line(aes(color=v2))+
  geom_point()+
  labs(x="",y="Taxa de acerto")+
  ylim(0.93,0.99)

#==================================================================

vet2<-c()
dados <- dados1[c(1:ct0),]

for(j in 1:30){

  dados <- dados[sample(1:nrow(dados)),]
  
  k <- 10
  ct1 <- 1
  vet <- c()
  
  for(i in 1:k){
    treino <- dados[-(ct1:(ct1+(nrow(dados)/k))-1),] 
    teste <- dados[ct1:(ct1+(nrow(dados)/k)-1),]
    ct1 <- ct1 + nrow(dados)/k
    modelo <- rpart(diabetes ~ .,data = treino,
                    method="class",control=rpart.control(cp=0,maxdepth = j))
    vet[i] <- sum(predict(modelo,teste,"class")==teste[,9])/(nrow(teste))
    remove(treino)
    remove(teste)
  }
  
  vet2[j] <- mean(vet)
}

v <- 1:30
grafico <- data.frame(vet2,v)

ggplot(data=grafico,aes(x=v,y=vet2, group = 1))+
  geom_line()+
  geom_point()+
  labs(x="",y="Taxa de acerto")

vet3 <- 1-vet2

grafico2 <- data.frame(vet3,v)

ggplot(data=grafico2,aes(x=v,y=vet3, group = 1))+
  geom_line()+
  geom_point()+
  labs(x="Profundidade máxima",y="Taxa de erro")

#==================================================================

vet2<-c()
dados <- dados1[c(1:1000),]

for(j in 1:30){
  
  ran <- sample(1:nrow(dados), 0.75 * nrow(dados))
  treino <- dados[ran,] 
  teste <- dados[-ran,]
  modelo <- rpart(diabetes ~ .,data = treino,method="class",
                  control=rpart.control(cp=0,maxdepth = j))
  vet2[j] <- sum(predict(modelo,teste,"class")==teste[,9])/(nrow(teste))
  
}

v <- 1:30
grafico <- data.frame(vet2,v)

ggplot(data=grafico,aes(x=v,y=vet2, group = 1))+
  geom_line()+
  geom_point()+
  labs(x="",y="Taxa de acerto")

vet3 <- 1-vet2

grafico2 <- data.frame(vet3,v)

ggplot(data=grafico2,aes(x=v,y=vet3, group = 1))+
  geom_line()+
  geom_point()+
  labs(x="Profundidade máxima",y="Taxa de erro")