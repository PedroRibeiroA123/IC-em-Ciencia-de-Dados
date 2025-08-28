{ 
  library(tidyverse)
  library(rpart)
  library(rpart.plot)
  library(randomForest)
  
  titanic <- read.table(file = "titanic.txt", header = TRUE, sep=",")
  titanic <- titanic[,c(-1,-9,-11)]
  titanic$Survived <- as.factor(titanic$Survived)
  
  for(x in 1:nrow(titanic)){
    if(grepl("Miss.",titanic[x,"Name"])){
      titanic[x,"Pronomes"]<-c("Miss.")
    }
    else if(grepl("Mr.",titanic[x,"Name"])){
      titanic[x,"Pronomes"]<-c("Mr.")
    }
    else if(grepl("Mrs.",titanic[x,"Name"])){
      titanic[x,"Pronomes"]<-c("Mrs.")
    }
    else if(grepl("Master.",titanic[x,"Name"])){
      titanic[x,"Pronomes"]<-c("Master.")
    }
    else
      titanic[x,"Pronomes"]<-c("Other.")
  }
  
  titanic <- titanic[sample(1:nrow(titanic)),-3]
  treino <- titanic[1:710,]
  teste <- titanic[710:891,]
}

#==================================================================
#Árvore

modelo1 <- rpart(Survived~.,data=treino[,-4],method="class",control=rpart.control(cp=0.011))
plotcp(modelo1)

1-sum(predict(modelo1,teste,"class")==teste[,1])/(nrow(teste))

rpart.plot(modelo1,extra = 101)

#==================================================================
#Bagging

vet <-c()
for(i in 1:500){
  
  modelo2 <- randomForest(Survived ~.,data=treino[,-4],mtry=7,ntree=i)
  
  vet[i] <- sum(predict(modelo2,teste,"class")==teste[,1])/(nrow(teste))
}

vet <- 1-vet
vet2<-modelo2$err.rate[,1]
vet3 <- c(vet,vet2)

v <- rep(1:500,2)
v2 <- rep(c("Erro do modelo","Erro OOB"),each =500)
grafico <- data.frame(vet3,v,v2)

ggplot(data=grafico,aes(x=v,y=vet3, color=v2))+
  geom_point()+
  labs(x="Numero de arvores",y="Taxa de erro")

#==================================================================
#Floresta

vet <-c()
vet2 <- c()

for(j in 1:7){
for(i in seq(2,1000,2)){
  
  modelo3 <- randomForest(Survived ~.,data=treino[,-4],mtry=j,ntree=i)
  
  vet[((j-1)*500)+i/2] <- sum(predict(modelo3,teste,"class")==teste[,1])/(nrow(teste))
}
}

vet <- 1-vet
v <- rep(seq(2,1000,2),7)
variaveis <- rep(1:7,each =500)
grafico <- data.frame(vet,v,variaveis)
grafico$variaveis <- as.factor(grafico$variaveis)

ggplot(data=grafico,aes(x=v,y=vet, color=variaveis))+
  geom_point()+
  ylim(0.16,0.24)+
  labs(x="Numero de arvores",y="Taxa de erro")

#=================================
#Grafico erro x quantidade de variavel

vet4 <- c()

for(k in 1:7){
  vet4[k] <- min(vet[(1+(k-1)*500):(k*500)])
  vet4[k+7] <- max(vet[(1+(k-1)*500):(k*500)])
  vet4[k+14] <- mean(vet[(1+(k-1)*500):(k*500)])
}

v <- rep(1:7,3)
variaveis <- rep(c("Min","Max","Media"),each =7)
grafico <- data.frame(vet4,v,variaveis)

ggplot(data=grafico,aes(x=v,y=vet4, color=variaveis))+
  geom_point()+
  geom_line()+
  labs(x="Numero de variaveis",y="Taxa de erro")

#=================================
#Erro OOB x quantidade de variavel

vet6<- c()

for(j in 1:7){
    
    modelo6 <- randomForest(Survived ~.,data=treino[,-4],mtry=j,ntree=1000)
    vet6 <- c(vet6,modelo6$err.rate[,1])
}

v <- rep(1:1000,7)
variaveis <- rep(1:7,each =1000)
grafico <- data.frame(vet6,v,variaveis)
grafico$variaveis <- as.factor(grafico$variaveis)

ggplot(data=grafico,aes(x=v,y=vet6, color=variaveis))+
  geom_point()+
  ylim(0.16,0.20)+
  labs(x="Numero de arvores",y="Taxa de erro")

vet7 <- c()

for(k in 1:7){
  vet7[k] <- min(vet6[(1+(k-1)*500):(k*1000)])
  vet7[k+7] <- max(vet6[(1+(k-1)*500):(k*1000)])
  vet7[k+14] <- mean(vet6[(1+(k-1)*500):(k*1000)])
}

v <- rep(1:7,3)
variaveis <- rep(c("Min","Max","Media"),each =7)
grafico <- data.frame(vet7,v,variaveis)

ggplot(data=grafico,aes(x=v,y=vet7, color=variaveis))+
  geom_point()+
  geom_line()+
  labs(x="Numero de variaveis",y="Taxa de erro")

#=================================
#Importancia de variavel

vet5 <-c()
for(i in seq(2,1000,2)){
  
  modelo2 <- randomForest(Survived ~.,data=treino[,-4],mtry=3,ntree=i)
  
  vet5[i/2] <- sum(predict(modelo2,teste,"class")==teste[,1])/(nrow(teste))
}

vet5 <- 1-vet5
vet6<-modelo2$err.rate[seq(2,1000,2),1]
vet7 <- c(vet5,vet6)

v <- rep(seq(2,1000,2),2)
v2 <- rep(c("Erro do modelo","Erro OOB"),each =500)
grafico <- data.frame(vet7,v,v2)

ggplot(data=grafico,aes(x=v,y=vet7, color=v2))+
  geom_point()+
  ylim(0.15,0.225)+
  labs(x="Numero de arvores",y="Taxa de erro")

modelo3 <- randomForest(Survived ~.,data=treino[,-4],mtry=3,ntree=500)

randomForest::varImpPlot(modelo3,sort=TRUE,main="Variable Importance Plot",pch=16)

#==================================================================

abalone <- read.csv(file = "abalone.csv", header = TRUE)

Idade <- c()
for(i in 1:nrow(abalone)){
  if(abalone[i,9]<=5)
    Idade[i] <- "joven"
  else if(abalone[i,9]<=13)
    Idade[i] <- "adulto"
  else
    Idade[i] <- "velho"
}

abalone <- data.frame(abalone[,-9],Idade)
abalone$Idade <- as.factor(abalone$Idade)

modelo4 <- randomForest(Idade ~.,data=abalone,mtry=8,ntree=500)

randomForest::varImpPlot(modelo4,sort=TRUE,main="Variable Importance Plot",pch=16)

diabetes <- read.csv(file = "diabetes.csv", header = TRUE)
diabetes$diabetes <- as.factor(diabetes$diabetes)

modelo5 <- randomForest(diabetes ~.,data=diabetes,mtry=8,ntree=500)

randomForest::varImpPlot(modelo5,sort=TRUE,main="Variable Importance Plot",pch=16)

#==================================================================

Comp <- read.csv(file = "test.csv", header = TRUE, sep=",")
Comp <- Comp[,c(-8,-10)]
Comp[154,8] <- mean(Comp$Fare)

for(x in 1:nrow(Comp)){
  if(grepl("Miss.",Comp[x,"Name"])){
    Comp[x,"Pronomes"]<-c("Miss.")
  }
  else if(grepl("Mr.",Comp[x,"Name"])){
    Comp[x,"Pronomes"]<-c("Mr.")
  }
  else if(grepl("Mrs.",Comp[x,"Name"])){
    Comp[x,"Pronomes"]<-c("Mrs.")
  }
  else if(grepl("Master.",Comp[x,"Name"])){
    Comp[x,"Pronomes"]<-c("Master.")
  }
  else
    Comp[x,"Pronomes"]<-c("Other.")
}

modeloComp <- randomForest(Survived ~.,data=titanic[,-4],mtry=3,ntree=250)

Survived <- predict(modeloComp,Comp,"class")

submission <- data.frame(PassengerId=Comp$PassengerId,Survived=Survived)

write.csv(submission,"G:\\Meu Drive\\UFU\\IC\\semana 13\\Submission.csv",row.names=TRUE)
