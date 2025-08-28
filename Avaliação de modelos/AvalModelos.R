{ 
  library(tidyverse)
  library(class)
  library(rpart)
  library(rpart.plot)
  library(wrapr)
  library(WVPlots)
  library(randomForest)
  
  dados <- read.csv(file = "diabetes.csv", header = TRUE)
  dados$diabetes <- as.factor(dados$diabetes)
  
  nomes <- c("Acuracia geral","Positivos","Negativos","Falsos positivos",
             "Falsos negativos", "Verdadeiros positivos", "Veradeiros negativos",
             "% de positivos detectados")
  metodos <- c("Null","KNN","Arvore","Floresta","Logistica")
  
  matriz <- matrix(nrow=8,ncol=5)
  row.names(matriz) <- nomes
  colnames(matriz) <- metodos
  
  fillMatriz <- function(t1){
    return(c((t1[1,1]+t1[2,2])/sum(t1),
             t1[2,1]+t1[2,2],
             t1[1,1]+t1[1,2],
             t1[1,2],
             t1[2,1],
             t1[2,2],
             t1[1,1],
             t1[2,2]/(t1[2,2]+t1[2,1]) 
              ))
  }
  
  shuffle <- function(dados1){
    ran <- sample(1:nrow(dados1), nrow(dados1))
    return(dados[ran,])
  }
  
  kFoldPartitionTeste <- function(dados,k,ite){
    a <- (1+(ite-1)*(nrow(dados)/k))
    b <- ((ite)*(nrow(dados)/k))
    return(dados[a:b,])
  }
  
  kFoldPartitionTreino <- function(dados,k,ite){
    a <- (1+(ite-1)*(nrow(dados)/k))
    b <- ((ite)*(nrow(dados)/k))
    return(dados[-(a:b),])
  }
  
  
}

dados_fim <- shuffle(dados)
treino_fim <- kFoldPartitionTreino(dados_fim,10,5)
teste_fim <- kFoldPartitionTeste(dados_fim,10,5)

{
#KNN
dados_norm = scale(dados1[-c(1,5,9)])

Ks <- seq(from = 1, to = 10, by = 1)
det2 <- c()

for(j in Ks){
  
detection <- c()

for(i in 1:10){
teste_knn <- kFoldPartitionTeste(dados_norm,10,i)
treino_knn <- kFoldPartitionTreino(dados_norm,10,i)
actual_knn <- kFoldPartitionTeste(dados1,10,i)$diabetes
treino2 <- kFoldPartitionTreino(dados1,10,i)

test_pred <- knn(
  train = treino_knn, 
  test = teste_knn,
  cl = treino2$diabetes, 
  k=j
)

cm <- table(actual_knn,test_pred)
detection <- c(detection,cm[2,2]/(cm[2,2]+cm[2,1]))

}

det2 <- c(det2,mean(detection))
}

max(det2)
Ks[which(det2==max(det2))]

ggplot(mapping = aes(x=Ks,y=det2,col="darkred"))+
  geom_line()+
  labs(x="Vizinhos",y="Acurácia")

#Arvore

detection <- c()

for(i in 1:10){

teste_arv <- kFoldPartitionTeste(dados1,10,i)
treino_arv <- kFoldPartitionTreino(dados1,10,i)
actual_arv <- teste_arv$diabetes

modelo <- rpart(diabetes ~ .,data = treino_arv,
                method="class",control=rpart.control(cp=0))

pred1 <- predict(modelo,teste_arv,"class")

tab <- table(actual_arv,pred1)

detection <- c(detection,tab[2,2]/(tab[2,1]+tab[2,2]))
}

mean(detection)

#Floresta

fs <- seq(from = 300, to = 1500, by = 200)
det4 <- c()

for(j in fs){
  
  detection <- c()
  
  for(i in 1:10){
    teste_fore <- kFoldPartitionTeste(dados1,10,i)
    treino_fore <- kFoldPartitionTreino(dados1,10,i)
    actual_fore <- teste_fore$diabetes
    
    modelo2 <- randomForest(diabetes ~.,data=treino_fore,ntree=9)
    
    pred2 <- predict(modelo2,teste_fore,"class")
    
    cm2 <- table(actual_fore,pred2)
    detection <- c(detection,cm2[2,2]/(cm2[2,2]+cm2[2,1]))
  }
  print(j)
  det4 <- c(det4,mean(detection))
}

ggplot(mapping = aes(x=fs,y=det4,col="darkred"))+
  geom_line()+
  labs(x="Árvores",y="Acurácia")

max(det4)
fs[which(det4==max(det4))]

#Logistica

det5 <- c()
cuts <- c()

for(j in 1:10){
  
teste_logit <- kFoldPartitionTeste(dados1,10,j)
treino_logit <- kFoldPartitionTreino(dados1,10,j)

modelo3 <- glm(diabetes~., data = treino_logit,
               family = binomial(link = "logit"))

teste1 <- teste_logit

teste1$pred <- predict(modelo3, newdata=teste_logit, type="response")
treino_logit$pred <- predict(modelo3, newdata=treino_logit, type = "response")

DoubleDensityPlot(treino_logit, "pred", "diabetes",
                  title = "Distribuição de diabetes")
i <- 0
l <- 0.01
while(i < 0.90){
tab4 <- table(actual = teste1$diabetes,pred = teste1$pred >l)
l <- l + 0.01
i <- tab4[1,1]/(tab4[1,1]+tab4[1,2])
}
det5 <- c(det5,tab4[2,2]/(tab4[2,2]+tab4[2,1]))
cuts <- c(cuts, l)
}
}

#Null
{
  actual <- teste_fim$diabetes
  pred <-rep(0,nrow(teste_fim))
  t1 <- table(actual,pred)
  
  matriz[1,1] <- diag(t1)/sum(t1)
  matriz[2,1] <- t1[2]
  matriz[3,1] <- t1[1]
  matriz[4,1] <- 0
  matriz[5,1] <- t1[2]
  matriz[6,1] <- 0
  matriz[7,1] <- t1[1]
  matriz[8,1] <- 0}

#KNN
treino_norm = scale(treino_fim[-c(1,5,9)])
teste_norm = scale(teste_fim[-c(1,5,9)])

test_pred <- knn(
  train = treino_norm, 
  test = teste_norm,
  cl = treino_fim$diabetes, 
  k=1
)

cm <- table(actual,test_pred)

matriz[,2]<-fillMatriz(cm)

cm[2,2]/(cm[2,1]+cm[2,2])

#Arvore

modelo <- rpart(diabetes ~ .,data = treino_fim,
                method="class",control=rpart.control(cp=0))

pred1 <- predict(modelo,teste_fim,"class")

tab <- table(actual,pred1)

matriz[,3]<-fillMatriz(tab)

tab[2,2]/(tab[2,1]+tab[2,2])

#Floresta

modelo2 <- randomForest(diabetes ~.,
                        data=treino_fim,ntree=900)

pred2 <- predict(modelo2,teste_fim,"class")

tab2 <- table(actual,pred2)

matriz[,4]<-fillMatriz(tab2)

tab2[2,2]/(tab2[2,1]+tab2[2,2])

#logística

modelo3 <- glm(diabetes~., data = treino_fim,
               family = binomial(link = "logit"))

teste1 <- teste_fim

teste1$pred <- predict(modelo3,
                       newdata=teste_fim, type="response")

ctab.test <- table(actual = teste_fim$diabetes,
                   pred = teste1$pred > 0.11)

matriz[,5]<-fillMatriz(ctab.test)

ctab.test[2,2]/(ctab.test[2,1]+ctab.test[2,2])