{ 
  library(tidyverse)
  library(class)
  library(MASS) 
  library(reshape2) 
  library(reshape)
  library(corrplot)
  library(rpart)
  library(rpart.plot)

  dados1 <- read.csv(file = "diabetes.csv", header = TRUE)
  dados1$diabetes <- as.factor(dados1$diabetes)
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

corMatMy <- cor(dados1[,c(2,6,7,8)])
corrplot(corMatMy, order = "hclust", tl.cex = 0.7)

ggplot(data = dados1, mapping = aes(x = gender))+
  geom_bar()+
  facet_wrap(~diabetes)

ggplot(data = dados1, mapping = aes(x = smoking_history))+
  geom_bar()+
  facet_wrap(~diabetes)

ggplot(data = dados1, mapping = aes(x = hypertension))+
  geom_bar()+
  facet_wrap(~diabetes)

ggplot(data = dados1, mapping = aes(x = heart_disease))+
  geom_bar()+
  facet_wrap(~diabetes)

#==================================================================

ggplot(data = treino, mapping = aes(x = blood_glucose_level,y = HbA1c_level,col = diabetes))+
  geom_point()+
  scale_color_manual(values = c("#5aae61","#4393c3"))

nor <-function(x) {(x -min(x))/(max(x)-min(x))}
distancia <- function(x,y) {return(sqrt(sum((x-y)**2)))}

dados0 <- data.frame(lapply(dados1[,c(2,6,7,8)], nor),diabetes)

treino <- dados0[1:1000,]
teste <- dados0[1001:1100,]

KNN <-function(teste, treino){

previsao <- c()

for (k in 1:nrow(teste)) {
  distancias <- c()
  for (j in 1:nrow(treino)) {
    distancias[j] <- distancia(teste[k,1:4],treino[j,1:4])
  }
  previsao[k] <- treino$diabetes[order(distancias)[1]]
}

previsao <- previsao-1
return(mean(previsao == teste$diabetes))}

#==================================================================

ct2 <-1
ct3 <-30
vet2<-c()
diabetes <- dados1[,9]
dados0 <- data.frame(lapply(dados1[,c(7,8)], nor))
dados <- dados0[c(1:1000),]
dadosZ <- dados1[c(1:1000),]

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
  ct2 <- ct2 + 30
  ct3 <- ct3 + 30
}

v <-  rep(c(1:30),3)
v3 <- rep(c("iteração 1","iteração 2","iteração 3"),each =30)
grafico <- data.frame(vet2,v,v3)

ggplot(data=grafico,aes(x=v,y=vet2, group = v3))+
  geom_line(aes(color=v3))+
  geom_point()+
  labs(x="K",y="Taxa de acerto")

#==================================================================

dados0 <- data.frame(lapply(dados1[,c(7,8)], nor))

vet <-c()

treino <- dados0[1:1000,]
teste <- dados0[1001:2000,]
treinoC <- dados1[1:1000,9]
testeC <- dados1[1001:2000,9]

for(i in 1:30){

pr <- knn(treino,teste,cl=treinoC,k=i)
tab <- table(pr,testeC)
vet[i] <- accuracy(tab)/100

}

v <- 1:30
grafico <- data.frame(vet,v)

ggplot(data=grafico,aes(x=v,y=vet, group = 1))+
  geom_line()+
  geom_point()+
  labs(x="K",y="Taxa de acerto")

#==================================================================

diabeticos <- dados1[dados1$diabetes==1,]
Ndiabeticos <- dados1[!dados1$diabetes==1,]

#gini da partição hypertension=1

gini1 <- (1-(sum(diabeticos$hypertension==1)/sum(dados1$hypertension==1))**2
        -(sum(Ndiabeticos$hypertension==1)/sum(dados1$hypertension==1))**2)

#gini da partição hypertension=0

gini2 <- (1-(sum(diabeticos$hypertension==0)/sum(dados1$hypertension==0))**2
          -(sum(Ndiabeticos$hypertension==0)/sum(dados1$hypertension==0))**2)

#gini total da variável

giniT <- gini1*sum(dados1$hypertension==1)/nrow(dados1) +
  gini2*sum(dados1$hypertension==0)/nrow(dados1)

#gini da partição heart disease=1

gini1 <- (1-(sum(diabeticos$heart_disease==1)/sum(dados1$heart_disease==1))**2
          -(sum(Ndiabeticos$heart_disease==1)/sum(dados1$heart_disease==1))**2)

#gini da partição heart disease=0

gini2 <- (1-(sum(diabeticos$heart_disease==0)/sum(dados1$heart_disease==0))**2
          -(sum(Ndiabeticos$heart_disease==0)/sum(dados1$heart_disease==0))**2)

#gini total da variável

giniT <- gini1*sum(dados1$heart_disease==1)/nrow(dados1) +
  gini2*sum(dados1$heart_disease==0)/nrow(dados1)

#gini da partição gender=male

gini1 <- (1-(sum(diabeticos$gender=="Male")/sum(dados1$gender=="Male"))**2
          -(sum(Ndiabeticos$gender=="Male")/sum(dados1$gender=="Male"))**2)

#gini da partição gender=female

gini2 <- (1-(sum(diabeticos$gender=="Female")/sum(dados1$gender=="Female"))**2
          -(sum(Ndiabeticos$gender=="Female")/sum(dados1$gender=="Female"))**2)

#gini total da variável

giniT <- gini1*sum(dados1$gender=="Male")/nrow(dados1) +
  gini2*sum(dados1$gender=="Female")/nrow(dados1)

#gini da partição SH=never

gini1 <- (1-(sum(diabeticos$smoking_history=="never")/sum(dados1$smoking_history=="never"))**2
          -(sum(Ndiabeticos$smoking_history=="never")/sum(dados1$smoking_history=="never"))**2)

#gini da partição SH=ever

gini2 <- (1-(sum(diabeticos$smoking_history=="ever")/sum(dados1$smoking_history=="ever"))**2
          -(sum(Ndiabeticos$smoking_history=="ever")/sum(dados1$smoking_history=="ever"))**2)

#gini da partição SH=current

gini3 <- (1-(sum(diabeticos$smoking_history=="current")/sum(dados1$smoking_history=="current"))**2
          -(sum(Ndiabeticos$smoking_history=="current")/sum(dados1$smoking_history=="current"))**2)

#gini da partição SH=not current

gini4 <- (1-(sum(diabeticos$smoking_history=="not current")/sum(dados1$smoking_history=="not current"))**2
          -(sum(Ndiabeticos$smoking_history=="not current")/sum(dados1$smoking_history=="not current"))**2)

#gini da partição SH=former

gini5 <- (1-(sum(diabeticos$smoking_history=="former")/sum(dados1$smoking_history=="former"))**2
          -(sum(Ndiabeticos$smoking_history=="former")/sum(dados1$smoking_history=="former"))**2)

#gini da partição SH=No Info

gini6 <- (1-(sum(diabeticos$smoking_history=="No Info")/sum(dados1$smoking_history=="No Info"))**2
          -(sum(Ndiabeticos$smoking_history=="No Info")/sum(dados1$smoking_history=="No Info"))**2)

#gini total da variável

giniT <- gini1*sum(dados1$smoking_history=="never")/nrow(dados1) +
  gini2*sum(dados1$smoking_history=="ever")/nrow(dados1)+
  gini3*sum(dados1$smoking_history=="current")/nrow(dados1)+
  gini4*sum(dados1$smoking_history=="not current")/nrow(dados1)+
  gini5*sum(dados1$smoking_history=="former")/nrow(dados1)+
  gini6*sum(dados1$smoking_history=="No Info")/nrow(dados1)

#==================================================================

gini <- function(treino){
  resuls<-c()
  resulsGinis<-c()
  for(j in 1:4){
    ginis <- c()
    vet <- sort(treino[,j])
    
    for(i in 1:(nrow(treino)-1)){
      med <- (vet[i]+vet[i+1])/2
      a <-sum(treino[treino[,j]<med,]$diabetes==1)
      b <-sum(treino[treino[,j]<med,]$diabetes==0)
      c <- a+b
      d <-sum(treino[treino[,j]>=med,]$diabetes==1)
      e <-sum(treino[treino[,j]>=med,]$diabetes==0)
      f <- d+e
      g <- c+f
      
      if(c!=0){gi1 <- c/g * (1 - (a/c)**2 - (b/c)**2)
      }else{gi1 <- 0}
      
      if(f!=0){gi2 <- f/g * (1 - (d/f)**2 - (e/f)**2)
      }else{gi2 <- 0}
      
      ginis[i] <- gi1 + gi2
    }
    resulsGinis[j] <- min(ginis)
    resuls[j] <- vet[which(min(ginis) == ginis)]
  }
  return(resulsGinis)}

gini(dados1[1:1000,c(2,6,7,8,9)])
