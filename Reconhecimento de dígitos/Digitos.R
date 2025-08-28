{ library(tidyverse)
  library(class)
  library(MASS) 
  library(reshape2) 
  library(reshape)
  library(corrplot)
  library(rpart)
  library(rpart.plot)
  library(randomForest)
  
  digits <- read.csv(file = "train_digits.csv", header = TRUE)
  digits[,"label"]<-factor(digits[,"label"])
  digits2 <- read.csv(file = "test_digits.csv", header = TRUE)
  
  img <- function(data, row_index){
    r <- as.numeric(data[row_index, 2:785])
    im <- matrix(nrow = 28, ncol = 28)
    j <- 1
    for(i in 28:1){
      im[,i] <- r[j:(j+27)]
      j <- j+28
    }
    image(x = 1:28, 
          y = 1:28, 
          z = im, 
          col=gray((0:255)/255), 
          main = paste("Number:", data[row_index, 1]))
  }
  
  img2 <- function(data, row_index){
    r <- as.numeric(data[row_index, 1:784])
    im <- matrix(nrow = 28, ncol = 28)
    j <- 1
    for(i in 28:1){
      im[,i] <- r[j:(j+27)]
      j <- j+28
    }
    image(x = 1:28, 
          y = 1:28, 
          z = im, 
          col=gray((0:255)/255))
  }
}

#==================================================================

{removidos <- c()

for(i in 2:785){
  if(sum(digits[,i])==0){
    removidos <- append(removidos, i)
  }
}

digits_norm <- digits[,-removidos]
digits2_norm <- digits2[,-(removidos-1)]
}

{digits_norm2 <-digits_norm
for(j in 1:42000){
  for(i in 2:709){
    if(digits_norm[j,i]!=0){
      digits_norm2[j,i] <- 1
    }
  }
}
}

#==================================================================

{
  ran <- sample(1:nrow(digits_norm), 0.9 * nrow(digits))
  
  treino <- digits_norm[ran,] 
  teste <- digits_norm[-ran,]
  
  #treino2 <- digits_norm2[ran,] 
  #teste2 <- digits_norm2[-ran,]
}

#==================================================================

o <- sample(1:nrow(digits_norm),size=1500,replace=FALSE)
v <- sample(2:ncol(digits_norm),size=150,replace=FALSE)
v <- append(v, 1)

datasample <- treino[o,v]

arv <- rpart(label ~ .,data = datasample,method="class",control=rpart.control(cp=0))

#==================================================================

floresta <- randomForest(label ~ .,data = treino,ntree=10,mtry=15)

#==================================================================

{
  n <- sample(1:nrow(digits2_norm),1)
  img2(digits2,n)
  print(predict(arvore,newdata = digits2_norm[n,],type="class"))
  print(predict(arvore2,newdata = digits2_norm[n,],type="class"))
}
