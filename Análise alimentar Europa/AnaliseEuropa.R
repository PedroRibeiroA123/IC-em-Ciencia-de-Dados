#install.packages("mapdata")
#install.packages("sf")

library(ggplot2)
library(dplyr)
library(mapdata)
library(sf)

dados <- read.table(file = "protein.txt", header = TRUE, sep = ",")
dados[7,] <-c("Germany", (dados[7, -1] + dados[24, -1])/2) 
dados <- dados[-24,]
dados.scale <-scale(dados[,-1])
dados.dist <- dist(dados.scale)

WorldData <- map_data('worldLores') #use the old map
WorldData <- fortify(WorldData)

#====================================================

#Hierarquico

dados_dend <- hclust(dados.dist)
Grupos <- as.factor(cutree(dados_dend, 5))
dados[,11] <- rep("",24)
colnames(dados)[11] <- "Grupos"

df<-data.frame(country=dados[,1],stringsAsFactors=FALSE)

Div1 <- c("Balkans","Europa Central","Leste Europeu",
          "Países Nórdicos","Península Ibérica")

for(i in 1:length(Grupos)){
  dados[i,11] <- Div1[Grupos[i]]
}

ggplot(dados,aes(y=Fr.Veg,x=Grupos)) +
  geom_boxplot(fill="darkgreen")
  

ggplot() +
  geom_map(data=df, map=WorldData,
#          aes(fill=dados$RedMeat, map_id=country),
          aes(fill=Grupos, map_id=country),
          colour="black", size=0.5) +
#  scale_fill_gradient(low = "cyan", high = "purple")+
  coord_sf(xlim = c(-13.18, 51.69), ylim = c(71.86, 32.67), expand = T)

Div <- function(a){
  return(c(mean(dados[Grupos==1,a]),mean(dados[Grupos==2,a]),
           mean(dados[Grupos==3,a]),mean(dados[Grupos==4,a]),
           mean(dados[Grupos==5,a])))
  }

Div1FruitsVeg <- Div(10)

Temp <- data.frame(Div1,Div1FruitsVeg)

ggplot(data=Temp,aes(x=Div1,y=Div1FruitsVeg))+
  geom_bar(stat="identity",fill="darkgreen")

plot(dados_dend, xlab = "", sub = "", ylab = "",
     labels = dados[,1])

#====================================================
