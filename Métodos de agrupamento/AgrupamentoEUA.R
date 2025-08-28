library(datasets)

crimes <- USArrests
crimes_scale <-scale(crimes)

#====================================================

#Hierarquico

par(mfrow = c(1, 2))

crimes_dist <- dist(crimes_scale)

plot(hclust(crimes_dist), xlab = "", sub = "", ylab = "",
    labels = row.names(crimes), main = "Complete Linkage")
plot(hclust(crimes_dist, method = "average"), xlab = "",
     sub = "", ylab = "", labels = row.names(crimes),
     main = "Average Linkage")
plot(hclust(crimes_dist, method = "single"), xlab = "",
     sub = "", ylab = "", labels = row.names(crimes),
     main = "Single Linkage")

crimes_dend <- hclust(crimes_dist)
grupos <- cutree(crimes_dend, 5)
table(grupos, row.names(crimes))

row.names(crimes)[grupos==1]

par(mfrow = c(1, 1))

grupo1 <- crimes[grupos==1,]
grupo3 <- crimes[grupos==3,]

summary(grupo1)

#====================================================

#K médias

set.seed(sample(1:1000,1))

crimes_km <- kmeans(crimes_scale, 5)

grupos_km <- crimes_km$cluster

grupo_km1 <- crimes[grupos_km==1,]
grupo_km3 <- crimes[grupos_km==3,]

summary(grupo_km1)
summary(grupo_km3)

table(grupos_km, grupos)
