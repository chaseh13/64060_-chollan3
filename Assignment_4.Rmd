library(readr)
pharms <- read.csv("C:/Users/choll/Downloads/Pharmaceuticals.csv")

library(ggplot2)
library(factoextra)
library(flexclust)
library(cluster)
library(tidyverse)
summary(pharms)

SLV <- na.omit(pharms)
SLV
row.names <- SLV[,1]
pharms1 <-  SLV[,3:11]
head(pharms1)
pharms2 <- scale(pharms1)
head(pharms2)
fviz_nbclust(pharms2, kmeans, method = "wss") +
  labs(subtitle = "Elbow Method")
fviz_nbclust(pharms2, kmeans, method = "silhouette") + labs(subtitle = "Silhouette Method")
fviz_nbclust(pharms2, kmeans, method = "gap_stat") + labs(subtitle = "Gap Stat Method")

set.seed(64060)
k5 <- kmeans(pharms2, centers = 5, nstart = 25)
k5 $centers
fviz_cluster(k5, data = pharms2)
k5
dist <- dist(pharms2, method = "euclidian")
fviz_dist(dist)
fitting <- kmeans(pharms2,5)
aggregate(pharms2,by = list(fitting$cluster), FUN = mean)
Pharmaceuticals3 <- data.frame(pharms2,fitting$cluster)
Pharmaceuticals3
library(cluster)
clusplot(pharms2,fitting$cluster, color = TRUE, shade = TRUE, 
         labels = 2,
         lines = 0)

aggregate(pharms2, by = list(fitting$cluster), FUN = mean)
pharmacy <- data.frame(pharms2,k5$cluster)
pharmacy

S1 <- pharms[12:14] %>% mutate(Clusters=k5$cluster)
ggplot(S1, mapping = aes(factor(Clusters), fill =Median_Recommendation))+geom_bar(position='dodge')+labs(x ='Clusters',y ='Frequence')
ggplot(S1, mapping = aes(factor(Clusters),fill = Location))+
  geom_bar(position = 'dodge')+labs(x ='Clusters',y = 'Frequence')
ggplot(S1, mapping = aes(factor(Clusters),fill = Exchange))+geom_bar(position = 'dodge')+
  labs(x ='Clusters',y = 'Frequence')