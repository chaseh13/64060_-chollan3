library(cluster)
library(caret)
library(dendextend)
library(knitr)
library(factoextra)
library(readr)


Cereals <- read.csv("C:/Users/choll/Downloads/Cereals.csv")
NumData <- data.frame(Cereals[,4:16])


NumData <- na.omit(NumData)


CerealsNormalize <- scale(NumData)


Dist <- dist(CerealsNormalize, method = "euclidean")
HClust <- hclust(Dist, method = "complete")


plot(HClust, cex = 0.7, hang = -1)


SingleHClust <- agnes(CerealsNormalize, method = "single")
CompleteHClust <- agnes(CerealsNormalize, method = "complete")
AverageHClust <- agnes(CerealsNormalize, method = "average")
wardHClust <- agnes(CerealsNormalize, method = "ward")


print(SingleHClust$ac)
print(CompleteHClust$ac)
print(AverageHClust$ac)
print(wardHClust$ac)


pltree(wardHClust, cex = 0.5, hang = -1, main = "Dendrogram of agnes (Using Ward)")
rect.hclust(wardHClust, k = 5, border = 2:7)
SGroup <- cutree(wardHClust, k=5)
DFrame2 <- as.data.frame(cbind(CerealsNormalize,SGroup))


fviz_cluster(list(data = DFrame2, cluster = SGroup))


set.seed(123)
PartitionA <- NumData[1:55,]
PartitionB <- NumData[56:74,]


SingleSL <- agnes(scale(PartitionA), method = "single")
CompleteSL <- agnes(scale(PartitionA), method = "complete")
AverageSL <- agnes(scale(PartitionA), method = "average")
WardSL <- agnes(scale(PartitionA), method = "ward")
cbind(single=SingleSL$ac , complete=CompleteSL$ac , average= AverageSL$ac , ward= WardSL$ac)
pltree(WardSL, cex = 0.6, hang = -1, main = "Dendogram of Agnes with Partitioned Data (Using Ward)")
rect.hclust(WardSL, k = 6, border = 2:7)
Cut2 <- cutree(WardSL, k = 6)


ResultSL <- as.data.frame(cbind(PartitionA, Cut2))
ResultSL[ResultSL$Cut2==1,]
CentroidOne <- colMeans(ResultSL[ResultSL$Cut2==1,])
ResultSL[ResultSL$Cut2==2,]
CentroidTwo <- colMeans(ResultSL[ResultSL$Cut2==2,])
ResultSL[ResultSL$Cut2==3,]
CentroidThree <- colMeans(ResultSL[ResultSL$Cut2==3,])
ResultSL[ResultSL$Cut2==4,]
CentroidFour <- colMeans(ResultSL[ResultSL$Cut2==4,])
centroids <- rbind(CentroidOne, CentroidTwo, CentroidThree, CentroidFour)
x2 <- as.data.frame(rbind(centroids[,-14], PartitionB))


Dist1 <- get_dist(x2)
Matrix1 <- as.matrix(Dist1)
dataframe1 <- data.frame(data=seq(1,nrow(PartitionB),1), Clusters =rep(0,nrow(PartitionB)))
for(i in 1:nrow(PartitionB)) 
{dataframe1[i,2] <- which.min(Matrix1[i+4, 1:4])}
dataframe1
cbind(DFrame2$SGroup[56:74], dataframe1$Clusters)
table(DFrame2$SGroup[56:74] == dataframe1$Clusters)


HealthyCereals <- Cereals
HealthyCerealsRD <- na.omit(HealthyCereals)
clust <- cbind(HealthyCerealsRD, SGroup)
clust[clust$SGroup==1,]
clust[clust$SGroup==2,]
clust[clust$SGroup==3,]
clust[clust$SGroup==4,]


mean(clust[clust$SGroup==1,"rating"])
mean(clust[clust$SGroup==2,"rating"])
mean(clust[clust$SGroup==3,"rating"])
mean(clust[clust$SGroup==4,"rating"])
