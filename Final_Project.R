
knitr::opts_chunk$set(echo = TRUE)


FuelCosts <- read.csv("C:/Users/choll/Downloads/fuel_receipts_costs_eia923.csv")


library(caret)
set.seed(3272)
FuelCosts.Train<-createDataPartition(FuelCosts$fuel_type_code_pudl,p=0.02,list=F)
SampleData = FuelCosts[FuelCosts.Train,]

TrainIndex=createDataPartition(SampleData$fuel_type_code_pudl, p=0.75, list=F)
TrainData=SampleData[TrainIndex,]
ValidationData=SampleData[-TrainIndex,]


library(dplyr)
sapply(TrainData,function(x) sum(is.na(x)))
head(TrainData)


FuelCosts.Train <- TrainData[c(2,15,16,17,18,20)]
head(FuelCosts.Train)
summary(FuelCosts.Train)


FuelCosts.Train2 <- FuelCosts.Train[,-6]
summary(FuelCosts.Train2)

FuelCosts1Validation <- ValidationData[c(2,15,16,17,18,20)]
summary(FuelCosts1Validation)


FuelCosts2Validation <- FuelCosts1Validation[,-6]
summary(FuelCosts2Validation)


library(tidyverse)
FuelCosts.Train3 <- scale(FuelCosts.Train2[,1:5])
head(FuelCosts.Train3)
FuelCosts3Validation<-scale(FuelCosts2Validation[,1:5])
head(FuelCosts3Validation)


library(factoextra)
wss <- fviz_nbclust(FuelCosts.Train3,kmeans,method="wss")
wss


silhouete <- fviz_nbclust(FuelCosts.Train3,kmeans,method="silhouette")
silhouete


cluster.kmean <- kmeans(FuelCosts.Train3,centers=7,nstart=25)
cluster.k1<-kmeans(FuelCosts.Train3,centers = 7, nstart = 25)
fviz_cluster(cluster.k1, data = FuelCosts.Train3)
cluster.k1$size
cluster.k1$centers
