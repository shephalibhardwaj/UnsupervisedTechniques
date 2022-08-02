library(tidyverse)
library(cluster)
library(factoextra)
library(fastDummies)

primarydata <- read.csv("EastWestAirlinesCluster.csv",header=TRUE)
dim(primarydata)
set.seed(1111)

primarydata <- dummy_cols(primarydata,select_columns = 'cc1_miles')
primarydata <- dummy_cols(primarydata,select_columns = 'cc2_miles')
primarydata <- dummy_cols(primarydata,select_columns = 'cc3_miles')
airlinesdata <- scale(primarydata[1:3999,c(2:3,7:11)])

finaldata <- cbind(primarydata[,c(12:25)],airlinesdata)

originaldata <- read.csv("EastWestAirlinesCluster.csv",header=TRUE)
originaldata1 <- originaldata[1:3999,2:12]
set.seed(1111)
standardized_data <- scale(originaldata[1:3999,2:12])

#Question 1
data1 <- dist(finaldata, method = "euclidean")
fit1 <- hclust(data1, method="ward.D")
plot(fit1,labels = NULL,hang = -1)
options(scipen=9999)
rect.hclust(fit1, k=3, border = 'blue')

data2 <- dist(standardized_data, method = "euclidean")
fit2 <- hclust(data2, method="ward.D")
plot(fit2,labels = NULL,hang = -1)
options(scipen=9999)
rect.hclust(fit2, k=3, border = 'red')

#Question 2

groups <- cutree(fit1, k=3)
table(groups)
fviz_cluster(list(data = finaldata, cluster = groups))

groups <- cutree(fit2,k=3)
table(groups)
fviz_cluster(list(data = standardized_data, cluster = groups))

#Question 3

clust.centroid = function(i, dat, groups) {
  ind = (groups == i)
  colMeans(dat[ind,])
}
sapply(unique(groups), clust.centroid, originaldata1, groups)

membership<-as.matrix(groups)
Nonfreq_Fliers <- subset(originaldata1,membership[,1]==1)
Premium_Fliers <- subset(originaldata1,membership[,1]==2)
FreqEconomy_Fliers <- subset(originaldata1,membership[,1]==3)

colnames(membership)[1] <- "Cluster ID"
AirlinesWith_HClusters <- cbind(originaldata1,membership)
write.csv(AirlinesWith_HClusters,file="AirlineData_WithHierarchicalClusters.csv")

#Question 4

sample <- sample(nrow(originaldata1),nrow(originaldata1)*0.95)
normalized_data_95 <- scale(originaldata1[sample,])
data3 <- dist(normalized_data_95, method = "euclidean")
fit3 <- hclust(data3, method="ward.D")
plot(fit3,labels = NULL,hang = -1)
rect.hclust(fit3, k=3, border="green")

groups1 <- cutree(fit3, k=3)

table(groups1)

#k-mean clustering Question 5

fit4 <- kmeans(standardized_data, centers=3, iter.max=10, nstart=10)
fit4$size
fit4$centers

Cluster_Variability <- matrix(nrow=8, ncol=1)
for (i in 1:8) Cluster_Variability[i] <- kmeans(standardized_data,centers=i, nstart=10)$tot.withinss
plot(1:8, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")

#Question 6

sapply(unique(groups), clust.centroid, originaldata1, groups)
fit5 <- kmeans(originaldata1, centers=3, iter.max=10, nstart=10)
fit5$centers

#question 7

ArlinesWithClusters_kmean <- cbind(originaldata1, fit4$cluster)
write.csv(ArlinesWithClusters_kmean,file="AirlineData_WithkmeanClusters.csv")
