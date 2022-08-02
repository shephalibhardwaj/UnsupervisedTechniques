library(cluster)
library(factoextra)
library(ggbiplot)

winedata <- read.csv("wine-data.csv",header = TRUE)
dim(winedata)

winedata1 <- winedata[,2:14]
head(winedata1)

#Step 2 
WinePCA <- princomp(winedata1, cor = TRUE, scores = TRUE, covmat = NULL)
summary(WinePCA)
plot(WinePCA)

#Q1(b)
WinePCA$loadings
WinePCA$scores

biplot(WinePCA$scores[,1:2],WinePCA$loading[,1:2])

screeplot(WinePCA, npcs = 24, type = "lines")


#Step 3 part 1
standardized_winedata <- scale(winedata1)
set.seed(1111)
fit <- kmeans(standardized_winedata, centers=3, iter.max=10, nstart=10)
fit$size

Cluster_Variability <- matrix(nrow=8, ncol=1)
for (i in 1:8) Cluster_Variability[i] <- kmeans(standardized_winedata,centers=i, nstart=10)$tot.withinss
plot(1:8, Cluster_Variability, type="b", xlab="Number of clusters", ylab="Within groups sum of squares")

t(fit$centers)

fviz_cluster(list(data = winedata, cluster = fit$cluster))

WineWithClusters <- cbind(winedata, fit$cluster)
write.csv(WineWithClusters,file="WineWithClusters.csv")

#step 3 part 2

scaled_data <- scale(winedata1)
rmse <- matrix(nrow=2,ncol=1)
for (i in 1:2) {
  scaled_data_recovered <- WinePCA$scores[,1:i] %*% t(WinePCA$loadings[,1:i]) ## if compressed using two components
  scaled_data_recovered
  rmse[i] <- sum((scaled_data_recovered - scaled_data)^2) ## errors
}
rmse
scaled_data_recovered

fit1 <- kmeans(scaled_data_recovered, centers=3, iter.max=10, nstart=10)
fit1$size
t(fit1$centers)

fviz_cluster(list(data = winedata, cluster = fit1$cluster))

WineWithClusters_PCA <- cbind(winedata, fit1$cluster)
write.csv(WineWithClusters_PCA,file="WineWithClusters_PCA.csv")