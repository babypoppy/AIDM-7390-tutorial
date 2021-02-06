#######################################
## Agglomerative Hierachical         ##
#######################################

library(stats)

## the IRIS dataset
str(iris)
summary(iris)


## hierarchical clustering
set.seed(2835)
# draw a sample of 40 records from the iris data, so that the clustering plot will not be over crowded
idx <- sample(1:dim(iris)[1], 40)
iris3 <- iris[idx,]
# remove class label
iris3$Species <- NULL
# hierarchical clustering
hc <- hclust(dist(iris3), method="ave")
# plot clusters
plot(hc, hang = -1, labels=iris$Species[idx])
# cut tree into 3 clusters
rect.hclust(hc, k=3)
# get cluster IDs
groups <- cutree(hc, k=3)
groups
plot(hc, hang = -1, labels=iris$Species[idx])
rect.hclust(hc, k=3) # cut tree into 3 clusters

#######################################
## Divisive Hierachical              ##
#######################################

## clustering with DIANA
library(cluster)
diana.result <- diana(iris3)
## plot(diana.result, which.plots=2, labels=iris$Species[idx])
plot(diana.result, which.plots=2, labels=iris$Species[idx])

#######################################
## Density Based                     ##
#######################################

## Density-based Clustering
library(fpc)
iris2 <- iris[-5] # remove class tags
ds <- dbscan(iris2, eps=0.42, MinPts=5)
ds
# compare clusters with actual class labels
table(ds$cluster, iris$Species)
library(gmodels)
CrossTable(ds$cluster, iris$Species)

plot(ds, iris2)
plot(ds, iris2[, c(1,4)])
plotcluster(iris2, ds$cluster)

#######################################
## Cluster Prediction                ##
#######################################

# create a new dataset for labeling
set.seed(435) 
idx <- sample(1:nrow(iris), 10)
# remove class labels
new.data <- iris[idx,-5]
# add random noise
new.data <- new.data + matrix(runif(10*4, min=0, max=0.2), 
                              nrow=10, ncol=4)
# label new data
pred <- predict(ds, iris2, new.data) 
table(pred, iris$Species[idx]) # check cluster labels
plot(iris2[, c(1,4)], col=1+ds$cluster)
points(new.data[, c(1,4)], pch="+", col=1+pred, cex=3)