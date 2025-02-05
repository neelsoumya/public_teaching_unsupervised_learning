###################################################
# Practicals
# Chapter 10 Lab 1: Principal Components Analysis
###################################################

# load library
#  install.packages('ISLR')
library(ISLR)

# Try demo below
https://projector.tensorflow.org/
  

states=row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

pr.out$center
pr.out$scale
pr.out$rotation

dim(pr.out$x)

#################
# plot biplot
#################
biplot(pr.out, scale=0)

pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

#########################
# Generate scree plot
#########################
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
a=c(1,2,8,-3)
cumsum(a)

# TODO: apply to image data
#   https://www.rpubs.com/a_pear_9/pca_on_images

# TODO: apply to bulk sequencing data
library(raster)
# Import the picture as a raster
pepper <- stack("Pepper.PNG")[[1:3]]
# Plot it
plotRGB(pepper)

plot(
  pepper[[3]], 
  col = rev(blues9), 
  asp = 1, 
  axes = FALSE,
  legend = FALSE
)


# To perform PCA, we can’t leave it in image format. We want to create a data.frame with each pixel in a single row and each colour band as a column.


# Extract all pixels as data frame
pepper.df <- as.data.frame(pepper)
# Have a quick look
head(pepper.df)

# Compute the principle components
# Remember to scale the variables
pca <- prcomp(pepper.df, scale. = TRUE)
# Save the principle components
pepper.pc <- pca$x
pepper$pc1 <- pepper.pc[,1]
pepper$pc2 <- pepper.pc[,2]
pepper$pc3 <- pepper.pc[,3]

# The interesting thing about these principle components
# is that each pixel in the image has a value of each 
# of the principle components, so we can plot an image 
# of the principle components. 
plot(pepper$pc1, col = cm.colors(15), axes = FALSE)

plot(pepper$pc2, col = cm.colors(15), axes = FALSE)

plot(pepper$pc3, col = cm.colors(15), axes = FALSE)

# The first principle component image is basically
# the original image. 
# But the other two are very interesting. 
# The second principle component picks out my hand 
# – a distinct part of the image that isn’t white-grey-black.
# The third principle component picks out Pepper’s collar, 
# which is another distinct part of the image.

#################################
# Chapter 10 Lab 2: Clustering
#################################

# K-Means Clustering

set.seed(2)
x=matrix(rnorm(50*2), ncol=2)
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

plot(x)

# nstart = multiple initial cluster assignments
km.out=kmeans(x,centers=2,nstart=20)
km.out$cluster

# READ the following
#    https://medium.com/analytics-vidhya/comparison-of-initialization-strategies-for-k-means-d5ddd8b0350e
# from all initialization strategies, choose the one that gives you minimum within cluster sum of squared

plot(x, col=(km.out$cluster), main="K-Means Clustering Results with K=2", xlab="", ylab="", pch=20, cex=2)

set.seed(4)
km.out=kmeans(x,centers=3,nstart=20)
km.out

plot(x, col=(km.out$cluster), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

set.seed(3)
km.out=kmeans(x,centers=3,nstart=1)
km.out$tot.withinss
plot(x, col=(km.out$cluster), main = "K-Means clustering results with K=2 and nstart=1", xlab="", ylab="", pch=20, cex=2)

km.out=kmeans(x,centers=3,nstart=20)
km.out$tot.withinss
plot(x, col=(km.out$cluster), main = "K-Means clustering results with K=2 and nstart=20", xlab="", ylab="", pch=20, cex=2)

############################
# Hierarchical Clustering
############################

# The dist() function is used
# to compute the 50 × 50 inter-observation Euclidean distance matrix

dist(x)

dist_x = dist(x)
View(dist_x)

hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")

par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)

cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
cutree(hc.single, 4)

# scale and then do clustering
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")

###################################################
# Exercise: do the other dissimilarity functions
#    and compare
###################################################

# correlation based distance
x_3d = matrix(rnorm(30*3), ncol=3)
correlation_distance = as.dist(1-cor(t(x_3d)))
plot(hclust(correlation_distance, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")

# EXERCISE: try the other dissimilarity measures

########################################
# Chapter 10 Lab 3: NCI60 Data Example
########################################

# The NCI60 data

library(ISLR)
nci.labs=NCI60$labs
nci.data=NCI60$data
dim(nci.data)
nci.labs[1:4]
table(nci.labs)

# PCA on the NCI60 Data

pr.out=prcomp(nci.data, scale=TRUE)
Cols=function(vec){
    cols=rainbow(length(unique(vec)))
    return(cols[as.numeric(as.factor(vec))])
  }
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19,xlab="Z1",ylab="Z3")
summary(pr.out)
plot(pr.out)
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")

# Clustering the Observations of the NCI60 Data

sd.data=scale(nci.data)
par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs, main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,  main="Single Linkage", xlab="", sub="",ylab="")
hc.out=hclust(dist(sd.data))
hc.clusters=cutree(hc.out,4)
table(hc.clusters,nci.labs)
par(mfrow=c(1,1))
plot(hc.out, labels=nci.labs)
abline(h=139, col="red")
hc.out
set.seed(2)
km.out=kmeans(sd.data, 4, nstart=20)
km.clusters=km.out$cluster
table(km.clusters,hc.clusters)
hc.out=hclust(dist(pr.out$x[,1:5]))
plot(hc.out, labels=nci.labs, main="Hier. Clust. on First Five Score Vectors")
table(cutree(hc.out,4), nci.labs)

