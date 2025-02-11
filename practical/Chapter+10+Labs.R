###################################################
# Practicals
# Chapter 10 Lab 1: Principal Components Analysis
###################################################

# load library
#  install.packages('ISLR')
#  install.packages('raster')
library(ISLR)
library(raster)

########################################
# Try demo below
# https://projector.tensorflow.org/
########################################
  

states=row.names(USArrests)
states
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# Apply PCA
pr.out=prcomp(USArrests, scale=TRUE)
names(pr.out)

# means and standard deviations of variables (before scaling)
pr.out$center 
pr.out$scale
# get loadings 
pr.out$rotation

dim(pr.out$x)
pr.out$x # scores

#################
# plot biplot
#################
biplot(pr.out, scale=0) # scale = 0: arrows are scaled to represent loadings

# I can rotate the co-ordinate system 
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)

#########################
# Generate scree plot
#########################
pr.out$sdev # standard deviation of each principal component
pr.var=pr.out$sdev^2 # get variance
pr.var
pve=pr.var/sum(pr.var)
pve

plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')

# plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
#a=c(1,2,8,-3)
#cumsum(a)

#################################################
# apply to image data
#   adapted from 
#   https://www.rpubs.com/a_pear_9/pca_on_images
#################################################

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


# To perform PCA, we can’t leave it in image format. 
# We want to create a data.frame with each pixel in
# a single row and each colour band as a column.


# Extract all pixels as data frame
pepper.df <- as.data.frame(pepper)
# Have a quick look
head(pepper.df)

# Compute the principle components
# Remember to scale the variables
pca <- prcomp(pepper.df, scale = TRUE)

# Save the principle components
pepper.pc <- pca$x
pepper$pc1 <- pepper.pc[,1] # take scores for 1st PC
pepper$pc2 <- pepper.pc[,2] # take scores for 2nd PC
pepper$pc3 <- pepper.pc[,3] # take scores for 3rd PC

# CONCEPT: you can plot these scores

# The interesting thing about these principal components
# is that each pixel in the image has a value of each 
# of the principle components, so we can plot an image 
# of the principle components. 
plot(pepper$pc1, col = cm.colors(15), axes = FALSE)

plot(pepper$pc2, col = cm.colors(15), axes = FALSE)

plot(pepper$pc3, col = cm.colors(15), axes = FALSE)

# The first principal component image is basically
# the original image. 
# But the other two are very interesting. 
# The second principle component picks out a hand 
# – a distinct part of the image that isn’t white-grey-black.
# The third principle component picks out Pepper’s collar, 
# which is another distinct part of the image.

# biplot(pca, scale=0)

####################################
#  Advanced exercise
#  apply to bulk sequencing data
#    see genomic_data_PCA.R
####################################


#################################
# Chapter 10 Lab 2: Clustering
#################################

# K-Means Clustering

set.seed(2) # set seed (k-means is random)
# create random data
x=matrix(rnorm(50*2), ncol=2)

# add some noise
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4

x

plot(x)

# nstart = multiple initial cluster assignments
?kmeans
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

# TODO: plot withinss as a function of number of clusters (k)

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

#cutree(hc.complete, 2)
#cutree(hc.average, 2)
#cutree(hc.single, 2)
#cutree(hc.single, 4)

# scale and then do clustering
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")

###################################################
# Exercise: do the other dissimilarity functions
#    and compare
###################################################

# correlation based distance
# Example
cor(x=c(1,3,10), y=c(0, 6, 20))

#Correlation-based distance can be computed using 
# the as.dist() funcation, which converts an arbitrary
# square symmetric matrix into a form that
# the hclust() function recognizes as a distance matrix. 
# However, this only makes sense for data with at least 
# three features since the absolute correlation
# between any two observations with measurements on two
# features is always 1. Hence, we will cluster a 
# three-dimensional data set.

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

