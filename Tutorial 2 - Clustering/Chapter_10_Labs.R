# Chapter 10 Lab 1: Principal Components Analysis

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
biplot(pr.out, scale=0)
pr.out$rotation=-pr.out$rotation
pr.out$x=-pr.out$x
biplot(pr.out, scale=0)
pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')
a=c(1,2,8,-3)
cumsum(a)


# Chapter 10 Lab 2: Clustering

# K-Means Clustering

# set seed to make the code reproducible (get the same random numbers)
set.seed(2)
# create 50 observations (25 x 2 matrix)
x=matrix(rnorm(50*2), ncol=2)
# skewing the dataset:
# shift values of first column byq +3
x[1:25,1]=x[1:25,1]+3
# shift values of second column by -3
x[1:25,2]=x[1:25,2]-4

# perform K-means clustering with K = 2
km.out=kmeans(x,2,nstart=20)
# cluster assignments of the 50 observations are contained in:
km.out$cluster
# plot the data, with each observation colored according to its cluster
# assignment
plot(x, 
     col=(km.out$cluster+1), 
     main="K-Means Clustering Results with K=2", 
     xlab="", 
     ylab="", 
     pch=20, 
     cex=2)

# We could instead have performed K-means clustering on this example with K = 3
set.seed(4)
km.out=kmeans(x,3,nstart=20)
km.out
plot(x, col=(km.out$cluster+1), main="K-Means Clustering Results with K=3", xlab="", ylab="", pch=20, cex=2)

# To run the kmeans() function in R with multiple initial cluster assignments, we use the nstart argument. If a value of nstart greater than one
# is used, then K-means clustering will be performed using multiple random
# assignments in Step 1 of Algorithm 10.1, and the kmeans() function will
# report only the best results. Here we compare using nstart=1 to nstart=20.
set.seed(3)
km.out=kmeans(x,3,nstart=1)
# total within-cluster sum of squares,
# which we seek to minimize by performing K-means clustering
km.out$tot.withinss
km.out=kmeans(x,3,nstart=20)
km.out$tot.withinss

# Hierarchical Clustering

# The hclust() function implements hierarchical clustering in R. In the fol- hclust()
# lowing example we use the data from Section 10.5.1 to plot the hierarchical
# clustering dendrogram using complete, single, and average linkage cluster-
#   ing, with Euclidean distance as the dissimilarity measure. We begin by
# clustering observations using complete linkage. The dist() function is used dist()
# to compute the 50 × 50 inter-observation Euclidean distance matrix
hc.complete=hclust(dist(x), method="complete")
hc.average=hclust(dist(x), method="average")
hc.single=hclust(dist(x), method="single")
#  plot the dendrograms obtained
# The numbers at the bottom of the plot identify each observation
par(mfrow=c(1,3))
plot(hc.complete,main="Complete Linkage", xlab="", sub="", cex=.9)
plot(hc.average, main="Average Linkage", xlab="", sub="", cex=.9)
plot(hc.single, main="Single Linkage", xlab="", sub="", cex=.9)
# To determine the cluster labels for each observation associated with a
# given cut of the dendrogram, we can use the cutree() function:
cutree(hc.complete, 2)
cutree(hc.average, 2)
cutree(hc.single, 2)
# For this data, complete and average linkage generally separate the observa-
# tions into their correct groups. However, single linkage identifies one point
# as belonging to its own cluster. A more sensible answer is obtained when
# four clusters are selected, although there are still two singletons.
cutree(hc.single, 4)
# To scale the variables before performing hierarchical clustering of the
# observations, we use the scale() function: 
xsc=scale(x)
plot(hclust(dist(xsc), method="complete"), main="Hierarchical Clustering with Scaled Features")
# Correlation-based distance can be computed using the as.dist() func- as.dist()
# tion, which converts an arbitrary square symmetric matrix into a form that
# the hclust() function recognizes as a distance matrix. However, this only
# makes sense for data with at least three features since the absolute corre-
#   lation between any two observations with measurements on two features is
# always 1. Hence, we will cluster a three-dimensional data set
x=matrix(rnorm(30*3), ncol=3)
dd=as.dist(1-cor(t(x)))
plot(hclust(dd, method="complete"), main="Complete Linkage with Correlation-Based Distance", xlab="", sub="")


# Chapter 10 Lab 3: NCI60 Data Example

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

