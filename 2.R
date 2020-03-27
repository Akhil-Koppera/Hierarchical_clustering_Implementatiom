###########################################################################
## This code is to aplly Ch10Ex11.csv for an unsupervised model
## Akhil Koppera 
## Created: March 17, 2020
###########################################################################

rm(list = ls())

# set the current working directory
setwd("F:/SDM 2/akhilkop_hw2") 

library (ISLR)
library(ElemStatLearn)
library("cluster")

data=read.csv("Ch10Ex11.csv", header=F)
hc.complete =hclust (as.dist(1-cor(data)), method = "complete")
hc.single =hclust (as.dist(1-cor(data)), method = "single")
hc.average =hclust (as.dist(1-cor(data)), method = "average")



x11()
par(mfrow=c(3,1))
plot(hc.complete,main =" Complete Linkage with correlation", xlab="", sub="", cex =.9)
rect.hclust(hc.complete , k = 2, border = 2:6)
abline(h = 2, col = 'red')

#x11()
plot(hc.single,main =" Single Linkage with correlation", xlab="", sub="", cex =.9)
rect.hclust(hc.single , k = 2, border = 2:6)
abline(h = 2, col = 'red')

#x11()
plot(hc.average,main =" Average Linkage with correlation", xlab="", sub="", cex =.9)
rect.hclust(hc.average , k = 2, border = 2:6)
abline(h = 2, col = 'red')

pca=prcomp(t(data))
summary(pca)
head(pca$rotation)
x11()
#PVE
pve=pca$sdev^2/sum(pca$sdev^2)
plot(pve, xlab = "Principal Component", ylab = "PVE",type = 'b')

#Factor loadings
fac_load=apply(pca$rotation, 1, sum)
gene_indices=order(abs(fac_load), decreasing = TRUE)
gene_indices[1:20]


