###########################################################################
## This code is to aplly USArrests for an unsupervised model
## Akhil Koppera 
## Created: March 17, 2020
###########################################################################

rm(list = ls())

# set the current working directory
setwd("F:/SDM 2/akhilkop_hw2") 

library(cluster)
library (ISLR)
library(ElemStatLearn)
library(caret)
library(dendextend)
library(fpc)
library(factoextra)
library (ISLR)

data=USArrests
data.labs=data[0]
hc.complete =hclust (dist(data,method='euclidean'), method = "complete")

x11()
plot(hc.complete ,main =" Complete Linkage ", xlab="", sub ="", cex =.9,hang=-1)

rect.hclust(hc.complete , k = 3, border = 2:6)
abline(h = 3, col = 'red')

cutree(hc.complete,3)

hc.clusters =cutree (hc.complete ,3)
hc.clusters[(hc.clusters==1)]
hc.clusters[(hc.clusters==2)]
hc.clusters[(hc.clusters==3)]
table(hc.clusters)



sd.data=scale(data)

hc.sd =hclust (dist(sd.data,method='euclidean'), method = "complete")

x11()
plot(hc.sd ,main =" Complete Linkage for Scaled Data", xlab="", sub ="", cex =.6)

rect.hclust(hc.sd , k = 3, border = 2:6)
abline(h = 3, col = 'red')


sd_clusters<-cutree(hc.sd,3)
sd_clusters[(sd_clusters==1)]
sd_clusters[(sd_clusters==2)]
sd_clusters[(sd_clusters==3)]

table(sd_clusters)

confusionMatrix(as.factor(sd_clusters), as.factor(hc.clusters))
