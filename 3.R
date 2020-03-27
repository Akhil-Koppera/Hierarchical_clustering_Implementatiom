###########################################################################
## This code is to aplly seeds_dataset.txt for an unsupervised model
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

data <- read.delim("seeds_dataset.txt",header = T)
#data<-scale(data)
data<-na.omit(data)
x=subset(data,select=-c(Seed.Group))
x<-scale(x)
y=data.frame(data$Seed.Group)
hc.complete =hclust (dist(x), method = "complete")
hc.single =hclust (dist(x), method = "single")
hc.average =hclust (dist(x), method = "average")
dim(unique(y))

x11()
gap_stat <- clusGap(x, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


x11()
par(mfrow=c(3,1))
plot(hc.complete,main =" Complete Linkage with correlation", xlab="", sub="", cex =.9)
rect.hclust(hc.complete , k = 3, border = 2:6)
abline(h = 3, col = 'red')
ct_complete<-cutree(hc.complete ,3)

plot(hc.single,main =" Single Linkage with correlation", xlab="", sub="", cex =.9)
rect.hclust(hc.single , k = 3, border = 2:6)
abline(h = 3, col = 'red')
ct_single<-cutree(hc.single ,3)

plot(hc.average,main =" Average Linkage with correlation", xlab="", sub="", cex =.9)
rect.hclust(hc.average , k = 3, border = 2:6)
abline(h = 3, col = 'red')
ct_avg<-cutree(hc.average ,3)
#table(cutree(hc.average ,3),as.numeric(y))

confusionMatrix(as.factor(as.numeric(y$data.Seed.Group)), as.factor(ct_complete))
confusionMatrix(as.factor(as.numeric(y$data.Seed.Group)), as.factor(ct_single))
confusionMatrix(as.factor(as.numeric(y$data.Seed.Group)), as.factor(ct_avg))


k=10

d=data.frame(k=NA,WCSS=NA)

for (i in 1:k) {
  
  km.out =kmeans (x,i, nstart =20)
  d[i,]=c(i,km.out$tot.withinss)
  
  
}
set.seed(5)


x11()
plot(d$k,d$WCSS,type="l",main="Elbow Method for Selecting K-Value",xlab="K-Value",ylab = "WCSS")


km_model<-kmeans(x,3,nstart=15)


x11()
gap_kmeans=clusGap(x, kmeans, nstart = 15, K.max = 10)
plot(gap_kmeans)


  
km3<-pamk(x,3)
par(mfrow=c(1,2))
plot(km3$pamobject)

confusionMatrix(as.factor(as.numeric(y$data.Seed.Group)),as.factor(km_model$cluster))

