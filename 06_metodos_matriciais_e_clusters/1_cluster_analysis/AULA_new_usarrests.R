us=USArrests

usdrivers=us[,2:4]

par(mfrow=c(1,3))
boxplot(usdrivers$murder, col=2, main="murder")
boxplot(usdrivers$assault, col=3, main="assault")
boxplot(usdrivers$rape, col=4, main="rape")

print(cor(usdrivers), digits = 2)

us.scale=scale(usdrivers)
head(us.scale)

us.dist=dist(us.scale)
hc=hclust(us.dist, method = "ward.D2")
plot(hc, hang=-1, main="USArrests com ward.D2")

us$hc=cutree(hc,2)
par(mfrow=c(1,3))
boxplot(usdrivers$murder~us$hc, col=2, main="murder")
boxplot(usdrivers$assault~us$hc, col=3, main="assault")
boxplot(usdrivers$rape~us$hc, col=4, main="rape")

boxplot(us$UrbanPop ~us$hc, col=5, main="UrbanPop")
library(gmodels)
CrossTable(us$SFR,us$hc, prop.c = F, prop.chisq = F, prop.t = F)

aggregate(us[,2:4],list(us$hc), median)

us$state[us$hc==1]
us$state[us$hc==2]

#determinação do numero de clusters , uso do package Nbclust 
library(NbClust)
nb=NbClust(data=us.scale,diss= us.dist, distance = NULL, min.nc = 2,max.nc = 8,method = "ward.D2", index = "all" )


########################################################
###   K-means    ######

#determinação de número de clusters
library(NbClust)
nk=NbClust(data=us.scale, min.nc = 2,max.nc = 8,method = "kmeans", index = "all" )

set.seed(18)
kmn=kmeans(us.scale,2,nstart=25) #25 partições iniciais
us$kmn=kmn$cluster
kmn$size
kmn$centers
table(us$hc,us$kmn)
