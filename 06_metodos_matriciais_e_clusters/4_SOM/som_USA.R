usa=USArrests
usa.num=usa[,2:4]
cor(usa.num)
usa.scl=scale(usa.num)

library(kohonen)
usa.grid = somgrid(xdim = 5, ydim=5, topo="rectangular")
usa.som = som(usa.scl, grid=usa.grid, rlen=800, alpha=c(0.05,0.01))


plot(usa.som, type="changes")

plot(usa.som, type="count")

plot(usa.som, type="dist.neighbours")

codigos= usa.som$codes; codigos
plot(usa.som, type="quality")

plot(usa.som, type="mapping")

plot(usa.som, type="mapping", labels = usa$state)

codigos=data.frame(usa.som$codes[[1]])

plot(usa.som, type="property", property = codigos$murder )


## use hierarchical clustering to cluster the codebook vectors
hc=hclust(dist(usa.som$codes[[1]]), method = "ward.D2")
dim(usa.som$codes[[1]])
plot(hc, hang = -1)
codebk=data.frame(usa.som$codes[[1]])
class(codebk)
codebk$clust=cutree(hc, 2); length(codebk$clust)
codebk$clust

boxplot(codebk$murder~codebk$clust)

# plot
plot(usa.som, type="mapping", bgcol=rainbow(4)[codebk$clust],
     cex=.7, labels = usa$state)

#cluster boundaries
add.cluster.boundaries(usa.som, codebk$clust)




summary(usa.som$unit.classif)

rela=data.frame(usa.som$unit.classif,usa$state) 

ordenado=rela[order(rela$usa.som.unit.classif),]; ordenado



