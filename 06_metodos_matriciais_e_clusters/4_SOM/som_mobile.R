mob=mobile2_ajustado[,-1]
names(mob)
num=model.matrix(data=mob,~.)
num=as.data.frame(num)
colnames(num)
num=num[,-1]

cor(mob[,1:6])
num$loginc <- NULL

num.scl=scale(num)
print(head(num.scl),4)

library(kohonen)
#aquele autor sugerem 5*sqrt(74) =40
mob.grid = somgrid(xdim = 4, ydim=4, topo="rectangular")
set.seed(123)
mob.som = som(num.scl, grid=mob.grid, rlen=2000 )


plot(mob.som, type="changes")

plot(mob.som, type="count", shape = "straight")

plot(mob.som, type="dist.neighbours")

#"quality" 
#shows the mean distance of objects mapped to a unit to the codebook vector 
#of that unit. The smaller the distances, 
#the better the objects are represented by the codebook vectors.
plot(mob.som, type="quality")

plot(mob.som, type="mapping")

mob.som$unit.classif
plot(mob.som, type="mapping",cex=.5, labels = mobile2_ajustado$PAÍS)

codigos=data.frame(mob.som$codes[[1]])
dim(mob.som$codes[[1]])

plot(mob.som, type="property", 
     property = codigos$IDH, main="idademed" )


## use hierarchical clustering to cluster the codebook vectors
hc=hclust(dist(mob.som$codes[[1]]), method = "ward.D2")
plot(hc, hang = -1)
codebk=data.frame(mob.som$codes[[1]])
class(codebk)
codebk$clust=cutree(hc, 4)
codebk$clust

# plot
plot(mob.som, type="mapping", bgcol=rainbow(4)[codebk$clust],
     cex=.5, labels = mobile2_ajustado$PAÍS, shape="straight")

#cluster boundaries
add.cluster.boundaries(mob.som, codebk$clust, col="yellow")

summary(mob.som$unit.classif)
relato=data.frame(mob.som$unit.classif,mobile2_ajustado$PAÍS) 
ordenado=relato[order(relato$mob.som.unit.classif),]; ordenado

hc=hclust(dist(num.scl), method = "ward.D2")
plot(hc, hang = -1)
xx=cutree(hc,4)
mobile2_ajustado$PAÍS[codebk$clust==1]
mobile2_ajustado$PAÍS[codebk$clust==2]
mobile2_ajustado$PAÍS[codebk$clust==3]
mobile2_ajustado$PAÍS[codebk$clust==4]

plot(mob.som, type="codes", shape = "straight",bgcol=topo.colors(4)[codebk$clust] )
add.cluster.boundaries(mob.som, codebk$clust)
