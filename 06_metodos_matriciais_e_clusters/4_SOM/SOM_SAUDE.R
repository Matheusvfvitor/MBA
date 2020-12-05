sau=indicadoressaude
sau.num=sau[,4:12]
names(sau.num)
round(cor(sau.num),2)
sau.num$REND_MED <- NULL
round(cor(sau.num),2)
sau.num$RESP_TOTAL <- NULL
round(cor(sau.num),2)

sau.scl=scale(sau.num) 


library(kohonen)
sau.grid = somgrid(xdim = 3, ydim=3, topo="hexagonal")
sau.som = som(sau.scl, grid=sau.grid, rlen=400, alpha=c(0.05,0.01))

sau.som$code[[1]]  #vetores 
jaco=dist(sau.som$code[[1]])
jaoc=as.matrix(jaco)
jaco


plot(sau.som, type="changes")

plot(sau.som, type="count")

plot(sau.som, type="dist.neighbours")

codigos= sau.som$codes; codigos
plot(sau.som, type="quality")

plot(sau.som, type="mapping")

sau.som$unit.classif
plot(sau.som, type="mapping", labels = sau$UF)

codigos=data.frame(sau.som$codes[[1]])

plot(sau.som, type="property", property = sau$IDOSO )


## use hierarchical clustering to cluster the codebook vectors
hc=hclust(dist(sau.som$codes[[1]]), method = "ward.D2")
plot(hc, hang = -1)
km=kmeans(sau.som$codes[[1]], 4, nstart = 25)
codebk=data.frame(sau.som$codes[[1]]);codebk
class(codebk)
codebk$kmn=km$cluster
codebk$k
km$cluster


# plot
plot(sau.som, type="mapping", bgcol=rainbow(5)[codebk$kmn],
     cex=.7, labels = sau$UF)

#cluster boundaries
add.cluster.boundaries(sau.som, codebk$kmn)

boxplot(codebk$LEITOS_TOTAL~codebk$kmn)



rela=data.frame(sau.som$unit.classif,sau$UF) 

rela
ordenado=rela[order(rela$sau.som.unit.classif),]; ordenado



