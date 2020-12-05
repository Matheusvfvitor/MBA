ind=indic
names(ind)
ind.num=ind[,3:10] 
#.....analisar as variáveis  

#verificar correlação 
round(cor(ind.num),2)
#remover IV
ind.num$IV <- NULL

ind.scl=scale(ind.num) #padronizando subtraindo media e dividindo pelo dp

library(kohonen)

#topologia
ind.grid = somgrid(xdim = 4, ydim=4, topo="hexagonal")

#utilizei os alpha default
set.seed(0602)
ind.som = som(ind.scl, grid=ind.grid, rlen=1000)

# descrições dos gráficos baseadas em 
# https://en.proft.me/2016/11/29/modeling-self-organising-maps-r/ 

#Training Progress. As the SOMs training iterations progress, 
#the distance from each node's weights to the samples represented by that node
# is reduced. Ideally, this distance should reach a minimum plateau. 
# This plot option shows the progress over time. 
# If the curve is continually decreasing, more iterations are required.
plot(ind.som, type="changes")


#Node Counts. The Kohonen packages allows us to visualise the count of 
#how many samples are mapped to each node on the map. 
#This metric can be used as a measure of map quality – 
#ideally the sample distribution is relatively uniform. 
#Large values in some map areas suggests that a larger map would be beneficial. 
#Empty nodes indicate that your map size is too big for the number of samples. 
#Aim for at least 5-10 samples per node when choosing map size.
plot(ind.som, type="count", shape = "straight")

set.seed(2401)
ind.grid = somgrid(xdim = 3, ydim=3, topo="hexagonal")
ind.som = som(ind.scl, grid=ind.grid, rlen=400)
plot(ind.som, type="changes")
plot(ind.som, type="count", shape = "straight" )


# "mapping": shows where objects are mapped. 
#It needs the "classif" argument, and a "labels" or "pchs" argument.
plot(ind.som, type="mapping",col=1,labels=ind$UF, cex=.6, font=2, bgcol ='lightgreen', shape = "straight" )

#Neighbour Distance. Often referred to as the U-Matrix
#If we want to see nodes that have the closest or farthest neighbours, 
#we can plot a plot based on dist.neighbours. 
#Nodes that have darker colors mean that the nodes have a vector 
#input that is closer, whereas nodes that have lighter colors 
#mean that the nodes have vector inputs that are farther apart.
plot(ind.som, type="dist.neighbours", shape = "straight" )

#"quality" shows the mean distance of objects mapped to a unit to the codebook vector of that unit.
#The smaller the distances, the better the objects are represented by the codebook vectors.
plot(ind.som, type="quality", shape = "straight" )


#vetores correspondentes ao neuronios
codebk=ind.som$codes[[1]]
codebk=as.data.frame(codebk)
round(codebk,3)
# associando neuronios a UF
relato=data.frame(ind.som$unit.classif,ind$UF) 
ordenado=relato[order(relato$ind.som.unit.classif),]; ordenado
colnames(ordenado)[1] <- "neuronio"
colnames(ordenado)[2] <- "UF"
ordenado
ordenado$UF[ordenado$neuronio==4] #etc

 
## use kmeans clustering to cluster the codebook vectors
#alternativa1
cod.dist=dist(codebk)
hh=hclust(cod.dist, method = "ward.D2")
plot(hh, hang = -1)  #==> sugere 3 clusters

#alternativa2 (minha referida)
set.seed(111)
km=kmeans(codebk, 2, nstart = 25)
km$cluster
codebk$kmn=km$cluster
# plot
plot(ind.som, type="mapping", cex=.6, labels = ind$UF, bgcol=rainbow(2)[codebk$kmn], shape = "straight") 
#cluster boundaries
add.cluster.boundaries(ind.som, codebk$kmn, col=3, lwd=10)


#Heatmaps. A SOM heatmap allows the visualisation of the distribution of a single
# variable across the map. Typically, a SOM investigative process involves 
# the creation of multiple heatmaps, and then the comparison of these heatmaps 
# to identify interesting areas on the map. 
# It is important to remember that the individual sample positions 
# do not move from one visualisation to another, 
# the map is simply coloured by different variables.
par(mfrow=c(2,2))
plot(ind.som, type="property", property = codebk$IDH, main = "IDH", shape = "straight"  )
add.cluster.boundaries(ind.som, codebk$kmn)
plot(ind.som, type="property", property = codebk$TBN, main = "TBN" , shape = "straight" )
add.cluster.boundaries(ind.som, codebk$kmn)
plot(ind.som, type="property", property = codebk$TBM, main = "TBM", shape = "straight"  )
add.cluster.boundaries(ind.som, codebk$kmn)
plot(ind.som, type="property", property = codebk$EVN, main = "EVN" , shape = "straight" )
add.cluster.boundaries(ind.som, codebk$kmn)
#Codes/Weight vectors. The node weight vectors, or codebook, 
#are made up of normalised values of the original variables used to generate the SOM.
# Each node’s weight vector is representative / similar of the samples mapped to that
#  node. By visualising the weight vectors across the map, we can see patterns 
#  in the distribution of samples and variables. 
#  The default visualisation of the weight vectors is a fan diagram, 
#  where individual fan representations of the magnitude of each variable 
#  in the weight vector is shown for each node.
#  razoável quando temos poucas variáveis
par(mfrow=c(1,2))
plot(ind.som, type="codes", shape = "straight",bgcol=rainbow(8)[codebk$kmn] )
add.cluster.boundaries(ind.som, codebk$kmn)
plot(ind.som, type="mapping", cex=.6, labels = ind$UF, bgcol=rainbow(2)[codebk$kmn], shape = "straight") 
add.cluster.boundaries(ind.som, codebk$kmn, col=3, lwd=10)
#"quality" shows the mean distance of objects mapped to a unit to the codebook vector 
#of that unit. The smaller the distances, 
#the better the objects are represented by the codebook vectors.
#plot(ind.som, type="quality")

######################################################################
# identificando o cluster de cada UF no arquivo original ind 
ind.som$unit.classif  # identifica neuronio associado com cada UF
head(codebk)
codebk$kmn #cluster de cada neuronio
cluster_assignment <- codebk$kmn[ind.som$unit.classif]
ind$cluster <- cluster_assignment
View(ind)
#####################################################################
# Previsao de um out-of-sample case (vou inventar um parecido com AC)
novo=data.frame(TBN=19, TBM=5,  TMI=15, TFT=2,
                RDT=52, EVN=75, IDH=0.66)
x=as.matrix(novo) #tem que ser matrix
gg=predict(ind.som, newdata = x)
gg$unit.classif
