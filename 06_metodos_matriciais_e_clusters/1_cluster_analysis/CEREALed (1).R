cc <- CEREALSed[,-1]
summary(cc)

table(cc$mfr, useNA = "ifany")

#calories
summary(cc$calories)
par(mfrow=c(1,2))
b=boxplot(cc$calories, main="calorias", col = 11)
hist(cc$calories, main="calorias", col = 11)
# nao vamos eliminar pontos fora do boxplot : formato histograma

#protein
summary(cc$protein)
table(cc$protein)
# proteins varia de 1 a 6 (sempre inteiros); 
# box plot nao é bom grafico nestes casos
b=boxplot(cc$protein, main="proteins", col=11)
#eventualmente poderiamos fundir 4, 5 e 6 em uma categoria
cc$protein=ifelse(cc$protein>4,4,cc$protein)
table(cc$protein)

# fat
summary(cc$fat)
table(cc$fat)
# fat varia de 1 a 5 (sempre inteiros); 
# box plot nao é bom grafico nestes casos
#eventualmente poderiamos fundir 3 e 5 em uma categoria
cc$nufat=ifelse(cc$fat==5, 3, cc$fat)
table(cc$nufat)

#sodium
summary(cc$sodium)
hist(cc$sodium)
b=boxplot(cc$sodium)

#fiber
summary(cc$fiber)
b=boxplot(cc$fiber, main="fibra", col=11)
hist(cc$fiber, col=11) #alternativa interessante: transformar variavel ou eliminar outlier
cc$logfiber=log(cc$fiber+1) # +1 pois temos casos de fiber=0
boxplot(cc$logfiber,main="fiber",col=20)
hist(cc$logfiber)

#carbo
summary(cc$carbo)  # temos um caso impossivel carbo = -1
cc=subset(cc, cc$carbo>=0)
#cc=cc[!cc$carbo== -1,]
summary(cc$carbo)   
b=boxplot(cc$carbo, main="carbo",col=20)
hist(cc$carbo, main="carbo",col=20)
#sugars
summary(cc$sugars)
table(cc$sugars)
b=boxplot(cc$sugars,main="sugars",col=20)
hist(cc$sugars,main="sugars",col=20)
#potass
summary(cc$potass)
cc=cc[!cc$potass== -1,]
b=boxplot(cc$potass, main="potass",col=20)
hist(cc$potass, col=20)  
# dist assimetrica; nao vamos eliminar pontos fora do bplot
# manter sem transformação 

#rating
table(cc$rating, useNA = "ifany")


###################################################################
# cluster só com quantitativas

names(cc)
#drivers: eliminar mfr fat e fiber e rating (foram transformadas)
cc.num=cc[,-c(1,4,6,10)]
names(cc.num)

#antes de rodar cluster importante matriz de correlações

round(cor(cc.num),2)  # nao temos corrs maiores que 0,9 em módulo

cc.scale=scale(cc.num) #padroniza os dados -subtrai média  divide pelo dp
head(cc.num,4)
head(cc.scale,4)

cc.dist=dist(cc.scale) 
#cálculo da matriz de distâncias dos dados padronizados

# hierárquica aglomerativa, usando WARD
hc=hclust(cc.dist, method = "ward.D2")
plot(hc, hang = -1)
abline(h=11.8, col=2)
# analisando o dendro "acho" uma boa sugestão k=3
cc$hcl=cutree(hc, 3)
table(cc$hcl)

#para analisar e descrever/batizar os clusters 
##utilizamos as drivers
par(mfrow=c(2,2))
boxplot(cc$calories~cc$hcl, main= 'calories', col=rainbow(4))
boxplot(cc$protein~cc$hcl,main= 'protein', col=rainbow(4))
boxplot(cc$fat~cc$hcl,main= 'fat', col=rainbow(4))
boxplot(cc$sugars~cc$hcl,main= 'sugars', col=rainbow(4))

#etc.etc.....

#analisando as demais vars (descritivas)
table(cc$mfr, cc$hcl, dnn=c('mfr', 'hcl'))
table(cc$rating, cc$hcl, dnn=c('rating', 'hcl'))

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
#cluster hierárquico com quali(rating) e quanti
library(cluster)
#requer vars no formato factor ou ordered
cc$rating=as.ordered(cc$rating)
cc.mix=cc[,-c(1,4,6,13)]
names(cc.mix)

#para calular a matriz de distâncias cluster
dist.mix=daisy(cc.mix)

hc2=hclust(dist.mix, method = "ward.D2")
plot(hc2,hang = -1)

cc$hcl2=cutree(hc2,4)

par(mfrow=c(2,2))
boxplot(cc$calories~cc$hcl2, main= 'calories', col=topo.colors(4))
boxplot(cc$protein~cc$hcl2,main= 'protein', col=rainbow(4))
boxplot(cc$fat~cc$hcl2,main= 'fat', col=rainbow(4))
boxplot(cc$sugars~cc$hcl2,main= 'sugars', col=rainbow(4))
#analisando as demais drivers
table(cc$rating, cc$hcl2, dnn=c('rating', 'hcl2'))


table(cc$mfr, cc$hcl2, dnn=c('mfr', 'hcl2'))

################################################################
#kmedoid com as numericas
#
library(fpc)
set.seed(123)  #por causa da partição inicial (o ideal seria testar com vários set.seed)
kmd=pamk(cc.dist, diss = T, k=2:10, criterion="asw", critout = T)
#utiliza criterio silhouette para avaliar k
kmd$nc #sugere 8
cc$kmd=kmd$pamobject$clustering
table(cc$kmd)
cc$mfr[cc$kmd==1]

#talvez utilizaria k= 5. 
set.seed(123)  #por causa da partição inicial
kmd=pamk(cc.dist, diss = T, k=5)
#utiliza criterio silhouette para avaliar k
kmd$nc
cc$kmd5=kmd$pamobject$clustering
table(cc$kmd5)

#comparar os resultados dos dois métodos hclust e kmd5
table(cc$hcl,cc$kmd5, dnn = c('hc','km'))
cluster.stats(cc.dist,  cc$kmd5 )$within.cluster.ss
###################################################################
# método mais geral de avaliar k <--  30 critérios
library(NbClust)
nb=NbClust(cc.scale, diss= cc.dist, distance = NULL, min.nc = 2, max.nc = 10,
        method = "kmeans" )

###############################################################################
#                utilizando a variavel quali rating e as quanti
##############################################################################
#para calcular a distância vamos utilizar pacote cluster
# as qualis tem que estar como factor ou ordered (que é o caso em rating)
cc$rating=as.ordered(cc$rating)

names(cc)
cc.mix=cc[, -c(1,4,6,13,14)]
names(cc.mix)

# já analisamos a matriz de correlações no exemplo anterior (lá em cima)

library(cluster)
names(cc.mix)
#pesos=(1,1,1,1,1,1,.5,1,1)
#daisy(cc.mix, weights = pesos)
distmix=daisy(cc.mix) # tendo mix de vars nao eh necessario padronizar antes
# a funçao dist fica para o caso em que só temos quanti
# a função daisy para mistura (qunt+quali) ou somente quali
hc2=hclust(distmix, method = "ward.D2")
plot(hc2, hang = -1) # vamos escolher 4 (influencia dos resultados acima...)
cc$hcl2=cutree(hc2,4)
table(cc$hcl2)
boxplot(cc$carbo~cc$hcl2)
table(cc$hcl2,cc$rating) #lembrando driver quali ordenada

# analisar / comparar clusters....vcs fazem
# 

library(fpc)
set.seed(18)
kmd2=pamk(distmix, diss = T, k=2:8, criterion = "asw"  , critout = T)
#utiliza criterio silhouette para avaliar k
kmd2$nc
cc$kmd2=kmd2$pamobject$clustering
table(cc$kmd2)
# analisar / comparar  >>>>>>>>>>>>>>>   cs c vira

table(cc$rating, cc$kmd2, dnn = c('rating', 'kmd2'))

table(cc$hcl2, cc$kmd2, dnn = c('hc', 'km'))

# tipo correlação entre os resultados
cluster.stats(distmix, cc$hcl2, cc$kmd2)$corrected.rand
cluster.stats(distmix, cc$kmd2 )$within.cluster.ss

###################################################################
# método mais geral de avaliar k <--  30 critérios
#nao funciona com mix de vars
###################################################################



###############################################################################
#  componentes principais
names(cc)
cc2=cc[,-c(5,7,10,11)]
names(cc2)

ccc=cc2[,-c(1,2)]
names(ccc)

pc=prcomp(ccc, scale. = T)
plot(pc)
abline(h=1, col=2)
abline(h=.75, col=4)
summary(pc)
#vamos selecionar m=4: satisfaz as 2 regras e cobre quase 90% da variancia

round(cor(ccc, pc$x[,1:4]),2)

#############################################################################
#       cluster utilizando dbscan
#   algoritmo DBSCAN
#####################################################################
library(dbscan)

#vamos fixar vizinhança de k= 3 pontos (nossa amostra é pequena)
kNNdistplot(distmix, k = 3);grid(col=3)
abline(h=.15, col = "red", lty=2)
abline(h=.10, col = "blue", lty=2)
#note como eh sensivel
res <- dbscan(distmix, eps = .15, minPts = 3);res
res <- dbscan(distmix, eps = .10, minPts = 3);res

#vamos fixar k= 2 pontos (nossa amostra é pequena)
kNNdistplot(distmix, k = 2);grid(col=3)
abline(h=.10, col = "red", lty=2)
abline(h=.12, col = "blue", lty=2)

#note como eh sensivel
res <- dbscan(distmix, eps = .05, minPts = 2); res
res <- dbscan(distmix, eps = .10, minPts = 2); res
res <- dbscan(distmix, eps = .11, minPts = 2); res
cc$dbs=res$cluster
boxplot(cc$calories~cc$dbs)

hullplot(cc.num,res, lwd=1.5, cex=1.5,pch=20, solid = F, main = "iris com k=5 e dados padronizados")
#coloquei cc.num pois ele trabalha com CP e só pode ser quantitativos
which(cc$dbs==3)

##########################################################################
#########################################################################
#cluster utilizando componentes principais
#
#matriz de distancias
distcp=dist(cc.cp)
hc.cp=hclust(distcp, method="ward.D2")
plot(hc.cp, hang = -1)
cc$hccp=cutree(hc.cp,3)

#compara a solução com os dados originais e com os PC
table(cc$hcl,cc$hccp, dnn=c('original', 'compo'))

plot(cc.cp[,1],cc.cp[,3], type = "n")
points(cc.cp[,1],cc.cp[,3],col=c(2,3,4), pch=19)
grid(col=1)
abline(h=0, lwd=6)
abline(v=0, lwd=6)
biplot(pc)

