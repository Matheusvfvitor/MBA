tur=TURISMOATB

################################################
#análise das vars --> outliers, correlação
###############################################
boxplot(tur$INSTAL)
boxplot(tur$ATEND);grid(col=4)
boxplot(tur$TEMP)
tur=subset(tur, tur$ATEND>4)


round(cor(tur[,2:4]),2) # vars quantitativas

#####################################################################
#  caso 1: drivers: apenas as quantitativas INSTAL, ATEND E TEMP
####################################################################

#padronizar vars
turdrivers=tur[,2:4] #dataset so com os drivers
turpad=scale(turdrivers)
head(turpad)


#calcular matriz distancias dos dados padronizados
turdist=dist(turpad)

#vamos rodar o cluster utilizando ligação pela média
hcmed=hclust(turdist, method = "average")
plot(hcmed, hang = -1)

#vamos rodar o cluster utilizando o método de Ward
hcwrd=hclust(turdist, method = "ward.D2")
plot(hcwrd, hang = -1)
abline(h=4.5, col="red", lwd=2)


#analisar o comportamento dos drivers nos 4 clusters
tur$hcw=cutree(hcwrd,4)
table(tur$hcw)

boxplot(tur$INSTAL~tur$hcw,main="instaalccoes", col=rainbow(4))
boxplot(tur$ATEND~tur$hcw,main="atendimento", col=rainbow(4))
boxplot(tur$TEMP~tur$hcw, main="tempo",col=rainbow(4))

#analisar as descritivas
m=table(tur$LOCAL, tur$hcw)
prop.table(m,1)

#####################################################################
#  caso 2: drivers: todas as vars quali e quanti (exceto AGE)
####################################################################

tur.drivers=tur[,c(2:6)] 
names(tur.drivers)

####################################################################
#calcular a distancia entre as filiais
####################################################################

#neste caso temos mistura de vars (quant e quali)

library(cluster)

#pacote requer formato factor
tur.drivers$FREEPARK=as.factor(tur.drivers$FREEPARK)
tur.drivers$LOCAL=as.factor((tur.drivers$LOCAL))

table(tur$FREEPARK,useNA = "ifany")
table(tur$LOCAL,useNA = "ifany")

tur.dist=daisy(tur.drivers, metric ="gower") 
# daisy calcula a distancia utilizando método de Gower
# se só houver vars quant temos que usar a opção metric="gower"

####################################################
##### algoritmo hierárquico                        #
####################################################
hc.tur=hclust(tur.dist, method = "ward.D2")
plot(hc.tur, hang = -1) #vamos considertar 2 clusters
abline(h=1, col=2)

tur$hcqq=cutree(hc.tur, 2) #var que identifica o cluster

par(mfrow=c(1,3))
boxplot(tur$INSTAL~tur$hcqq, main="INSTAL", col=rainbow(2))
boxplot(tur$ATEND~tur$hcqq, main="ATEND", col=rainbow(2))
boxplot(tur$TEMP~tur$hcqq, main="TEMP", col=rainbow(2))

aggregate(tur[,c(2:4)], list(tur$hcqq), median)

library(gmodels)
CrossTable(tur$FREEPARK,tur$hcqq, prop.chisq = F, prop.t = F)
CrossTable(tur$LOCAL,tur$hcqq,  prop.chisq = F, prop.t = F)
 
##########################################################
#           vamos rodar o k medoid
##########################################################

library(fpc)
set.seed(2) # recomenda-se que testemos com vários set seeds para modificar partição inicial
tur.kmed=pamk(tur.dist, krange = 2:8, diss = T, critout = T)
#melhor resultado para maior valor de asw (critério default do pamk)--> k=8
tur.kmed$nc #clusters deu igual a 8
tur.kmed$pamobject$medoids

#vamos forçar k=2 (arbitraria): baseado no resultado do dendrograma
set.seed(1954)
tur.kmed=pamk(tur.dist, krange = 2, diss = T)
tur.kmed$pamobject$medoids
tur$kmed=tur.kmed$pamobject$clustering  #esta var no aqruivo tur vai identificar os clusters
table(tur$kmed)

################################################################
# analisar os 2 clusters
###############################################################
par(mfrow=c(1,3))
boxplot(tur$INSTAL~tur$kmed, main="INSTAL", col=rainbow(5))
boxplot(tur$ATEND~tur$kmed, main="ATEND", col=rainbow(5))
boxplot(tur$TEMP~tur$kmed, main="TEMP", col=rainbow(5))
aggregate(tur[,c(2:4,7,8)], list(tur$kmed), median)
CrossTable(tur$FREEPARK,tur$kmed, prop.chisq = F, prop.t = F)
CrossTable(tur$LOCAL,tur$kmed,  prop.chisq = F, prop.t = F)
table(tur$hc, tur$kmed) #cruzando os resultados dos dois métodos

######################################################
#   analisar uma descritiva
#####################################################
boxplot(tur$AGE~tur$kmed, main="AGE", col=rainbow(5))

tur$PAÍS[tur$kmed==1]
tur$PAÍS[tur$kmed==2]



######################################################
#   comparar o ward e o pamk
#####################################################

table(tur$hcqq,tur$kmed, dnn = c("hier", "medoid"))

clust_stats <- cluster.stats(tur.dist, tur$hcqq, tur$kmed) # Corrected Rand index
clust_stats$corrected.rand


######################################################
#   vamos ponderar as vars quali para calcular distancias
#####################################################

ww=c(1,1,1,.5,.5)
tur.dist=daisy(tur.drivers, metric ="gower",weights = ww)









