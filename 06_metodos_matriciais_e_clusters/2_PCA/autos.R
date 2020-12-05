qq=Autos[ ,-1]; names(qq)
boxplot(qq$MPG)
table(qq$Cylinder)
boxplot(qq$EngineD)
boxplot(qq$Horsepow)
boxplot(qq$Weight)
boxplot(qq$Accelera)

qq=qq[,-c(7,8)]
names(qq)
round(cor(qq),2)
##################################################################
pc=prcomp(qq,scale. = T)
pc$x #com as 6 colunas representando as CP
summary(pc)
plot(pc, ylim=c(0,5), col=11, main="variancias das CP") #eixo vertical as variancias das CP


#regras para determinar com qtas CP ficar
abline(h=1, col=2, lwd=2)
abline(h=.75, col=4, lwd=2)
#as duas regras sugerem 2 CP

duascomp=pc$x[,1:2] #ela contem as 6 CPs; mas nos vamos trabalhar só com as duas primeiras
duascomp

#calcular as correlações entre os X e as CP --> interpretar
round(cor(qq, duascomp),2)

plot(pc$x[,1], pc$x[,2],type = "n")
cores=ifelse(Autos$Origin=="American", "red","blue")
pixel=ifelse(Autos$Origin=="American", 11,19)
text(pc$x[,1], pc$x[,2], col=cores, labels = Autos$Origin )
grid(col=2)

biplot(pc, xlab="cp1",ylab="cp2",xlabs=Autos$Code,cex=.5, scale=0)
abline(h=0,v=0,col="blue",lty=1)
grid(col="green")


#################################################################
# cluster com as vars quantitativas ...qq
names(qq)
qq.scale=scale(qq)
head(qq.scale)
dd=dist(qq.scale)
hco=hclust(dd, method = "ward.D2")
plot(hco, hang = -1)
abline(h=6, col=2)
Autos$hcoriginal=cutree(hco, 2)
 

#cluster com as 2 CP
ddcp=dist(duascomp) # CP já sao padronizadas
hcc=hclust(ddcp, method = "ward.D2")
plot(hcc, hang = -1)
Autos$hccomp=cutree(hcc,2)
 

table(Autos$hcoriginal, Autos$hccomp)




