aval=AVALIACOES[,-c(1,8)]

#vamos verificar se há outliers que podem impactar no valor das correlações(base das CP)
par(mfrow=c(2,3))
boxplot(aval$AV1, col=2,main="AV1")
boxplot(aval$AV2, col=3,main="AV2")
boxplot(aval$AV3,col=4,main="AV3")
boxplot(aval$AV4,col=5,main="AV4")
boxplot(aval$AV5,col=7,main="AV5")
boxplot(aval$AV6,col=8,main="AV6") 

summary(aval)


# matriz de correlações 
mat.correl=cor(aval)
round(mat.correl,digits=2)

#componentes principais
# center =T é default. scale.= é para padronizar os dados com média zero e variância = 1
aval.cp=prcomp(aval, scale. = T)
#summary dá os desvios padrao das CP e as proporcões de variância acumuladas
summary(aval.cp)

final=cbind(AVALIACOES,aval.cp$x)

#aval.cp$rotation dá os coeficientes w das CP; nao utilizamos em geral
print(aval.cp$rotation, digits=2)

#variâncias das CP (eingenvalues / autovalores)
par(mfrow=c(1,1))
plot(aval.cp) # plota as variancias (eigenvalues) das CP
abline(h=1,col="blue", lwd=4, lty=3)
abline(h=0.75, col="blue", lwd=2, lty=4)
plot(aval.cp,type="l") #grafico denominado screeplot
grid(col = "red")


#as correlações entre as vars e os fatores sao dados por correlação (Zi, CPj) = wij * desvio padrao (CP)
#chamadas components loadings ou 'cargas das componentes"
#Vamos considerar apenas as 3 primeiras componentes
aval.loads=aval.cp$sdev * t(aval.cp$rotation[,1:3]) 
print(t(aval.loads), digits = 2)



#grafico especial
escoresx=aval.cp$x[,1] #VETOR COLUNA
names(escoresx)<-AVALIACOES$PROFESSOR  
#Representa cada valor pelo nome do respectivo professor
ordem=order(escoresx,decreasing = T)
barplot(escoresx[ordem],las=2)
box()

biplot(aval.cp, xlab="cp1",ylab="cp2",xlabs=AVALIACOES_$PROFESSOR)
abline(h=0,v=0,col="darkblue",lty=5)


plot(final$PC1,final$PC2,type = "n")
text(final$PC1,final$PC2,AVALIACOES_$PROFESSOR)
abline(h=0,v=0,col="blue",lty=5)

