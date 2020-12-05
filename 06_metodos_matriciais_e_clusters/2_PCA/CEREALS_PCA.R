
ww=CEREALSed[,3:10] # oito variáveis
names(ww)

boxplot(ww$calories)
hist(ww$calories)
boxplot(ww$protein);grid()
table(ww$protein)
ww=subset(ww, protein < 4.5) #eliminar os outliers
boxplot(ww$protein);grid()
table(ww$fat)
ww$fat=ifelse(ww$fat>3,3,ww$fat)

boxplot(ww$sodium, col=11)

boxplot(ww$fiber)
table(ww$fiber); 
hist(ww$fiber)
ww=ww[ww$fiber<7,]

summary(ww$carbo)
boxplot(ww$carbo)
ww=ww[ww$carbo>0,]

boxplot(ww$sugars)
summary(ww$potass) #lembrem-se que a análise das vars começa SEMPRE com este comando
ww=ww[ww$potass>0,]


#########################################################################
#########################################################################
round(cor(ww),2)
cp=prcomp(ww, scale. = T)
plot(cp,col=11)
abline(h=1, col=2, lwd=4)
abline(h=.75, col=4, lwd=4)
summary(cp)

#interpretar>>>>> matriz de correlações
round(cor(ww, cp$x[,1:4]),2)
biplot(cp, xlab="cp1",ylab="cp2", scale=0)
abline(h=0,v=0,col="blue",lty=1)
grid(col="green")

plot(cp$x[,2],cp$x[,3], type = "n")
text(cp$x[,2],cp$x[,3],pch=19, col=4)
abline(h=0, col=4)
abline(v=0, col=4)

##########################################################################

#cluster com as vars originais
ww.scl=scale(ww)
dd=dist(ww.scl)
h1=hclust(dd, method="ward.D2")
plot(h1, hang = -1)
ww$hc1=cutree(h1,4)
which(ww$hc1==1)
which(ww$hc1==2)
which(ww$hc1==3)
which(ww$hc1==4)

#cluster com as 4 cp
dd2=dist(cp$x[,1:4])
h2=hclust(dd2, method="ward.D2")
plot(h2, hang=-1)
ww$hc2=cutree(h2, 4)
which(ww$hc2==1)
which(ww$hc2==2)
which(ww$hc2==3)
which(ww$hc2==4)

boxplot(ww$calories~ww$hc2)
boxplot(ww$carbo~ww$hc2)
boxplot(ww$sodium~ww$hc2)
boxplot(ww$protein~ww$hc2)
boxplot(ww$fiber~ww$hc2)


table(ww$hc1, ww$hc2)
