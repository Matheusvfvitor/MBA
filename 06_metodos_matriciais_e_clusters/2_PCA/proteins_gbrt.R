qq=PROTEINAS
par(mfrow=c(1,3))
boxplot(qq$RedMeat)
which(qq$RedMeat>15)
boxplot(qq$WhiteMeat)
boxplot(qq$Eggs)
which(qq$Eggs<1)
boxplot(qq$Milk)
boxplot(qq$Fish)
boxplot(qq$Cereals)
boxplot(qq$Starch)
which(qq$Starch<1.5)
boxplot(qq$Nuts)
boxplot(qq$`Fr&Veg`)

#eliminar Albania e França <-- outliers



qq=PROTEINAS[-c(1,7),-c(1,2)]
cor(qq)
pc=prcomp(qq,scale. = T) #note a opção de padronização
summary(pc)
plot(pc)
abline(h=1, col=2)
abline(h=.75, col=4)

round(cor(qq, pc$x[,1:4]),2)
plot(pc$x[,1], pc$x[,2],type = "n")
text(pc$x[,1], pc$x[,2],labels =PROTEINAS[-c(1,7),]$Code, col=4 )
grid(col=2)
duas.cps=cbind(PROTEINAS[-c(1,7),1],pc$x[,1:2]) #paises e valores das componentes principais

biplot(pc, xlab="cp1",ylab="cp2",xlabs=PROTEINAS[-c(1,7),]$Code, scale=0)
abline(h=0,v=0,col="blue",lty=5)
grid(col="green")
