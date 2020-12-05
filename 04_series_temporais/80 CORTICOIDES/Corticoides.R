install.packages("fpp2")
install.packages("forecast")
install.packages("tseries")
install.packages("seasonal")
install.packages)("urca")

library(fpp2)
library(forecast)
library(tseries)
library(seasonal)
library(urca)
library(gridExtra)

 data(h02)

g1 <- autoplot(h02)+ 
         ggtitle("Venda mensal de Corticoides (em milhões)")


g2 <- ggseasonplot(h02, year.labels = TRUE, year.label.left = FALSE)+
  ylab("Vendas de Corticoides em milhões")+ xlab("Meses")+
  ggtitle("Venda mensal de corticoides (em milhões)")

grid.arrange(g1, g2, nrow = 2)


