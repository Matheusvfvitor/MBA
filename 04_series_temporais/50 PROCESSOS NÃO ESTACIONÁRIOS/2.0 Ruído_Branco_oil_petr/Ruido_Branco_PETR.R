# Ruído branco
# install.packages(c("fpp2", "tidyverse", " quantmod"))

library(fpp2)
library(tidyverse)
library(gridExtra)
library(quantmod)

# Simulação

set.seed(1234)

n <- 1000

# Geração de 1000 variáveis aleatórias N iid (0,1)
# wn = white noise

wn <- rnorm(n)  # white noise
wn
class(wn)

#Transformando wn em ts
wn.ts <- ts(wn)
class(wn.ts)

# Plotando

autoplot(wn.ts, col = "black")+
geom_hline(yintercept =0,colour='darkred', linetype='solid', size=1.0)
 
Acf(wn.ts, lag = 1000)

lim <- 1.96/sqrt(n)
lim

# A maioria dos coeficientes de autocorrelação estão dentro dos limites, 
# indicando visualmente que os dados são ruído branco. 


# Vamos a um um caso real

library(quantmod)

getSymbols('PETR4.SA')

# Omitindo as NAs

PETR4.SA_clean <- na.omit(PETR4.SA["2015-01-02/2020-09-10"]) # datas limites

PETR4.SA_clean <- PETR4.SA[index(PETR4.SA_clean)]

#View(PETR4.SA_clean)

nrow(PETR4.SA_clean)
#Temos 1414 observações

petr_close <- as.numeric(PETR4.SA_clean$PETR4.SA.Close)
class(petr_close)

petr_ts <- ts(petr_close)
class(petr_ts)

autoplot(petr_ts, lwd = 1.1, col= "lightcoral")+ 
  ggtitle("PETR4 - Cotações Obtidas no Yahoo_Finance")+
  ylab("Cotação em R$")

# Taxa instantânea

#tx_instantanea <- log(p1/po)
#tx_instantanea

#exp(tx_instantanea)-1

# Autocorrelação

Acf(petr_ts)

1.96/(nrow(PETR4.SA_clean))^0.5

# A série é estacionaria? 

# Vamos aos retornos, onde retorno = (Preço hoje/ Preço ontem)-1

retorno <- diff(log(petr_close))
class(retorno)

retorno <- as.data.frame(retorno)
nrow(retorno)

retorno_ts <- ts(retorno)

autoplot(retorno_ts)+
  geom_hline(yintercept =- 0,colour='darkred', linetype='solid', size=1.0)+
  geom_hline(yintercept = 1.96/sqrt(length(retorno_ts)),colour='darkred', linetype='solid', size=1.0)+
  geom_hline(yintercept =- 1.96/sqrt(length(retorno_ts)),colour='darkred', linetype='solid', size=1.0)+
  ggtitle("Retorno diário da PETR4")
  
lim <- 1.96/sqrt(length(retorno_ts))
lim

Acf(retorno_ts, lag = 1000, main="Correlograma dos Retorno")  

#E agora?
#Uma série de ruídos brancos visualmente é estacionária.
