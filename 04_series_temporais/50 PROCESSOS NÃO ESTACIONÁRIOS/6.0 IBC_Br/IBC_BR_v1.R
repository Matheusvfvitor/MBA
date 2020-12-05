#install.packages(c("fpp2", "tidyverse", "readxl", "TSstudio",  "tseries", 
#                  "BETS", "knitr","writexl"))

library(fpp2)
library(tidyverse)
library(readxl)
library(TSstudio)
library(tseries)
library(writexl) 
library(BETS) 
library(dplyr) 
library(knitr)

# procure a identificação
df.res <- BETSsearch("Central Bank Economic Activity Index",
                     view = FALSE) 

# Verificando o resultado
df.res %>%
  kable() %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"))

# seleciona a identificação
id <- 24363 

# datas de início e término da série 

inicio = '2003-01-01' 

fim = as.character(Sys.Date()) 

# obtém os dados 

df.BETS <- BETSget(code = id, 
                   data.frame = TRUE, 
                   from = inicio, 
                   to = fim)
df <- df.BETS

#df <- read_excel("ibc_br3.xlsx")

head(df)

tail(df)

ibc <- df$value

ts.ibc <- ts(ibc, frequency =  12, start=c(2003,1), end = c(2020,06))
ts.ibc

autoplot(ts.ibc, lwd = 1.2, col = "steelblue")+
  labs(y = 'IBC-BR', 
       x = 'Data',
       title = 'Índice de Atividade do Banco Central do Brasil',
       caption = 'Fonte: BACEN')
  
  
# (pacote TSstudio)
ts_seasonal(ts.ibc, type = "box")

# Correlograma

acf <- ggAcf(ts.ibc)
acf

pacf <- ggPacf(ts.ibc)
pacf

# A ´serie é claramente não estacionária.
# O Pacf sugere que a séria ficaria estacionária no lag = 3


############################################
# Utlizaremos os testes de estacionariedade#
# com o pacote tseries                     #
############################################

# Regra de outro: P-valor baixo: rejeita Ho.


#Teste ADF - No nível

# Ho: A série não é estacionária
# H1: A série é estacionária

adf.test(ts.ibc)
# rejeita-se Ho. p-valor:0.756
# Note que o ADF está sugerindo um lag = 5

# Teste KPSS

# Ho: A série é estacionária
# H1: A série não é estacionária

kpss.test(ts.ibc)

# p-value smaller than printed p-value. Rejeita a hipótese nula

# Teste Philipps - Perron

# Ho: A série não é estacionária
# H1: A série é estacionária

pp.test(ts.ibc)
# Não rejeita a hipótese nula: p-value = 0.2136


# Efetuando a diferença

diff <- diff(ts.ibc)

lim <- 1.96/sqrt(length(ibc))

autoplot(diff, col ="blue", lwd = 1.1)+
  geom_hline(yintercept = 0,
             colour='darkred', 
             linetype='solid', 
             size=1.0)


# Efetuando o correlograma

acf.diff <- ggAcf(diff, lag.max = 24)
acf.diff

# uma diferenciação de lag =2, mas observa-se a sazonalidade
# nos instantes 6 e 12 e 18 e 24.
# teste sugere 4 lags (5-1)


pacf.diff <- ggPacf(diff, lag.max=24)
pacf.diff 

# Teste ADF - Na diferença

# Ho: A série não é estacionária
# H1: A série é estacionária

adf.test(diff)

# Teste KPSS

# Ho: A série é estacionária
# H1: A série não é estacionária


kpss.test(diff)


# Phillips - Perron
# Ho: A série não é estacionária
# H1: A série é estacionária

pp.test(diff)


# AR(p)

ar <- arima(ts.ibc, order = c(2,1,0))
ar

#Efetuando as previsões

fit <- forecast(ar, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.ibc, series = "Serie_Observada")+
  autolayer(fit$fitted, series = "Série Predita_AR")+
  autolayer(fit, series = "Projeção h =12")

# MA(q)

ma <- arima(ts.ibc, order = c(0,1,2))
ma

#Efetuando as previsões

fit <- forecast(ma, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.ibc, series = "Serie_Observada")+
  autolayer(fit$fitted, series = "Série Predita_ MA")+
  autolayer(fit, series = "Projeção h =12")

#ARIMA(p,d,q)

arima <- arima(ts.ibc, order = c(2,1,2))
arima

fit <- forecast(arima, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.ibc, series = "Serie_Observada")+
         autolayer(fit$fitted, series = "Série Predita_ ARIMA")+
              autolayer(fit, series = "Projeção h =12")

# SARIMA(p,d,q)(P,D,Q)[s]

sarima <-  arima(ts.ibc, order= c(2,1,0),
                seasonal = list(order = c(0,1,1), period = 12))    

sarima

#Efetuando as previsões

fit <- forecast(sarima, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.ibc, series = "Serie_Observada")+
  autolayer(fit$fitted, series = "Série Predita_ SARIMA")+
  autolayer(fit, series = "Projeção h =12", showgap = FALSE)

# usando o Auto.Arima

auto.arima <- auto.arima(ts.ibc, seasonal = TRUE,
                         stepwise = FALSE, 
                         approximation = FALSE)
auto.arima

# SARIMA(3,1,0)(0,1,1)[12]

fit <- forecast(auto.arima, h = 12) # SE QUISER FIXAR IC level = 95)

fit

autoplot(ts.ibc, series = "Serie_Observada")+
  autolayer(fit$fitted, series = "Série Predita_ AUTO.ARIMA")+
  autolayer(fit, series = "Projeção h =12")

round(accuracy(ar),  4)
round(accuracy(ma),  4)
round(accuracy(arima), 4)
round(accuracy(sarima), 4)
round(accuracy(auto.arima), 4)

# MELHOR MODELO AUTOARIMA

# Checando a consistência dos parâmetros

# install.packages("BETS")
library(BETS)

t_test(model = auto.arima)

# Os parâmetros são significatios


# DIAGNÓSTICO

# teste de ausência de autocorrelação serial

# teste Ljung-Box

# Ho: Os resíduos são independentes e identicamente (Inexiste aurocorrelação)
#           distribuídos (ou seja: não há dependência serial) 
#           para todas as defasagens).

# o MODELO SARIMA(3,1,0)(0,1,1)[12] 

#  IMPORTANTE 

#  fitdf = p+q+P+Q = 3+0+0+1 = 4

Box.test(x = auto.arima$residuals, lag=24, type = "Ljung-Box", fitdf = 4)

# Não podemos rejeitar Ho. A série é iid

checkresiduals(auto.arima, col ="blue")

# Checando a Heterocedasticidade

# Teste proposto por Engle (1982) # GARCH

# Ho: Os resíduos ao quadrado são uma sequência de
#       ruídos brancos, ou 	seja, os resíduos são homocedásticos.

# H1: a série é heterocedástico

#install.packages("FinTS")

require(FinTS)
library(FinTS)

ArchTest(auto.arima$residuals, lags = 36)

# Não rejeitamos a hipóse nula.

# NORMALIDADE

# Ho : ~ N(mi, sigma2)
# H1 : Rejeita Ho

# install.packages("normtest")
require(normtest)
library(normtest)

jb.norm.test(auto.arima$residuals, nrepl = 2000)

shapiro.test(auto.arima$residuals)

hist(auto.arima$residuals)

round(summary(auto.arima$residuals), digits = 4)

# calculando a moda
# Criando a função

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- moda(as.numeric(auto.arima$residuals))
print(result)

# Estamos violando esse diagóstico, porém se efetuarmos o arrendondamento
# média = mediana = moda = zero

fit

# O script está detalhado no Rpubs

# https://rpubs.com/rogerioguerra/658690


################################
# TRBALHO EM GRUPO             #
################################

df.res <- BETSsearch("retail sales",
                     view = FALSE) 

# Verificando o resultado
df.res %>%
  kable() %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"))

# Buscar por 
# Sales volume index in the retail sector - Total - Southeast
# Rever o script e incluir hw aditivo e multiplicativo.



