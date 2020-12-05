# Série Temporal relativa ao volume de turistas 
# do 1o. trimestre de 1998 até o 3o. trimestre de 
# 2017

## 1. Instalação dos pacotes

 install.packages(c("fpp",
                      "fpp2", 
                         "forecast", 
                             "readxl"))

library(fpp)
library(fpp2)
library(forecast)
library(readxl)

df <- read_excel("turismo.xlsx")

class(df$turismo)

## 2. Transformando df em classe ts

tour <- ts(df$turismo, frequency = 4, 
                start = c(1998,1), end = c(2017,4))

tour

autoplot(tour)+
  xlab("Trimestres")+
  ylab("Turistas em milhões")+
  ggtitle("Turismo na Austrália")

## 3. Efetuar uma breve Análise Exploratória

ggseasonplot(tour, year.labels = TRUE,
             year.label.left = TRUE) +
                   ylab("Turistas em milhões")+ 
                    xlab("Trimestre")+
                      ggtitle("Turismo na Austrália")

boxplot(tour~cycle(tour),
          xlab="Trimestres",
                ylab = "Turistas em milhões" ,
                  col="orange", 
                    main ="Turismo na Austrália",
                       par(bg = '#E0E0E0'))

## 4. Decompor a série

decomp <- decompose(tour, type = "multiplicative")
autoplot(decomp)

## 5. Os modelos

## HW ADITIVO

hw.a <- hw(tour, seasonal = "additive", h = 8, level = 95) 
                                        # h é o horizonte de previsão
                                        # que você deseja efetuar  

fit.a <- hw.a$fitted # Ajuste do modelo aos dados observados
fit.a

hw.a # a previsão 

autoplot(tour, series = "Série_Real", lwd= 1.1)+ #serie_original
  autolayer(fit.a, series = "modelo HW- Aditivo", lwd = 1.1)+ # Ajuste do modelo
  autolayer(hw.a, series = "Previsão", showgap = FALSE) # previsão h= n períodos 

accuracy(hw.a)


# MODELO HW MULTIPLICATIVO

hw.m <- hw(tour, seasonal = "multiplicative", h = 8, level = 95)
fit.m <- hw.m$fitted
fit.m

hw.m

autoplot(tour, series = "Série Orignal", lwd= 1.1)+ #serie_original
  autolayer(fit.m, series = "modelo HW-Multiplicativo", lwd =1.1)+# Ajuste do modelo
  autolayer(hw.m ,series = "Previsão", showgap = FALSE) # previsão h= n períodos 

# Comparativo entre os modelos: 

accuracy(hw.a$model)
accuracy(hw.m$model)

#  O RSME (Root Mean Squared Error)e o MAPE (Mean Absolute Percentage Error)
#  são menores no HW. Aditivo


# MODELO SARIMA(p,d,q)(P,D,Q)[sazonalidade]

# Primeira estapa: verificar a ausência de autocorrelação serial

ggAcf(tour)

# Visualmente, a série não é estacionãria

ggPacf(tour)


# TESTES FORMAIS : 

# Utilizaremos o pacote tseries (que é de melhor compreensão)

#install.packages("tseries")
library(tseries)


# Regra de Ouro: P-VALOR BAIXO INDICA REJEITAR Ho #

# AUGMENTED DICKEY-FULLER - É o teste mais usado

# Ho: A série não é estacionária (status quo)
# H1: A série é estacionária (alternativa)

adf.test(tour) # na série observada

#data:  tour
#Dickey-Fuller = -1.6233, Lag order = 4, p-value = 0.7298
#alternative hypothesis: stationary

#  Resposta: NÃO REJEITAMOS A HIPÓTESE NULA


# 2. KPSS # esse é o único teste em que a Ho é série estacionária

# Ho: A série é estacionária (status quo)
# H1: A série não é estacionária

tseries::kpss.test(tour)

# data:  tour
# KPSS Level = 0.38567, Truncation lag parameter = 3, p-value = 0.08333

# resposta: Não rejeitamos Ho.

pp.test(tour)

# Ho: A série não é estacionária (status quo)
# H1: A série é estacionária

# data:  tour
# Dickey-Fuller Z(alpha) = -88.275, Truncation lag parameter = 3, p-value = 0.01
#  alternative hypothesis: stationary

checkresiduals(tour)

# TESTE PELA 1a. DIFERENÇA

diff.tour <- diff(tour)

ggAcf(diff.tour)

ggPacf(diff.tour)

# REVISITANDO OS TESTES FORMAIS

# 1.  AUGMENTED DICKEY-FULLER

# Ho: A série não é estacionária (status quo)
# H1: A série é estacionária

adf.test(diff.tour)

#data:  diff.tour
##alternative hypothesis: stationary

#Warning message:
#  In adf.test(diff.tour) : p-value smaller than printed p-value

# Rejeitamos a hipótese Ho da série ser não estacionária


# 2. KPSS

# Ho: A série é estacionária (status quo)
# H1: A série não é estacionária

tseries::kpss.test(diff.tour)

#data:  diff.tour
#KPSS Level = 0.18908, Truncation lag parameter = 3, p-value = 0.1

##In tseries::kpss.test(diff.tour) : p-value greater than printed p-value

# Não rejeitamos Ho. Continua haver a necessidade de difererenciação

pp.test(diff.tour)

# Ho: A série não é estacionária (status quo)
# H1: A série é estacionária

checkresiduals(diff.tour)

# Note a heterocedasticidade nos resíduos.

# CORRIGINDO A HETEROCEDASTICIDADE 

# Trabalhando a com a diferença do log da Série Temporal
# lag = 4 pois estamos trabalhando trimestres (se fosse mensal lag = 12)

diff_log <- (diff(log(tour),lag = 4))

library(gridExtra)

g1 <- ggAcf(diff_log, 
            lag.max = 60)+
  ylab("diff(log(tour)")+
  ggtitle("ACF diff_log")

g2 <- ggPacf(diff_log,
             lag.max=24)+
  ggtitle("PACF diff_log")


grid.arrange(g1, g2, nrow =2)

# ADF

# Ho: A série não é estacionária (status quo)
# H1: A série é estacionária

adf.test(diff_log)

# KPSS

# Ho: A série é estacionária (status quo)
# H1: A série não é estacionária

kpss.test(diff_log)

# PP
# Ho: A série não é estacionária (status quo)
# H1: A série é estacionária

pp.test(diff_log)


# Modelando o SARIMA

sarima.1 <- Arima(tour, order = c(1,1,1), 
                  seasonal = list(order = c(1,1,1), period = 4),
                            method = "ML", 
                                lambda = 0)

# Quando LAMBDA = ZERO, o comando fará a transformação por log.

sarima.1

# Checando a consistência dos parâmetros

#install.packages("BETS")
library(BETS)

t_test(model = sarima.1)

#NOK (Não OK)

# o teste indica um modelo SARIMA (0,1,1)(0,1,1)[4]

# Remodelando

sarima.2 <- Arima(tour, order = c(0,1,1), 
                  seasonal = list(order = c(0,1,1), period = 4),
                    method = "ML", 
                            lambda = 0)

sarima.2

t_test(model = sarima.2)

## Esse é o modelo que literalmente modelamos.SARIMA(0,1,1)(0,1,1)[4] 


# DIAGNÓSTICO

# teste de ausência de autocorrelação serial

# teste Ljung-Box

# Ho: Os resíduos são independentes e identicamente (Inexiste aurocorrelação)
#           distribuídos (ou seja: não há dependência serial) 
#           para todas as defasagens).

# fitdf = p+q+P+Q = 0+1+0+1 = 2

Box.test(x = sarima.2$residuals, lag=24, type = "Ljung-Box", fitdf = 2)

# Não podemos rejeitar Ho. A série é iid

checkresiduals(sarima.2)

# Checando a Heterocedasticidade

# Teste proposto por Engle (1982) # GARCH

# Ho: Os resíduos ao quadrado são uma sequência de
#       ruídos brancos, ou 	seja, os resíduos são homocedásticos.

# H1: a série é heterocedástico

require(FinTS)
library(FinTS)

ArchTest(sarima.2$residuals, lags = 36)

# Não rejeitamos a hipóse nula. A série se comporta como um WN.


# NORMALIDADE

# Ho : ~ N(mi, sigma2)
# H1 : Rejeita Ho

require(normtest)
library(normtest)

jb.norm.test(sarima.2$residuals, nrepl = 2000)

shapiro.test(sarima.2$residuals)

hist(sarima.2$residuals)

round(summary(sarima.2$residuals), digits = 3)

# calculando a moda
# Criando a função

moda <- function(v) {
          uniqv <- unique(v)
            uniqv[which.max(tabulate(match(v, uniqv)))]
                    }

result <- moda(as.numeric(sarima.2$residuals))
print(result)

# média=mediana=moda (praticamente zero)


# Previsão

forecast.2 <- forecast(object = sarima.2, h = 8, level = 95)

t_test(model = sarima.2)

forecast.2$fitted

forecast.2

autoplot(tour, series = "Série Real")+ # serie original
  autolayer(forecast.2$fitted, series = "Modelo SARIMA")+ # fit do modelo
  autolayer(forecast.2, series = "Previsão", showgap = FALSE)+ #previsão
  xlab("Trimestre")+
  ylab("Turistas em milhões")+
  ggtitle("Turismo na Austrália")

AIC(sarima.2)
AIC(hw.a$model)
AIC(hw.m$model)

# O MODELO 3: usando o auto.arima() 

# Rodando o algoritmo do auto.arima()

auto.arima(tour, seasonal = TRUE, 
                             stepwise = FALSE,
                                      approximation = FALSE)

# Resposta: SARIMA(1,0,0)(0,1,2)[4] 


sarima.3 <- Arima(tour, order = c(1,0,0), list(order = c(0,1,2), period = 4),
                  method = "ML", lambda = 0)
sarima.3

# Diagnostico

# testanto a ausencia de autocorrelação

#fitdf = p+q+P+Q = 1 + 0+ 0 + 2 = 3

# Ho: Os resíduos são independentes e identicamente (Inexiste aurocorrelação)
#           distribuídos (ou seja: não há dependência serial) 
#           para todas as defasagens).

# H1: rejeita Ho

Box.test(x = sarima.3$residuals, lag= 24, type = "Ljung-Box", fitdf = 3)

# Testando a Heterocasticidade

# Ho A série é homocedástica
# H1 A série é heterocedáastica

ArchTest(sarima.3$residuals, lags = 36)

# Normalidade

# Ho: ~ N(mu, sigma2)
# H1: Rejeita Ho

require(normtest)
library(normtest)

jb.norm.test(sarima.3$residuals, nrepl = 2000)

summary(sarima.3$residuals)

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- moda(as.numeric(sarima.2$residuals))
print(result)

# Previsão

require(forecast)

forecast.3 <- forecast(object = sarima.3, h = 8, level = 95)

forecast.3$fitted

autoplot(tour, series = "Original")+
  autolayer(forecast.3$fitted, series = "Modelo AUTOARIMA")+
  autolayer(forecast.3, series = "Previsão", showgap = FALSE)+
  xlab("Trimestres")+
  ylab("Turistas em milhões")+
  ggtitle("Turismo na Austrália")

AIC(hw.a$model)
AIC(hw.m$model)
AIC(sarima.2)# meu modelo
AIC(sarima.3) #auto.arima

# ESCOLHA DO MODELO MENOR AIC
# O MELHOR MODELO É O DO AUTO_ARIMA()

sarima.3
forecast.3

autoplot(tour, series = "Original", lwd =1.05)+
  #autolayer(fit.a, series = "Modelo HW.A")+
 #autolayer(fit.m, series = "Modelo HW.M")+
  autolayer(forecast.2$fitted, series = "SARIMA (1,0,0)(0,1,2)[4]", lwd =1.05)+
  autolayer(forecast.3$fitted, series = "AUTO_ARIMA", lwd =1.05) #+
  # autolayer(forecast.3, series = "Previsão", showgap = FALSE, lwd = 1.1)

# Voilá!!


