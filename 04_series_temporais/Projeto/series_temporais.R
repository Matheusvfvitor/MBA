# Série Temporal relativa ao Consumo de energia elétrica no Brasil.

## 1. Instalação dos pacotes
install.packages(c("fpp",
                   "fpp2", 
                   "forecast", 
                   "readxl","BETS","FinTS","normtest","knitr","writexl","xlsx","TSA","tmtest"))

library(forecast)
library(fpp)
library(fpp2)
library(readxl)
library(BETS)
library(FinTS)
library(normtest)
library(knitr)
library(writexl)
library(xlsx)
library(TSA)
library(tmtest)

#Lendo no R studio Cloud
library(readxl)
consumo <- read_excel("consumo.xlsx")
View(consumo)
df = consumo

class(df$Consumo)


cons <- ts(df$Consumo, frequency = 12, 
            start = c(1979,1))


#Separando a base entre teste e treino
set.seed(1234)
ts.train <- window(cons, start=c(1979,1), end=c(2018,12))
ts.test <- window(cons, start = c(2019, 1), end = c(2020, 4))
cons = ts.train
cons
View(cons)

#Plotando a base de treino
autoplot(cons, color="purple")+
  xlab("Anos")+
  ylab("Consumo de Energia Elétrica em Kwh")+
  ggtitle("Consumo de Energia Elétrica no Brasil")+
  theme_classic()


## 3. Efetuar uma breve Análise Exploratória
ggseasonplot(cons, year.labels = TRUE,
             year.label.left = TRUE) +
  ylab("Consumo de Energia Elétrica em Kwh")+ 
  xlab("mês")+
  ggtitle("Consumo de Energia Elétrica no Brasil")

boxplot(cons~cycle(cons),
        xlab="mês",
        ylab = "Consumo em Kwh" ,
        col="blue", 
        main ="Consumo de Energia Elétrica no Brasil no Brasil",
        par(bg = '#E0E0E0'))

## 4. Decompor a série
decomp <- decompose(cons, type = "multiplicative")
autoplot(decomp)

decomp <- decompose(cons, type = "additive")
autoplot(decomp)

## 5. Modelos por suavização exponencial
## HW ADITIVO
hw.a <- hw(cons, seasonal = "additive", h = 36, level = 95) 
fit.a <- hw.a$fitted # Ajuste do modelo aos dados observados
fit.a

hw.a # a previsão 

autoplot(cons, series = "Série_Real", lwd= 1.1)+ #serie_original
  autolayer(fit.a, series = "modelo HW- Aditivo", lwd = 1.1)+ # Ajuste do modelo
  autolayer(hw.a, series = "Previsão", showgap = FALSE) # previsão h= n períodos 

accuracy(hw.a)

# MODELO HW MULTIPLICATIVO
hw.m <- hw(cons, seasonal = "multiplicative", h = 36, level = 95)
fit.m <- hw.m$fitted

fit.m
hw.m
accuracy(hw.m)


autoplot(cons, series = "Série Orignal", lwd= 1.1)+ #serie_original
  autolayer(fit.m, series = "modelo HW-Multiplicativo", lwd =1.1)+# Ajuste do modelo
  autolayer(hw.m ,series = "Previsão", showgap = FALSE) # previsão h= n períodos 

# Comparativo entre os modelos: 
accuracy(hw.a$model)
accuracy(hw.m$model)

#  O RSME (Root Mean Squared Error)e o MAPE (Mean Absolute Percentage Error)
#  são menores no HW. Aditivo

# Verificando os Correlogramas
ggAcf(cons)

# Visualmente, a série não é estacionãria
ggPacf(cons)


# TESTES FORMAIS : 
#install.packages("tseries")
library(tseries)

# Regra de Ouro: P-VALOR BAIXO INDICA REJEITAR Ho #
# AUGMENTED DICKEY-FULLER - É o teste mais usado
# Ho: A série não é estacionária (status quo)
# H1: A série é estacionária (alternativa)

adf.test(cons) # na série observada
# 2. KPSS # esse é o único teste em que a Ho é série estacionária
# Ho: A série é estacionária (status quo)
# H1: A série não é estacionária

tseries::kpss.test(cons)
pp.test(cons)

# Ho: A série não é estacionária (status quo)
# H1: A série é estacionária
checkresiduals(cons)

# Realizando a Diferenciação

diff.cons <- diff(cons)
ggAcf(diff.cons)
ggPacf(diff.cons)



# CORRIGINDO A HETEROCEDASTICIDADE 

# Trabalhando a com a diferença do log da Série Temporal
# Utilizando o periodo 12 sugerido pelo auto-arima

diff_log <- (diff(log(cons),lag = 12))
install.packages("gridExtra")
library(gridExtra)

g1 <- ggAcf(diff_log, 
            lag.max = 24)+
  ylab("diff(log(fert)")+
  ggtitle("ACF diff_log")


g2 <- ggPacf(diff_log,
             lag.max=24)+
  ggtitle("PACF diff_log")

grid.arrange(g1, g2, nrow =2)

#Verificando Residuos
checkresiduals(diff_log)

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
# Rodando o algoritmo do auto.arima()
auto <- auto.arima(cons, seasonal = TRUE, stepwise = FALSE,approximation = FALSE)
auto

forecast.model <- forecast(object = auto, h = 36, level = 95)
forecast.model

forecast.model$fitted
forecast.model$residuals

# Resposta: SARIMA(1,0,0)(2,1,0)[12] with drift
# Diagnostico

# testanto a ausencia de autocorrelação

#fitdf = p+q+P+Q = 1 + 2+ 0 + 0 = 3

# Ho: Os resíduos são independentes e identicamente (Inexiste aurocorrelação)
#           distribuídos (ou seja: não há dependência serial) 
#           para todas as defasagens).

# H1: rejeita Ho

Box.test(x = forecast.model$residuals, lag= 20, type = "Ljung-Box", fitdf = 3)

# Testando a Heterocasticidade

# Ho A série é homocedástica
# H1 A série é heterocedáastica

ArchTest(forecast.model$residuals, lags = 20)

# Normalidade

# Ho: ~ N(mu, sigma2)
# H1: Rejeita Ho

require(normtest)
library(normtest)

jb.norm.test(forecast.model$residuals, nrepl = 2000)

summary(forecast.model$residuals)

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- moda(as.numeric(forecast.model$residuals))
print(result)

#Modelando Sarima
# MODELO SARIMA(p,d,q)(P,D,Q)[sazonalidade]

arima <- Arima(cons, order = c(1,0,0), list(order = c(2,1,0), period = 12),
               method = "ML",lambda=0)

arima
forecast.arima <- forecast(object = arima, h = 36, level = 95)
forecast.arima

forecast.arima$fitted
forecast.arima$residuals

# Diagnostico
# testanto a ausencia de autocorrelação

#fitdf = p+q+P+Q = 1 + 2+ 0 + 0 = 3

# Ho: Os resíduos são independentes e identicamente (Inexiste aurocorrelação)
#           distribuídos (ou seja: não há dependência serial) 
#           para todas as defasagens).

# H1: rejeita Ho

Box.test(x = forecast.arima$residuals, lag= 24, type = "Ljung-Box", fitdf = 3)

# Testando a Heterocasticidade

# Ho A série é homocedástica
# H1 A série é heterocedáastica

ArchTest(forecast.arima$residuals, lags = 36)

# Normalidade

# Ho: ~ N(mu, sigma2)
# H1: Rejeita Ho

require(normtest)
library(normtest)

jb.norm.test(forecast.arima$residuals, nrepl = 2000)

summary(forecast.arima$residuals)

moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

result <- moda(as.numeric(forecast.arima$residuals))
print(result)


#Plotando o modelo Sarima
autoplot(cons, series = "Original")+
  autolayer(forecast.arima$fitted, series = "Modelo AUTOARIMA")+
  autolayer(forecast.arima, series = "Previsão", showgap = FALSE)+
  xlab("Anos")+
  ylab("Consumo de Energia Elétrica em KWh")+
  ggtitle("Consumo de energia elétrica no Brasil")

AIC(hw.a$model)
AIC(hw.m$model)
AIC(forecast.arima$model)# sarima
AIC(forecast.model$model) #auto.arima

# ESCOLHA DO MODELO MENOR AIC
# O MELHOR MODELO É O SARIMA

forecast.arima
fcst <- window(forecast.arima, start=c(2010,1))

autoplot(forecast.arima, color="red")+
  xlab("Anos")+
  ylab("Consumo de Energia Elétrica em Kwh")+
  ggtitle("Forecast de consumo de energia elétrica no Brasil final de 2021")+
  theme_classic()


