#install.packages(c("fpp2", "forecast", "readxl", "TSstudio", "tseries"))
library(fpp2)
library(forecast)
library(readxl)
library(TSstudio)
library(gridExtra)
library(tseries)

# https://www.amtrak.com/home

Amtrak.data <- read.csv("Amtrak data.csv")
# View(Amtrak.data)

head(Amtrak.data)
tail(Amtrak.data)

pas.ts <- ts(Amtrak.data$Ridership, start = c(1991, 1), end = c(2004, 3), freq = 12)
pas.ts
autoplot(pas.ts)

pas.ts <- window(pas.ts, start= c(1996,1))
autoplot(pas.ts)

# (pacote TSstudio)
ts_seasonal(pas.ts, type = "box")

# AUTOCORRELAÇÃO SERIAL

acf.pas <- ggAcf(pas.ts)
acf.pas

# O exame da autorrelação da serie nos ajuda a identificar os
# padrões de sazonalidade. Neste caso, nota-se um uma forte 
# autocorrelação  no lag 12.

# Vendo o grafico, podemos notar o
# padrão de alto verão e baixo inverno.

# Podemos também examinar a série de residuos. Se modelarmos
# corretamente a série de resíduos a plotagem passará a não 
# mais mostrar a autocorrelação serial.

pacf <- ggPacf(pas.ts)
pacf

# MODELO AR(p)

# Diferenciando a serie

diff <- diff(pas.ts)

ggAcf(diff)
ggPacf(diff)

AR <- arima(pas.ts, order = c(2,0,0))
AR

#Efetuando a previsão do modelo

fit.ar <- forecast(AR, h = 20)

autoplot(pas.ts) +
    autolayer(fit.ar$fitted)+
    autolayer(fit.ar)
    
# Modelo não convenceu!!!!

# MA(q)

MA <- arima(pas.ts, order = c(0,0,1))
MA

fit.ma <- forecast(MA, h = 20)

autoplot(pas.ts) +
  autolayer(fit.ma$fitted)+
  autolayer(fit.ma)

# Também não convenceu!!

# ARMA(p,q)

ARMA <- arima(pas.ts, order = c(2,0,1))
ARMA

fit <- forecast(ARMA, h = 20)

autoplot(pas.ts) +
  autolayer(fit$fitted)+
  autolayer(fit)

#AUTOARIMA 

autoARIMA <- auto.arima(pas.ts, seasonal= TRUE,
                                      stepwise = FALSE, 
                                        approximation = FALSE)
autoARIMA

fit.autoARIMA <- forecast(autoARIMA, h=20)

autoplot(pas.ts, series ="Dados reais") +
  autolayer(fit.autoARIMA$fitted, series = "Modelo SARIMA")+
  autolayer(fit.autoARIMA, series = "Previsão", showgap = FALSE)

AIC(AR)
AIC(MA)
AIC(ARMA)
AIC(autoARIMA)

round(accuracy(AR),4)
round(accuracy(MA),4)
round(accuracy(ARMA),4)
round(accuracy(autoARIMA),4)

# O melhor modelo? 
