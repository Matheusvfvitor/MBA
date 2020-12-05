#install.packages(c("Quandl", "fpp","fpp2", "xlsx", "readxl"))

library(Quandl) #  data scraping
library(fpp)
library(fpp2)
library(xlsx)
library(readxl)
library(gridExtra)

robusta<-Quandl(code = "ODA/PCOFFROB_USD",
                  collapse="quarterly", # tranformando a série em trimestres.
                    type = "ts",
                     end_date = "2020-05-01")


#(robusta, "robusta.xlsx")

#robusta <- read_xlsx("robusta.xlsx")

class(robusta)

robusta
head(robusta)
tail(robusta)

ts.robusta <- ts(robusta, frequency = 4, start = c(1980, 1))

class(ts.robusta)

autoplot(ts.robusta)

# EFETUANDO UM CORTE DE JAN-2000 EM DIANTE

ts.robusta <- window(ts.robusta, start = c(2000, 1))

decomp <- decompose(ts.robusta, type = "multiplicative")
decomp
autoplot(decomp)

acf.robusta <- ggAcf(ts.robusta, lag.max=24)
acf.robusta

# VIF = VARIANCE INFLATION FACTOR

pacf.robusta <- ggPacf(ts.robusta)
pacf.robusta

#Teste ADF - pacote tseries

adf.test(ts.robusta)

kpss.test(ts.robusta)

pp.test(ts.robusta)

# Augmented Dickey-Fuller Test
# data:  ts.robusta
# Dickey-Fuller = -1.5112, Lag order = 6, p-value = 0.7815
#  alternative hypothesis: stationary

# Não rejeita Ho: série é não estacionária

#library(urca)

#adf_robusta<- ur.df(ts.robusta, type = "trend", lags = 6, selectlags = "AIC")

#summary(adf_robusta)

# Value of test-statistic is: -1.2859 0.9315 1.3005 

# Critical values for test statistics: 
#      1pct  5pct 10pct
#tau3 -3.99 -3.43 -3.13

# Não rejeita Ho. Série não-estacionária.

# KPSS

#kpss_robusta<- ur.kpss(ts.robusta, type = "tau", lags = "short")
#summary(kpss_robusta)

# Test is of type: tau with 4 lags. 

#   Value of test-statistic is: 0.8227 

#  Critical values for test statistics: 
#          1pct  5pct 10pct
#   tau3 -3.99 -3.43 -3.13

# Rejeita H0. Série não-estacionária. (KPSS é diferente!)

# Efetuando a diferença

diff <- diff(ts.robusta)

library(gridExtra)

h1 <- autoplot(ts.robusta)
h2 <- autoplot(diff)
grid.arrange(h1, h2, nrow = 2)

# Em 2008, subida e quedas dos preços 
#não retornaram à série original.
# No entanto, na diferença percebe-se
#que após a oscilação, série 
# retorna ao seu comportamento original.

# Efetuando os testes de autocorrelação serial

acf.diff <- ggAcf(diff, lag.max = 12)
acf.diff

pacf.diff <- ggPacf(diff, lag.max=12)
pacf.diff

# MA(1)

ma_1 <- arima(ts.robusta, order = c(0,0,1))
ma_1

# bem melhor!

ma_1a <- arima(ts.robusta, order = c(0,1,1))
ma_1a

j1 <- autoplot(ma_1)
j2 <- autoplot(ma_1a)
grid.arrange(j1, j2, nrow = 2)

#Efetuando as previsões

forecast <- forecast(ma_1) # SE QUISER FIXAR IC level = 95)
autoplot(forecast)
forecast$fitted

forecast_a <- forecast(ma_1a) # SE QUISER FIXAR IC level = 95)
autoplot(forecast_a)

autoplot(ts.robusta, series = "CAFÉ ROBUSTA")+
  autolayer(forecast$fitted, series = "Modelo MA(1)")+
  autolayer(forecast, series = "Previsão ")

# melhor!
autoplot(ts.robusta, series = "CAFÉ ROBUSTA")+
  autolayer(forecast_a$fitted, series = "Modelo MA(1)a")+
  autolayer(forecast_a, series = "Previsão a")

# Vamos particionar com train & test

ts.train <- window(ts.robusta, start = c(2000, 1), end = c(2019, 04))

ts.test <- window(ts.robusta, start = c(2020, 1))

ts.test

tail(ts.train)
head(ts.test)

# MA(1)

ma_1.train <- arima(ts.train, order = c(0,0,1))
ma_1.train

ma_1a.train <- arima(ts.train, order = c(0,1,1))
ma_1a.train

#Efetuando as previsões

forecast <- forecast(ma_1.train, h = 14) # SE QUISER FIXAR IC level = 95)
autoplot(forecast)
forecast$fitted
forecast


autoplot(ts.train, series = "Train")+
  autolayer(ts.test, series = "Test")+
  autolayer(forecast$fitted, series = "Previsão MA(1)", PI = TRUE)+
  autolayer(forecast)+
  xlab("mes")+
  ylab("US$")+
  ggtitle("COTAÇÃO MENSAL CAFÉ ROBUSTA EM US$")

comp <- data.frame(ts.test, forecast)
(comp$erro <- ((comp$ts.test/comp$Point.Forecast)-1)*100)
(SS <- sum((comp$ts.test-comp$Point.Forecast)^2))

# O melhor modelo
forecast_a <- forecast(ma_1a.train, h = 14) # SE QUISER FIXAR IC level = 95)
autoplot(forecast_a)

autoplot(ts.train, series = "Train")+
  autolayer(ts.test, series = "Test")+
  autolayer(forecast_a$fitted, series = "Previsão MA(1)a", PI = TRUE)+
  autolayer(forecast_a)+
  xlab("mês")+
  ylab("US$")+
  ggtitle("COTAÇÃO MENSAL CAFÉ ROBUSTA EM US$")

comp_a <- data.frame(ts.test, forecast_a)
(comp_a$erro <- ((comp_a$ts.test/comp_a$Point.Forecast)-1)*100)

comp_a
]]
