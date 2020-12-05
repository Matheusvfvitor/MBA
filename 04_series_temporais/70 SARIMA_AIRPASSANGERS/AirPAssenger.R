# Instalação dos pacotes
# BETS Brazilian Econometrics Time Series
# urca Unit Root and Cointegration Test for Time Series Data 
# TSA  Time Series Analysis
# forecast Forecasr functions for Time Series and Linear Models
# tmtest Testig Linear Regression Model
# normtest Tests for Normality
# FinTS Analysis od Financial Time Series
# xlsx (read, write format Excel)
# fpp2 Rob J Hyndman and George Athanasopoulos
#  TSstudio 

#install.packages(c("BETS", "urca", "TSA", "forecast", "tmtest", "normtest", "FinTS", "xlsx", "fpp2", "TSstudio"))

library(BETS)
library(urca)
library(TSA)
library(forecast)
library(tmtest)
library(normtest)
library(xlsx)
library(fpp2)
library(TSstudio)             
      

data("AirPassengers")
class(AirPassengers)


AirPassengers <- ts(AirPassengers, frequency = 12)


autoplot(AirPassengers, xlab = "ano", 
              ylab="Vendas de Passagens Aéreas",
         main = "Passageiros Mensais (000´s) de 1949 a 1960", 
         col ="blue", lwd=2)

ggseasonplot(AirPassengers, year.labels = TRUE, year.label.left = TRUE)+
  ylab("Vendas de Passagens Aéreas")+ xlab("Ano")+
  ggtitle("Passageiros Mensais (000´s) de 1949 a 1960")


boxplot(AirPassengers~cycle(AirPassengers),col = rep(c("gold",
                                           "darkgreen",
                                           "blue", "red"), each=3),
        xlab="Mês", ylab= "Vendas de Passagens Aéreas",
        main ="Passageiros Mensais (000´s) de 1949 a 1960",
        par(bg="white"))

# Alternativamente (pacote TSstudio)

AirPassengers <- ts_seasonal(AirPassengers, type = "box")


# Decomposição da série

autoplot(decompose(AirPassengers, type = "multiplicative"))

corrgram(AirPassengers, lag.max=36)

ggAcf(AirPassengers, lag.max = 36)

# teste ADF

install.packages("tseries")
library(tseries)

adf.test(AirPassengers)

summary(ur.kpss(AirPassengers))


library(urca)

adf.air <- ur.df(y = AirPassengers, type = c("drift"), 
                      lags = 24, selectlags = "AIC")

adf.air@teststat  #estatística de teste

adf.air@cval  #valores tabulados por MacKinnon (1996)

# mais informações sobre o teste RU

summary(adf.air)

# Diferenciação

a1 <- autoplot(diff(AirPassengers, lag = 1, differences = 1))+
        ylab("diff(AirPassengers)")+
        ggtitle("ST de vendas de passagens aéreas (em milhares) com uma diferença")

a2 <- ggAcf(diff(AirPassengers, lag = 1, differences = 1),
              lag.max = 36)+
        ggtitle("ACF: ST de vendas de passagens aéreas (em milhares) com uma diferença")

library(gridExtra)
grid.arrange(a1, a2, nrow = 2)

# Passando o log (para reduzir a heterocedasticidade)

ts.plot(diff(log(AirPassengers),lag = 1,differences = 1))


l1 <- autoplot((diff(log(AirPassengers),lag = 1,differences = 1)))+
        ylab("diff(log(AirPassengers)")+
        ggtitle("Log da ST de vendas de passagens aéreas (em milhares) com uma diferença")



l2 <- ggAcf(diff(log(AirPassengers), lag = 1, differences = 1),
              lag.max=48)+
              ggtitle("ACF: Log da ST de vendas de passagens aéreas (em milhares)com uma diferença")


grid.arrange(l1, l2, nrow =2)

# Aumentando para lag =12

k1 <- autoplot(diff(diff(log(AirPassengers), lag = 1, differences = 1),
             lag = 12, differences = 1))+
             ylab("diff(log(AirPassengers) lag =12")+
  ggtitle("Log da ST de vendas de passagens aéreas(em milhares) com uma diferença")

k2 <- ggAcf(diff(diff(log(AirPassengers), lag = 1, differences = 1),
            lag = 12, differences = 1), lag.max = 48)+
            ylab("diff(log(AirPassengers) lag =12")+
  ggtitle("ACF: Log da ST de vendas de passagens aéreas(em milhares) com uma diferença")

grid.arrange(k1, k2, nrow =2)      

# Revisitando ADF test

adf.air2 <- ur.df(diff(diff(log(AirPassengers), lag = 1), lag = 12),
                  type = "drift", lags = 24, selectlags = "AIC")
adf.air2@teststat  #estatística de teste

adf.air2@cval  #valores tabulados por MacKinnon (1996)

# Especificação

acf <- ggAcf(diff(diff(log(AirPassengers), lag = 1, differences = 1),
                   lag = 12, differences = 1), lag.max = 48)

pacf <- ggPacf (diff(diff(log(AirPassengers), lag = 1, differences = 1),
                   lag = 12, differences = 1), lag.max = 48)

grid.arrange(acf, pacf, nrow = 2)

library("forecast")

#PRIMEIRO MODELO SARIMA(1,1,1)(1,1,1)12

fit.air.1 <- Arima(AirPassengers, order = c(1,1,1), seasonal = c(1,1,1),
                 method = "ML", lambda = 0)
fit.air.1

# teste de significância para o modelo SARIMA(1,1,1)(1,1,1)12

t_test(fit.air.1)

# SEGUNDO MODELO SARIMA (0,1,1)(0,1,1)12

fit.air.2 <- Arima(AirPassengers, order = c(0,1,1), seasonal = c(0,1,1),
                 method = "ML", lambda=0)
fit.air.2

t_test(fit.air.2)


# utizando a função auto.arima()pacote fpp2
# auto.arima(AirPassengers, seasonal= TRUE, stepwise = FALSE, approximation = FALSE)

fit.air.3 <- Arima(AirPassengers, order = c(2,1,1), seasonal = c(0,1,0),
                 method = "ML", lambda=0)
fit.air.3

t_test(fit.air.3)



# Criterio de Informação

fit.air.1$aicc
fit.air.2$aicc
fit.air.3$aicc

# Escolha do melhor modelo: O menor AICc
# ARIMA(0,1,1)(0,1,1)[12]

autoplot(fit.air.2$residuals) + theme_minimal()+
  ylab("Resíduos")+
  ggtitle("ARIMA(0,1,1)(0,1,1)12 - Resíduos")

hist(fit.air.2$residuals)

df <- data.frame(fit.air.2$residuals)

ggplot(df, aes(x = fit.air.2$residuals)) + 
  geom_histogram(aes(y=..density..), colour="darkblue", fill="white")+
                     geom_density(alpha=.1, fill="steelblue", lwd=0.3)+
  ggtitle("Histograma e densidade dos resíduos")+
  xlab("Frequência")+
  ylab("densidade")+theme_minimal()

round(summary(fit.air.2$residuals), digits = 4)

#Ljung Box
diag <- tsdiag(fit.air.2, gof.lag = 20)

Box.test(x = fit.air.2$residuals, lag = 24, type = "Lj", fitdf=2)

checkresiduals(fit.air.2)

# Heterocedasticidade
require(FinTS)
ArchTest(fit.air.2$residuals,lags = 36)

#
require(normtest)

jb.norm.test(fit.air.2$residuals, nrepl=2000)


#install.packages("lawstat")
library(lawstat)

rjb.test(
  fit.air.2$residuals,
  option = c( "JB"),
  crit.values = c("chisq.approximation", "empirical"),
  N = 2000
)

round(summary(fit.air.2$residuals), digits =4)

shapiro.test(fit.air.2$residuals)

# previsão

library(forecast)

fit.air.2 <- Arima(AirPassengers, order = c(0,1,1), seasonal = c(0,1,1),
                   method = "ML", lambda=0)

fit <- forecast(object = fit.air.2, h = 12, level =0.95)

#install.packages("ggthemes")
library(ggthemes)


autoplot(object=fit, xlab="Mês",
                            ylab="Vendas Passagens Aéreas por mês em milhares",
                    main="AirPassengers - ARIMA(0,1,1)(0,1,1)[12]",
                    PI=TRUE, fcol = "steelblue")+ theme_fivethirtyeight()

#https://pkg.robjhyndman.com/forecast/reference/plot.forecast.html

round(accuracy(fit.air.2), digits = 4)



# exportando previsões
# csv
write.csv2(data.frame(previsao),"previsao.csv")

# xlsx
require(xlsx)
write.xlsx(data.frame(previsao),"previsao.xlsx")
