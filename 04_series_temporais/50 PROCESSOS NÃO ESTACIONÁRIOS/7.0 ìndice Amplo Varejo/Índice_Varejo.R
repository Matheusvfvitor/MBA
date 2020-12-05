# Volume de vendas no comércio varejista (Índice base fixa (2014=100))
# https://www.ibge.gov.br/estatisticas/economicas/comercio/9227-pesquisa-mensal-de-comercio.html?=&t=series-historicas

#install.packages(c("fpp2", "tidyverse", "readxl", "TSstudio",  "tseries", 
#                   "BETS", "knitr","writexl"))


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
df.res <- BETSsearch("Retail Sales",
                     view = FALSE) 

# Verificando o resultado
df.res %>%
  kable() %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"))

# seleciona a identificação
id <- 1455 

# datas de início e término da série 

inicio = '2000-01-31' 

fim = as.character(Sys.Date()) 

# obtem os dados 

df.BETS <- BETSget(code = id, 
                   data.frame = TRUE, 
                   from = inicio, 
                   to = fim)
df <- df.BETS

# write_xlsx(df, "Vendas_Varejo.xlsx")

head(df)

tail(df)

varejo <- df$value

ts.varejo <- ts(varejo, frequency =  12, start=c(2000,1), end = c(2020,06))
ts.varejo

autoplot(ts.varejo, lwd = 1.2, col = "steelblue")+
  labs(y = 'ìndice Amplo Varejo', 
       x = 'Data',
       title = "Volume de vendas no comércio varejista (Índice base fixa (2014=100))",
       caption = 'Fonte: IBGE')


# (pacote TSstudio)
ts_seasonal(ts.varejo, type = "box")

# Correlograma

acf <- ggAcf(ts.varejo)
acf

pacf <- ggPacf(ts.varejo)
pacf

# A ´serie é claramente não estacionária.
# O Pacf sugere que a séria ficaria estacionária no lag = 5,
# mas existe a sazonalidade


############################################
# Utlizaremos os testes de estacionariedade#
# com o pacote tseries                     #
############################################

# Regra de outro: P-valor baixo: rejeita Ho.


#Teste ADF - No nível

# Ho: A série não é estacionária
# H1: A série é estacionária

adf.test(ts.varejo)
# rejeita-se Ho. p-valor:0.756
# Note que o ADF está sugerindo um lag = 5

# Teste KPSS

# Ho: A série é estacionária
# H1: A série não é estacionária

kpss.test(ts.varejo)

# p-value smaller than printed p-value. Rejeita a hipótese nula

# Teste Philipps - Perron

# Ho: A série não é estacionária
# H1: A série é estacionária

pp.test(ts.varejo)
# Não rejeita a hipótese nula: p-value = 0.2136


# Efetuando a diferença

diff <- diff(ts.varejo)

lim <- 1.96/sqrt(length(ts.varejo))
lim

autoplot(diff, col ="blue", lwd = 1.1)+
  geom_hline(yintercept = 0,
             colour='darkred', 
             linetype='solid', 
             size=1.0)


# Efetuando o correlograma

acf.diff <- ggAcf(diff, lag.max = 24)
acf.diff

# uma diferenciação de lag =6, mas observa-se a sazonalidade

# O Acf é para medir o Modelo MA(q)

pacf.diff <- ggPacf(diff, lag.max=24)
pacf.diff 
# Conte os primeiros spikes do PACF
# para o lag do AR(p)

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

ar <- arima(ts.varejo, order = c(2,1,0))
ar

#Efetuando as previsões

fit <- forecast(ar, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.varejo, series = "Serie_Observada")+
  autolayer(fit$fitted, series = "Série Predita_AR")+
  autolayer(fit, series = "Projeção h =12")

# MA(q)

ma <- arima(ts.varejo, order = c(0,1,1))
ma

#Efetuando as previsões

fit <- forecast(ma, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.varejo, series = "Serie_Observada")+
  autolayer(fit$fitted, series = "Série Predita_ MA")+
  autolayer(fit, series = "Projeção h =12")


#ARMA(p,q)

arma <- arima(ts.varejo, order = c(2,1,1))
arma

#Efetuando as previsões

fit <- forecast(arma, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.varejo, series = "Serie_Observada")+
  autolayer(fit$fitted, series = "Série Predita_ ARMA")+
  autolayer(fit, series = "Projeção h =12")

# usando o Auto.Arima

auto.arima <- auto.arima(ts.varejo, seasonal = TRUE,
                         stepwise = FALSE, 
                         approximation = FALSE)
auto.arima

fit <- forecast(auto.arima, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.varejo, series = "Serie_Observada")+
  autolayer(fit$fitted, series = "Série Predita_ AUTO.ARIMA")+
  autolayer(fit, series = "Projeção h =12")

round(accuracy(ar),  4)
round(accuracy(ma),  4)
round(accuracy(arma), 4)
round(accuracy(auto.arima), 4)

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

checkresiduals(auto.arima, col ="red")

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
# média = mediana =moda = zero

####################
# TRABALHO EM GRUPO#
####################

# DIVIDIR PARA CONQUISTAR

# TRAIN E TEST 

ts.train <- window(ts.varejo, start=c(2000,1), end=c(2019,12))

ts.test <- window(ts.varejo, start = c(2020, 1), end = c(2020, 3))

tail(ts.train, n= 3)

head(ts.test, n = 3)

# Efetuar o modelos

# utilizando a ts.train
# Efetuar a correlaçaõ total e parcial da serie
# Testar a estacionariedade da serie com base nos testes
# ADF, KPSS, PP.
# Efetuar a diferenciação
# Mesmo procedimento acima
# Montar os modelos
# AR(p)
# MA(q)
# ARMA (p,q)
# ARIMA(p.d.q) ou SARIMA(p.d.q)(P,D,Q)[s]

# Efetuar os testes de diagósticos
# Escolher o melhor modelo
# Efetuar a projeção para h = 21)
# Comparar com os dados reais da série ts.test



