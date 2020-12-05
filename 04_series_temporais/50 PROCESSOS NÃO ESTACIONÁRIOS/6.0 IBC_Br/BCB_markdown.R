
## ----Carrega_pacotes------------------------------------------------------------------------------------------------
library(fpp2)
library(tidyverse)
library(readxl)
library(TSstudio)
library(tseries)
library(writexl) 
library(BETS) 
library(dplyr) 
library(knitr)

#' 
#' ## Procurando a identificação do índice
#' 
## ----busca_indice---------------------------------------------------------------------------------------------------
df.res <- BETSsearch("Central Bank Economic Activity Index",
                     view = FALSE) 

#' 
#' ## Verificando o resultado e selecionando o índice
#' 
## ----verifica_resultado---------------------------------------------------------------------------------------------
df.res %>%
  kable() %>%
  kableExtra::kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"))

id <- 24363 

#' 
#' ## Define as datas de início e término da série
#' 
## ----datas----------------------------------------------------------------------------------------------------------
inicio = '2003-01-01' 

fim = as.character(Sys.Date() - 1) # ontem

#' 
#' ## Obtém e visualiza os dados
#' 
## ----obtem_dados----------------------------------------------------------------------------------------------------
df.BETS <- BETSget(code = id, 
                   data.frame = TRUE, 
                   from = inicio, 
                   to = fim)
df <- df.BETS

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
  
  
  ggtitle("Índice de Atividade do Banco Central do Brasil")

#' 
#' ## Usando o pacote `TSstudio`
#' 
## ----tsstudio-------------------------------------------------------------------------------------------------------
ts_seasonal(ts.ibc, 
            type = 'box', 
            title = 'Sazonalidade - Índice de Atividade do BCB')

#' 
#' ## Correlograma
#' 
## ----correlograma---------------------------------------------------------------------------------------------------
acf <- ggAcf(ts.ibc)
acf

pacf <- ggPacf(ts.ibc)
pacf

#' 
#' A série é claramente não estacionária.  O Pacf sugere que a séria ficaria estacionária no lag = 3.
#' 
#' Utlizaremos os testes de estacionariedade com o pacote `tseries`.
#' 
#' ### Teste ADF - no nível:
#' 
#' $H_0$: A série **não é** estacionária  
#' 
#' $H_1$: A série é estacionária
#' 
## ----adf_test-------------------------------------------------------------------------------------------------------
adf.test(ts.ibc)

#' 
#' * Rejeita-se Ho. p-valor: 0.756   
#' * Note que o ADF está sugerindo um lag = 5
#' 
#' ### Teste KPSS (**note a diferença da hipótese básica do teste**)
#' 
#' * $H_0$: A série **é** estacionária   
#' 
#' * $H_1$: A série não é estacionária
#' 
## ----kpss-----------------------------------------------------------------------------------------------------------
kpss.test(ts.ibc)

#' 
#' Rejeita-se a hipótese nula (*p-value smaller than printed p-value*).
#' 
#' ### Teste Philipps - Perron
#' 
#' * $H_0$: A série não é estacionária
#' * $H_1$: A série é estacionária
#' 
## ----pp-------------------------------------------------------------------------------------------------------------
pp.test(ts.ibc)

#' 
#' Não se pode rejeitar a hipótese nula a menos de $\alpha$ = 21%: p-valor = 0.2136
#' 
#' ## Efetuando a diferença
#' 
## ----diff-----------------------------------------------------------------------------------------------------------
diff <- diff(ts.ibc)

lim <- 1.96/sqrt(length(ibc))

autoplot(diff, col ="blue", lwd = 1.1) +
  geom_hline(yintercept = 0,
             colour = 'darkred', 
             linetype = 'solid', 
             size = 0.75) +
  xlab('Data') +
  ylab('Diff')    

#' 
#' ## Efetuando o correlograma
#' 
## ----correlograma2--------------------------------------------------------------------------------------------------
acf.diff <- ggAcf(diff, lag.max = 24)
acf.diff

#' 
#' Uma diferenciação de lag = 2, mas observa-se a sazonalidade nos instantes 6 e 12 e 18 e 24. O teste sugere 4 lags (5-1).
#' 
#' O Acf é útil para medir o Modelo MA(q).  
#' 
## ----maq------------------------------------------------------------------------------------------------------------
pacf.diff <- ggPacf(diff, lag.max = 24)
pacf.diff 

#' 
#' Conte os primeiros *spikes* do PACF para o lag do AR(p).  
#' 
#' ## Teste ADF - na diferença
#' 
#' * $H_0$: A série não é estacionária
#' * $H_1$: A série é estacionária
#' 
## ----adf_diff-------------------------------------------------------------------------------------------------------
adf.test(diff)

#' 
#' ### Teste KPSS na diferença
#' 
#' * $H_0$: A série **é** estacionária   
#' 
#' * $H_1$: A série não é estacionária
#' 
## ----kpss_diff------------------------------------------------------------------------------------------------------
kpss.test(diff)

#' 
#' ### Teste Philipps - Perron na diferença
#' 
#' * $H_0$: A série não é estacionária
#' * $H_1$: A série é estacionária
#' 
## ----pp_diff--------------------------------------------------------------------------------------------------------
pp.test(diff)

#' 
#' ## AR(p)
#' 
## ----ar_p-----------------------------------------------------------------------------------------------------------
ar <- arima(ts.ibc, order = c(2,0,0))
ar

#' 
#' ## Efetuando as previsões com o modelo AR(p)
#' 
## ----previsoes_ar_p-------------------------------------------------------------------------------------------------
fit <- forecast(ar, h = 12) #  IC level = 95

autoplot(ts.ibc, series = "Série_Observada") +
    autolayer(fit$fitted, series = "Série Predita_AR") +
    autolayer(fit, series = "Projeção h = 12") +
    xlab('Data')

#' 
#' ## MA(q)
#' 
## ----ma_q-----------------------------------------------------------------------------------------------------------
ma <- arima(ts.ibc, order = c(0,0,2))
ma

#' 
#' ## Efetuando as previsões com o modelo MA(q)
#' 
## ----previsoes_ma_q-------------------------------------------------------------------------------------------------
fit <- forecast(ma, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.ibc, series = "Série_Observada") +
    autolayer(fit$fitted, series = "Série Predita_MA") +
    autolayer(fit, series = "Projeção h = 12") +
    xlab('Data')

#' 
#' #$ ARMA(p,q)
#' 
## ----arma_p_q-------------------------------------------------------------------------------------------------------
arma <- arima(ts.ibc, order = c(2,0,2))
arma

#' 
#' ## Efetuando as previsões com o modelo ARMA(p,q)
#' 
## ----previsoes_arma_p_q---------------------------------------------------------------------------------------------
fit <- forecast(arma, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.ibc, series = "Série_Observada") +
  autolayer(fit$fitted, series = "Série Predita_ARMA") +
  autolayer(fit, series = "Projeção h = 12") +
  xlab('Data')  

#' 
#' ## Usando o Auto.Arima
#' 
## ----auto_arima-----------------------------------------------------------------------------------------------------
auto.arima <- auto.arima(ts.ibc, seasonal = TRUE,
                         stepwise = FALSE, 
                         approximation = FALSE)
auto.arima

fit <- forecast(auto.arima, h = 12) # SE QUISER FIXAR IC level = 95)

autoplot(ts.ibc, series = "Série_Observada") +
  autolayer(fit$fitted, series = "Série Predita_AUTO.ARIMA") +
  autolayer(fit, series = "Projeção h = 12") +
  xlab('Data')  

round(accuracy(ar),  4)
round(accuracy(ma),  4)
round(accuracy(arma), 4)
round(accuracy(auto.arima), 4)

#' 
#' ## Checando a significância estatística dos parâmetros
#' 
## ----testa_param----------------------------------------------------------------------------------------------------
t_test(model = auto.arima)

#' 
#' Os parâmetros são significativos.
#' 
#' ## Diagnóstico
#' 
#' ### Teste de ausência de autocorrelação serial
#' 
#' Teste Ljung-Box
#' 
#' * $H_0$: Os resíduos são independentes e identicamente distribuídos (inexiste aurocorrelação), ou seja, não há dependência serial para todas as defasagens.  
#' * $H_1$: não $H_0$
#' 
#' #### O modelo SARIMA(3,1,0)(0,1,1)[12] 
#' 
#' IMPORTANTE: 
#' 
#' $$fitdf = p + q + P  +Q = 3 + 0 + 0 + 1 = 4$$
## ----box_teste------------------------------------------------------------------------------------------------------
Box.test(x = auto.arima$residuals, 
         lag = 24, 
         type = "Ljung-Box", 
         fitdf = 4)

#' 
#' Não podemos rejeitar $H_0$. A série é *iid*.
#' 
#' Resíduos
#' 
## ----residuos-------------------------------------------------------------------------------------------------------
checkresiduals(auto.arima, col ="blue")

#' 
#' ##### Checando a Heterocedasticidade
#' 
#' Teste proposto por Engle (1982) - GARCH
#' 
#' * $H_0$: Os resíduos ao quadrado são uma sequência de ruídos brancos, ou seja, os resíduos são homocedásticos.
#' 
#' * $H_1$: A série é heterocedástica
#' 
## ----carrega_fints--------------------------------------------------------------------------------------------------
library(FinTS)
ArchTest(auto.arima$residuals, lags = 36)

#' 
#' Não rejeitamos a hipótese nula.
#' 
#' ### NORMALIDADE
#' 
#' * $H_0$: $\sim N (\mu, \sigma^2)$
#' * $H_1$: não $H_0$
#' 
## ----normalidade----------------------------------------------------------------------------------------------------
library(normtest)

jb.norm.test(auto.arima$residuals, nrepl = 2000)

shapiro.test(auto.arima$residuals)

hist(auto.arima$residuals,
     main = 'Histograma dos resíduos',
     xlab = 'resíduos',
     ylab = 'Frequências')

round(summary(auto.arima$residuals), digits = 4)

#' 
#' ### Calculando a moda
#' 
## ----moda-----------------------------------------------------------------------------------------------------------
moda <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result <- moda(as.numeric(auto.arima$residuals))
print(result)

mean(auto.arima$residuals, trim = 0.05)

#' 
#' Estamos violando esse diagóstico, porém se efetuarmos o arrendondamento, 
#' $$média \approx mediana \approx moda \approx zero$$
#' 
