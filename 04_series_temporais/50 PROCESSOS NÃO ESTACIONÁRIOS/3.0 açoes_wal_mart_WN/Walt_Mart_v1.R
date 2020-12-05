#install.packages(c("fpp2", "quantmod"))

library(fpp2)
library(quantmod)

start = "2005-01-01"
end   = Sys.Date()

getSymbols("WMT", src = "yahoo", 
                      from = start, 
                          to = end,
                              auto.assign=TRUE)

# Plotando com a data

Cl(na.omit(WMT)) %>% 
          autoplot()+
            xlab("Ano")+
                ylab("Fehamento em US$")

class(WMT)

head(WMT)
tail(WMT)

((142.83/53.35)-1)*100

# Transformando em mês - usando a cotação de fechamento

cot_mensal <- to.monthly(WMT$WMT.Close,
                             indexAt = "lastof",
                             OHLC = FALSE)

head(cot_mensal)

tail(cot_mensal)

class(cot_mensal)

# embora já esteja em formato ts faremos a transformação
# para o pacote fpp2

cot.mensal <- na.omit(ts(cot_mensal))

ggAcf(cot.mensal)

ggPacf(cot.mensal)

ndiffs(cot.mensal)

#diferenciando

diff.cot.mensal <- diff(cot.mensal)

library(gridExtra)

g1 <- ggAcf(diff.cot.mensal)
g2 <- ggPacf(diff.cot.mensal)

grid.arrange(g1, g2, nrow =2)

ggtsdisplay(diff.cot.mensal)

# Efetuando o modelo AR(p)

# Arima(p, d, q) onde p é originario do modelo AR(p), d = defasagem
# q ? origin?rio do modelo MA(q)

(modelo.AR <- Arima(cot.mensal, order = c(0,1,0),seasonal = TRUE))

model <- auto.arima(cot.mensal, stepwise = FALSE,
                                   approximation = TRUE)
                                            
model

modelo.AR$fitted

forecast <- forecast(modelo.AR, h = 12)

forecast

autoplot(forecast(modelo.AR),series = "Previsão")+
        autolayer(modelo.AR$fitted, series = "Modelo", lwd =1.5)+
        autolayer(cot.mensal, series = "Cotação Real", lwd = 1.5)+
              xlab("Meses")+
              ylab("Cotação Mensal")+
              ggtitle("Wal_Mart Cotação em US$ - NYSE")

accuracy(forecast)
