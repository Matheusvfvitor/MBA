# install.packages(c("fpp","fpp2", "forecast", "readxl"))

library(fpp)
library(fpp2)
library(forecast)
library(readxl)

df <- read_excel("Fertilizantes.xlsx")

head(df)
tail(df)
class(df$consumo)

## 2. Transformando df em classe ts

fertilizantes.ts <- ts(df$consumo , frequency = 12, 
                        start = c(1998,1), end = c(2019,9))

fertilizantes.ts

autoplot(fertilizantes.ts)+
  xlab("meses")+
  ylab("Demanda Mensal em Mil Toneladas")+
  ggtitle("Fertilizantes Entregues ao Mercado em Milhares de Toneladas")
