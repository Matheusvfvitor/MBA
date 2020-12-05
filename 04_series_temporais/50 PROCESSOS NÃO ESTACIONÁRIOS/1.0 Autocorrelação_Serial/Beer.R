#install.packages("fpp2")
#install.packages("forecast")
#install.packages("tidyverse")


library(fpp2)
library(forecast)
library(tidyverse)



data(ausbeer)

class(ausbeer)

autoplot(ausbeer)

beer2 <- window(ausbeer,frequency = 4, start = c(1992,1), end = c(2010, 2))
beer2

autoplot(beer2)

gglagplot(beer2)

# Há uma forte relação positiva entre os lags 4 e 8
# refletindo a forte sazonalidade nos dados.

# Há também uma forte relação negativa entre os lags 2 e 6.
# A relação negativa ocorre pois o pico está no 4 T.
# Vamos ao boxplot

seasonplot(beer2, 4, col=rainbow(12), year.labels=TRUE, main="Cerveja")

boxplot(beer2 ~ cycle(beer2),col="orange",xlab="Mês", 
        ylab="Taxa %")
            title("Boxplot cerveja")

ac <- ggAcf(beer2)
ac$data
ac

pacf <- ggPacf(beer2)
pacf$data
pacf
