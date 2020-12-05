library(fpp2)
library(forecast)
library(tidyverse)

elec
class(elec)
autoplot(elec)

elec <- window(elec, start = c(1980,1), frequency =12)

autoplot(elec) + xlab("Year")+ ylab("Gwh")

ggAcf(elec, lag = 60)

ggPacf(elec, lag = 60)
