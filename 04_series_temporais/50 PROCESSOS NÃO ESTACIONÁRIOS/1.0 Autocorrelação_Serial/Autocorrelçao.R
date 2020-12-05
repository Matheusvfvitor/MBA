install.packages("readxl")
install.packages("fpp")
install.packages("devtools")
devtools::install_github("robjhyndman/fpp2-package")

library(readxl)
library(fpp)
library(fpp2)


#Ler arquivo data

df <- read_excel("novo.xlsx",sheet="DATA")

head(df)

data <- df$Data
data

#transformando em ts

ts.data <- ts(data, frequency = 1)
class(ts.data)

autoplot(ts.data)

# conforme exemplo no Excel

acf <- ggAcf(ts.data)
acf
acf$data

pacf <- ggPacf(ts.data, lag.max =24)
pacf
pacf$data



