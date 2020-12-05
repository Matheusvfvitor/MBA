#install.packages("tidyverse")
#install.packages("quantmod")
#install.packages("fpp2")

library(tidyverse)
library(quantmod)
library(fpp2)

start <- as.Date("2015-01-02")
end <- as.Date("2020-09-10")  

getSymbols('DCOILWTICO', src = 'FRED', from = start, to = end)

DCOILWTICO_clean <- na.omit(DCOILWTICO["2015-01-02/2020-09-10"]) 

oil <- DCOILWTICO_clean

head(oil, 3)

tail(oil, 3)

summary(oil)

# Calculando o retorno instantÃ¢neo

oil.ret <- diff(log(oil))
head(oil.ret)

# omitindo a primeira observação

oil.ret <- na.omit(oil.ret)
head(oil.ret,3)

# Altenativamente
#oil.ret <- dailyReturn((DCOILWTICO_clean), type='log')
#head(oil.ret)

par(mfrow=c(2,2))

plot(oil)
acf(oil)

plot(oil.ret, col="red")
acf(oil.ret, col = "red")

par(mfrow=c(1,1))

#A autocorrelaÃ§Ã£o serial

acf(oil, main="ACF Petroleo")

acf(oil.ret)
lim <- 1.96/sqrt(length(oil.ret))
lim

#Calculando mÃ©dia e desvio padrÃ£o dos retornos

m=mean(oil.ret);s=sd(oil.ret);

#Histograma do retorno e DistribuiÃ§Ã£o Empirica do retoro

par(mfrow=c(1,2))

hist(oil.ret, nclass=40, freq=FALSE, main='Histogram');curve(dnorm(x,
                                                               mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
plot(density(oil.ret), main='Empirical Distribution');curve(dnorm(x,
                                                              mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
par(mfrow=c(1,1))

mu <-  m # mean of log returns
sigma <- s # sd of log returns 

median(oil)

p0 = median(oil) #MEDIANA

# numero de simulaÃ§Ãµes 

N <- 252 #(calendario financeiro = 21 dias uteis x 12 meses)
r <- rnorm(N, mu, sigma)
log_price <- log(p0)+cumsum(r)
prices <- exp(log_price)
plot(prices, type="l", col="blue", main="SMC")

####################################################################

#V?rios passeios aleat?rios

start <- p0
m <- mu
sigma <- sigma
n.passos <- 252  #252 dias uteis 
n.random <- 1000    # numero de passeios aleat?rios

# Criando uma matriz vazia
WALK <- matrix(NA, nrow = n.random, ncol = n.passos)

# Come?ando o passeio
WALK[, 1] <- start

# Passeios 
for(i in 2:n.passos){
  WALK[, i] <- exp(log(WALK[, i - 1]) + (rnorm(n.random, m, sigma)))  
  print(i)
}


plot(NA, ylim = c(min(WALK), max(WALK)), xlim = c(0, n.passos), cex.main = 1.0, cex.lab = 1.0,
     xlab = "Tempo", ylab = "Barril WTI ($)", las = 1, font.lab = 2, main = "Random Walk")

for(i in 1:nrow(WALK)){
  lines(WALK[i,], col = rainbow(n.random)[i])
}
par(mfrow=c(1,2))

hist(WALK, nclass=40, freq=FALSE, main='Random Walk Histogram');curve(dnorm(x,
                                                                            mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
plot(density(WALK), main='Random Walk Empirical Distribution');curve(dnorm(x,
                                                                           mean=m,sd=s), from = -0.3, to = 0.2, add=TRUE, col="red")
par(mfrow=c(1,1))

mean(WALK)
median(WALK)
sd(WALK)
min(WALK)
max(WALK)

#install.packages("tigerstats")
library(tigerstats)

pnormGC(40, region="below", mean=mean(WALK),
        sd= sd(WALK) ,graph=TRUE)

qnormGC(0.20, region="below", mean=mean(WALK),
        sd= sd(WALK) ,graph=TRUE)

qnormGC(0.5,region="between",mean=mean(WALK),
        sd= sd(WALK), graph=TRUE)

