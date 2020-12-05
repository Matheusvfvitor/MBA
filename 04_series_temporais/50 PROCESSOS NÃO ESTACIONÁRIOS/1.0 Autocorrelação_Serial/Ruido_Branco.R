set.sed(1234)

y <- ts(rnorm(500))

autoplot(y)+ggtitle("Ruído Branco")

lim <- 1.96/ sqrt(length(y))
lim

ggAcf(y, lag =500, col = "darkred", lwd = 1.2)
ggPacf(y, lag = 500,col = "darkred", lwd = 1.2)
