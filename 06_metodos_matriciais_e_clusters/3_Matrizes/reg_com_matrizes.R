sim=SIMULA80_reg
reg.lm=lm(data = sim, y~.)
reg.lm

simx=model.matrix(data=sim,~.)
head(simx)
simx=as.data.frame(simx)
X=simx[,1:5]; X=as.matrix(X)
Y=simx[6] # forma mais conveniente; experimentem simx [,6]
Y=as.matrix(Y)
# dos betas
betas=solve(t(X)%*%X)%*%t(X)%*%Y
betas

#gere uma nova coluna x5=x1+x2 e repita. 
X=as.data.frame(X)
X$x5=X$x1+X$x2
#veja o que acontece com multicolinearidade
betas=solve(t(X)%*%X)%*%t(X)%*%Y
betas