a=c(2,3,8)
b=c(6,9,1)
c=c(5,5,5)

#gerar uma matriz com essas tres linhas
A=rbind(a,b,c);A
B=cbind(a,b);B

#produto de duas matrizes
# A(3,3)  e B(3,2)
A %*% B

B %*% A  #problema: B(3,2) A(3,3)

x=c(4,9,5)
x
dim(as.matrix(x))
A%*%x #   A(3,3)  x(3,1)

x=c(1,5);y=c(3,8)
C=cbind(x,y);C
determinante=1*8-5*3; determinante
det(C) #determinante de C  (matriz quadrada)

det(A)
A
t(A) #transposta de A

#inverso de 5 = 1/5   5/ * 1/5 =1

#matriz inversa
Ai=solve(A)
Ai
round(A%*%Ai,2)  #matriz identidade I

a=c(1,4,3)
b=c(3,8,4)
c=c(7,20,11) #reparem que vetor c= vetor a + 2* vetor b

D=rbind(a,b,c);D
det(D)
Di=solve(D)

#############################################################

A=CEREALSed[,6:10]
names(A)
class(A)
dim(A)
#correlação entre essas vars 
R=cor(A);R # em geral utiliza-se R para denotar a matriz de correlação

round(R,3)
dim(R)
 
det(R)
round(solve(R),3) #inversa de R
round(solve(R)%*%R,3) #inversa de R  vezes a matriz R --> identidade

L=eigen(R);L

prod1=R%*%L$vectors[,1]
prod1 
0.186*2.167

prod2=R%*%L$vectors[,2]
prod2

A
pc=prcomp(A,scale. = T)
pc
L

L$values #autovalores
sqrt(L$values) #raiz quadrada dos aouto valores

summary(pc)
plot(pc);grid()
