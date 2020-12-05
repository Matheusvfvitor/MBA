#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# ANALISE PREDITIVA
# AULA 1 - CONCEITOS E DADOS
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

library(summarytools)
library(dplyr)

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# FEATURE ENGINEERING - Transformaces nas variaveis
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

# Selecionando o working directory
setwd('D:/AULAS FGV/2. ANÁLISE PREDITIVA/0. Desenvolvimento/DADOS')

# Lendo os arquivos no formato .csv (comma separated values)
DATA_REG  <- read.csv("dataset_reg.csv", sep = ",", stringsAsFactors = T) 
DATA_CLAS <- read.csv("dataset_clas.csv",sep = ",",dec = '.', stringsAsFactors = T) 

library(caret)

#--------------------------------------------------------------------------------#
# BASE DE DADOS PARA REGRESSAO - Direct Marketing
#--------------------------------------------------------------------------------#

# Variaveis explicativas
X_REG <- DATA_REG[,1:7]
# Variavel resposta
Y_REG <- DATA_REG[,8, drop = F] # impedir que torne-se um vetor: drop = F

#--------------------------------------------------------------------------------#
# 1) Criacao de dummy variables para variaveis categoricas

# Criando o objeto que constroi as dummies das variaveis categoricas
DUMMY_MODEL <- dummyVars(' ~ .', data = X_REG, sep = '_', fullRank = T)
# Formato: NOMECOLUNA_categoria
# Quantidade: apenas n - 1 dummies, onde n e o numero de categorias
# Obs: o algortimo deleta a primeira na ordem alfabetica)

# Aplicando o objeto na base com as variaveis explicativas (aqui precisa
# converter novamente em data.frame)
X_REG <- as.data.frame(predict(DUMMY_MODEL, newdata = X_REG))

#--------------------------------------------------------------------------------#
# 2) Correcao de assimetria na variavel resposta 

# Relevante para regressao linear

# Impacto de diferentes transforma?oes
par(oma = c(2,3,0,0), mar = c(3,3,2,1), mfrow = c(2,2)) # parametros graficos

hist((DATA_REG$AMOUNT), breaks = 10,
     main = 'Distribuicao original', cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,300), col = 'darkorange', border = 'darkorange4') 
hist(log10(DATA_REG$AMOUNT), breaks = 10,
     main = expression(log(AMOUNT)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,250), col = 'darkorange', border = 'darkorange4') 
hist(log(DATA_REG$AMOUNT), breaks = 10,
     main = expression(ln(AMOUNT)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,250), col = 'darkorange', border = 'darkorange4')
hist(sqrt(DATA_REG$AMOUNT), breaks = 10,
     main = expression(sqrt(AMOUNT)), cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,300), col = 'darkorange', border = 'darkorange4') 
mtext(text="CORRECAO DA ASSIMETRIA DA VARIAVEL RESPOSTA",
      side = 1, line = 0, outer = TRUE)
mtext(text="FREQUENCIA (#)", side = 2, line = 0, outer = TRUE)

# Aplicando a raiz quadrada
summary(Y_REG)            # quantidade original
summary(sqrt(Y_REG))      # quantidade transformada
summary(sqrt(Y_REG)**2)   # voltando para o original

Y_REG <- Y_REG %>% mutate(AMOUNT = sqrt(AMOUNT))

#--------------------------------------------------------------------------------#
# 3) [EXTRA] Padronizacao das variaveis

# Necessario para diversos algortimos (PCA, clusterizacao, SVM, Redes neurais)
# Criando o objeto com os valores de padronizacao das variaveis
PREPROC_VALUES <- preProcess(X_REG, method = c('center', 'scale'))

# Aplicando o objeto na base com as variaveis explicativas
X_REG_STANDRD <- predict(PREPROC_VALUES, X_REG)

# Para nosso curso nao sera necessario fazer padronizao

#--------------------------------------------------------------------------------#
# 4) BASE FINAL 

# Conectando as variaveis explicativas e a variavel resposta processadas
# DATA_REG_PREPROC <- cbind(X_REG,Y_REG)
DATA_REG_PREPOC <- bind_cols(X_REG,Y_REG)

#--------------------------------------------------------------------------------#
# BASE DE DADOS PARA CLASSIFICACAO - Telco Churning
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 1) Criacao de variavel explicativa a partir de outras variaves

# As variaveis INCOME e EXPENSES podem ser combinadas de forma a informar o 
# endividamento do cliente:
#           Debt to Income (Endividamento) = Expenses/Income
# DATA_CLAS$DEBTINC <- DATA_CLAS$EXPENSES/DATA_CLAS$INCOME
DATA_CLAS <- DATA_CLAS %>% mutate(DEBTINC = EXPENSES/INCOME)

summary(DATA_CLAS$DEBTINC)

# Analisando o comportamento das variaveis
layout(matrix(c(1,2,3,4,5,5,6,6), nrow = 2, ncol = 4, byrow = TRUE))
par(oma = c(2,3,0,0), mar = c(3,3,2,1))

hist(DATA_CLAS$INCOME, breaks = 12,
     main = 'INCOME', cex.main = 1.0, 
     cex.axis= 1.2,  
     ylim = c(0,1500), col = 'dodgerblue', border = 'dodgerblue4') 

boxplot(INCOME ~ CHURN, data = DATA_CLAS,
        main = 'INCOME vs CHURN', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(10,45000), col = c('dodgerblue','dodgerblue4'), border = 'gray20')

hist(DATA_CLAS$EXPENSES, breaks = 12,
     main = 'EXPENSES', cex.main = 1.0, 
     cex.axis= 1.2,  
     ylim = c(0,1000), col = 'dodgerblue', border = 'dodgerblue4') 

boxplot(EXPENSES ~ CHURN, data = DATA_CLAS,
        main = 'EXPENSES vs CHURN', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(10,6000), col = c('dodgerblue','dodgerblue4'), border = 'gray20')

hist(DATA_CLAS$DEBTINC, breaks = 12,
     main = 'DEBTINC', cex.main = 1.0, 
     cex.axis= 1.2,  
     ylim = c(0,800), col = 'mediumorchid1', border = 'mediumorchid4') 

boxplot(DEBTINC ~ CHURN, data = DATA_CLAS,
        main = 'DEBTINC vs CHURN', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(0,0.5), col = c('mediumorchid1','mediumorchid4'), border = 'gray20')

# Variaveis explicativas
# X_CLAS <- DATA_CLAS[,c(1:8,10)]
X_CLAS <- DATA_CLAS %>% select(-CHURN)

# Variavel resposta
# Y_CLAS <- DATA_CLAS[,9, drop = F] # impedir que torne-se um vetor: drop = F
Y_CLAS <- DATA_CLAS %>% select(CHURN)

#--------------------------------------------------------------------------------#
# 2) Criacao de dummy variables para variaveis categoricas

# Criando o objeto que constroi as dummies das variaveis categoricas
DUMMY_MODEL <- dummyVars(' ~ .', data = X_CLAS, sep = '_', fullRank = T)
# Formato: NOMECOLUNA_categoria
# Quantidade: apenas n - 1 dummies, onde n e o numero de categorias
# Obs: o algortimo deleta a primeira na ordem alfabetica)

# Aplicando o objeto na base com as variaveis explicativas (aqui precisa
# converter novamente em data.frame)
X_CLAS <- as.data.frame(predict(DUMMY_MODEL, newdata = X_CLAS))

# Removendo variavel redundante
graphics.off()
plot(log(X_CLAS$INCOME),log(X_CLAS$DEBTINC))
plot(log(X_CLAS$EXPENSES),log(X_CLAS$DEBTINC))
 
X_CLAS <- X_CLAS %>% select(-EXPENSES)

#--------------------------------------------------------------------------------#
# 3) [EXTRA] Padronizacao das variaveis

# Necessario para diversos algortimos (PCA, clusterizacao, SVM, Redes neurais)
# Criando o objeto com os valores de padronizacao das variaveis
PREPROC_VALUES <- preProcess(X_CLAS, method = c('center', 'scale'))

# Aplicando o objeto na base com as variaveis explicativas
X_CLAS_STANDRD <- predict(PREPROC_VALUES, X_CLAS)

# Para nosso curso nao sera necessario fazer padronizacao

#--------------------------------------------------------------------------------#
# 4) TRANSFORMANDO A VARIAVEL RESPOSTA

# Para classificacao muitos pacotes conseguem lidar com factor como variavel
# resposta

# Aqui precisamos saber: que evento queremos modelar?
#       Yes: aconteceu o evento de churn, o cliente cancelou o contrato
#     OU No: nao aconteceu o evento de churn, o cliente permaneceu como cliente

# Queremos medir a probabilidade do cliente cancelar o contrato e deixar de ser
# cliente da empresa ou a probabilidade do cliente estar fidelizado com ela?
# Aqui seguiremos olhando para o evento, e com isso nao e necessario alterar os 
# niveis do fator, mas caso precisasse inverter no momento antes da modelagem
# poderiamos usar:
levels(Y_CLAS$CHURN)

#--------------------------------------------------------------------------------#
# 5) BASE FINAL 

# Conectando as variaveis explicativas e a variavel resposta processadas
# DATA_CLAS_PREPROC <- cbind(X_CLAS,Y_CLAS)
DATA_CLAS_PREPROC <- bind_cols(X_CLAS,Y_CLAS)

#--------------------------------------------------------------------------------#
# SALVANDO AS BASES DE DADOS TRATADAS
#--------------------------------------------------------------------------------#
write.csv(DATA_REG_PREPOC, 'dataset_reg_proc.csv', row.names = FALSE)
write.csv(DATA_CLAS_PREPROC,'dataset_clas_proc.csv',row.names = FALSE)

rm(list = ls()) # Removendo todos os arquivos da memoria RAM

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# FIM
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#