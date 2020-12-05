#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# ANALISE PREDITIVA
# AULA 4 - ARVORES DE DECISAO
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

library(caret)
library(Metrics)
library(dplyr)

#--------------------------------------------------------------------------------#
# ARVORE DE REGRESSAO
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 0) Lendo a base de dados

# Selecionando o working directory
setwd('D:/AULAS FGV/2. AN�LISE PREDITIVA/0. Desenvolvimento/DADOS')

# Lendo os arquivos no formato .csv (comma separated values)
DATA <- read.csv("dataset_reg_proc.csv", sep = ",") 
str(DATA)

#--------------------------------------------------------------------------------#
# 1) Divisao da base de modelagem em treino e teste com amostragem

set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN <- createDataPartition(DATA$AMOUNT, p = 0.7, list = F)
TRAIN_SET <- DATA[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- DATA[-INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET$AMOUNT); summary(TEST_SET$AMOUNT)

#--------------------------------------------------------------------------------#
# 2) Treino do algoritmo de arvore de regresssao
library(rpart)

# aqui comecamos a arvore o mais completa possivel
MDL_FIT <- rpart(AMOUNT ~.,
                 data = TRAIN_SET,
                 method = 'anova',
                 control = rpart.control(minbucket = 10, cp = -1))
??rpart.control

# saida da arvore
MDL_FIT
summary(MDL_FIT)

# avaliando a necessidade da poda da arvore
printcp(MDL_FIT)
plotcp(MDL_FIT)

# aqui conseguimos podar a árvore controlando o cp que reduz o valor minimo do 
# erro, que é um parametro de controle
PARM_CTRL <- MDL_FIT$cptable[which.min(MDL_FIT$cptable[,"xerror"]),"CP"]

MDL_FIT.PRUNE <- prune(MDL_FIT, cp = PARM_CTRL)

# saida da árvore
MDL_FIT.PRUNE
summary(MDL_FIT.PRUNE)

#--------------------------------------------------------------------------------#
# 3) Plotando a arvore
library(rattle)
fancyRpartPlot(MDL_FIT.PRUNE)

library(rpart.plot)
# https://www.rdocumentation.org/packages/rpart.plot/versions/3.0.8/topics/rpart.plot
rpart.plot(MDL_FIT.PRUNE, 
           cex = 0.5, 
           type = 3,
           box.palette = "BuRd",
           branch.lty = 3, 
           shadow.col ="gray", 
           nn = TRUE,
           main = 'Regression Trees')

rpart.plot(MDL_FIT.PRUNE,
           type = 3, 
           cex = 0.5, 
           clip.right.labs = FALSE,
           branch = .4,
           box.palette = "BuRd",       # override default GnBu palette
           main = 'Regression Trees')

#--------------------------------------------------------------------------------#
# 4) Realizando as predicoes

# Valor de AMOUNT pela arvore regressao com maior desenvolvimento
Y_VAL_TRAIN <- predict(MDL_FIT.PRUNE) 
Y_VAL_TEST  <- predict(MDL_FIT.PRUNE, newdata = TEST_SET)

#--------------------------------------------------------------------------------#
# 5) Avaliando a performance dos modelos e existencia de overfitting

# Arvore
postResample(pred = Y_VAL_TRAIN, obs = TRAIN_SET$AMOUNT)
postResample(pred = Y_VAL_TEST,  obs = TEST_SET$AMOUNT)

# sinais de overfitting entre as amostras de treino e teste? 
MDL_FINAL <- MDL_FIT.PRUNE

#--------------------------------------------------------------------------------#
# 6) Importancia das variaveis (Modelo final)
varImp(MDL_FINAL)

# o algoritmo de arvore tambem possui uma saida com a importancia das variaveis
round(MDL_FINAL$variable.importance, 3)

#--------------------------------------------------------------------------------#
# 7) Inspecao dos valores previstos vs observados (modelo final)

# Convertendo a variavel para unidade original
RESULT_TRAIN <- data.frame(AMOUNT_OBS  = TRAIN_SET$AMOUNT**2,
                           AMOUNT_PRED = Y_VAL_TRAIN**2) %>%
                mutate(RESIDUO = AMOUNT_PRED - AMOUNT_OBS)

RESULT_TEST  <- data.frame(AMOUNT_OBS  = TEST_SET$AMOUNT**2,
                           AMOUNT_PRED = Y_VAL_TEST**2) %>%
                mutate(RESIDUO = AMOUNT_PRED - AMOUNT_OBS)

# Plotando os resultados
layout(matrix(c(1,2,3,4,3,4), nrow = 3, ncol = 2, byrow = TRUE))
par(oma = c(1,1,0,0),  mar = c(5,5,2,1))

hist(RESULT_TRAIN$RESIDUO, breaks = 12, xlim = c(-2000,2000),
     main = 'Amostra de Treino', cex.main = 1.2, 
     xlab = 'RESIDUAL', ylab = 'FREQUENCIA (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

hist(RESULT_TEST$RESIDUO, breaks = 12, xlim = c(-2000,2000),
     main = 'Amostra de Teste', cex.main = 1.2, 
     xlab = 'RESIDUAL', ylab = 'FREQUENCIA (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

plot(RESULT_TRAIN$AMOUNT_PRED,RESULT_TRAIN$AMOUNT_OBS,
     #main = 'Amostra de Treino', cex.main = 1.2, 
     xlab = 'AMOUNT US$ (previsto)', ylab = 'AMOUNT US$ (observado)',
     cex.axis = 1.2, pch = 19, cex = 0.5, ylim = c(0,6000),
     col = 'darkorange')
abline(lm(AMOUNT_OBS ~ AMOUNT_PRED, data = RESULT_TRAIN), 
       col = 'firebrick', lwd = 3)
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")

plot(RESULT_TEST$AMOUNT_PRED, RESULT_TEST$AMOUNT_OBS, 
     #main = 'Amostra de Teste', cex.main = 1.2, 
     xlab = 'AMOUNT US$ (previsto)', ylab = 'AMOUNT US$ (observado)',
     cex.axis = 1.2, pch = 19, cex = 0.5, ylim = c(0,6000),
     col = 'darkorange')
abline(lm(AMOUNT_OBS ~ AMOUNT_PRED, data = RESULT_TEST), 
       col = 'firebrick', lwd = 3)
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")

graphics.off()
rm(list = ls())

#--------------------------------------------------------------------------------#
# ARVORE DE CLASSIFICACAO
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 0) Lendo a base de dados

# Selecionando o working directory
setwd('D:/AULAS FGV/2. AN?LISE PREDITIVA/0. Desenvolvimento/DADOS')

# Lendo os arquivos no formato .csv (comma separated values)
DATA <- read.csv("dataset_clas_proc.csv",sep = ",",dec = '.',stringsAsFactors = T) 

#--------------------------------------------------------------------------------#
# 1) Divisao da base de modelagem em treino e teste com amostragem

set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN <- createDataPartition(DATA$CHURN, p = 0.7, list = F)
TRAIN_SET <- DATA[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- DATA[-INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET$CHURN);summary(TEST_SET$CHURN)
prop.table(table(TRAIN_SET$CHURN));prop.table(table(TEST_SET$CHURN))

#--------------------------------------------------------------------------------#
# 2) Treino do algoritmo de arvore de regresssao

# aqui comecamos a arvore o mais completa possivel
MDL_FIT <- rpart(CHURN ~.,
                 data = TRAIN_SET,
                 method = 'class',
                 control = rpart.control(minbucket= 20, cp = -1))

# saida da arvore
MDL_FIT
summary(MDL_FIT)

# avaliando a necessidade da poda da arvore
printcp(MDL_FIT)
plotcp(MDL_FIT)

# aqui conseguimos podar a arvore controlando o cp que reduz o valor minimo do 
# erro, que e um parametro de controle
PARM_CTRL <- MDL_FIT$cptable[which.min(MDL_FIT$cptable[,"xerror"]),"CP"]

MDL_FIT.PRUNE <- prune(MDL_FIT, cp = PARM_CTRL)

# saida da arvore
MDL_FIT.PRUNE
summary(MDL_FIT.PRUNE)

#--------------------------------------------------------------------------------#
# 3) Plotando a arvore
library(rattle)
fancyRpartPlot(MDL_FIT.PRUNE)

# https://www.rdocumentation.org/packages/rpart.plot/versions/3.0.8/topics/rpart.plot
rpart.plot(MDL_FIT.PRUNE, 
           cex = 1, 
           type = 3,
           box.palette = "BuRd",
           branch.lty = 3, 
           shadow.col ="gray", 
           nn = TRUE,
           main = 'Classification Trees')

rpart.plot(MDL_FIT.PRUNE,
           type = 3, 
           cex = 1, 
           clip.right.labs = FALSE,
           branch = .4,
           box.palette = "BuRd",       # override default GnBu palette
           main = 'Classification Trees')

#--------------------------------------------------------------------------------#
# 4) Realizando as predicoes

# Probabilidade de classificacao de CHURN
Y_PROB_TRAIN <- predict(MDL_FIT.PRUNE, type = 'prob')[,2]
Y_PROB_TEST  <- predict(MDL_FIT.PRUNE, newdata = TEST_SET, type = 'prob')[,2]

head(Y_PROB_TRAIN) 

#--------------------------------------------------------------------------------#
# 5) Avaliando a performance dos modelos e existencia de overfitting

library(hmeasure) 
HMeasure(TRAIN_SET$CHURN,Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$CHURN, Y_PROB_TEST)$metrics

# sinais de overfitting entre as amostras de treino e teste? 
MDL_FINAL <- MDL_FIT.PRUNE

#--------------------------------------------------------------------------------#
# 6) Importancia das variaveis (Modelo final)
varImp(MDL_FINAL)

# o algoritmo de arvore tambem possui uma saida com a importancia das variaveis
round(MDL_FINAL$variable.importance, 3)

#--------------------------------------------------------------------------------#
# 7) Inspecao dos valores previstos vs observados (modelo final)

# Geracao da matriz de confusao para diferentes pontos de corte (amostra teste)

# Label observado
Y_OBS <- TEST_SET$CHURN
levels(Y_OBS)

# Label previsto usando: 
#       se PROB > 50% -> 1 (Yes)
#       se PROB > 30% -> 1 (Yes)
Y_CLAS1 <- factor(ifelse(Y_PROB_TEST > 0.5,1,0),
                  levels = c(0,1),
                  labels = c('No','Yes')) 
Y_CLAS2 <- factor(ifelse(Y_PROB_TEST > 0.3,1,0),
                  levels = c(1,0),
                  labels = c('No','Yes'))

confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = 'Yes')
confusionMatrix(data = Y_CLAS2, reference = Y_OBS, positive = 'Yes')

# Distribuicao do score
graphics.off()
par(mfrow = c(1,2))

# df auxiliar
AUX <- data.frame(Y_PROB = Y_PROB_TEST,
                  Y_OBS  = TEST_SET$CHURN)

boxplot(Y_PROB ~ Y_OBS, data = AUX,
        main = 'Boxplot probs', cex.main = 1.2, cex.axis = 1.2, 
        xlab = 'PROBABILITIES', ylab = 'TARGET',
        ylim = c(0,1), horizontal = T,
        col = c('darkorange','darkorange4'), border = 'gray20')
hist(AUX$Y_PROB, breaks = 12, xlim = c(0,1),
     main = 'Histogram probs', cex.main = 1.2, 
     xlab = 'PROBABILITIES', ylab = 'FREQUENCIA (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

graphics.off()

#--------------------------------------------------------------------------------#
# 8) Curva ROC

library(pROC)
ROC1 <- roc(TRAIN_SET$CHURN,Y_PROB_TRAIN)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(TEST_SET$CHURN,Y_PROB_TEST)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5,
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('TRAIN SET','TEST SET'), lty = 1, col = c('tomato3','cyan3'))

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# FIM
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#