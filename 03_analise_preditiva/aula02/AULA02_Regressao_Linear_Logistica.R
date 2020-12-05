#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# ANALISE PREDITIVA
# AULA 3 - REGRESSOES LINEAR MULTIPLA E LOGISTICA
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

library(caret)
library(Metrics)
library(dplyr)

#--------------------------------------------------------------------------------#
# REGRESSAO LINEAR MULTIPLA
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 0) Lendo a base de dados

# Selecionando o working directory
setwd('D:/AULAS FGV/2. ANÁLISE PREDITIVA/0. Desenvolvimento/DADOS')

# Lendo os arquivos no formato .csv (comma separated values)
DATA <- read.csv("dataset_reg_proc.csv", sep = ",") 

#--------------------------------------------------------------------------------#
# 1) Divisao da base de modelagem em treino e teste com amostragem

set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN <- createDataPartition(DATA$AMOUNT, p = 0.7, list = F)
TRAIN_SET <- DATA[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- DATA[-INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET$AMOUNT);summary(TEST_SET$AMOUNT)

#--------------------------------------------------------------------------------#
# 2) Treino do algoritmo de regressao linear
MDL_FIT <- lm(AMOUNT ~ ., data = TRAIN_SET)
MDL_FIT

# Determinando os coeficientes das variaveis explicativas
summary(MDL_FIT)

# Avaliando multicolinearidade via library car
library(car)
vif(MDL_FIT)
?vif
# https://www.statisticshowto.com/variance-inflation-factor/

# O calculo do vif nao encontrou correlacao, geralmente valores > 5 de VIF indi-
# cam correlacao

#--------------------------------------------------------------------------------#
# 3) Refinando o ajuste atraves do processo stepwise
library(MASS)
MDL_FIT.STEP <- stepAIC(MDL_FIT,direction = 'both', trace = TRUE)
?stepAIC
MDL_FIT.STEP

# Determinando os coeficientes das variaveis explicativas
summary(MDL_FIT.STEP)

#--------------------------------------------------------------------------------#
# 4) Realizando as predicoes

# Valor de AMOUNT pela regressao full
Y_VAL_TRAIN <- predict(MDL_FIT) 
Y_VAL_TEST  <- predict(MDL_FIT, newdata = TEST_SET)

# Valor de AMOUNT pela regressao com stepwise
Y_VAL_TRAIN.STEP <- predict(MDL_FIT.STEP) 
Y_VAL_TEST.STEP  <- predict(MDL_FIT.STEP, newdata = TEST_SET)

#--------------------------------------------------------------------------------#
# 5) Avaliando a performance dos modelos e existencia de overfitting

# Regressao full
postResample(pred = Y_VAL_TRAIN, obs = TRAIN_SET$AMOUNT)
postResample(pred = Y_VAL_TEST,  obs = TEST_SET$AMOUNT)

# Regressao com stepwise
postResample(pred = Y_VAL_TRAIN.STEP, obs = TRAIN_SET$AMOUNT)
postResample(pred = Y_VAL_TEST.STEP,  obs = TEST_SET$AMOUNT)

# Modelo final
# Os resultados sao muito parecidos, porem a regressao com stepwise resultou em
# um modelo "mais enxuto", i.e. com menos variaveis e com mesma performance
MDL_FINAL <- MDL_FIT.STEP

#--------------------------------------------------------------------------------#
# 6) Importancia das variaveis (Modelo final)
varImp(MDL_FINAL) # usem com cuidado

#anova(MDL_FINAL, test= "Chisq")

#https://cran.r-project.org/web/packages/dominanceanalysis/vignettes/da-logistic-regression.html
#--------------------------------------------------------------------------------#
# 7) Inspecao dos valores previstos vs observados (modelo final)

# Convertendo a variavel para unidade original
RESULT_TRAIN <- data.frame(AMOUNT_OBS  = TRAIN_SET$AMOUNT**2,
                           AMOUNT_PRED = Y_VAL_TRAIN.STEP**2) %>%
                mutate(RESIDUO = AMOUNT_PRED - AMOUNT_OBS)

RESULT_TEST  <- data.frame(AMOUNT_OBS  = TEST_SET$AMOUNT**2,
                           AMOUNT_PRED = Y_VAL_TEST.STEP**2) %>%
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
# REGRESSAO LOGISTICA
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 0) Lendo a base de dados

# Selecionando o working directory
setwd('D:/AULAS FGV/2. ANÁLISE PREDITIVA/0. Desenvolvimento/DADOS')

# Lendo os arquivos no formato .csv (comma separated values)
DATA       <- read.csv("dataset_clas_proc.csv",sep = ",",dec = '.', stringsAsFactors = T) 
DATA$CHURN <- factor(DATA$CHURN, levels = c('No','Yes'))
str(DATA)

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
# 2) Treino do algoritmo de regressao logistica

MDL_FIT <- glm(CHURN ~ ., data= TRAIN_SET, family= binomial(link='logit'))
MDL_FIT

# Determinando os coeficientes das vari?veis explicativas
summary(MDL_FIT)

# Avaliando multicolinearidade via library car
library(car)
vif(MDL_FIT)

# O calculo nao encontrou correlacao

#--------------------------------------------------------------------------------#
# 3) Refinando o ajuste atraves do processo stepwise
library(MASS)
MDL_FIT.STEP <- stepAIC(MDL_FIT,direction = 'both', trace = TRUE)

MDL_FIT.STEP

# Determinando os coeficientes das vari?veis explicativas
summary(MDL_FIT.STEP)

#--------------------------------------------------------------------------------#
# 4) Realizando as predicoes

# Probabilidade CHURN pela regressao full
Y_PROB_TRAIN <- predict(MDL_FIT, type = 'response') 
Y_PROB_TEST  <- predict(MDL_FIT, newdata = TEST_SET, type = 'response')

head(Y_PROB_TRAIN) # perceba que geraram duas colunas com probs (soma 1)

# Probabilidade CHURN pela regressao  stepwise
Y_PROB_TRAIN.STEP <- predict(MDL_FIT.STEP, type = 'response')  
Y_PROB_TEST.STEP  <- predict(MDL_FIT.STEP, newdata = TEST_SET, type = 'response')

# [EXTRA] Verificando a aderencia do ajuste logistico (teste Spiegelhalter)
library(rms)
val.prob(Y_PROB_TRAIN,ifelse(TRAIN_SET$CHURN == 'Yes',1,0), smooth = F)[c('S:z','S:p')]
# p valor > 5%, nao podemos rejeitar a hipotese nula

#--------------------------------------------------------------------------------#
# 5) Avaliando a performance dos modelos e existencia de overfitting

# Regressao full
library(hmeasure) 
HMeasure(TRAIN_SET$CHURN,Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$CHURN, Y_PROB_TEST)$metrics

# Regressao com stepwise
HMeasure(TRAIN_SET$CHURN,Y_PROB_TRAIN.STEP)$metrics
HMeasure(TEST_SET$CHURN, Y_PROB_TEST.STEP)$metrics

# Os resultados sao muito parecidos, porem a regressao com stepwise resultou em
# um modelo "mais enxuto", com mesma performance

# Modelo final
# Os resultados sao muito parecidos, porem a regressao com stepwise resultou em
# um modelo "mais enxuto", i.e. com menos vari?veis e com mesma performance
MDL_FINAL <- MDL_FIT.STEP

#--------------------------------------------------------------------------------#
# 6) Importancia das variaveis (Modelo final)

#https://cran.r-project.org/web/packages/dominanceanalysis/vignettes/da-logistic-regression.html
anova(MDL_FINAL, test= "Chisq")

#--------------------------------------------------------------------------------#
# 7) Inspecao dos valores previstos vs observados (modelo final)

# Geracao da matriz de confusao para diferentes pontos de corte (amostra teste)

# Label observado
Y_OBS <- TEST_SET$CHURN

# Label previsto usando: 
#       se PROB > 50% -> 1 (Yes)
#       se PROB > 30% -> 1 (Yes)
Y_CLAS1 <- factor(ifelse(Y_PROB_TEST.STEP > 0.5,1,0),
                  levels = c(0,1),
                  labels = c('No','Yes')) 
Y_CLAS2 <- factor(ifelse(Y_PROB_TEST.STEP > 0.3,1,0),
                  levels = c(0,1),
                  labels = c('No','Yes'))

confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = 'Yes')
confusionMatrix(data = Y_CLAS2, reference = Y_OBS, positive = 'Yes')

# Distribuicao do score
graphics.off()
par(mfrow = c(1,2))

# df auxiliar
AUX <- data.frame(Y_PROB = Y_PROB_TEST.STEP,
                  Y_OBS  = TEST_SET$CHURN)

boxplot(Y_PROB ~ Y_OBS, data = AUX,
        main = 'Boxplot probs', cex.main = 1.2, cex.axis = 1.2, 
        xlab = 'PROBABILITIES', ylab = 'Target',
        ylim = c(0,1), horizontal = T,
        col = c('darkorange','darkorange4'), border = 'gray20')
hist(AUX$Y_PROB, breaks = 20, xlim = c(0,1),
     main = 'Histogram probs', cex.main = 1.2, 
     xlab = 'PROBABILITIES', ylab = 'FREQUENCIA (#)', cex.axis = 1.2,  
     col = 'darkorange', border = 'brown')

graphics.off()

#--------------------------------------------------------------------------------#
# 8) Curva ROC

library(pROC)
ROC1 <- roc(TRAIN_SET$CHURN,Y_PROB_TRAIN.STEP)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(TEST_SET$CHURN,Y_PROB_TEST.STEP)
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