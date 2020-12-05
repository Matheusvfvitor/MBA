#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# ANALISE PREDITIVA
# AULA 5 - ENSEMBLE METHODS (Random Forest)
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

library(caret)
library(Metrics)

#--------------------------------------------------------------------------------#
# RANDOM FOREST PARA REGRESSAO
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
summary(TRAIN_SET$AMOUNT); summary(TEST_SET$AMOUNT)

#--------------------------------------------------------------------------------#
# 2) Treino do algoritmo do random forest para regresssao
library(randomForest)
 
MDL_FIT <- randomForest(AMOUNT ~ .,
                        data = TRAIN_SET,
                        importance = T,
                        mtry       = 3,
                        nodesize   = 50, 
                        ntree      = 1000)
?randomForest

# saida do modelo
MDL_FIT
plot(MDL_FIT, main = 'Out-of-bag error')

# acessando cada arvore individualmente
getTree(MDL_FIT, k = 500, labelVar=TRUE)

# a partir de 400 arvores o erro estabiliza e nao ha mais melhoria
MDL_FIT <- randomForest(AMOUNT ~ .,
                        data = TRAIN_SET,
                        importance = T,
                        mtry       = 3,
                        nodesize   = 50, 
                        ntree      = 400)

#--------------------------------------------------------------------------------#
# 3) Realizando as predicoes

# Valor de AMOUNT pelo random forest 
Y_VAL_TRAIN <- predict(MDL_FIT) 
Y_VAL_TEST  <- predict(MDL_FIT, newdata = TEST_SET)

#--------------------------------------------------------------------------------#
# 4) Avaliando a performance dos modelos e existencia de overfitting

# Rando forest para regressão
postResample(pred = Y_VAL_TRAIN, obs = TRAIN_SET$AMOUNT)
postResample(pred = Y_VAL_TEST,  obs = TEST_SET$AMOUNT)

# sinais de overfitting entre as amostras de treino e teste? 
MDL_FINAL <- MDL_FIT

#--------------------------------------------------------------------------------#
# 5) Importancia das variaveis (Modelo final)
varImp(MDL_FINAL)

# o pacote random forest também possui uma forma de ver a importancia das 
# variaveis
varImpPlot(MDL_FINAL, sort= T, main = 'Importancia das Variaveis')

#--------------------------------------------------------------------------------#
# 6) Inspecao dos valores previstos vs observados (modelo final)

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

plot(RESULT_TRAIN$AMOUNT_OBS,RESULT_TRAIN$AMOUNT_PRED,
     #main = 'Amostra de Treino', cex.main = 1.2, 
     xlab = 'AMOUNT US$ (observado)', ylab = 'AMOUNT US$ (previsto)',
     cex.axis = 1.2, pch = 19, cex = 0.5, ylim = c(0,6000),
     col = 'darkorange')
abline(lm(AMOUNT_PRED ~ AMOUNT_OBS, data = RESULT_TRAIN), 
       col = 'firebrick', lwd = 3)
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")

plot(RESULT_TEST$AMOUNT_OBS, RESULT_TEST$AMOUNT_PRED, 
     #main = 'Amostra de Teste', cex.main = 1.2, 
     xlab = 'AMOUNT US$ (observado)', ylab = 'AMOUNT US$ (previsto)',
     cex.axis = 1.2, pch = 19, cex = 0.5, ylim = c(0,6000),
     col = 'darkorange')
abline(lm(AMOUNT_PRED ~ AMOUNT_OBS, data = RESULT_TEST), 
       col = 'firebrick', lwd = 3)
abline(0, 1, col = 'blue', lwd = 3, lty = "dashed")

graphics.off()
rm(list = ls())

# e se modificassemos os parametros de variaveis ou numero de observacoes nos
# nos das folhas?

#--------------------------------------------------------------------------------#
# RANDOM FOREST CLASSIFICACAO
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 0) Lendo a base de dados

# Selecionando o working directory
setwd('D:/AULAS FGV/2. ANÁLISE PREDITIVA/0. Desenvolvimento/DADOS')

# Lendo os arquivos no formato .csv (comma separated values)
DATA       <- read.csv("dataset_clas_proc.csv",sep = ',',
                       dec = '.',
                       stringsAsFactors = T)
#DATA$CHURN <- factor(DATA$CHURN, levels = c('Yes','No'))

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
# 2) Treino do algoritmo de random forest para classificacao
library(randomForest)

MDL_FIT <- randomForest(CHURN ~ .,
                        data = TRAIN_SET,
                        importance = T,
                        mtry       = 3,
                        nodesize   = 50, 
                        ntree      = 1000)
?randomForest

# saida do modelo
MDL_FIT
plot(MDL_FIT, main = 'Out-of-bag error')
legend("topright", c('Out-of-bag',"1","0"), lty=1, col=c("black","green","red"))

# acessando cada arvore individualmente
getTree(MDL_FIT, k = 500, labelVar=TRUE)

# a partir de 400 arvores o erro estabiliza e nao ha mais melhoria
MDL_FIT <- randomForest(CHURN ~ .,
                        data = TRAIN_SET,
                        importance = T,
                        mtry       = 3,
                        nodesize   = 50, 
                        ntree      = 200)

#--------------------------------------------------------------------------------#
# 3) Realizando as predicoes

# Probabilidade de classificaçao de CHURN
Y_PROB_TRAIN <- predict(MDL_FIT, type = 'prob')[,2]
Y_PROB_TEST  <- predict(MDL_FIT, newdata = TEST_SET, type = 'prob')[,2]

head(Y_PROB_TRAIN) # perceba que geraram duas colunas com probs (soma 1)

#--------------------------------------------------------------------------------#
# 4) Avaliando a performance dos modelos e existencia de overfitting

library(hmeasure) 
HMeasure(TRAIN_SET$CHURN,Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$CHURN, Y_PROB_TEST)$metrics

# sinais de overfitting entre as amostras de treino e teste? 
MDL_FINAL <- MDL_FIT

#--------------------------------------------------------------------------------#
# 5) Importancia das variaveis (Modelo final)
varImp(MDL_FINAL)

# o pacote random forest também possui uma forma de ver a importancia das 
# variaveis
varImpPlot(MDL_FINAL, sort= T, main = 'Importancia das Variaveis')

#--------------------------------------------------------------------------------#
# 6) Inspecao dos valores previstos vs observados (modelo final)

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
# 7) Curva ROC

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