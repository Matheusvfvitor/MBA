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
# ADABOOST PARA REGRESSAO
#--------------------------------------------------------------------------------#

# Não há a implementação do algoritmo adaboost para regressao no R :-(
# adaboost.r2 -> algoritmo

#--------------------------------------------------------------------------------#
# ADABOOST PARA CLASSIFICACAO
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 0) Lendo a base de dados

# Selecionando o working directory
setwd('D:/AULAS FGV/2. ANÁLISE PREDITIVA/0. Desenvolvimento/DADOS')

# Lendo os arquivos no formato .csv (comma separated values)
DATA       <- read.csv("dataset_clas_proc.csv",sep = ',',
                       dec = '.',
                       stringsAsFactors = T)

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
# 2) Treino do algoritmo de adaboost para classificacao
library(adabag)

MDL_FIT <- boosting(CHURN ~ ., 
                    data      = TRAIN_SET, 
                    mfinal    = 500,
                    coeflearn = "Breiman", 
                    control   = rpart.control(maxdepth = 1))
??boosting

# saida do modelo
MDL_FIT

# avaliando a evolucao do erro conforme o número de iteracoes aumenta
plot(errorevol(MDL_FIT, TRAIN_SET))

# o erro estabiliza a partir de ~50 iteracoes
MDL_FIT <- boosting(CHURN ~ ., 
                    data      = TRAIN_SET, 
                    mfinal    = 50,
                    coeflearn = "Breiman", 
                    control   = rpart.control(maxdepth = 1))

#--------------------------------------------------------------------------------#
# 3) Realizando as predicoes

# Probabilidade de classificaçao de CHURN

Y_PROB_TRAIN <- predict.boosting(MDL_FIT, TRAIN_SET)$prob[,2]
Y_PROB_TEST  <- predict.boosting(MDL_FIT, TEST_SET)$prob[,2]

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

# nativo do boosting
importanceplot(MDL_FINAL)

# outra forma
MDL_FINAL$importance[order(MDL_FINAL$importance,decreasing = T)]

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