#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# ANALISE PREDITIVA
# AULA 1 - MODELAGEM E METRICAS DE PERFORMANCE
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# LEITURA DA BASES DE DADOS
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

# Selecionando o working directory
setwd('D:/AULAS FGV/2. ANÁLISE PREDITIVA/0. Desenvolvimento/DADOS')

# Lendo os arquivos no formato .csv (comma separated values)
MODEL_OUTPUT <- read.csv("model_performance.csv",sep = ";",dec = ',') 

# Contexto: 763 pacientes do sexo feminino, medicao de glucose no sangue e 
#           indicador se desenvolveu diabetes apos um periodo de tempo

# Foram construidos alguns modelos preditivos usando diversas variaveis das
# pacientes:
#     2 modelos de regressao para prever o valor da glucose no sangue
#     2 modelos de classificacao para prever se a paciente desenvolveu diabetes

# Outputs:

# PBCLAS1 e PBCLAS2: probabilidades originadas pelos classificadores 1 e 2
# VLREG1 e VLREG2: valor estimado de glucose pelas regressoes 1 e 2

#--------------------------------------------------------------------------------#
# RESULTADOS DOS MODELOS DE REGRESSAO
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 1) Comportamento dos resultados observados vs modelados
par(mfrow = c(1,2)) # parametros graficos

plot(MODEL_OUTPUT$VLREG1,MODEL_OUTPUT$GLUCOSE,
     main = 'Modelo de Regressao 1', cex.main = 1.0, 
     xlab = 'GLUCOSE (previsto)', ylab = 'GLUCOSE (observado)',
     cex.axis = 1.2, pch = 19, cex = 0.5,
     col = 'darkorange')
abline(lm(VLREG1 ~ GLUCOSE, data = MODEL_OUTPUT), col = 'firebrick', lwd = 3)
abline(0,1, col = 'blue', lwd = 3, lty=  "dashed")

plot(MODEL_OUTPUT$VLREG2,MODEL_OUTPUT$GLUCOSE,
     main = 'Modelo de Regressao 2', cex.main = 1.0, 
     xlab = 'GLUCOSE (previsto)', ylab = 'GLUCOSE (observado)',
     cex.axis = 1.2, pch = 19, cex = 0.5,
     col = 'darkorange')
abline(lm(VLREG2 ~ GLUCOSE, data = MODEL_OUTPUT), col = 'firebrick', lwd = 3)
abline(0,1, col = 'blue', lwd = 3, lty=  "dashed")

#--------------------------------------------------------------------------------#
# 2) Calculos das metricas de erro para regressao

# Via library caret
library(caret)
postResample(pred = MODEL_OUTPUT$VLREG1, obs = MODEL_OUTPUT$GLUCOSE)
postResample(pred = MODEL_OUTPUT$VLREG2, obs = MODEL_OUTPUT$GLUCOSE)

# Via library metrics
# https://www.rdocumentation.org/packages/Metrics/versions/0.1.4
library(Metrics)

# Mean Squared Error
mse(MODEL_OUTPUT$GLUCOSE, MODEL_OUTPUT$VLREG1)
mse(MODEL_OUTPUT$GLUCOSE, MODEL_OUTPUT$VLREG2)
# Root Mean Squared Error
rmse(MODEL_OUTPUT$GLUCOSE, MODEL_OUTPUT$VLREG1)
rmse(MODEL_OUTPUT$GLUCOSE, MODEL_OUTPUT$VLREG2)
# Mean Absolute Error
mae(MODEL_OUTPUT$GLUCOSE, MODEL_OUTPUT$VLREG1)
mae(MODEL_OUTPUT$GLUCOSE, MODEL_OUTPUT$VLREG2)
# Sum Squared Error
sse(MODEL_OUTPUT$GLUCOSE, MODEL_OUTPUT$VLREG1)
sse(MODEL_OUTPUT$GLUCOSE, MODEL_OUTPUT$VLREG2)

# Calculando manualmente
SSE  <- sum((MODEL_OUTPUT$VLREG2 - MODEL_OUTPUT$GLUCOSE)^2)
MSE  <- mean((MODEL_OUTPUT$VLREG2 - MODEL_OUTPUT$GLUCOSE)^2)
RMSE <- sqrt(MSE)
MAE  <- mean(abs(MODEL_OUTPUT$VLREG2 - MODEL_OUTPUT$GLUCOSE))

SST    <- sum((MODEL_OUTPUT$GLUCOSE - mean(MODEL_OUTPUT$GLUCOSE))^2)
R2     <- 1 - SSE/SST # pode ter divergencia de valor com outros pacotes
R2_ADJ <- 1 - ((1 - R2)*(nrow(MODEL_OUTPUT) - 1)/(nrow(MODEL_OUTPUT) - (1) - 1))

#--------------------------------------------------------------------------------#
# RESULTADOS DOS MODELOS DE CLASSIFICACAO
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 1) Comportamento das probabilidades dos modelos vs target
library(dplyr)

# Tratando a variável resposta para fator
MODEL_OUTPUT <- MODEL_OUTPUT %>% mutate(DIABETES = factor(DIABETES_Yes,
                                                          levels = c(0,1),
                                                          labels = c('No','Yes')))

graphics.off()
par(mfrow = c(1,2)) # parametros graficos

# Comportamento de discriminacao (analise bivariada)
boxplot(PBCLAS1 ~ DIABETES, data = MODEL_OUTPUT,
        main = 'Modelo de Classificacao 1', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(0,1), 
        col = c('darkorange','darkorange4'), border = 'gray20')

boxplot(PBCLAS2 ~ DIABETES, data = MODEL_OUTPUT,
        main = 'Modelo de Classificacao 2', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(0,1), 
        col = c('darkorange','darkorange4'), border = 'gray20')


#--------------------------------------------------------------------------------#
# 2) Geracao da matriz de confusao para diferentes pontos de corte

# Label observado
Y_OBS   <- MODEL_OUTPUT$DIABETES
head(Y_OBS)

# Label previsto usando: se PROB > 50% -> 1 (Yes)
Y_CLAS1 <- factor(ifelse(MODEL_OUTPUT$PBCLAS1 > 0.5,1,0),
                  levels = c(0,1),
                  labels = c('No','Yes')) 
Y_CLAS2 <- factor(ifelse(MODEL_OUTPUT$PBCLAS2 > 0.5,1,0),
                  levels = c(0,1),
                  labels = c('No','Yes'))

# via library gmodels
library(gmodels)
CrossTable(Y_OBS, Y_CLAS1, prop.c= F, prop.t= T, prop.chisq= F)
CrossTable(Y_OBS, Y_CLAS2, prop.c= F, prop.t= T, prop.chisq= F)

# via summary tools
library(summarytools)
view(ctable(x = Y_OBS, y = Y_CLAS1, prop = "t"))
view(ctable(x = Y_OBS, y = Y_CLAS2, prop = "t"))

# via library caret (+ métricas)
confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = 'Yes')
confusionMatrix(data = Y_CLAS2, reference = Y_OBS, positive = 'Yes')

confusionMatrix(data = Y_CLAS1, reference = Y_OBS, 
                positive = 'Yes',mode = 'prec_recall')
confusionMatrix(data = Y_CLAS2, reference = Y_OBS,
                positive = 'Yes',mode = 'prec_recall')

#--------------------------------------------------------------------------------#
# 3) Calculos das metricas de erro para classificacao

# via library Hmeasure
library(hmeasure) 
HMeasure(MODEL_OUTPUT$DIABETES_Yes,MODEL_OUTPUT$PBCLAS1)$metrics
HMeasure(MODEL_OUTPUT$DIABETES_Yes,MODEL_OUTPUT$PBCLAS2)$metrics

# via library metrics (exemplo com modelo 1)
auc(MODEL_OUTPUT$DIABETES_Yes,MODEL_OUTPUT$PBCLAS1)
accuracy(MODEL_OUTPUT$DIABETES_Yes,ifelse(MODEL_OUTPUT$PBCLAS1 > 0.5,1,0))
precision(MODEL_OUTPUT$DIABETES_Yes,ifelse(MODEL_OUTPUT$PBCLAS1 > 0.5,1,0))
recall(MODEL_OUTPUT$DIABETES_Yes,ifelse(MODEL_OUTPUT$PBCLAS1 > 0.5,1,0))


#--------------------------------------------------------------------------------#
# 4) Curva ROC
graphics.off()

library(pROC)
ROC1 <- roc(MODEL_OUTPUT$DIABETES,MODEL_OUTPUT$PBCLAS1)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(MODEL_OUTPUT$DIABETES_Yes,MODEL_OUTPUT$PBCLAS2)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5,
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('MOD 1','MOD 2'), lty = 1, col = c('tomato3','cyan3'))

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# FIM
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#