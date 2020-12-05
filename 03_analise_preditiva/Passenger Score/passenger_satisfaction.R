library(lattice)
library(rpart)
library(ggplot2)
library(lubridate)
library(summarytools)
library(dplyr)
library(caret)
library(Metrics)
library(gmodels)
library(hmeasure)
library(pROC)
library(readr)
library(readxl)
library(dplyr)
library(tidyverse)

getwd()
setwd('C:/Users/mathe/OneDrive/Documentos/R/Passenger Score')

df = read_excel('passenger_satisfaction.xlsx')
raw_data = read_excel('passenger_satisfaction.xlsx')
data = df

##Vericando se existem variáveis nulas e missing:
summary(data)

#0,3% (393 de 129880) de variáveis NUlas
data = na.omit(data)

#Drop na coluna ID
data$id = NULL

#Verificando a coluna Satisfaction
unique(data$Satisfaction)
data = data %>% mutate('Satisfaction' = ifelse(Satisfaction == 'satisfied', 1 ,0))

unique(data$Gender)
#Dumificando a variável GENDER:
data = data %>% mutate('Gender' =  ifelse(Gender=='Male',0,1))

unique(data$`Customer Type`)
data = data %>% mutate('Customer Type' =  ifelse(`Customer Type` == 'disloyal Customer',0,1))

unique(data$`Type of Travel`)
data = data %>% mutate('Type of Travel' =  ifelse(`Type of Travel` == 'Personal Travel',0,1))

unique(data$Class)
data = data %>% mutate(CodBusiness = (Class=='Business')*1,
                             CodEco = (Class=='Eco')*1)
data$Class = NULL

df = data
names(df)
names(df)[3] = 'Customer_Type'
names(df)[5] = 'Type_of_Travel'
names(df)[6] = 'Flight_Distance'
names(df)[7] = 'Inflight_Wifi_Service'
names(df)[8] = 'Departure_Arrival_time_convenient'
names(df)[9] = 'Ease_of_Online_booking'
names(df)[10] = 'Gate_location'
names(df)[11] = 'Food_and_drink'
names(df)[12] = 'Online_boarding'
names(df)[13] = 'Seat_comfort'
names(df)[14] = 'Inflight_entertainment'
names(df)[15] = 'Onboard_service'
names(df)[16] = 'Leg_room_service'
names(df)[17] = 'Baggage_handling'
names(df)[18] = 'Checkin_service'
names(df)[19] = 'Inflight_service'
names(df)[21] = 'Departure_Delay_in_Minutes'
names(df)[22] = 'Arrival_Delay_in_Minutes'

df$Satisfaction = as.factor(df$Satisfaction)

#--------------------------------------------------------------------------------#
#Definindo a semente
set.seed(123)
index_train <- createDataPartition(df$Satisfaction ,p = 0.7,list=F)
df_train <- df[index_train, ] # base de desenvolvimento: 70%
df_tst  <- df[index_train,] # base de teste: 30%

#mdl_fit = randomForest(Satisfaction ~ .,data = df_train, importance = T,mtry= 3,nodesize = 50,ntree = 200) 
#mdl_fit

#plot(mdl_fit, main = 'Out-of-bag error')
#mdl_fit = randomForest(Satisfaction ~ .,data = df_train, importance = T,mtry= 3,nodesize = 50,ntree = 50) 

#mdl_fit
#plot(mdl_fit, main = 'Out-of-bag error')

mdl_fit = randomForest(Satisfaction ~ .,data = df_train, importance = T,mtry= 3,nodesize = 5,ntree = 35) 
plot(mdl_fit, main = 'Out-of-bag error')

Y_PROB_TRAIN <- predict(mdl_fit, type = 'prob')[,2]
Y_PROB_TEST  <- predict(mdl_fit, newdata = df_tst, type = 'prob')[,2]

HMeasure(df_train$Satisfaction,Y_PROB_TRAIN)$metrics
HMeasure(df_tst$Satisfaction, Y_PROB_TEST)$metrics

mdl_final <- mdl_fit
varImp(mdl_fit)

varImpPlot(mdl_fit, sort= T, main = 'Importancia das Variaveis')
#--------------------------------------------------------------------------------#
# 6) Inspecao dos valores previstos vs observados (modelo final)

# Geracao da matriz de confusao para diferentes pontos de corte (amostra teste)
# Label observado

Y_OBS <- df_tst$Satisfaction
Y_OBS
levels(Y_OBS)

Y_PROB_TEST
Y_CLAS1


# Label previsto usando: 
#       se PROB > 50% -> 1 (Yes)
#       se PROB > 30% -> 1 (Yes)

Y_CLAS1 <- factor(ifelse(Y_PROB_TEST > 0.5,1,0),
                  levels = c(0,1))
                   
Y_CLAS2 <- factor(ifelse(Y_PROB_TEST > 0.3,1,0),
                  levels = c(1,0))

confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = "1")
confusionMatrix(data = Y_CLAS2, reference = Y_OBS, positive = "1")

# Distribuicao do score
graphics.off()
par(mfrow = c(1,2))

# df auxiliar
AUX <- data.frame(Y_PROB = Y_PROB_TEST,
                  Y_OBS  = df_tst$Satisfaction)

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

library(pROC)
ROC1 <- roc(df_train$Satisfaction,Y_PROB_TRAIN)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(df_tst$Satisfaction,Y_PROB_TEST)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5,
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('TRAIN SET','TEST SET'), lty = 1, col = c('tomato3','cyan3'))




#--------------------------------------------------------------------------------#
# REGRESSAO LOGISTICA
#--------------------------------------------------------------------------------#

df = data


#--------------------------------------------------------------------------------#
# 1) Divisao da base de modelagem em treino e teste com amostragem

set.seed(123) # garantindo reprodutibilidade da amostra

INDEX_TRAIN <- createDataPartition(df$Satisfaction, p = 0.7, list = F)
TRAIN_SET <- df[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- df[-INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET$Satisfaction);summary(TEST_SET$Satisfaction)
prop.table(table(TRAIN_SET$Satisfaction));prop.table(table(TEST_SET$Satisfaction))

#--------------------------------------------------------------------------------#
# 2) Treino do algoritmo de regressao logistica

MDL_FIT <- glm(Satisfaction ~ ., data= TRAIN_SET, family= binomial(link='logit'))
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

# Probabilidade Satisfaction pela regressao full
Y_PROB_TRAIN <- predict(MDL_FIT, type = 'response') 
Y_PROB_TEST  <- predict(MDL_FIT, newdata = TEST_SET, type = 'response')

head(Y_PROB_TRAIN) # perceba que geraram duas colunas com probs (soma 1)

# Probabilidade Satisfaction pela regressao  stepwise
Y_PROB_TRAIN.STEP <- predict(MDL_FIT.STEP, type = 'response')  
Y_PROB_TEST.STEP  <- predict(MDL_FIT.STEP, newdata = TEST_SET, type = 'response')

# [EXTRA] Verificando a aderencia do ajuste logistico (teste Spiegelhalter)
install.packages('rms')
library(rms)
val.prob(Y_PROB_TRAIN,ifelse(TRAIN_SET$Satisfaction == 'Yes',1,0), smooth = F)[c('S:z','S:p')]
# p valor > 5%, nao podemos rejeitar a hipotese nula
#--------------------------------------------------------------------------------#
# 5) Avaliando a performance dos modelos e existencia de overfitting

# Regressao full
library(hmeasure) 
HMeasure(TRAIN_SET$Satisfaction,Y_PROB_TRAIN)$metrics
HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST)$metrics

# Regressao com stepwise
HMeasure(TRAIN_SET$Satisfaction,Y_PROB_TRAIN.STEP)$metrics
HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST.STEP)$metrics

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
Y_OBS <- TEST_SET$Satisfaction

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
                  Y_OBS  = TEST_SET$Satisfaction)

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
ROC1 <- roc(TRAIN_SET$Satisfaction,Y_PROB_TRAIN.STEP)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(TEST_SET$Satisfaction,Y_PROB_TEST.STEP)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5,
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('TRAIN SET','TEST SET'), lty = 1, col = c('tomato3','cyan3'))


#--------------------------------------------------------------------------------#
# ADABOOST PARA CLASSIFICACAO
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# 0) Lendo a base de dados

df = data
names(df)[3] = 'Customer_Type'
names(df)[5] = 'Type_of_Travel'
names(df)[6] = 'Flight_Distance'
names(df)[7] = 'Inflight_Wifi_Service'
names(df)[8] = 'Departure_Arrival_time_convenient'
names(df)[9] = 'Ease_of_Online_booking'
names(df)[10] = 'Gate_location'
names(df)[11] = 'Food_and_drink'
names(df)[12] = 'Online_boarding'
names(df)[13] = 'Seat_comfort'
names(df)[14] = 'Inflight_entertainment'
names(df)[15] = 'Onboard_service'
names(df)[16] = 'Leg_room_service'
names(df)[17] = 'Baggage_handling'
names(df)[18] = 'Checkin_service'
names(df)[19] = 'Inflight_service'
names(df)[21] = 'Departure_Delay_in_Minutes'
names(df)[22] = 'Arrival_Delay_in_Minutes'

df$Satisfaction = as.factor(df$Satisfaction)
#--------------------------------------------------------------------------------#
# 1) Divisao da base de modelagem em treino e teste com amostragem

library(caret)

set.seed(123) # garantindo reprodutibilidade da amostra
df2 = df
df2_index = createDataPartition(df$Satisfaction, p=0.01, list = F)
df2 = df2[df2_index,]

INDEX_TRAIN <- createDataPartition(df2$Satisfaction, p = 0.7, list = F)
TRAIN_SET <- df2[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- df2[-INDEX_TRAIN,] # base de teste: 30%

# Avaliando a distribuicao da variavel resposta
summary(TRAIN_SET$Satisfaction);summary(TEST_SET$Satisfaction)
prop.table(table(TRAIN_SET$Satisfaction));prop.table(table(TEST_SET$Satisfaction))

#--------------------------------------------------------------------------------#
# 2) Treino do algoritmo de adaboost para classificacao
library(adabag)

class(df$Satisfaction)

TRAIN_SET


MDL_FIT <- boosting(Satisfaction ~ ., 
                    data      = TRAIN_SET, 
                    mfinal    = 50,
                    coeflearn = "Breiman", 
                    control   = rpart.control(maxdepth = 1))

# saida do modelo
MDL_FIT

# avaliando a evolucao do erro conforme o número de iteracoes aumenta
plot(errorevol(MDL_FIT, TRAIN_SET))

# o erro estabiliza a partir de ~50 iteracoes
MDL_FIT <- boosting(Satisfaction ~ ., 
                    data      = TRAIN_SET, 
                    mfinal    = 50,
                    coeflearn = "Breiman", 
                    control   = rpart.control(maxdepth = 1))

#-----------------------------------------------------------------------------------------------------------------------------------#
#Não foi possível gerar o modelo através do adabost nem com 1% da base dados, por essa razão o modelo vencedor será o Random Forrest.
df = data
names(df)
names(df)[3] = 'Customer_Type'
names(df)[5] = 'Type_of_Travel'
names(df)[6] = 'Flight_Distance'
names(df)[7] = 'Inflight_Wifi_Service'
names(df)[8] = 'Departure_Arrival_time_convenient'
names(df)[9] = 'Ease_of_Online_booking'
names(df)[10] = 'Gate_location'
names(df)[11] = 'Food_and_drink'
names(df)[12] = 'Online_boarding'
names(df)[13] = 'Seat_comfort'
names(df)[14] = 'Inflight_entertainment'
names(df)[15] = 'Onboard_service'
names(df)[16] = 'Leg_room_service'
names(df)[17] = 'Baggage_handling'
names(df)[18] = 'Checkin_service'
names(df)[19] = 'Inflight_service'
names(df)[21] = 'Departure_Delay_in_Minutes'
names(df)[22] = 'Arrival_Delay_in_Minutes'

df$Satisfaction = as.factor(df$Satisfaction)

#--------------------------------------------------------------------------------#
#Definindo a semente
library(randomForest)
library(caret)
library(hmeasure)

set.seed(123)
index_train <- createDataPartition(df$Satisfaction ,p = 0.7,list=F)
df_train <- df[index_train, ] # base de desenvolvimento: 70%
df_tst  <- df[index_train,] # base de teste: 30%

#mdl_fit = randomForest(Satisfaction ~ .,data = df_train, importance = T,mtry= 3,nodesize = 50,ntree = 200) 
#mdl_fit

#plot(mdl_fit, main = 'Out-of-bag error')
#mdl_fit = randomForest(Satisfaction ~ .,data = df_train, importance = T,mtry= 3,nodesize = 50,ntree = 50) 

#mdl_fit
#plot(mdl_fit, main = 'Out-of-bag error')

mdl_fit = randomForest(Satisfaction ~ .,data = df_train, importance = T,mtry= 3,nodesize = 5,ntree = 35) 
plot(mdl_fit, main = 'Out-of-bag error')

Y_PROB_TRAIN <- predict(mdl_fit, type = 'prob')[,2]
Y_PROB_TEST  <- predict(mdl_fit, newdata = df_tst, type = 'prob')[,2]

HMeasure(df_train$Satisfaction,Y_PROB_TRAIN)$metrics
HMeasure(df_tst$Satisfaction, Y_PROB_TEST)$metrics

mdl_final <- mdl_fit
varImp(mdl_fit)

varImpPlot(mdl_fit, sort= T, main = 'Importancia das Variaveis')
#--------------------------------------------------------------------------------#
# 6) Inspecao dos valores previstos vs observados (modelo final)

# Geracao da matriz de confusao para diferentes pontos de corte (amostra teste)
# Label observado

Y_OBS <- df_tst$Satisfaction
Y_OBS
levels(Y_OBS)

Y_PROB_TEST
Y_CLAS1


# Label previsto usando: 
#       se PROB > 50% -> 1 (Yes)
#       se PROB > 30% -> 1 (Yes)

Y_CLAS1 <- factor(ifelse(Y_PROB_TEST > 0.5,1,0),
                  levels = c(0,1))

Y_CLAS2 <- factor(ifelse(Y_PROB_TEST > 0.3,1,0),
                  levels = c(1,0))

confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = "1")
confusionMatrix(data = Y_CLAS2, reference = Y_OBS, positive = "1")

# Distribuicao do score
graphics.off()
par(mfrow = c(1,2))

# df auxiliar
AUX <- data.frame(Y_PROB = Y_PROB_TEST,
                  Y_OBS  = df_tst$Satisfaction)

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

library(pROC)
ROC1 <- roc(df_train$Satisfaction,Y_PROB_TRAIN)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(df_tst$Satisfaction,Y_PROB_TEST)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5,
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('TRAIN SET','TEST SET'), lty = 1, col = c('tomato3','cyan3'))

#---Fazendo a análise das Três Variáveis mais importantes #---
#1) Inflight_Wifi_service
library(dplyr)
#Verificando a coluna Satisfaction
df$Gender = as.factor(df$Gender)
df$Customer_Type = as.factor(df$Customer_Type)
df$Type_of_Travel = as.factor(df$Type_of_Travel)

class(df$Gender)

levels(df$Satisfaction) = c('Neutro ou não Satisfeito','Satisfeito')
levels(df$Gender) = c('Masculino', 'Feminino')
levels(df$Customer_Type) = c('Não Leal', 'Leal')
levels(df$Type_of_Travel) = c('Pessoal', 'Negócios')

df = df %>% mutate('class' = case_when(
  df$CodBusiness == 1 ~ 'Business',
  df$CodEco == 1 ~ 'Eco',
  TRUE ~ 'EcoPlus'))

ggplot(df, aes(x = df$Inflight_Wifi_Service))+
  geom_histogram(fill='blue')+
  ggtitle("Histograma Qualidade Wi-fi")+
  labs(x = "Inflight Wifi service", y = "Observações")+ 
  theme_classic()

ggplot(df, aes(x = df$Inflight_Wifi_Service, fill = Satisfaction))+
  geom_histogram()+
  ggtitle("Histograma Qualidade Wi-fi")+
  labs(x = "Inflight Wifi service", y = "Observações", color = 'Satisfação')+
  theme_classic()

ggplot(df, aes(x = df$Inflight_entertainment, fill=Satisfaction))+
  geom_bar()+
  ggtitle("Histograma Qualidade Wi-fi")+
  labs(x = "Inflight Wifi service", y = "Observações")+ 
  facet_wrap(Type_of_Travel~.)
  theme_classic()

varImpPlot(mdl_fit, sort= T, main = 'Importancia das Variaveis')

df3 = df

df3$Food_and_drink = NULL
df3$Gender = NULL
df3$Gate_location = NULL
df3$Departure_Arrival_time_convenient = NULL
df3$Onboard_service = NULL

#----------------------------------------------
#Retirando as variáveis menos importantes do modelo.

df = df3

#--------------------------------------------------------------------------------#
#Definindo a semente
summary(df)

library(randomForest)
library(caret)
library(hmeasure)

set.seed(123)
index_train <- createDataPartition(df$Satisfaction ,p = 0.7,list=F)
df_train <- df[index_train, ] # base de desenvolvimento: 70%
df_tst  <- df[-index_train,] # base de teste: 30%

mdl_fit = randomForest(Satisfaction ~ .,data = df_train, importance = T,mtry= 3,nodesize = 5,ntree = 35) 
plot(mdl_fit, main = 'Out-of-bag error')

Y_PROB_TRAIN <- predict(mdl_fit, type = 'prob')[,2]
Y_PROB_TEST  <- predict(mdl_fit, newdata = df_tst, type = 'prob')[,2]

HMeasure(df_train$Satisfaction,Y_PROB_TRAIN)$metrics
HMeasure(df_tst$Satisfaction, Y_PROB_TEST)$metrics

mdl_final <- mdl_fit
varImp(mdl_fit)

varImpPlot(mdl_fit, sort= T, main = 'Importancia das Variaveis')
#--------------------------------------------------------------------------------#
# 6) Inspecao dos valores previstos vs observados (modelo final)

# Geracao da matriz de confusao para diferentes pontos de corte (amostra teste)
# Label observado

Y_OBS <- df_tst$Satisfaction
Y_OBS
levels(Y_OBS)

Y_PROB_TEST
Y_CLAS1


# Label previsto usando: 
#       se PROB > 50% -> 1 (Yes)
#       se PROB > 30% -> 1 (Yes)

Y_CLAS1 <- factor(ifelse(Y_PROB_TEST > 0.5,1,0),
                  levels = c(0,1))

Y_CLAS2 <- factor(ifelse(Y_PROB_TEST > 0.3,1,0),
                  levels = c(1,0))

confusionMatrix(data = Y_CLAS1, reference = Y_OBS, positive = "1")
confusionMatrix(data = Y_CLAS2, reference = Y_OBS, positive = "1")

# Distribuicao do score
graphics.off()
par(mfrow = c(1,2))

# df auxiliar
AUX <- data.frame(Y_PROB = Y_PROB_TEST,
                  Y_OBS  = df_tst$Satisfaction)

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

library(pROC)
ROC1 <- roc(df_train$Satisfaction,Y_PROB_TRAIN)
Y1   <- ROC1$sensitivities
X1   <- 1 - ROC1$specificities

ROC2 <- roc(df_tst$Satisfaction,Y_PROB_TEST)
Y2   <- ROC2$sensitivities
X2   <- 1 - ROC2$specificities

plot(X1,Y1, type ="n", cex.axis = 1.2, cex = 0.5,
     xlab = '1 - ESPECIFICIDADE', ylab = 'SENSITIVIDADE')
lines(X1, Y1, lwd = 3, lty = 1, col = 'tomato3') 
lines(X2, Y2, lwd = 3, lty = 1, col = 'cyan3') 
abline(0, 1, lty = 2)
legend('bottomright',c('TRAIN SET','TEST SET'), lty = 1, col = c('tomato3','cyan3'))

#---Fazendo a análise das Três Variáveis mais importantes #---
#1) Inflight_Wifi_service
library(dplyr)
#Verificando a coluna Satisfaction
df$Customer_Type = as.factor(df$Customer_Type)
df$Type_of_Travel = as.factor(df$Type_of_Travel)

levels(df$Satisfaction) = c('Neutro ou não Satisfeito','Satisfeito')
levels(df$Customer_Type) = c('Não Leal', 'Leal')
levels(df$Type_of_Travel) = c('Pessoal', 'Negócios')

df = df %>% mutate('class' = case_when(
  df$CodBusiness == 1 ~ 'Business',
  df$CodEco == 1 ~ 'Eco',
  TRUE ~ 'EcoPlus'))

ggplot(df, aes(x = df$Inflight_Wifi_Service))+
  geom_histogram(fill='blue')+
  ggtitle("Histograma Qualidade Wi-fi")+
  labs(x = "Inflight Wifi service", y = "Observações")+ 
  theme_classic()

ggplot(df, aes(x = df$Inflight_Wifi_Service, fill = Satisfaction))+
  geom_histogram()+
  ggtitle("Histograma Qualidade Wi-fi")+
  labs(x = "Inflight Wifi service", y = "Observações", color = 'Satisfação')+
  theme_classic()

ggplot(df, aes(x = df$Inflight_entertainment, fill=Satisfaction))+
  geom_bar()+
  ggtitle("Histograma Qualidade Wi-fi")+
  labs(x = "Inflight Wifi service", y = "Observações")+ 
  facet_wrap(Type_of_Travel~.)+
  theme_classic()

ggplot(df, aes(x = df$Inflight_entertainment, fill=Satisfaction))+
  geom_bar()+
  ggtitle("Histograma Qualidade Wi-fi")+
  labs(x = "Inflight Wifi service", y = "Observações")+ 
  facet_wrap(class~.)+
  theme_classic()

ggplot(df, aes(x = Arrival_Delay_in_Minutes, y=Departure_Delay_in_Minutes, color = Satisfaction))+
  geom_point(alpha = 0.7)+
  theme_classic()


varImpPlot(mdl_fit, sort= T, main = 'Importancia das Variaveis')

##--- Type Of Travel
ggplot(df, aes(x = Type_of_Travel))+
  geom_histogram(fill='blue', stat = 'count')+
  ggtitle("Quantidade por Tipo de Viagens")+
  labs(x = "Tipo de viagem", y = "Observações")+ 
  theme_classic()

ggplot(df, aes(x = Type_of_Travel, fill = Satisfaction))+
  geom_histogram(stat = 'count')+
  ggtitle("Quantidade por Tipo de Viagens")+
  labs(x = "Tipo de viagem", y = "Observações")+ 
  theme_classic()

ggplot(df, aes(x = Type_of_Travel, fill=Satisfaction))+
  geom_bar()+
  ggtitle("Quantidade por Tipo de Viagens")+
  labs(x = "Tipo de viagem", y = "Observações")+ 
  facet_wrap(class~.)+
  theme_classic()

ggplot(df, aes(x = df$Inflight_entertainment, fill=Satisfaction))+
  geom_bar()+
  ggtitle("Histograma Qualidade Wi-fi")+
  labs(x = "Inflight Wifi service", y = "Observações")+ 
  facet_wrap(class~.)+
  theme_classic()

ggplot(df, aes(x = Arrival_Delay_in_Minutes, y=Departure_Delay_in_Minutes, color = Satisfaction))+
  geom_point(alpha = 0.7)+
  theme_classic()

varImpPlot(mdl_fit, sort= T, main = 'Importancia das Variaveis')

##--- Customer Type
ggplot(df, aes(x = Customer_Type))+
  geom_histogram(fill='blue', stat = 'count')+
  ggtitle("Quantidade por Tipo de Viagens")+
  labs(x = "Tipo de viagem", y = "Observações")+ 
  theme_classic()

ggplot(df, aes(x = Customer_Type, fill = Satisfaction))+
  geom_histogram(stat = 'count')+
  ggtitle("Quantidade por Tipo de Viagens")+
  labs(x = "Tipo de viagem", y = "Observações")+ 
  theme_classic()

ggplot(df, aes(x = Customer_Type, fill=Satisfaction))+
  geom_bar()+
  ggtitle("Quantidade por Tipo de Viagens")+
  labs(x = "Tipo de viagem", y = "Observações")+ 
  facet_wrap(class~.)+
  theme_classic()

varImpPlot(mdl_fit, sort= T, main = 'Importancia das Variaveis')

##--- Customer Type
ggplot(df, aes(x = Checkin_service))+
  geom_histogram(fill='blue', stat = 'count')+
  ggtitle("Quantidade por Tipo de Viagens")+
  labs(x = "Tipo de viagem", y = "Observações")+ 
  theme_classic()

ggplot(df, aes(x = Checkin_service, fill = Satisfaction))+
  geom_histogram(stat = 'count')+
  ggtitle("Quantidade por Tipo de Viagens")+
  labs(x = "Tipo de viagem", y = "Observações")+ 
  theme_classic()

ggplot(df, aes(x = Checkin_service, fill=Satisfaction))+
  geom_bar()+
  ggtitle("Quantidade por Tipo de Viagens")+
  labs(x = "Tipo de viagem", y = "Observações")+ 
  facet_wrap(class~.)+
  theme_classic()

varImpPlot(mdl_fit, sort= T, main = 'Importancia das Variaveis')