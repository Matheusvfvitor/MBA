install.packages('caret')
install.packages('randomForest')
install.packages('e1071')

#Libraries
library(tidyverse)
library(lubridate)
library(data.table)
library(ggplot2)
library(caret)
library(randomForest)
library(e1071)

setwd("C:/Users/mvv/OneDrive/ESI_drive/OneDrive - ESI Group/Matheus/R/AnalisePreditiva")
getwd()

#Lendo os arquivos CSV
df = read.csv("uber_nyc_enriched.csv", fileEncoding = "UTF-8",header = TRUE, quote = ",")

colnames(df)[1] <- "pickup_date"
colnames(df)[2] <- "bairro"
colnames(df)[3] <- "n.pickups"
colnames(df)[4] <- "velocidade.vento"
colnames(df)[5] <- "visibilidade"
colnames(df)[6] <- "temp"
colnames(df)[7] <- "umidade"
colnames(df)[8] <- "pressao"
colnames(df)[9] <- "precipitacao.1.hora"
colnames(df)[10] <- "precipitacao.6.horas"
colnames(df)[11] <- "precipitacao.24.horas"
colnames(df)[12] <- "profundidade.neve"
colnames(df)[13] <- "feriado"

#Alterando bairro de factor para caracter
df$bairro = df$bairro %>% as.character()

#Substituindo as "" que foram carregadas nas strings
df$bairro = str_replace(df$bairro,"[\"]","")
df$pickup_date = str_replace(df$pickup_date,"[\"]","")
df$feriado = str_replace(df$feriado,"[\"]","")

head(df)

#Alterando as colunas com números para numeric
df$n.pickups = as.numeric(df$n.pickups)
df$velocidade.vento = as.numeric(df$velocidade.vento)
df$visibilidade = as.numeric(df$visibilidade)
df$temp = as.numeric(df$temp)
df$umidadae = as.numeric(df$umidadae)
df$pressao = as.numeric(df$pressao)
df$precipitacao.1.hora = as.numeric(df$precipitacao.1.hora)
df$precipitacao.6.horas = as.numeric(df$precipitacao.6.horas)
df$precipitacao.24.horas = as.numeric(df$precipitacao.24.horas)
df$profundidade.neve = as.numeric(df$profundidade.neve)


#Aterando a temperatura para Celsius.
df = df %>% mutate(temp = (df$temp-32)*(5/9))

#Alterando de milhas/h para km/h
df = df %>% mutate(velocidade.vento = (velocidade.vento * 1.60934))

#Alterando a visibilidade de milhas para km
df = df %>% mutate(visibilidade = (visibilidade * 1.60934))

#Alterando a temperatura de orvalho
df = df %>% mutate(umidadae = (umidade -32 * (5/9)))
                   
#Alterando a profundidade da neve para cm
df = df %>% mutate(profundidade.neve = (profundidade.neve * 2.54))

#Alterando a data de character para date
df = df %>% mutate(pickup_date = ymd_hms(pickup_date))

#Criando ramificação de dias
df = df %>% mutate(dia.semana = weekdays(pickup_date),
                   dia.mes = day(pickup_date),
                   mes = month(pickup_date),
                   ano = year(pickup_date),
                   )

#Definindo se é fds
df = df %>% mutate(fds = case_when(
  dia.semana == "Saturday" ~ TRUE,
  dia.semana == "Sunday" ~ TRUE,
  dia.semana == "Monday" ~ FALSE,
  dia.semana == "Tuesday" ~ FALSE,
  dia.semana == "Wednesday" ~ FALSE,
  dia.semana == "Thursday" ~ FALSE,
  dia.semana == "Friday" ~ FALSE,
))

#Filtrando os bairros que não possuem NA
df = df %>% filter(!is.na(bairro))

#filtrando os bairros manhatam, brookling, queens
df2 = df %>% filter((bairro == "Brooklyn" | bairro == "Queens" | bairro == "Manhattan"))

#Criando as variáveis dummies
mutate(df2, codManhattan = (bairro=="Manhattan")*1,
       codQueens = (bairro=="Queens")*1,
       codBrooklyn = (bairro=="Brooklyn")*1)->df3

mutate(df3, codhday = (feriado=="Y")*1,
       codQueens = (feriado=="N")*1)->df3

#Definindo o código de dia da semana
df3 = df3 %>% mutate(cod.dia.semana = case_when(
  dia.semana == "Saturday" ~ 6,
  dia.semana == "Sunday" ~ 0,
  dia.semana == "Monday" ~ 1,
  dia.semana == "Tuesday" ~ 2,
  dia.semana == "Wednesday" ~ 3,
  dia.semana == "Thursday" ~ 4,
  dia.semana == "Friday" ~ 5,
))

#drop nas colunas String
df3$bairro = NULL
df3$dia.semana = NULL
df3$feriado = NULL

summary(df)
df_clean <- df3


#Divisão da base de dados
set.seed(123)

index <- createDataPartition(df_clean$n.pickups, p = 0.7, list = F)
df_train  <- df_clean[index,] # base de desenvolvimento: 70%
df_tst  <- df_clean[-index,] # base de teste: 30%


head(df_tst)

summary(df_train)

modelo <- randomForest(n.pickups ~ .,
                                  data = df_train,
                                  importance = T,
                                  mtry       = 2,
                                  nodesize   = 10, 
                                  ntree      = 100)
modelo
plot(modelo, main = 'Out-of-bag error')

#Após 80 árvores o modelo se establiza então, vamos alterar o modelo para 80 árvores:
modelo <- randomForest(n.pickups ~ .,
                       data = df_train,
                       importance = T,
                       mtry       = 2,
                       nodesize   = 10, 
                       ntree      = 80)

plot(modelo, main = 'Out-of-bag error')


#Iniciando as predições
res_df_train <- predict(modelo) 
res_df_test  <- predict(modelo, newdata = df_tst)

res_df_train
df_train

postResample(pred = res_df_train, obs = df_train$n.pickups)
postResample(pred = res_df_test,  obs = df_tst$n.pickups)

modeloFinal <- modelo
varImp(modeloFinal)

varImpPlot(modeloFinal, sort= T, main = 'Importancia das Variaveis')

#Vamos tirar as variáveis menos importantes do modelo e rodar novamente.
df5 <- df3
df5$ano = NULL
df5$precipitacao.1.hora = NULL
df5$profundidade.neve = NULL
df5$velocidade.vento = NULL
df5$visibilidade = NULL
df5$precipitacao.6.horas = NULL
df5$pressao = NULL
df5$umidadae = NULL

df5 = df5 %>% mutate(hora = hour(pickup_date))
df5$pickup_date = NULL

df_clean <- df5


#Divisão da base de dados
set.seed(123)

index <- createDataPartition(df_clean$n.pickups, p = 0.7, list = F)
df_train  <- df_clean[index,] # base de desenvolvimento: 70%
df_tst  <- df_clean[-index,] # base de teste: 30%


head(df_tst)

summary(df_train)

modelo <- randomForest(n.pickups ~ .,
                       data = df_train,
                       importance = T,
                       mtry       = 2,
                       nodesize   = 10, 
                       ntree      = 100)
modelo
plot(modelo, main = 'Out-of-bag error')

#Após 80 árvores o modelo se establiza então, vamos alterar o modelo para 80 árvores:
modelo <- randomForest(n.pickups ~ .,
                       data = df_train,
                       importance = T,
                       mtry       = 2,
                       nodesize   = 10, 
                       ntree      = 80)

plot(modelo, main = 'Out-of-bag error')

#Iniciando as predições
res_df_train <- predict(modelo) 
res_df_test  <- predict(modelo, newdata = df_tst)

res_df_train
df_train

postResample(pred = res_df_train, obs = df_train$n.pickups)
postResample(pred = res_df_test,  obs = df_tst$n.pickups)

modeloFinal <- modelo
varImp(modeloFinal)


varImpPlot(modeloFinal, sort= T, main = 'Importancia das Variaveis')


# Convertendo a variavel para unidade original

df_train

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

#Iniciando as predições
res_df_train <- predict(modelo) 
res_df_test  <- predict(modelo, newdata = df_tst)

res_df_train
df_train

postResample(pred = res_df_train, obs = df_train$n.pickups)
postResample(pred = res_df_test,  obs = df_tst$n.pickups)

modeloFinal <- modelo
varImp(modeloFinal)

varImpPlot(modeloFinal, sort= T, main = 'Importancia das Variaveis')

#Vamos tirar as variáveis menos importantes do modelo e rodar novamente.
df6 <- df3
df6$ano = NULL
df6$precipitacao.1.hora = NULL
df6$profundidade.neve = NULL
df6$velocidade.vento = NULL
df6$visibilidade = NULL
df6$pressao = NULL
df6$codhday = NULL
df6$fds = NULL
df6$umidadae = NULL
df6$umidade = NULL
df6$mes = NULL
df6$precipitacao.6.horas = NULL
df6$precipitacao.24.horas = NULL

df6 = df6 %>% mutate(hora = hour(pickup_date))
df6$pickup_date = NULL

df_clean <- df6
#Divisão da base de dados
set.seed(123)

index <- createDataPartition(df_clean$n.pickups, p = 0.7, list = F)
df_train  <- df_clean[index,] # base de desenvolvimento: 70%
df_tst  <- df_clean[-index,] # base de teste: 30%


head(df_tst)

summary(df_train)

modelo <- randomForest(n.pickups ~ .,
                       data = df_train,
                       importance = T,
                       mtry       = 2,
                       nodesize   = 10, 
                       ntree      = 100)
modelo
plot(modelo, main = 'Out-of-bag error')

#Após 80 árvores o modelo se establiza então, vamos alterar o modelo para 80 árvores:
modelo <- randomForest(n.pickups ~ .,
                       data = df_train,
                       importance = T,
                       mtry       = 2,
                       nodesize   = 10, 
                       ntree      = 80)

plot(modelo, main = 'Out-of-bag error')


#Iniciando as predições
res_df_train <- predict(modelo) 
res_df_test  <- predict(modelo, newdata = df_tst)

res_df_train
df_train

postResample(pred = res_df_train, obs = df_train$n.pickups)
postResample(pred = res_df_test,  obs = df_tst$n.pickups)

modeloFinal <- modelo
varImp(modeloFinal)

varImpPlot(modeloFinal, sort= T, main = 'Importancia das Variaveis')

##Análises gráficas
result_train <- data.frame(n.pickups_OBS  = df_train$n.pickups**2,
                           n.pickups_PRED = res_df_train**2) %>%
  mutate(RESIDUO = n.pickups_PRED - n.pickups_OBS)

result_test  <- data.frame(n.pickups_OBS  = df_tst$n.pickups**2,
                           n.pickups_PRED = res_df_test**2) %>%
  mutate(RESIDUO = n.pickups_PRED - n.pickups_OBS)

# Plotando os resultados
layout(matrix(c(1,2,3,4,3,4), nrow = 3, ncol = 2, byrow = TRUE))
par(oma = c(1,1,0,0),  mar = c(5,5,2,1))

dev.off()

ggplot(result_train, aes(x =RESIDUO), bins = 10)+
  geom_histogram(fill="blue")+
  xlim(-300000,300000)+
  ggtitle("Histograma do Erro Resídual Treinamento")+
  theme_classic()

ggplot(result_test,aes(RESIDUO), bins = 10)+
  geom_histogram(fill="red")+
  xlim(-300000,300000)+
  ggtitle("Histograma do Erro Resídual Teste")+
  theme_classic()

ggplot(result_train, aes(x = n.pickups_OBS, y=n.pickups_PRED))+
  geom_point(color = "orange", alpha = 0.7)+
  theme_classic()+
  ggtitle("Dispersão entre Observado e Previsto")

ggplot(result_train, aes(x = n.pickups_OBS, y=n.pickups_PRED))+
  geom_abline(color = "blue")+
  theme_classic()+
  ggtitle("Dispersão entre Observado e Previsto")

abline(lm(n.pickups_PRED ~ n.pickups_OBS, data = result_train), 
       col = 'firebrick', lwd = 3)

ggplot(result_test, aes(x = n.pickups_OBS, y=n.pickups_PRED))+
  geom_point(color = "orange", alpha = 0.7)+
  theme_classic()+
  ggtitle("Dispersão entre Observado e Previsto")

ggplot(result_test, aes(x = n.pickups_OBS, y=n.pickups_PRED))+
  geom_abline(color = "blue")+
  theme_classic()+
  ggtitle("Dispersão entre Observado e Previsto")


#Fazendo a análise das 3 variaveis mais importantes.
df = df %>% mutate(horas = hour(pickup_date))

df7 = df6

df7$hora = as.factor(df7$hora)

ggplot(df7, aes(x=hora, y=n.pickups))+
  geom_col(fill="blue")+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df7, aes(x=cod.dia.semana, y=n.pickups))+
  geom_col(fill="blue")+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df2, aes(x = bairro, y = n.pickups , fill = bairro))+
  geom_col()+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df2, aes(x = temp, y = n.pickups, color = dia.semana))+
  geom_point(alpha = 0.5)+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df2, aes(x = temp, y = n.pickups, color =n.pickups))+
  geom_point()+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df7, aes(x = temp, y = n.pickups, size = temp, color = cod.dia.semana))+
  geom_point(alpha = 0.5)+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

df$horas = as.character(df$horas)

ggplot(df, aes(x = horas, y = n.pickups))+
  geom_boxplot()+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df, aes(x = dia.semana, y = n.pickups))+
  geom_boxplot(outlier.shape = NA)+
  theme_classic()+
  ylim(0,500)+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df, aes(x = dia.semana, y = n.pickups))+
  geom_boxplot()+
  theme_classic()+
  ylim(0,500)+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df2, aes(x = bairro, y = n.pickups))+
  geom_boxplot()+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df, aes(x = horas, y = n.pickups))+
  geom_boxplot()+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")
  

ggplot(df7, aes(x=n.pickups, y=temp))+
  geom_col(fill="blue")+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")
####--------------------------------------------------------------####
##Estudando sómente manhatam
df_manhatam = df %>% filter (bairro == "Manhattan") 

df2 = df_manhatam

#Criando as variáveis dummies
df2$bairro = NULL

df3 = mutate(df2, codhday = (feriado=="Y")*1)->df3

#Definindo o código de dia da semana
df3 = df3 %>% mutate(cod.dia.semana = case_when(
  dia.semana == "Saturday" ~ 6,
  dia.semana == "Sunday" ~ 0,
  dia.semana == "Monday" ~ 1,
  dia.semana == "Tuesday" ~ 2,
  dia.semana == "Wednesday" ~ 3,
  dia.semana == "Thursday" ~ 4,
  dia.semana == "Friday" ~ 5,
))

#drop nas colunas String
df3$bairro = NULL
df3$dia.semana = NULL
df3$feriado = NULL

summary(df)
df_clean <- df3


#Divisão da base de dados
set.seed(123)

index <- createDataPartition(df_clean$n.pickups, p = 0.7, list = F)
df_train  <- df_clean[index,] # base de desenvolvimento: 70%
df_tst  <- df_clean[-index,] # base de teste: 30%


head(df_tst)

summary(df_train)

modelo <- randomForest(n.pickups ~ .,
                       data = df_train,
                       importance = T,
                       mtry       = 2,
                       nodesize   = 10, 
                       ntree      = 100)
modelo
plot(modelo, main = 'Out-of-bag error')

#Após 40 árvores o modelo se establiza então, vamos alterar o modelo para 80 árvores:
modelo <- randomForest(n.pickups ~ .,
                       data = df_train,
                       importance = T,
                       mtry       = 2,
                       nodesize   = 10, 
                       ntree      = 60)

plot(modelo, main = 'Out-of-bag error')


#Iniciando as predições
res_df_train <- predict(modelo) 
res_df_test  <- predict(modelo, newdata = df_tst)

res_df_train
df_train

postResample(pred = res_df_train, obs = df_train$n.pickups)
postResample(pred = res_df_test,  obs = df_tst$n.pickups)

modeloFinal <- modelo
varImp(modeloFinal)

varImpPlot(modeloFinal, sort= T, main = 'Importancia das Variaveis')

#Retirando as variáveis menos importantes
df <- df3

df$ano = NULL
df$precipitacao.1.hora = NULL
df$profundidade.neve = NULL
df$velocidade.vento = NULL
df$visibilidade = NULL
df$pressao = NULL
df$codhday = NULL
df$umidadae = NULL
df$umidade = NULL
df$mes = NULL
df$precipitacao.6.horas = NULL
df$precipitacao.24.horas = NULL

df = df %>% mutate(horas = hour(pickup_date))

summary(df)
df_clean <- df


#Divisão da base de dados
set.seed(123)

index <- createDataPartition(df_clean$n.pickups, p = 0.7, list = F)
df_train  <- df_clean[index,] # base de desenvolvimento: 70%
df_tst  <- df_clean[-index,] # base de teste: 30%


head(df_tst)

summary(df_train)

modelo <- randomForest(n.pickups ~ .,
                       data = df_train,
                       importance = T,
                       mtry       = 2,
                       nodesize   = 10, 
                       ntree      = 100)
modelo
plot(modelo, main = 'Out-of-bag error')

#Após 40 árvores o modelo se establiza então, vamos alterar o modelo para 80 árvores:
modelo <- randomForest(n.pickups ~ .,
                       data = df_train,
                       importance = T,
                       mtry       = 2,
                       nodesize   = 10, 
                       ntree      = 60)

plot(modelo, main = 'Out-of-bag error')

#Iniciando as predições
res_df_train <- predict(modelo) 
res_df_test  <- predict(modelo, newdata = df_tst)

res_df_train
df_train

postResample(pred = res_df_train, obs = df_train$n.pickups)
postResample(pred = res_df_test,  obs = df_tst$n.pickups)

modeloFinal <- modelo
varImp(modeloFinal)

varImpPlot(modeloFinal, sort= T, main = 'Importancia das Variaveis')



df7 = df6

df7$hora = as.factor(df7$hora)

ggplot(df7, aes(x=hora, y=n.pickups))+
  geom_col(fill="blue")+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df7, aes(x=cod.dia.semana, y=n.pickups))+
  geom_col(fill="blue")+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df2, aes(x = temp, y = n.pickups, color = dia.semana))+
  geom_point(alpha = 0.5)+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df2, aes(x = temp, y = n.pickups))+
  geom_point()+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

df$horas = as.character(df$horas)

df$horas
df$horas = as.factor(df$horas, levels(c(0:23)))
df$horas = as.numeric(df$horas)

df$horas = factor(df$horas, levels=(0:23))

ggplot(df, aes(x = horas, y = n.pickups))+
  geom_boxplot()+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df, aes(x = dia.semana, y = n.pickups))+
  geom_boxplot()+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

ggplot(df, aes(x = horas, y = n.pickups))+
  geom_boxplot()+
  theme_classic()+
  ggtitle("Variação de corridas ao longo do dia")

head(YYY)