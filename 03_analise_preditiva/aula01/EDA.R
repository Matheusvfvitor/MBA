#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# MBA EM BUSINESS ANALYTICS E BIG DATA
# ANALISE PREDITIVA
# AULA 1 - CONCEITOS E DADOS
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
DATA_REG  <- read.csv("dataset_reg.csv", sep = ",") 
DATA_CLAS <- read.csv("dataset_clas.csv",sep = ",",dec = '.') 

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# ANALISE EXPLORATORIA DAS VARIAVEIS
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#

#--------------------------------------------------------------------------------#
# BASE DE DADOS PARA REGRESSAO - Direct Marketing
#--------------------------------------------------------------------------------#

# 8 variaveis no total:
#     7 variaveis explicativas (atributos, vars. independentes)
#         4 variavariaveis qualitativas
#               AGE | GENDER | MARITAL | HISTORY
#         3 variaveis quantitativas
#               INCOME | CHILDREN | CATALOGS
#     1 variavel resposta continua (target, var. dependente)
#              AMOUNT

graphics.off()

#--------------------------------------------------------------------------------#
# 1) Analise univariada - distribuicao da variavel

# Variavel resposta
hist((DATA_REG$AMOUNT), breaks = 8,
     xlab = 'AMOUNT SPENT (US$)', ylab = 'FREQUENCIA (#)',
     main = 'VARIAVEL TARGET',
     cex.axis= 1.2, cex.main= 1.2,
     ylim = c(0,600), col = 'darkorange', border = 'darkorange4') 

# Apresentando as distribuicoes univariadas das variaveis explicativas
par(oma = c(2,3,0,0), mar = c(3,3,2,1), mfrow = c(2,4)) # parametros graficos

barplot(table(DATA_REG$AGE),
        main = 'AGE',  cex.main = 1.0, 
        cex.axis= 1.2, cex.names = 1.2,  
        ylim = c(0,600), col = 'chartreuse', border = 'chartreuse4')

barplot(table(DATA_REG$GENDER),
        main = 'GENDER', cex.main = 1.0, 
        cex.axis= 1.2, cex.names = 1.2,  
        ylim = c(0,600), col = 'chartreuse', border = 'chartreuse4')

barplot(table(DATA_REG$MARITAL),
        main = 'MARITAL STATUS', cex.main= 1.0, 
        cex.axis= 1.2, cex.names = 1.2,  
        ylim = c(0,600), col = 'chartreuse', border = 'chartreuse4')

hist(DATA_REG$INCOME, breaks = 8,
     main = 'INCOME', cex.main = 1.0, 
     cex.axis= 1.2,  
     ylim = c(0,250), col = 'dodgerblue', border = 'dodgerblue4')      

barplot(table(DATA_REG$CHILDREN),
        main = 'NUMBER CHILDREN', cex.main = 1.0, 
        cex.axis= 1.2, cex.names = 1.2,  
        ylim = c(0,500), col = 'cyan', border = 'darkcyan')           

barplot(table(DATA_REG$HISTORY),
        main = 'HISTORY PREV. SHOP.', cex.main = 1.0, 
        cex.axis= 1.2, cex.names = 1.2, 
        ylim = c(0,350), col = 'chartreuse', border = 'chartreuse4')

barplot(table(DATA_REG$CATALOGS),
        main = 'NUMBER CATALOGS', cex.main = 1.0,
        cex.axis= 1.2, cex.names = 1.2, 
        ylim = c(0,300), col = 'cyan', border = 'darkcyan')           

mtext(text="DISTRIBUIÃÇÃO DAS VARIAVEIS EXPLICATIVAS",
      side = 1, line = 0, outer = TRUE)
mtext(text="FREQUENCIA (#)", side = 2, line = 0, outer = TRUE)

graphics.off()

#--------------------------------------------------------------------------------#
# 2) Analise bivariada - distribuicao da variavel vs variavel target
par(oma = c(2,3,0,0), mar = c(3,3,2,1), mfrow = c(2,4)) # parametros graficos


boxplot(AMOUNT ~ AGE, data = DATA_REG,
        main = 'AGE', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(0,6500), col = 'chartreuse', border = 'gray20')

boxplot(AMOUNT ~ GENDER, data = DATA_REG,
        main = 'GENDER', cex.main= 1.0, cex.axis = 1.2,
        ylim = c(0,6500), col = 'chartreuse', border = 'gray20')

boxplot(AMOUNT ~ MARITAL, data = DATA_REG,
        main = 'MARITAL STATUS', cex.main = 1.0, cex.axis = 1.2,
        ylim = c(0,6500), col = 'chartreuse', border = 'gray20')

plot(DATA_REG$INCOME, DATA_REG$AMOUNT,
     main = 'INCOME', cex.main = 1.0,  cex.axis = 1.2, pch = 19, cex = 0.5,
     col = 'dodgerblue', ylim = c(0,6500))
abline(lm(AMOUNT ~ INCOME, data = DATA_REG), col = 'firebrick', lwd = 2)

plot(DATA_REG$CHILDREN, DATA_REG$AMOUNT,
     main = 'NUMBER CHILDREN', cex.main = 1.0,  cex.axis = 1.2, pch = 19, cex = 0.5,
     col = 'cyan', ylim = c(0,6500))
abline(lm(AMOUNT ~ CHILDREN, data = DATA_REG), col = 'firebrick', lwd = 2)

boxplot(AMOUNT ~ HISTORY, data = DATA_REG,
        main = 'HISTORY PREV. SHOP.', cex.main = 1.0, cex.axis = 1.2,
        ylim = c(0,6500), col = 'chartreuse', border = 'gray20')

plot(DATA_REG$CATALOGS, DATA_REG$AMOUNT,
     main = 'NUMBER CATALOGS', cex.main = 1.0,  cex.axis = 1.2, pch = 19, cex = 0.5,
     col = 'cyan', ylim = c(0,6500))
abline(lm(AMOUNT ~ CATALOGS, data = DATA_REG), col = 'firebrick', lwd = 2)

mtext(text="DISTRIBUICAO DAS VARIAVEIS EXPLICATIVAS vs TARGET",
      side = 1, line = 0, outer = TRUE)
mtext(text="AMOUNT SPENT (US$)", side = 2, line = 0, outer = TRUE)

graphics.off()

#--------------------------------------------------------------------------------#
# BASE DE DADOS PARA CLASSIFICACAO - Telco Churning
#--------------------------------------------------------------------------------#

# 9 variaveis no total:
#     8 variaveis explicativas (atributos, vars. independentes)
#         3 variaveis qualitativas
#               REGION | CABLETV | AUTODEBT | HISTORY
#         5 variaveis quantitativas
#               AGE | CLITIME | INCOME | EXPENSES | RESIDTIME
#     1 variavel resposta categorica (target, var. dependente)
#              CHURN

#--------------------------------------------------------------------------------#
# 1) Analise univariada - distribuicao da variavel

# Variavel resposta
table(DATA_CLAS$CHURN); prop.table(table(DATA_CLAS$CHURN))

barplot(table(DATA_CLAS$CHURN),
        main = 'CHURNING', cex.main= 1.2,
        cex.axis= 1.2, cex.names = 1.4, 
        ylim = c(0,1600), col = 'darkorange', border = 'darkorange4') 

# Apresentando as distribuicoes univariadas das variaveis explicativas
par(oma = c(2,3,0,0), mar = c(3,3,2,1), mfrow = c(2,4)) # parametros graficos

hist(DATA_CLAS$AGE, breaks = 8,
     main = 'AGE', cex.main = 1.0, 
     cex.axis= 1.2,  
     ylim = c(0,500), col = 'cyan', border = 'darkcyan')   

hist(DATA_CLAS$CLITIME, breaks = 8,
     main = 'TIME AS CLIENTE', cex.main = 1.0, 
     cex.axis= 1.2,  
     ylim = c(0,600), col = 'cyan', border = 'darkcyan')   

hist(DATA_CLAS$INCOME, breaks = 12,
     main = 'INCOME', cex.main = 1.0, 
     cex.axis= 1.2,  
     ylim = c(0,1500), col = 'dodgerblue', border = 'dodgerblue4')  

hist(DATA_CLAS$EXPENSES, breaks = 12,
     main = 'EXPENSES', cex.main = 1.0, 
     cex.axis= 1.2,  
     ylim = c(0,1000), col = 'dodgerblue', border = 'dodgerblue4')  

hist(DATA_CLAS$RESIDTIME, breaks = 12,
     main = 'TIME IN RESIDENCE', cex.main = 1.0, 
     cex.axis= 1.2,  
     ylim = c(0,400), col = 'cyan', border = 'darkcyan') 

barplot(table(DATA_CLAS$REGION),
        main = 'REGION', cex.main = 1.0, 
        cex.axis = 1.2, cex.names = 1.2, 
        ylim = c(0,800), col = 'chartreuse', border = 'chartreuse4')

barplot(table(DATA_CLAS$CABLETV),
        main = 'CABLE TELEVISION', cex.main = 1.0, 
        cex.axis = 1.2, cex.names = 1.2, 
        ylim = c(0,1500), col = 'chartreuse', border = 'chartreuse4')

barplot(table(DATA_CLAS$AUTODEBT),
        main = 'AUTO DEBT', cex.main = 1.0, 
        cex.axis = 1.2, cex.names = 1.2, 
        ylim = c(0,1500), col = 'chartreuse', border = 'chartreuse4')

mtext(text="DISTRIBUICAO DAS VARIAVEIS EXPLICATIVAS",
      side = 1, line = 0, outer = TRUE)
mtext(text="FREQUENCIA (#)", side = 2, line = 0, outer = TRUE)

graphics.off()

#--------------------------------------------------------------------------------#
# 2) Analise bivariada - distribuicao da variavel vs variÃ¡vel target
par(oma = c(2,3,0,0), mar = c(3,3,2,1), mfrow = c(2,4)) # parametros graficos


boxplot(AGE ~ CHURN, data = DATA_CLAS,
        main = 'AGE', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(0,80), col = c('cyan','darkcyan'), border = 'gray20')

boxplot(CLITIME ~ CHURN, data = DATA_CLAS,
        main = 'TIME AS CLIENTE', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(10,60), col = c('cyan','darkcyan'), border = 'gray20')

boxplot(INCOME ~ CHURN, data = DATA_CLAS,
        main = 'INCOME', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(10,50000), col = c('dodgerblue','dodgerblue4'), border = 'gray20')

boxplot(EXPENSES ~ CHURN, data = DATA_CLAS,
        main = 'EXPENSES', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(10,6000), col = c('dodgerblue','dodgerblue4'), border = 'gray20')

boxplot(RESIDTIME ~ CHURN, data = DATA_CLAS,
        main = 'TIME IN RESIDENCE', cex.main = 1.0, cex.axis = 1.2, 
        ylim = c(0,15), col = c('dodgerblue','dodgerblue4'), border = 'gray20')

barplot(prop.table(table(DATA_CLAS$CHURN,DATA_CLAS$REGION),margin = 2),
        main = 'REGION', cex.main= 1.0,
        cex.axis = 1.2, cex.names = 1.2, 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_CLAS$CHURN,DATA_CLAS$CABLETV),margin = 2),
        main = 'CABLE TELEVISION', cex.main= 1.0,
        cex.axis = 1.2, cex.names = 1.2, 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

barplot(prop.table(table(DATA_CLAS$CHURN,DATA_CLAS$AUTODEBT),margin = 2),
        main = 'AUTO DEBT', cex.main= 1.0,
        cex.axis = 1.2, cex.names = 1.2, 
        ylim = c(0,1), col = c('chartreuse','chartreuse4'), border = 'gray20')

mtext(text="DISTRIBUICAO DAS VARIAVEIS EXPLICATIVAS vs TARGET",
      side = 1, line = 0, outer = TRUE)
mtext(text="VALORES & FREQUENCIA (%)", side = 2, line = 0, outer = TRUE)

graphics.off()

#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#
# FIM
#--------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------#