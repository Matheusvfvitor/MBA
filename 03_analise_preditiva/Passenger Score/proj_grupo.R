install.packages("dplyr")
install.packages("summarytools")
install.packages("caret")
install.packages("Metrics")
install.packages("gmodels")
install.packages("hmeasure")
install.packages("pROC")
install.packages("readr")
install.packages("lubridate")
install.packages("ggplot2")
install.packages("rpart")
install.packages("lattice")
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
T_GRP_Passenger_Satisfaction_base_grupo <- read_excel("T_GRP_Passenger Satisfaction_base_grupo.xlsx")
View(T_GRP_Passenger_Satisfaction_base_grupo)

base_1 = T_GRP_Passenger_Satisfaction_base_grupo[complete.cases(T_GRP_Passenger_Satisfaction_base_grupo), ]

base_2 <- mutate(base_1,Satisfaction=ifelse(Satisfaction=='satisfied',1,0))

## ARVORE DE DECISÃO ##
set.seed(123)
INDEX_TRAIN <- createDataPartition(base_2$Satisfaction ,p = 0.7,list=F)
TRAIN_SET <- base_2[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- base_2[-INDEX_TRAIN,] # base de teste: 30%

MDL_FIT <- rpart(Satisfaction ~.,
                 data = TRAIN_SET,
                 method = 'anova',
                 control = rpart.control(minbucket = 10, cp = -1))
??rpart.control
MDL_FIT
summary(MDL_FIT)
plotcp(MDL_FIT)
PARM_CTRL <- MDL_FIT$cptable[which.min(MDL_FIT$cptable[,"xerror"]),"CP"]
MDL_FIT.PRUNE <- prune(MDL_FIT, cp = PARM_CTRL)
MDL_FIT.PRUNE
summary(MDL_FIT.PRUNE)
printcp(MDL_FIT)
plotcp(MDL_FIT.PRUNE)

install.packages("rattle")
library(rattle)
fancyRpartPlot(MDL_FIT.PRUNE)
install.packages("rpart.plot")
library(rpart.plot)

Y_VAL_TRAIN <- predict(MDL_FIT.PRUNE) 
Y_VAL_TEST  <- predict(MDL_FIT.PRUNE, newdata = TEST_SET)

postResample(pred = Y_VAL_TRAIN, obs = TRAIN_SET$Satisfaction)
postResample(pred = Y_VAL_TEST,  obs = TEST_SET$Satisfaction)

MDL_FINAL <- MDL_FIT.PRUNE
varImp(MDL_FINAL)

##Regressão Logística

MDL_FIT <- lm(Satisfaction ~ ., data = TRAIN_SET)
MDL_FIT

summary(MDL_FIT)

library(car)
vif(MDL_FIT)
?vif

library(MASS)
MDL_FIT.STEP <- stepAIC(MDL_FIT,direction = 'both', trace = TRUE)
?stepAIC
MDL_FIT.STEP

summary(MDL_FIT.STEP)

Y_VAL_TRAIN <- predict(MDL_FIT) 
Y_VAL_TEST  <- predict(MDL_FIT, newdata = TEST_SET)

Y_VAL_TRAIN.STEP <- predict(MDL_FIT.STEP) 
Y_VAL_TEST.STEP  <- predict(MDL_FIT.STEP, newdata = TEST_SET)

postResample(pred = Y_VAL_TRAIN, obs = TRAIN_SET$Satisfaction)
postResample(pred = Y_VAL_TEST,  obs = TEST_SET$Satisfaction)

postResample(pred = Y_VAL_TRAIN.STEP, obs = TRAIN_SET$Satisfaction)
postResample(pred = Y_VAL_TEST.STEP,  obs = TEST_SET$Satisfaction)

MDL_FINAL <- MDL_FIT.STEP
varImp(MDL_FINAL) 

##Ensemble - Adaboost

prop.table(table(TRAIN_SET$Satisfaction));prop.table(table(TEST_SET$Satisfaction))

set.seed(123)
base_parc=filter(base_2,id < 30000) ##FILTRANDO 25% DA BASE PARA RODAR O ADABOOST,
                                    ## DEVIDO A DIFICULDADE NO PROCESSAMENTO.

INDEX_TRAIN <- createDataPartition(base_parc$Satisfaction ,p = 0.7,list=F)
TRAIN_SET <- base_parc[INDEX_TRAIN, ] # base de desenvolvimento: 70%
TEST_SET  <- base_parc[-INDEX_TRAIN,] # base de teste: 30%

library(adabag)

TRAIN_SET$Satisfaction <- as.factor(TRAIN_SET$Satisfaction)
TEST_SET$Satisfaction <- as.factor(TEST_SET$Satisfaction)

MDL_FIT <- boosting(Satisfaction ~ ., 
                    data      = TRAIN_SET, 
                    mfinal    = 10,
                    coeflearn = "Breiman", 
                    control   = rpart.control(maxdepth = 1))
??boosting

MDL_FIT

plot(errorevol(MDL_FIT, TRAIN_SET))

MDL_FIT <- boosting(Satisfaction ~ ., 
                    data      = TRAIN_SET, 
                    mfinal    = 50,
                    coeflearn = "Breiman", 
                    control   = rpart.control(maxdepth = 1))


Y_PROB_TRAIN <- predict.boosting(MDL_FIT, TRAIN_SET)$prob[,2]
Y_PROB_TEST  <- predict.boosting(MDL_FIT, TEST_SET)$prob[,2]

head(Y_PROB_TRAIN) 

library(hmeasure) 
HMeasure(TRAIN_SET$Satisfaction,Y_PROB_TRAIN)$metrics

HMeasure(TEST_SET$Satisfaction, Y_PROB_TEST)$metrics
MDL_FINAL <- MDL_FIT

importanceplot(MDL_FINAL)


