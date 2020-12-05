#install.packages(c("fpp", "fpp2", "readxl", "mFilter","TSA", "tseries"))
library(fpp)
library(fpp2)
library(readxl)
library(mFilter)
library(gridExtra)
library(tseries)

df <- read_excel("ibc.1.xlsx")
head(df)
df <- df[,-1]

ts.ibc <- ts(df, frequency=12, start=c(2003,1))
ts.ibc

g1 <- autoplot(ts.ibc, xlab = "Mês", ylab="IBC-BR", main = "IBC-BR")

#Correlograma

g2 <- ggAcf(ts.ibc)

grid.arrange(g1,g2 , nrow = 2, ncol = 1)

g2$data


################################################

t.ibc <- 1:132

det_ibcts <- ts(resid(lm(ts.ibc~t.ibc)))

d1 <- autoplot(det_ibcts, xlab="mês", ylab="IBC_BR", main = "Depois da remoção de Tendência")

d2 <- autoplot(diff(ts.ibc),xlab="mês", ylab="IBC_BR", main = "Depois da Diferenciação" )

e1 <- ggAcf(det_ibcts) + ggtitle("Depois da remoção de Tendência")

e2 <- ggAcf(diff(ts.ibc))+ ggtitle(" Depois da Diferenciação")

grid.arrange(d1,d2,e1,e2, nrow=2, ncol =2)

# Testes de Estacionariedade

library(urca)

# Dado que a série é mensal, é usual definir o número máximo 
# de defasagens (lags) em 12 porém, por conhecimento prévio da série,
# o definimos em 5. Ver adf.test()
# Por último, será utilizado o critério AIC (selectlags) para seleção do
# valor final de defasagens

#Revisitando a série

autoplot(ts.ibc, xlab = "Mês", ylab="IBC-BR", main = "IBC-BR")

#### Teste ADF ----

adf.test(ts.ibc) #

adf_ibc<- ur.df(ts.ibc, type = "trend", lags = 5, selectlags = "AIC")

summary(adf_ibc)

summary(adf_ibc)@teststat

summary(adf_ibc)@cval

# Tendo em mente o significado de cada estatística de teste, 
# as evidências sugerem a rejeição da hipótese nula de não estacionariedade
# a, pelo menos, 5%  para a principal estatística tau3
# Valor da estatística: - 3.482731
# Valor Crítico a 5%  : - 3,43

#### Teste KPSS ----

# Para o teste KPSS, a diferença em relação à especificação do ADF 
# é o valor short do parâmetro de defasagens, que escolhe o valor da
# defasagem baseado no tamanho da série.

# A ESTATISTICA DO TESTE é de 0,131
# A 10%, O VALOR CRÍTICO É 0,119 MENOR QUE A ESTATÍSTICA. PORTANTO
# SUGERE A REJEIÇÃO DA HIPÓTESE DE ESTACIONARIDADE. NO ENTANTO A 5%
# NÃO PODEMOS REJEITAR

kpss_ibc<- ur.kpss(ts.ibc, type = "tau", lags = "short")
summary(kpss_ibc)
summary(kpss_ibc)@teststat
summary(kpss_ibc)@cval

checkresiduals(ts.ibc)

 #### Teste de Phillips-Perron ----

# É NOTÁVEL A DIFERENÇA ENTRE A ESTATÍSTICA DO TESTE E 
# OS VALORES CRÍTICOS DO P.P.

pp_ibc<- ur.pp(ts.ibc, type = "Z-tau", lags = "short")
summary(pp_ibc)@teststat
summary(pp_ibc)@cval

pp <- ur.pp(diff(ts.ibc))
summary(pp)
        

#### Teste DF-GLS ----
gls_ibc<- ur.ers(ts.ibc, type = "DF-GLS", model = "trend" ,lag.max = 5)
summary(gls_ibc)@teststat
summary(gls_ibc)@cval

#### Teste Zivot-Andrews
za_ibc <- ur.za(ts.ibc, model="both", lag=5)
summary(za_ibc)

# Concluindo a evidência estatística é de que o IBC-BR é um processo de raiz
# unitária com drift.

library(urca)
ts.ibc%>% diff()%>%ur.kpss%>% summary()




