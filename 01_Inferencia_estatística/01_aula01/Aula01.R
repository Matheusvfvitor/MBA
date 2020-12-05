## 1a Aula de Inferencia Estatística
#Operações Aritméticas báscias

1+2
2+3
1-3
8/4
8*2
2^5
10 %% 3 #Mod ou resto da divisão
10 %/% 3 #parte inteira da divisão

#variáveis 
a = 2 
b = 3
c = a+b
c
m = "Matheus"

notas = c(10,9,9.5,7,4.5,8,7.5,8.5,9,5.5) #Vetor de 1 Dimensão

mean(notas) #média
sd(notas) #desvio Padrão
sum(notas) #soma
length(notas)#comprimento

notas + 1
notas ^ 2
sqrt(notas)

#Acesso aos valores individuais de notas

notas[2] #segundo índices
notas[2:9] # do segundo índice até o nono

notas
notas[5] = 6 #substituindo valores
notas

notas[-3] #todos os valores exceto o 3

#Variáveis com linha e coluna (Matrizes) Data Frames

notas = c(10,9,9.5,7,4.5,8,7.5,8.5,9,5.5)
nomes = c("Matheus","Vinicius","Felipe","Vitor","Luana","Vanessa","Santos",
          "Damião","Rosinei","Donivaldo")
sexo =c("m","m","m","m","f","f","f","m","f","m")

turma1 = data.frame(aluno = nomes,
                    genero = sexo,
                    pr.parcial = notas)

View(turma1) #Visualizar o data frame
library(dplyr) #Biblioteca de Manipulação de Data Frames (tibbles)

turma1 = tibble (aluno = nomes, genero = sexo, pr.parcial = notas)
turma1

#Trocar dados entre R e o ambiente
write.csv2(turma1,"turma1.csv",row.names = FALSE) #Salvar os dados no Excel

read.csv2("turma1.csv") -> turma1b #Ler dados em Excel
turma1b

#Visualização dos dados
install.packages("ggplot2")

library(ggplot2)
mpg
?mpg

#Tomando por base os valores presentes no data frame mpg,
#podemos dizer que carros com motor maior consomem mais combustível ?

View(mpg)
names(mpg)

#Motor maior -> tamanho do motor -> Cilindrada
summary(mpg$displ) #variável contínua (float)

#Consumo maior -> menor no. de milhas (km) por galão (l)
#Consumo maior -> menor desempenho -> meno hwy

summary(mpg$hwy) #Variável Contínua

#Gráfico de pontos (dispersão) no R básico
plot(mpg$displ, mpg$hwy)


#Gráfico de pontos (dispersão) através do ggplot2
ggplot(mpg,aes(x=displ,y=hwy)) + geom_point()


#Apesar de no geral a tendencia ter sido confimada existem pontos que
#nitidamente estão fora dela ? Em outras palavras Outliers

ggplot(mpg,aes(x=displ, y=hwy))+
  geom_point(color = 'blue')

ggplot(mpg, aes(x=displ, y=hwy))+
  geom_point(aes(color=class))

#Visualizar um modelo de previsão do hwy a partir do displ
ggplot(mpg, aes(x=displ, y = hwy))+
  geom_point(aes(color=class))+
  geom_smooth()

ggplot(mpg, aes(x=displ, y = hwy))+
  geom_point(aes(color=class))+
  geom_smooth(method = "lm", formula = 'y~x')

unique(mpg$drv)
ggplot(mpg, aes(x=displ, y = hwy))+
  geom_point(aes(color = class))+
  geom_smooth(method = "lm",formula = 'y~x',aes(lty = drv))


# Exercícios para treino
# Capítulo 3 - Data Visualization
# https://r4ds.had.co.nz/transform.html


# Será que apartir de uma certa cilindrada o aumento de tamanho do motor passa
# a ser feito pelo aumento do número de cilindros ?
ggplot(mpg, aes(x=displ, y = hwy))+
  geom_point(aes(color = class, 
                 size = cyl))+
  geom_smooth(method = "lm", formula = 'y~x', aes(lty = drv))

#Dividir o gráfico com todos os seus detalhes um para cada fabricante
ggplot(mpg, aes(x=displ, y = hwy))+
  geom_point(aes(color = class, size = cyl))+
  geom_smooth(method="lm", formula = 'y~x', aes(lty = drv))+
  facet_wrap(~manufacturer)

#Tipos de variáveis
#Numéricas
  #Contínuas : Com Vírgula
  #Numéricas : Discretas

#Categóricas
  #Nominais - não admitem ordenação
  #Ordinais - admitem ordenação

#Aula 02
#Filtragem de dados em tabelas (dplyr)
#Mesclagem de tabelas (dplyr)
#Ajuste de formato em Tabelas (dplyr)


#Dplyr em uma tabela
dados <- tibble(cor = c("azul", "preto", "preto", "azul", "azul"),
                valor = c(1,2,3,4,5))
dados

#filter, select, arrage, mutate, group_by, summarise

#Filtragem de linhas
filter(dados, cor == "azul")
filter(dados, valor > 2)
filter(dados, cor != "azul") #Operador Not
filter(dados, cor == "preto" & valor> 2) #Operador E
filter(dados, cor == "azul" | valor < 3) #Operador OU
filter(dados, cor %in% c("azul", "vermelho", "verde")) #Operador In

#SELECAO DE COLUNAS
select(dados, cor)
select(dados, -cor)
select(dados, valor, cor)
teste = "cor"

select(dados, !!teste)
select(dados, cor:valor)

#Ordenação de Colunas
arrange(dados,cor)
arrange(dados,desc(valor))
arrange(dados, -valor) #Só funciona para númericos
arrange(dados, cor,desc(valor))

#Criaçaõ de Colunas
mutate(dados, dobro = 2*valor)
mutate(dados, nível = ifelse(valor > 3,"alto","baixo"))
mutate(dados, resultado = (cor == "azul"))
mutate(dados, atraso1 = lag(cor,1,"nenhum"))
mutate(dados, atraso2 = lag(cor,2))
mutate(dados, avanço1 = lead(valor, 1))
mutate(dados, avanço1 = lead(valor, 2))

#Group by e Summarise - Agrupamento e Resumo de dados
summarise(dados, total = sum(valor))
group_by(dados, cor) -> dados2
dados2

summarise(dados2, total = sum(valor))
# Exercícios para treino
# Capítulo 5 - Data Transformation
# https://r4ds.had.co.nz/transform.html


#Dplyr em duas tabelas (Mesclagem de tabelas de dados)
x <- tibble(nome = c("John","Paul","Ringo","Harrison","Peter"),
            instrumento = c("Guitarra", "Baixo","Bateria","Guitarra","Teclado"))

y <- tibble(nome = c("John","Paul","Ringo","Harrison","Stwart","Davis"),
            sobrenome =c(TRUE, TRUE, TRUE,TRUE, FALSE, FALSE))

inner_join(x,y, by =c("nome" = "nome"))

left_join(x,y, by=c("nome" = "nome"))
left_join(y,x, by=c("nome" = "nome"))
right_join(x,y, by = c("nome"="nome"))

semi_join(x,y, by=c("nome" = "nome"))
semi_join(y,x, by=c("nome" = "nome"))

anti_join(x,y, by=c("nome" = "nome"))
anti_join(y,x, by=c("nome" = "nome"))

full_join(x,y, by =c("nome"="nome"))

#Formas de Carregamento de Dados

#1 - Leitura de .csv
  #read.csv()
  #read.csv2()
  #fread <- pacote 'data.table' MUUITO + RÁPIDO

#2 - Leitura por RODBC
  #library(RODBC)
  #conn <- odbcConnectExcel2007("arquivo.xlsx")
  #dados <- sqlFetch(conn, "planilha")
  #odbcClose(conn)
  #Prático, porém MUUUUIIITO lento
  
#O que é o Formato "tidy" ?
#Cada coluna representa apenas uma variável
#Cada linha representa um registro independente
#Cada célula (linha x coluna) contém um único valor

table1
table2
spread(table2, type, count)

table4a
gather(table4a, "year",  cases, '1999','2000') -> table4a2
table4a2

table4b
gather(table4b,"year", population, c('1999':'2000')) -> table4b2
table4b2

inner_join(table4a2, table4b2, by = c("country" = "country", "year" = "year"))

#Modificação de colunas Separate , Unite
table3

separate(table3,rate,into=c("cases","population"),"/") -> table3b
mutate(table3b, cases = as.numeric(cases), population = as.numeric(population))

table5
table5b = table5 %>% unite(ano, century, year, sep="") %>% 
  separate(rate, into=c("cases", "population"), sep="/") %>% 
  mutate( cases = as.numeric(cases), population = as.numeric(population))

table5b

# Exercícios para treino 
# Capítulo 12 - Tidy Data
# https://r4ds.had.co.nz/transform.html


#Estrutura Avançada de dados - Introdução aos modelos estatísticos. Capítulo 23, 24,25
#(modelr) (group_nest) (listas) (apply)
#(listas, cap.20 - vector) (apply, cap.21-iteração)


install.packages(modelr)
library(modelr)

#apply
dados <- tibble(col1 = c(1,2,3,4,5),
               col2 = c(10,20,30,40,50),
               col3 = c(100,200,300,400,500))

#Como tirar a média de todas as colunas do data frame

apply(dados, 2 , mean)
apply(dados,1,sum)
apply(dados, 2, function(x){mean(x)})

#Fazendo o mesmo para a função que está sendo aplicada as linhas do dataframe dados

apply(dados, 1, function(x){sum(x)})
dados
apply(dados, 2, function(x){sum(x)})

minhamedia <- function(x){
  media = mean(x)
  return(media)
}

apply(dados, 2, minhamedia)
apply(dados, 1, minhamedia)

#Listas
a<-1
b<- c("José", "Maria")
d<- dados

minha_lista = list(a,b,d)
minha_lista

minha_lista[1] #lista de um elemento
minha_lista[2]
minha_lista[3]

minha_lista[[1]] #lista como elemento de lista
minha_lista[[2]]
minha_lista[[3]]
  
  

































