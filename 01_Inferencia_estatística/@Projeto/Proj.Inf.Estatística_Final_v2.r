library(dplyr)
library(tidyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(treemap)

getwd()
setwd("C:/Users/mathe/OneDrive - ESI Group/Matheus/MBA_Inf_Estatistica")

df_account = read.table('account.asc',header=T,sep=";")
df_card = read.table('card.asc',header=T,sep=";")
df_client = read.table("client.asc",header=T,sep=";")
df_disp = read.table("disp.asc",header=T,sep=";")
df_district = read.table("district.asc",header=T,sep=";")
df_loan = read.table('loan.asc',header=T,sep=";")
df_order = read.table('order.asc',header=T,sep=";")
df_trans = read.table('trans.asc',header=T,sep=";")

today = as.Date(ymd('1999-12-31'))
is.Date(today) 

df_account = df_account %>% mutate('frequency' = case_when(df_account$frequency == 'POPLATEK MESICNE' ~ 'monthly issuance',
                                                             df_account$frequency == 'POPLATEK TYDNE'~ 'weekly issuance',
                                                             df_account$frequency == 'POPLATEK PO OBRATU' ~ 'issuance after transaction'))


head(df_account)

#transformar a coluna Date de int para Data
df_account$date = as.character(df_account$date)
df_account <- df_account %>% mutate('date' = ymd(df_account$date))

head(df_account)

#Organizar a tabela District
colnames(df_district) = c('district_id',
                          'district_name',
                          'region',
                          'no._of_habs',
                          'municipalities<499',
                          'municipalities<500-1999',
                          'municipalities<2000-9999',
                          'municipalities>10000',
                          'no._of_cities',
                          'ratio_of_urban habs',
                          'average_salary',
                          'unemploymant_rate_95',
                          'unemploymant_rate_96',
                          'enterpreneurs_per_1000 habs',
                          'crimes_95',
                          'crimes_96')

#Substituindo o district_id pelo nome do distrito
df_account <- left_join(df_account, df_district, by = 'district_id')
df_account <- df_account %>% select(account_id:region,average_salary)
df_account$district_id = NULL

head(df_account)

df_account <- df_account %>% mutate('account_age' = round((today - date)/360))

head(df_account)

#Definindo o tema dos gráficos.
my_theme = theme_light()+theme(axis.text.x = element_text(angle = 90), 
                                  axis.title = element_text(color="darkblue"),
                                  title = element_text(color = "darkblue"))

#freq issue
ggplot(df_account, aes(frequency))+
  geom_bar(fill = '#1d3557')+
  ggtitle('Frequency of issue visualization')

#Contas por Distrito
ggplot(df_account, aes(district_name))+
    geom_bar(fill = '#1d3557')+
    ggtitle('Frequency of issue visualization')+
    my_theme

#Contas treemap por distrito
district_df = df_account %>% group_by(district_name) %>% summarise('count' = n())
group = district_df$district_name
value = district_df$count

data = data.frame(group,value)
treemap(data,index="group",vSize="value",type="index", 
        title.legend = "Accounts per District", 
        title = "Accounts per District")

#Chart por region
ggplot(df_account, aes(region))+
  geom_bar(fill = '#1d3557')+
  ggtitle('Frequency of issue visualization')+
  my_theme

#treemap por região
df_region = df_account %>% group_by(region) %>% summarise('count' = n())
group = df_region$region
value = df_region$count
data = data.frame(group,value)
treemap(data,index="group",vSize="value",type="index", 
        title.legend = "Accounts per Region", 
        title = "Accounts per Region")

#Data de Abertura
df_date = df_account %>% group_by(date) %>% summarise('cont' = n())
df_date = df_date %>% mutate('accum' = cumsum(cont))

ggplot(df_date, aes(x = date, y = accum))+
  geom_line()+
  ggtitle('Abertura de contas por ano')+
  labs(x = 'Data', y= '# Contas abertas')

#1) Convertemos para String (Caracteres) os valores da coluna birth_number.
df_client = read.table("client.asc",header=T,sep=";")

#1) Convertemos para String (Caracteres) os valores da coluna birth_number.
df_client$birth_number = as.character(df_client$birth_number)

#2) Criamos a coluna mês com retirando os valores do centro da coluna, caracteres 3 e 4
df_client = df_client %>% mutate('mes' = str_sub(birth_number,3,4))

#3) Criamos a coluna 'sexo' fazendo com a lógica : Se o mês for maior que 12 é feminino caso contrário é masculino
df_client = df_client %>% mutate('sexo' = ifelse(mes > 12 , 'f', 'm'))

#4) Alteramos os valores da coluna mes para número
df_client$mes = as.numeric(df_client$mes)

#5) Retiramos o mês de aniversário da pessoa, com a formula : se mês > 12 , mes = mes - 50
df_client = df_client %>% mutate('mes' = ifelse(mes > 12 , mes - 50, mes))

#6) Criamos a coluna mês2 , para deixarmos todos valores com dois digitos de caracteres, por exemplo para o mês 2 : de 2 para 02
df_client = df_client %>% mutate('mes2' = ifelse(mes < 10, paste('0',mes,sep =''),mes))


#7) Concatenamos , dois primeiros dígitos da birth_number , mes2 (criado no passo acima), dois últimos dígitos da coluna birth_number, com separador vazio (")
df_client = df_client %>% mutate('birth' = paste('19',str_sub(birth_number,1,2),mes2,str_sub(birth_number,5,6), sep=''))

#8) Criamos a coluna birthdate transformando a string concatenada acima em tipo Date
df_client = df_client %>% mutate('birthdate' = ymd(df_client$birth))

#9) Retirmos todas as colunas intermediárias montadas nos passos acima.
df_client = df_client %>% select(client_id,district_id,sexo,birthdate)

#10) Alteramos o nome da coluna 'sexo' para 'gender' assim deixaremos todas em um padrão.
df_client = df_client %>% rename('gender' = sexo)

head(df_client)

# Fazendo o Join do dataset para trazer o distrito e região de cada cliente.
df_client = left_join(df_client, df_district, by = 'district_id')
df_client <- df_client %>% select(client_id:region)
df_client$district_id = NULL
df_client$mes = NULL
df_client$mes2 = NULL
df_client$birth_number = NULL
df_client$birth = NULL

ggplot(df_client, aes(x=gender, fill = gender))+
  geom_bar()+
  ggtitle("Total de Contas por Sexo")

#Gráfico Donut para sexo
df_client_gender = df_client %>% group_by(gender) %>% summarise('cont' = n())

#Gráfico de Donut
category <- df_client_gender$gender
count = df_client_gender$cont
data = data.frame(category,count)

# Computando as porcentagens
data$fraction <- data$count / sum(data$count)
# Computando as porcentagens acumuladas (top of each rectangle)
data$ymax <- cumsum(data$fraction)
# Computando os limites superiores de cada "retângulo"
data$ymin <- c(0, head(data$ymax, n=-1))
# Computando os labels de posição
data$labelPosition <- (data$ymax + data$ymin) / 2
# Compute as categorias
data$label <- paste0(data$category, "\n value: ", data$count)
# Construindo o gráfico.

ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
  scale_fill_brewer(palette=4) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  ggtitle("Contas abertas por sexo")+
  theme(legend.position = "none")

donut = function(category, count,title){
  data = data.frame(category,count)
  # Compute percentages
  data$fraction <- data$count / sum(data$count)
  # Compute the cumulative percentages (top of each rectangle)
  data$ymax <- cumsum(data$fraction)
  # Compute the bottom of each rectangle
  data$ymin <- c(0, head(data$ymax, n=-1))
  # Compute label position
  data$labelPosition <- (data$ymax + data$ymin) / 2
  # Compute a good label
  data$label <- paste(data$count)
  
  ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
    geom_rect() +
    geom_label( x=3.5, aes(y=labelPosition, label=label), size=6) +
    scale_fill_brewer(palette=4) +
    coord_polar(theta="y") +
    xlim(c(2, 4)) +
    theme_void()+
    ggtitle(title)
}

df_client_gender = df_client %>% group_by(gender) %>% summarise('cont' = n())

#Gráfico de Donut
category <- df_client_gender$gender
count = df_client_gender$cont
data = data.frame(category,count)
donut(category= category, count = count,"Contas Abertas por sexo")

df_client_region = df_client %>% group_by(region) %>% summarise('count' = n())
category <- df_client_region$region
count <- df_client_region$count
data = data.frame(category,count)
donut(category, count,"Contas Abertas por Região")


ggplot(df_client,aes(x = region, fill = gender))+
  geom_bar()+
    my_theme+
    ggtitle("Contas por região e por Sexo")


df_trans = read.table('trans.asc',header=T,sep=";")
unique(df_trans$type)

#Avaliando uma conta apenas
c2378 <- df_trans %>% filter(account_id == '2378')

c2378

#Alterando as labels do tipo 
df_trans$date = ymd(df_trans$date)
df_trans = df_trans %>% mutate('type' = case_when(df_trans$type == 'PRIJEM' ~ 'credit',
                                                  df_trans$type == 'VYDAJ' ~'withdraw',
                                                  df_trans$type == 'VYBER' ~'withdraw in Cash'
                                                  ))

#Alterando as labels de Operation
df_trans = df_trans %>% mutate('operation' = case_when(df_trans$operation == 'VYBER KARTOU' ~ 'credit card withdrawal',
                                                       df_trans$operation == 'VKLAD' ~'credit in cash',
                                                       df_trans$operation == 'PREVOD Z UCTU' ~ 'collection from another bank',
                                                       df_trans$operation == 'VYBER' ~ 'withdrawal in cash',
                                                       df_trans$operation == 'REVOD NA UCET' ~ 'remittance to another bank'))

#alterando as labels de ksymbol
df_trans = df_trans %>% mutate('k_symbol' = case_when(df_trans$k_symbol == 'POJISTNE' ~ 'insurrance payment',
                                                      df_trans$k_symbol == 'SLUZBY' ~'payment for statement',
                                                      df_trans$k_symbol == 'UROK' ~ 'interest credited',
                                                      df_trans$k_symbol == 'SANKC. UROK' ~ 'sanction interest if negative balance',
                                                      df_trans$k_symbol == 'SIPO' ~ 'household',
                                                      df_trans$k_symbol == 'DUCHOD' ~ 'old age pension ',
                                                      df_trans$k_symbol == 'UVER' ~ 'loan payment'))

df_trans

#criando um dataframe com os negativados 
negatives = df_trans %>% filter(balance < 0)

#criando uma coluna para as retiradas
df_trans = df_trans %>% mutate('withdraw' = ifelse(type == 'withdraw' | type == 'withdraw in Cash', amount, 0))

#criando uma coluna para os créditos
df_trans = df_trans %>% mutate('credit' = ifelse(type == 'credit',amount,0))

#criando uma coluna de avaliação do balanço / positivo de negativo
df_trans <- df_trans %>% mutate('negative' = ifelse(balance < 0, TRUE, FALSE))

#criando um filro 
df <- df_trans %>% filter (negative == TRUE)
df2 <- df %>% group_by (account_id) %>% summarise('firstNegativeDate' = min(date), 'LastNeagtiveDate' = max(date)) 

#criando um filtro
last_status = df_trans %>% group_by(account_id) %>% summarise (last_balance = last(balance), 
                                                               total_retiradas = sum(withdraw),
                                                               total_credit = sum(credit),
                                                               first_trans = min(date),
                                                               n_negatives = sum(negative))

last_status <- left_join(last_status, df2, by = "account_id")
c2 = df_trans %>% filter(account_id == 2)

last_status = last_status %>% mutate('negative' = ifelse(last_balance < 0 , TRUE, FALSE))
negatives = last_status %>% group_by(negative) %>% summarise ('totals' = n())
category <- c('negatives', 'positives')
count <- negatives$totals
data = data.frame(category, count)
donut(category = category, count = count, "Proporção de contas negativas")                                             

df_trans = df_trans %>% mutate(year = year(date))
new_df = df_trans %>% group_by(type, year) %>% summarise('total' = sum(amount))

ggplot(new_df, aes(year, y = total, color = type))+
  geom_line()

#Saídas
ggplot(new_df, aes(year, y = total, fill = type))+geom_col()

#balanço
balance = df_trans %>% group_by(year) %>% summarise('balance' = sum(balance))
  ggplot(balance, aes(year, y = balance))+geom_col()

credit = new_df %>% filter(type == 'credit')
ggplot(credit, aes(x = year, y=total))+geom_col()

withdraw = new_df %>% filter(type == 'withdraw' | type=="withdraw in Cash")
ggplot(withdraw, aes(x = year, y=total, fill = type))+geom_col()

#filtrando e fazendo um df de crédito
df_ksymbol %>% filter(type == 'credit') %>% ggplot(aes(x = year, y = totals, fill = k_symbol))+
  geom_col()

#Avaliação do Ksymbol e pelo ano
df_ksymbol = df_trans %>% group_by(type, year, k_symbol) %>% summarise('totals' =  n())

df_ksymbol %>% drop_na() %>% filter(type == 'credit') %>% ggplot(aes(x = year, y = totals, fill = k_symbol))+
  geom_col()

df_ksymbol %>% filter(type == 'withdraw' | type == 'withdraw in Cash') %>% ggplot(aes(x = year, y = totals, fill = k_symbol))+
  geom_col()

df_ksymbol %>% drop_na() %>% filter(type == 'withdraw' | type == 'withdraw in Cash') %>% ggplot(aes(x = year, y = totals, fill = k_symbol))+
  geom_col()

# --- Analisando a tabela Credit Card
df_card = read.table('card.asc',header=T,sep=";")
df_card$issued = as.character(df_card$issued)
df_card$issued = ymd_hms(df_card$issued)
df_disp$disp_id == df_disp$client_id
num_clientes = (unique(df_disp$client_id))
length(num_clientes)
length(df_disp$client_id)
length(df_disp$account_id)
length(unique(df_disp$account_id))
#Como o Account ID se repete, existem clientes com mais de uma conta.
#Cada cliente tem um único , disp_id e um único client_id
length(df_disp$disp_id)
length(unique(df_disp$disp_id))

#Trazendo o client_id de cada cliente com base no disp
df <- left_join(df_card, df_disp, by="disp_id")
df <- df %>% rename('card_type' = type.x)
df <- df %>% rename('disp_type' = type.y)
df <- left_join(df, df_client, by ='client_id')
df <- left_join(df, df_account, by = c("account_id" = "account_id", 'region' = 'region', 'district_name' = 'district_name'))
df <- left_join(df, last_status, by = "account_id")

df_card_final <- df
ggplot(df_card_final, aes(x = card_type, fill = gender))+geom_bar()

ggplot(df_card_final, aes(x=gender, fill = card_type))+geom_bar()

#gráfico de donut
df_gender_card <- df_card_final %>% group_by(gender) %>% summarise('totals' = n())
category <- df_gender_card$gender
count <- df_gender_card$totals
data = data.frame(category, count)
donut(category = category, count = count,"Total de cartões de crédito por sexo")

head(df_client)
df_client_gender = df_client %>% group_by(gender) %>% summarise ("Total" = n())

df_client_gender

#Gráfico Treemap de cartões por região
df_tree_card = df_card_final %>% group_by(region) %>% summarise ('total' = n())

#Criando função para facilitar criação de chart Treemap.
tree_chart = function(group,values,chart_title, type = 1){
  data = data.frame(group,values)
  data$label = paste(data$group, ',', data$values)
  if(type == 0){
    treemap(data, index = c('label'), vSize = 'values', type = 'index',
            title = chart_title)
  }else{
    treemap(data, index = 'group', vSize = 'values', type = 'index',
            title = chart_title)
  }
  
}

tree_chart(df_tree_card$region,df_tree_card$total,"Total de Cartões por Região",0)

#Fazendo um gráfico de donut para visualizar a distribuição de cartões nas regiões.
category <- df_tree_card$region
count <- df_tree_card$total
data = data.frame(category, count)
donut(category, count,'Total de Cartões por região')

#Gráfico de Cartões por região e sexo
ggplot(df_card_final, aes(x = region, fill = gender))+
  geom_bar()+
  my_theme

#Verificando a quantidade de cartões emitidos ao longo do tempo
summary(df_card_final)
card_issued <- df_card_final %>% group_by(issued) %>% summarise('total' = n(), 'accum') %>% mutate('accum' = cumsum(total)) 

#Observando a tendência de emissão de cartão de crédito.
ggplot(card_issued, aes(x = issued, y=accum))+
  geom_line()

card_issued <- df_card_final %>% group_by(gender,issued) %>% summarise('total' = n()) %>% mutate('accum' = cumsum(total)) 
card_issued$issued = as.character(card_issued$issued)
card_issued$issued

card_by_year <- card_issued %>% mutate('year' = year(issued)) %>% group_by(year) %>% summarise('Total'=sum(accum))

card_by_year

#Podemos perceber a evolução da emissão de cartões de créditos por genero é quase que proporcional.
ggplot(card_by_year, aes(x = year, y=Total))+
  geom_line()

#vamos verificar os tipos de cartão
card_type <-  df_card_final %>% group_by(card_type) %>% summarise('total' = n())
category <- card_type$card_type
count <- card_type$total
data = data.frame(category, count)
donut(category,count,'Total de Cartões por Tipo')

#vamos verificar a influencia da balanço no tipo de cartão
today = as.Date(ymd('1999-01-01')) #Considerado o última dia de 1999
is.Date(today) 

df_card_final

df_card_final$issued = as.character(df_card_final$issued)
df_card_final$issued = as.Date(df_card_final$issued)
df_card_final <- df_card_final %>% mutate('age' = round((today - birthdate)/360), 'card_age' = (today-issued))
df <- df_card_final
df <- df %>% mutate ('age2' = today - birthdate)
df_card_final$card_age2 = NULL

ggplot(df_card_final, aes(x = total_credit, y=last_balance, color = card_type, size = last_balance, alpha = 0.5))+
  geom_point()

ggplot(df_card_final, aes(x = age, y = card_age, color = card_type, size = last_balance, alpha = 0.5))+
  geom_point()

ggplot(df_card_final, aes(x = age, y = card_age, color = card_type, size = last_balance, alpha = 0.5))+
  geom_point()+
  facet_wrap(~ gender)

ggplot(df_card_final, aes(x = last_balance, y = age, color = card_type))+
  geom_point()+
  geom_smooth(se = FALSE)+
  facet_wrap(~ gender)

df_card_final %>% filter(n_negatives > 0) %>%
ggplot(aes(x = last_balance, y = age, color = card_type))+
  geom_point()+
  facet_wrap(~ gender)

#Tivemos 42 pessoas que ficaram negativas e possuem cartões de créditos
df_card_negatives = df_card_final %>% filter(n_negatives > 0)

ggplot(df_card_negatives,aes(x = region, fill = gender))+
  geom_bar()+
  ggtitle('Pessoas que já ficaram devendo por região e sexo')+
  my_theme

ggplot(df_card_negatives,aes(x = region, fill = card_type))+
    geom_bar()+
    facet_wrap(~ gender)+
    ggtitle('Pessoas que já ficaram devendo por região e sexo')+
  my_theme

ggplot(df_card_negatives,aes(x = card_type, fill = gender))+
  geom_bar()+
  ggtitle('Pessoas que já ficaram devendo por região e sexo')+
  my_theme

ggplot(df_card_negatives,aes(x = card_type, fill = gender))+
  geom_bar()+
  facet_wrap(~region)+
  ggtitle('Pessoas que já ficaram devendo por região e sexo')+
  my_theme

df <- df_trans %>% group_by(account_id, year) %>% summarise('year_credit' = sum(credit), 'montly_issue' = sum(credit)/12)
df <- df %>% group_by(account_id) %>% summarise('yearlyprofits' = mean(year_credit), 'montly_revenue' = mean(montly_issue))

account_revenue <- df
df_card_final <- left_join(df_card_final, account_revenue, by = 'account_id')

ggplot(df_card_final)+
  geom_point(aes(x = last_balance, y = montly_revenue, color = card_type, shape = gender))

ggplot(df_card_final)+
  geom_point(aes(x = last_balance, y = montly_revenue, color = card_type, shape = gender ))+
  facet_wrap(~gender)

ggplot(df_card_final, aes(x = age, y = montly_revenue, color = card_type, shape = gender ))+
  geom_point()

ggplot(df_card_final, aes(x = age, y = montly_revenue, color = card_type, shape = gender))+
  geom_point()+
  facet_wrap(~region)

#avaliando o tipo do cartão pela receita mensal
ggplot(df_card_final, aes(x = card_type, y = montly_revenue))+
  geom_boxplot(outlier.colour = 'red')

#avaliando o tipo do cartão pela receita mensal
ggplot(df_card_final, aes(x = region, y = montly_revenue))+
  geom_boxplot(outlier.colour = 'red')+
  my_theme

#avaliando o tipo do cartão pelo sexo
ggplot(df_card_final, aes(x = gender, y = montly_revenue))+
  geom_boxplot(outlier.colour = 'red')+
  my_theme

##Vamos avaliar as pessoas que ja ficaram devendo alguma vez para o banco e o tipo de cartão que elas têm.
#Tivemos 42 pessoas que ficaram negativas e possuem cartões de créditos
df_card_negatives = df_card_final %>% filter(n_negatives > 0)

ggplot(df_card_negatives,aes(x = region, fill = gender))+
  geom_bar()+
  ggtitle('Pessoas que já ficaram devendo por região e sexo')+
  my_theme

ggplot(df_card_negatives,aes(x = region, fill = card_type))+
  geom_bar()+
  facet_wrap(~ gender)+
  ggtitle('Pessoas que já ficaram devendo por região e sexo')+
  my_theme

ggplot(df_card_negatives,aes(x = card_type, fill = gender))+
  geom_bar()+
  ggtitle('Pessoas que já ficaram devendo por região e sexo')+
  my_theme

ggplot(df_card_negatives,aes(x = card_type, fill = gender))+
  geom_bar()+
  facet_wrap(~region)+
  ggtitle('Pessoas que já ficaram devendo por região e sexo')+
  my_theme

ggplot(df_card_negatives, aes(x = age, y = montly_revenue, color = card_type, size = last_balance))+
  geom_point()

df_negatives_card_type = df_card_negatives %>% group_by(card_type) %>% summarise('total' = n())
category <- df_negatives_card_type$card_type
count <- df_negatives_card_type$total
data = data.frame(category, count)
donut(category, count,'Total que ja foram negativadas por tipo de Cartão')

df_order = read.table('order.asc',header=T,sep=";")

#Organizando o dataFrame
df_order <- df_order %>% left_join(df_account, by = 'account_id')
df <- df_order
df_order <- df_order %>% mutate('k_symbol' =  case_when(df_order$k_symbol == 'POJISTNE' ~ 'insurance payment',
                                                        df_order$k_symbol == 'SIPO' ~ 'household payment',
                                                        df_order$k_symbol == 'LEASING' ~ 'leasing',
                                                        df_order$k_symbol == 'UVER' ~ 'loan payment'))


#Primeiro vamos verificar os bancos que mais vezes recebem transferências
df_order_banks <- df_order %>% group_by(bank_to) %>% summarise('number_transatios' = n(), 'value_transation' = sum(amount))

tree_chart(df_order_banks$bank_to, df_order_banks$number_transatios,'Número de transações por banco',type = 0)

tree_chart(df_order_banks$bank_to, df_order_banks$value_transation,'valores de transação por banco',type = 0)

#Vamos avaliar os tipos de transação
df_order_ksymbol <- df_order %>% group_by(bank_to, k_symbol) %>% summarise('n_vezes' = n(), 'ammount_total' = sum(amount))

ggplot(df_order_ksymbol, aes(x = bank_to, y = ammount_total, fill = k_symbol))+
         geom_col()+
         my_theme

df_order_ksymbol %>% drop_na()%>% ggplot(aes(x = bank_to, y = ammount_total, fill = k_symbol))+
  geom_col()+
  my_theme

df_order_date_total <- df_order %>% group_by(date) %>% summarise('total_amount' = sum(amount)) %>%
  mutate('cumsum' = cumsum(total_amount))

df_order_date_total %>% ggplot(aes(x = date, y = cumsum))+
  geom_line()

df_order <- df_order %>% mutate('year' = year(df_order$date))
df_order_date <- df_order %>% group_by(year,k_symbol,bank_to,date) %>% summarise('total_amount' = sum(amount))

ggplot(df_order_date, aes(x = year, y = total_amount, fill = k_symbol))+
  geom_col()

df_order_date %>% drop_na() %>% ggplot(aes(x = year, y = total_amount, fill = bank_to))+
  geom_col()

df_order_date %>% drop_na() %>% ggplot(aes(x = year, y = total_amount, fill = k_symbol))+
  geom_col()

df_order_date %>% drop_na() %>% ggplot(aes(x = year, y = total_amount, fill = bank_to))+
  geom_col()+
  facet_wrap(~k_symbol)

df_loan = read.table('loan.asc',header=T,sep=";")

df_loan$date = as.Date(ymd(df_loan$date))
df_loan = left_join(df_loan, df_account, by = 'account_id')
df_loan = left_join(df_loan, df_disp, by = 'account_id')
df_loan = left_join(df_loan, df_client, by='client_id')
df_loan = left_join(df_loan, account_revenue, by = 'account_id')
df_loan = left_join(df_loan, last_status, by = 'account_id')


head(df_loan)

df_loan = df_loan %>% select(-date.y,-district_name.y,-region.y,-district_name.x,-date.y,-disp_id,-client_id,-type)
df_loan = df_loan %>% mutate('client_age' = round((today - birthdate)/360)) %>% select(-birthdate)

df_loan = df_loan %>% mutate('status' = case_when(df_loan$status == 'A'~'paid',
                                                  df_loan$status == 'B'~ 'cancelled',
                                                  df_loan$status == 'C'~ 'running OK',
                                                  df_loan$status == 'D'~ 'in debit'))


df_canceled = df_loan %>% filter(status == 'cancelled')
##Todos que tiveram o contrato cancelado tiveram a conta negativada ao menos uma vez.

df_canceled

df_neg = df_loan %>% filter(n_negatives > 0)
df_neg_data = df_neg %>% group_by(status) %>% summarise('n_total' = n())
donut(df_neg_data$status,df_neg_data$n_total,'Status com história ruim')
## Nenhum dos clientes que esteve negativo ao menos uma vez está com contrato em dia, ou o contrato está atrasado ou cancelado.

df_loan %>% filter(status == 'cancelled' | status == 'in debit') %>% view()


##Vamos entrar mais a fundo nos que tem histórico negativo
data = df_neg %>% group_by(gender) %>% summarise('total' = n())
donut(data$gender,data$total,'total de empréstimos atrasados ou cancelados por sexo')

ggplot(df_neg,aes(x = status, fill = factor(gender)))+
  geom_bar()

ggplot(df_neg,aes(x = client_age, y = montly_revenue, color = status, size = amount))+
  geom_point(alpha = 0.7)

##-- Avaliando o cenário geral
ggplot(df_loan,aes(x = status, fill = frequency))+
  geom_bar()+
  facet_wrap(~gender)

ggplot(df_loan,aes(x = status, fill = status))+
  geom_bar()+
  facet_wrap(~region.x)

df_loan = df_loan %>% mutate('porc.parcela' = round(payments / montly_revenue,2))

ggplot(df_loan, aes(x = amount, y = porc.parcela, color = status))+
  geom_point()+
  facet_wrap(~region.x)

average_salaries = df_loan %>% group_by(region.x) %>% summarise('average_salary' = mean(average_salary))

ggplot(df_loan, aes(x =porc.parcela , y = montly_revenue , color = status, size = amount))+
  geom_point(alpha = 0.7)+
  geom_hline(data = average_salaries, aes(yintercept = average_salary),color = 'red', linetype = 'dashed')+
  facet_wrap(~region.x)

ggplot(df_loan,aes(x = client_age, y = montly_revenue, color = status, size = amount))+
  geom_point(alpha = 0.7)

df_loan = df_loan %>% mutate('porc.parcela' = round(payments/montly_revenue,2))

ggplot(df_loan, aes(x = porc.parcela, y=montly_revenue, size = amount, color = gender))+
  geom_point(alpha = 0.7)+
  facet_wrap(~status)

ggplot(df_loan, aes(x = porc.parcela, y=client_age, size = amount, color = gender))+
  geom_point(alpha = 0.7)+
  facet_wrap(~status)

ggplot(df_loan, aes(x = status, y=montly_revenue))+
  geom_boxplot(outlier.color = 'red')+
  my_theme

ggplot(df_loan, aes(x = status, y=amount))+
  geom_boxplot(outlier.color = 'red')+
  my_theme

ggplot(df_loan, aes(x = status, y=last_balance))+
  geom_boxplot(outlier.color = 'red')+
  my_theme

###-----------Criando uma tabela padrao para avaliacao da dependecncia das variaveis.
st_loan <- df_loan


st_loan <- st_loan %>% mutate('Cod_in_debit' = (status == 'in debit')*1,
                              'Cod_running_OK' = (status == 'running OK')*1,
                              'Cod_paid' = (status == 'paid')*1) -> st_loan1

st_loan1 <- st_loan1 %>% mutate('status' = case_when(status == 'in debit' ~ 1,
                                                     status == 'running OK' ~ 2,
                                                     status == 'paid' ~ 3,
                                                     status == 'cancelled' ~ 0))

st_loan1 <- st_loan1 %>% mutate('gender' = case_when(gender == 'f' ~ 1,
                                                     gender == 'm' ~ 2))

st_loan1 <- st_loan1 %>% mutate('frequency' = case_when(frequency == 'weekly issuance' ~ 0,
                                                        frequency == 'monthly issuance' ~ 1,
                                                        frequency == 'issuance after transaction' ~ 2))

#Transformando as regiões em fatores de 1 à 8
#criando um vetor com as variáveis únicas de region.
reg = unique(st_loan1$region.x)
st_loan1$region.x <- factor(st_loan$region.x, levels = reg, labels = (1:length(reg))) 

#Criação dos modelos:
lm(data = st_loan1, formula = status ~ frequency ) -> modelo
summary(modelo)

lm(data = st_loan1, formula = status ~ amount ) -> modelo
summary(modelo)

lm(data = st_loan1, formula = status ~ duration ) -> modelo
summary(modelo)

lm(data = st_loan1, formula = status ~ average_salary ) -> modelo
summary(modelo)

lm(data = st_loan1, formula = status ~ montly_revenue ) -> modelo
summary(modelo)

lm(data = st_loan1, formula = status ~ porc.parcela ) -> modelo
summary(modelo)

lm(data = st_loan1, formula = status ~ gender ) -> modelo
summary(modelo)

##Criando uma matriz de correlação entre todas as variáveis para os pedidos de empréstimo
#install.packages('corrplot')

library(corrplot)
library(RColorBrewer)

st_loan2 <- st_loan1

st_loan2$date.x = (as.numeric(st_loan2$date.x))
st_loan2$firstNegativeDate = (as.numeric(st_loan2$firstNegativeDate))
st_loan2$LastNeagtiveDate = (as.numeric(st_loan2$LastNeagtiveDate))
st_loan2$negative = (as.numeric(st_loan2$negative))
st_loan2$account_age = (as.numeric(st_loan2$account_age))
st_loan2$client_age = (as.numeric(st_loan2$client_age))
st_loan2$first_trans = (as.numeric(st_loan2$first_trans))
st_loan2$region.x = (as.numeric(st_loan2$region.x))
st_loan2$firstNegativeDate = NULL
st_loan2$LastNeagtiveDate = NULL

summary(st_loan2)

st_loan2$region.x = NULL

M <-cor(st_loan2)

corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))

##-------- Tabela final
df_client
df_client = df_client %>% mutate('age' = round((today - birthdate)/360,0))

df_client$age = as.numeric(df_client$age)
df_client$birthdate = NULL
df_client$district_name = NULL

df <- df_client
df <- left_join(df, df_disp, by = 'client_id')
df <- left_join(df, df_account, by = 'account_id')

df$region.y = NULL
df$district_name = NULL

df2 <- select(df_card_final, client_id, account_id, card_type)
df <- left_join(df,df2, by = c('account_id' = 'account_id', 'client_id' = 'client_id'))
df2 <- select(df_card_final, client_id, account_id, card_type)

df2 <- select(df_loan, loan_id, account_id, duration, payments, status, porc.parcela, date.x)
df2 <- df2 %>% rename('loan_date' = date.x)

df2 <- df2 %>% mutate('status' = case_when(status == 'running OK' ~ 2,
                                           status == 'paid' ~ 1,
                                           status == 'in debit' ~ 3,
                                           status == 'cancelled' ~ 4))

df4 <- df2 %>% group_by(account_id) %>% summarise('duration' = mean(duration),
                                          'payments' = mean(payments),
                                          'status' = max(status),
                                          'porc.parcela' = mean(porc.parcela),
                                          'last_date' = max(loan_date))


df3 <- left_join(df, df4, by = 'account_id')
df3 <- left_join(df3, account_revenue, by = 'account_id')
df5 <- left_join(df3, last_status, by = 'account_id')

#Análise da relação entre amount e status do empréstimo
mean(st_loan1$amount)
median(st_loan1$amount)
hist(st_loan1$amount)

boxplot(st_loan1$amount ~ st_loan1$status)
lm(formula = st_loan1$amount ~ st_loan1$status)-> modelo2
summary(modelo2)

#'in debit' ~ 1,'running OK' ~ 2, 'paid' ~ 3, 'cancelled' ~ 0))
#mean 151.801,507mediana 115992 #p-value 2.2e-16

#mean 0.1927811,507mediana 0.16 #p-value 0,001


#verificar a quantidade de contas diferentes de que possuem algum emprestimo.
length(unique(st_loan1$account_id))

#quantidade de clientes com emprÃ©stimo OK
df_loan_sum_status <- st_loan1%>% 
  group_by(status) %>% 
  summarise(count = n_distinct(account_id))
df_loan_sum_status


#Análise da relação entre porc. Parcela e o status do Empréstimo
mean(st_loan1$porc.parcela) 
median(st_loan1$porc.parcela) 
hist(st_loan1$porc.parcela)


boxplot(st_loan1$porc.parcela ~ st_loan1$status)
lm(formula = st_loan1$porc.parcela ~ st_loan1$status)-> modelo3
summary(modelo3)


