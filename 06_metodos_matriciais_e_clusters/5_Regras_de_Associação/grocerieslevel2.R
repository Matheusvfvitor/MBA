data(Groceries) #já está no formato de transactions quando copiamos do R
head(Groceries@itemInfo,15) # dar uma olhada na estrutura da base

GL2=aggregate(Groceries, by="level2")#para trabalhar com classes mais amplas apenas como exercício
head(GL2@itemInfo)
inspect(GL2)

######################################################################################

# já preparei o arquivo para evitar esse trabalho com GROCERIES_LEVEL2 no banco de dados do curso
# então utlizaremos o mesmo procedimento especificado no script grocerieslist

#gerando arquivo Excel com as transações
write(GL2, file = "grocerieslevel2.csv", format="basket", sep = ",", quote=F)
#############################################################################################

sort(itemFrequency(GL2), decreasing = T)

Grules=apriori(GL2, parameter = list(supp=.05,conf=0.5, minlen=2, maxlen=5, maxtime=25, target="rules"), 
               control = list(verbose=F),)
Grules
#podemos criar um conjunto de regras já ordenado. 
Grules.sorted=sort(Grules, by="confidence")
inspect(Grules.sorted[1:20])

#detectando regras redundantes
inspect(Grules.sorted[is.redundant(Grules.sorted)]) # output em branco se nao houver regras redundantes
Grules.pruned=Grules.sorted[!is.redundant(Grules.sorted, measure="confidence")];Grules.pruned
inspect(Grules.pruned)

sink( "Gregras.txt") #cria arquivo .txt em branco; na realidade direciona todo o output para um arquivo txt
inspect(Grules.sorted) #prenche o arquivo em branco
sink() #sai do modo sink (alternativa sink(file = NULL)), output volta para o painel de output

 

######################################################################################
# se nao baixarmos direto do R: 
# já preparei o arquivo para evitar esse trabalho com GROCERIES_LEVEL2 no banco de dados do curso
# então utlizaríamos o mesmo procedimento especificado no script grocerieslist

#gerando arquivo Excel com as transações
write(GL2, file = "grocerieslevel2.csv", format="basket", sep = ",", quote=F)
#############################################################################################