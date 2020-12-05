#considerando as colunas de produtos
cbc=CBOOK[,7:16]  
names(cbc)
head(cbc,10)
cbc=cbc[rowSums(cbc)>0,] #eliminar as linhas que só tem zero
which(rowSums(cbc)==0) #verificação
head(cbc)

#criando uma matriz binária indicando se comprou ou não (sem considerar quantos comprou)
cbc2=ifelse(cbc>0,1,0)
head(cbc2)

#antes de usar a base de dados devemos transforma-la em "matrix'
class(cbc2) #já é matriz, nao preciso trasformar em matriz
#e se nao fosse
cbc.mat=as.matrix(cbc2)  # só para vcs entenderem como faríamos
head(cbc.mat)
class(cbc.mat)

#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
# gerando a matriz de transações para pode rodar o algoritmo
library(arules)
cbc.tr=as(cbc.mat,"transactions")  #gera um objeto para trabalho do arules

inspect(cbc.tr[1:18]) # mostra os ITEMSETS
summary(cbc.tr)

#analisando os produtos graficamente
itemFrequency(cbc.tr) # suporte de cada item 
itemFrequencyPlot(cbc.tr, col=topo.colors(2)) #não ordena
itemFrequencyPlot(cbc.tr,topN=10, main="item support",ylim=c(0,.6), col=topo.colors(10));grid(col=3)
abline(h=.1,col=4, lwd=3)  #suporte mínimo de 10%, por exemplo


#geração das regras
rules=apriori(data = cbc.tr, parameter = list(supp=.05,conf=0.6, 
                                              minlen=2, maxlen=7, target="rules" ))
#maxlen: an integer value for the maximal number of items per item set (default: 10 items)
#maxtime: time limit in seconds for checking subsets. maxtime=0 disables the time limit. (default: 5 seconds)
#minlen=2 evita regras com LHS vazios
rules #dá o número de regras
inspect(sort(rules, by="conf")) #ordenado pela conf, coverage é o suporte do LHS
#Coverage (also called cover or LHS-support) is the support of the left-hand-side of the rule, i.e.,
#It represents a measure of to how often the rule can be applied.

library(arulesViz)
plot(rules, col=topo.colors(3)) 

#poderíamos criar um conjunto de regras já ordenado. 
#rules.sorted=sort(rules, by="confidence")
#inspect(rules.sorted[1:10])

#obtendo regras redundantes
# A rule is more general if it has the same RHS but one or more items removed from the LHS ("menorLHS").
#A rule is redundant if a more general rules with the same or a higher confidence exists.
#A more specific rule is redundant if it is only equally or even less predictive than a more general rule
# a rule X -> Y is redundant if for some X' (subset of) X, conf(X' -> Y) >= conf(X -> Y)

inspect(rules[is.redundant(rules)]) # output em branco se nao houver regras redundantes
rules.pruned=rules[!is.redundant(rules, measure="confidence")]
rules.pruned
inspect(sort(rules.pruned, by="conf"))


# focando em determinado produto ou itemset --> alvancar 

rules.child=apriori(data = cbc.tr, 
              parameter = list(supp=.05,conf=0.6, minlen=2, maxlen=5,  target="rules"), 
              appearance = list(rhs="ChildBks"))
inspect(sort(rules.child, by="confidence"))
inspect(rules.child[is.redundant(rules.child)]) 
rules.kids=rules.child[!is.redundant(rules.child)] 
inspect(rules.kids [1:5])

#outros gráficos 

inspect(rules.kids[1:10])
plot(rules.kids[1:10], method="grouped", col="navyblue")

#cria arquivo .txt em branco; na realidade direciona 
#todo o output para um arquivo txt
sink("regras.txt")

inspect(rules.kids) #prenche o arquivo em branco

sink(file = NULL ) #sai do modo sink (alternativa sink(file = NULL)), output volta para o painel de output

write(rules, file="CBC_rules_sorted.csv", sep=",")
inspect(rules) #prenche o arquivo em branco

#trabalhando apenas com os itemsets
is=apriori(data = cbc.tr, parameter = list(supp=.05,conf=0.5, minlen=2, maxlen=4, maxtime=25, target="frequent itemsets" ))
inspect(sort(is, by="supp"))

        