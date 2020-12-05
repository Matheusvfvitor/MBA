rec=RECORDES[,-1]
boxplot(rec$`100m_seg`, main="100m", col="lightblue")   
rec=subset(rec, rec$`100m_seg`<10.7)
boxplot(rec$`200m_seg`, main="200", col="lightblue")   
boxplot(rec$`400m_seg`, main="400m", col="lightblue")   
boxplot(rec$`800m_min`, main="800m", col="lightblue")   
boxplot(rec$`1500m_min`, main="1500m", col="lightblue");grid()
rec=subset(rec,rec$`1500m_min`<3.9)
boxplot(rec$`5000m_min`, main="5000m", col="lightblue")  
boxplot(rec$`10000m_min`, main="10000m", col="lightblue")  
hist(rec$`10000m_min`, main="10000m", col="lightblue")  
boxplot(rec$maratona_min, main="marat", col="lightblue")   
hist(rec$maratona_min, main="marat", col="lightblue")   

#####   analisar a matriz de correlações
round(cor(rec),2)
