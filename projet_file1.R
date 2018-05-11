#VOIESEMBERT Colette
#KANYAMIBWA Romaric
#MAIN4 Polytech-Sorbonne 2017-2018
#Projet Analyse de donnes

d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
data=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))
print(nrow(d2))
print(nrow(d1))
print(nrow(data)) # 382 students
# data$Medu=as.factor(data$Medu)
# data$Fedu=as.factor(data$Fedu)
data$traveltime.x=as.factor(data$traveltime.x)
data$traveltime.y=as.factor(data$traveltime.y)
data$studytime.x=as.factor(data$studytime.x)
data$studytime.y=as.factor(data$studytime.y)
# data$famrel.x=as.factor(data$famrel.x)
# data$famrel.y=as.factor(data$famrel.x)

head(data)#on verifie que la table est bien une table bien structuré
str(data)
attach(data)


#Regression Lineaire 
library(car)
res=lm(Dalc.x~.,data=data)
vif(res)

##### Anova-1 et 2 ####