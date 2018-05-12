#VOIESEMBERT Colette
#KANYAMIBWA Romaric
#MAIN4 Polytech-Sorbonne 2017-2018
#Projet Analyse de donnes

data=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
data2=merge(data,d2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))#,"guardian"
# print(nrow(d2))
# print(nrow(d1))
print(nrow(data)) # 382 students
# data$Medu=as.factor(data$Medu)
# data$Fedu=as.factor(data$Fedu)
data$traveltime=as.factor(data$traveltime)
# data$traveltime.y=as.factor(data$traveltime.y)
data$studytime=as.factor(data$studytime)
# data$studytime.y=as.factor(data$studytime.y)
# data$famrel.x=as.factor(data$famrel.x)
# data$famrel.y=as.factor(data$famrel.x)

head(data)#on verifie que la table est bien une table bien structuré
str(data)
attach(data)


#Regression Lineaire 
library(car)
resD=lm(Dalc~.,data=data)
resW=lm(Walc~.,data=data)
summary(resD)
summary(resW)
vif(res) #tous inferieur a 10, pas de colinearite
par(mfrow=c(2,2))
plot(res)

res=lm(log(Dalc)~.,data=data)
par(mfrow=c(2,2))
plot(res)
##### Anova-1 et 2 ####