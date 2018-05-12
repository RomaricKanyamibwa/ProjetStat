#VOIESEMBERT Colette
#KANYAMIBWA Romaric
#MAIN4 Polytech-Sorbonne 2017-2018
#Projet Analyse de donnes

library(ggplot2)
library(plyr)
library(gridExtra)

d1=read.table("student-mat.csv",sep=",",header=TRUE)
d2=read.table("student-por.csv",sep=",",header=TRUE)
data=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus",
                      "Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
                      "guardian","guardian","traveltime","studytime","failures",
                      "schoolsup","famsup","activities","higher","romantic",
                      "famrel","freetime","goout","Dalc","Walc","health","absences"))

#on fait la moyenne de notes en Portogais et en Mathe et on les injecte dans notre jeu de données
data$mathgrades=rowMeans(cbind(data$G1.x,data$G2.x,data$G3.x))
data$portgrades=rowMeans(cbind(data$G1.y,data$G2.y,data$G3.y))
#note moyenne
data$avggrades=rowMeans(cbind(data$portgrades,data$mathgrades))

data$Dalc <- as.factor(data$Dalc)      
data$Dalc <- mapvalues(data$Dalc, 
                       from = 1:5, 
                       to = c("Very Low", "Low", "Medium", "High", "Very High"))


data$Walc <- as.factor(data$Walc)      
data$Walc <- mapvalues(data$Walc, 
                       from = 1:5, 
                       to = c("Very Low", "Low", "Medium", "High", "Very High"))


print(nrow(data)) # 85 students
head(data)#on verifie que la table est bien une table bien structurΓ©
str(data)
attach(data)
table(Walc,Dalc)#les donées sont complet mais pas equilibre

res=lm(avggrades~Walc*Dalc,contrasts = c("contr.treatment","contr.treatment"))
par(mfrow=c(2,2))
plot(res)

#Validation du modele

shapiro.test(res$residuals)#pvalue >5% donc on garde H0, nos donnΓ©es semble gaussien
bartlett.test(res$residuals~Walc)#pvalue> 5% on garde H0 ,les residues semble
bartlett.test(res$residuals~Dalc)#pvalue< 5% on rejette H0 , le modele n'est pas valide



