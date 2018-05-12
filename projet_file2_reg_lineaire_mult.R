#VOIESEMBERT Colette
#KANYAMIBWA Romaric
#MAIN4 Polytech-Sorbonne 2017-2018
#Projet Analyse de donnes

library(ggplot2)
library(plyr)
library(gridExtra)
library(car)


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

data$Fedu <- as.factor(data$Fedu)      
data$Fedu <- mapvalues(data$Fedu, 
                       from = 0:4, 
                       to = c("None", "Primary education (4th grade)", "Primary education (5th to 9th grade)", "Secondary education", "Higher education"))

data$Medu <- as.factor(data$Medu)      
data$Medu <- mapvalues(data$Medu, 
                       from = 0:4, 
                       to = c("None", "Primary education (4th grade)", "Primary education (5th to 9th grade)", "Secondary education", "Higher education"))

print(nrow(data)) # 85 students
head(data)#on verifie que la table est bien une table bien structurΓ©
str(data)



#Choix du modele
res0=lm(avggrades ~1)
data0=data[,-c(30,31,32,33,34,35,36,37,38,39)]
#on enleve failures 
drops <- c("Medu")
data2=data0[ , !(names(data0) %in% drops)]
str(data0)
attach(data0)

res=lm(avggrades ~.,data = data0)
summary(res)

res_Stepwise=step(res0,scope=formula(res),direction="both")
# avggrades ~ failures + Mjob + schoolsup + Pstatus + Fjob + traveltime + romantic + nursery + studytime + age
summary(res_Stepwise)
res_backwart=step(res,direction="backward")
# avggrades ~ age + Pstatus + Mjob + Fjob + nursery + traveltime + studytime + failures + schoolsup + romantic
summary(res_backwart)

vif(res)
#on enleve Medu 
drops <- c("Medu")
data2=data0[ , !(names(data0) %in% drops)]
attach(data2)
res=lm(avggrades ~.,data = data2)
vif(res)

#on enleve Walc
drops <- c("Walc")
data2=data2[ , !(names(data2) %in% drops)]
res=lm(avggrades ~.,data = data2)
vif(res)

#on enleve Fjob
drops <- c("Fjob")
data2=data2[ , !(names(data2) %in% drops)]
res=lm(avggrades ~.,data = data2)
vif(res)

attach(data2)
res_Stepwise=step(res0,scope=formula(res),direction="both")
#avggrades ~ failures + Mjob + schoolsup + Pstatus + famsize + romantic
summary(res_Stepwise)
res_backwart=step(res,direction="backward")
summary(res_backwart)
# famsize + Pstatus + Mjob + failures + schoolsup + absences

#validation du modele
plot(res_Stepwise)#analyse de résidus
shapiro.test(res_Stepwise$residuals)#pvalue >5% donc on garde H0, nos donnΓ©es semble gaussien
bartlett.test(res_Stepwise$residuals~famsize)#pvalue> 5% on garde H0 ,les residues semble homoscedastique
bartlett.test(res_Stepwise$residuals~ Pstatus)#pvalue< 5% on rejete H0 ,les residues ne semble pa homoscedastique



