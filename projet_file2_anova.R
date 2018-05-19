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

#on fait la moyenne de notes en Portogais et en Mathe et on les injecte dans notre jeu de donnees, parwise t test
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
head(data)#on verifie que la table est bien une table bien structuré
str(data)
attach(data)

#courte analyse descriptive
str1=ggplot(data, aes(x=Dalc, y=portgrades, group=Dalc))+
  geom_boxplot()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Daily Alcohol consumption")+
  ylab("Average Grades")+
  ggtitle("Average Grade :Portugais")

str2=ggplot(data, aes(x=Walc, y=portgrades, group=Walc))+
  geom_boxplot()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Weekend Alcohol consumption")+
  ylab("Average Grades")+
  ggtitle("Average Grade :Portugais")


str3=ggplot(data, aes(x=Dalc, y=mathgrades, group=Dalc))+
  geom_boxplot()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Daily Alcohol consumption")+
  ylab("Average Grades")+
  ggtitle("Average Grade :Math")

str4=ggplot(data, aes(x=Walc, y=mathgrades, group=Walc))+
  geom_boxplot()+
  theme(legend.position="none")+
  scale_fill_manual(values=waffle.col)+
  xlab("Weekend Alcohol consumption")+
  ylab("Average Grades")+
  ggtitle("Average Grade :Math")

grid.arrange(str1,str2,str3,str4,nrow=2,ncol=2)

#Anova 1
res0=lm(avggrades~Medu,contrasts = "contr.treatement")
res=lm(avggrades~Walc,contrasts = "contr.treatement")
res2=lm(avggrades~Dalc,contrasts = "contr.treatement")
# 
# res3=lm(mathgrades~Walc,contrasts = "contr.treatement")
# res4=lm(mathgrades~Walc,contrasts = "contr.treatement")
# 
# res5=lm(portgrades~Walc,contrasts = "contr.treatement")
# res6=lm(portgrades~Dalc,contrasts = "contr.treatement")

summary(res)
summary(res0)
summary(res2)
summary(res)$coefficients
summary(res0)$coefficients
summary(res2)$coefficients
#validation du modele
par(mfrow=c(2,2))
plot(res2)

shapiro.test(res$residuals)#pvalue >5% donc on garde H0, nos données semble gaussien
shapiro.test(res2$residuals)#pvalue >5% donc on garde H0, nos données semble gaussien

bartlett.test(res$residuals~Walc)#pvalue> 5% on garde H0 ,les residues semble homoscedastique
bartlett.test(res2$residuals~Dalc)#pvalue> 5% on garde H0 ,les residues semble homoscedastique

#Type de consomation d'alcool qui different entre eux 2 a 2.

pairwise.t.test(avggrades,Walc,p.adjust="none")
pairwise.t.test(avggrades,Walc,p.adjust="bonf")
comp.Tuckey = TukeyHSD(aov(avggrades~Walc))
comp.Tuckey
plot(comp.Tuckey)

pairwise.t.test(avggrades,Dalc,p.adjust="none")
pairwise.t.test(avggrades,Dalc,p.adjust="bonf")
comp.Tuckey = TukeyHSD(aov(avggrades~Dalc))
comp.Tuckey
#pvalue >5% donc on garde H0,les differents types de consomation ne semble pas different
