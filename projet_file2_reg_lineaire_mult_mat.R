#VOIESEMBERT Colette
#KANYAMIBWA Romaric
#MAIN4 Polytech-Sorbonne 2017-2018
#Projet Analyse de donnes

library(ggplot2)
library(plyr)
library(gridExtra)
library(car)


data=read.table("student-mat.csv",sep=",",header=TRUE)
# d2=read.table("student-por.csv",sep=",",header=TRUE)
# data=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus",
#                       "Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
#                       "guardian","guardian","traveltime","studytime","failures",
#                       "schoolsup","famsup","activities","higher","romantic",
#                       "famrel","freetime","goout","Dalc","Walc","health","absences"))

#on fait la moyenne de notes en Portogais et en Mathe et on les injecte dans notre jeu de donnees
data$avggrades=rowMeans(cbind(data$G1,data$G2,data$G3))
# data$portgrades=rowMeans(cbind(data$G1.y,data$G2.y,data$G3.y))
# #note moyenne
# data$avggrades=rowMeans(cbind(data$portgrades,data$mathgrades))

data$Dalc <- as.factor(data$Dalc)      
data$Dalc <- mapvalues(data$Dalc, 
                       from = 1:5, 
                       to = c("Very Low", "Low", "Medium", "High", "Very High"))


data$Walc <- as.factor(data$Walc)      
data$Walc <- mapvalues(data$Walc, 
                       from = 1:5, 
                       to = c("Very Low", "Low", "Medium", "High", "Very High"))

# data$Fedu <- as.factor(data$Fedu)      
# data$Fedu <- mapvalues(data$Fedu, 
#                        from = 0:4, 
#                        to = c("None", "Primary education (4th grade)", "Primary education (5th to 9th grade)", "Secondary education", "Higher education"))
# 
# data$Medu <- as.factor(data$Medu)      
# data$Medu <- mapvalues(data$Medu, 
#                        from = 0:4, 
#                        to = c("None", "Primary education (4th grade)", "Primary education (5th to 9th grade)", "Secondary education", "Higher education"))

print(nrow(data)) # 395 students
head(data)#on verifie que la table est bien une table bien structuré
str(data)



#Choix du modele
res0=lm(avggrades ~1)
data0=data[,-c(31,32,33)]
str(data0)
#on enleve failures 
# drops <- c("failures")
# data0=data0[ , !(names(data0) %in% drops)]
str(data0)
attach(data0)

resCompl=lm(avggrades ~.,data = data0)
summary(resCompl)

res_Stepwise0=step(res0,scope=formula(resCompl),direction="both")
# avggrades ~ Medu + goout + higher + schoolsup + 
# sex + studytime + famsup + Mjob + health + age + romantic + 
# famsize
summary(res_Stepwise0)
res_backwart0=step(resCompl,direction="backward")
# sex + age + famsize + Medu + Mjob + studytime + schoolsup + 
# famsup + higher + romantic + goout + health
summary(res_backwart0)

vif(resCompl)

#validation du modele
plot(res_Stepwise0)#analyse de residus
shapiro.test(res_Stepwise0$residuals)#pvalue >5% donc on garde H0, nos données semble gaussien
bartlett.test(res_Stepwise0$residuals~famsize)#pvalue> 5% on garde H0 ,les residues semble homoscedastique
bartlett.test(res_Stepwise0$residuals~ Pstatus)#pvalue< 5% on rejete H0 ,les residues ne semble pa homoscedastique


#Arbre de classification
library(rpart)
library(DMwR)
rt<-rpart(avggrades~., data0)
prettyTree(rt)


#Foret aleatoire
library(randomForest)
rf<-randomForest(avggrades~., data=data0, ntree=1000, importance=T)

#Predictions de different modele 

#prediction avec tous les variables et tous les colonnes
lmcmpl.predictions<-predict(resCompl,data0)


rfpltdata1=data.frame(cbind(lmcmpl.predictions,data0[,"avggrades"]))
colnames(rfpltdata1)<-c("lmcmpl.predictions","avggrades")

errplt.rfD<-ggplot(rfpltdata1,aes(lmcmpl.predictions,avggrades))+
  geom_point(aes(color=data0[,"Dalc"]))+
  xlab("Predicted Grades (Linear Regression)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")

errplt.rfW<-ggplot(rfpltdata1,aes(lmcmpl.predictions,avggrades))+
  geom_point(aes(color=data0[,"Walc"]))+
  xlab("Predicted Grades (Linear Regression)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Weekly Alcohol \nConsumption")

grid.arrange(errplt.rfD,errplt.rfW,nrow=2)

#Plot Stepwise
lmcmpl.predictions<-predict(res_Stepwise0,data0)

rfpltdata1=data.frame(cbind(lmcmpl.predictions,data0[,"avggrades"]))
colnames(rfpltdata1)<-c("lmcmpl.predictions","avggrades")

errplt.rfD<-ggplot(rfpltdata1,aes(lmcmpl.predictions,avggrades))+
  geom_point(aes(color=data0[,"Dalc"]))+
  xlab("Predicted Grades (Linear Regression Stepwise)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")

errplt.rfW<-ggplot(rfpltdata1,aes(lmcmpl.predictions,avggrades))+
  geom_point(aes(color=data0[,"Walc"]))+
  xlab("Predicted Grades (Linear Regression Stepwise)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Weekly Alcohol \nConsumption")

grid.arrange(errplt.rfD,errplt.rfW,nrow=2)

#Plot Backward
lmcmpl.predictions<-predict(res_backwart0,data0)

rfpltdata1=data.frame(cbind(lmcmpl.predictions,data0[,"avggrades"]))
colnames(rfpltdata1)<-c("lmcmpl.predictions","avggrades")

errplt.rfD<-ggplot(rfpltdata1,aes(lmcmpl.predictions,avggrades))+
  geom_point(aes(color=data0[,"Dalc"]))+
  xlab("Predicted Grades (Linear Regression Backward)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")

errplt.rfW<-ggplot(rfpltdata1,aes(lmcmpl.predictions,avggrades))+
  geom_point(aes(color=data0[,"Walc"]))+
  xlab("Predicted Grades (Linear Regression Backward)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Weekly Alcohol \nConsumption")

grid.arrange(errplt.rfD,errplt.rfW,nrow=2)

#Plot Arbre Aleatoire
lmcmpl.predictions<-predict(rt,data0)

rfpltdata1=data.frame(cbind(lmcmpl.predictions,data0[,"avggrades"]))
colnames(rfpltdata1)<-c("lmcmpl.predictions","avggrades")

errplt.rfD<-ggplot(rfpltdata1,aes(lmcmpl.predictions,avggrades))+
  geom_point(aes(color=data0[,"Dalc"]))+
  xlab("Predicted Grades (Regression Tree)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")

errplt.rfW<-ggplot(rfpltdata1,aes(lmcmpl.predictions,avggrades))+
  geom_point(aes(color=data0[,"Walc"]))+
  xlab("Predicted Grades (Regression Tree)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Weekly Alcohol \nConsumption")

grid.arrange(errplt.rfD,errplt.rfW,nrow=2)

#Random Forest
rf.predictions<-predict(rf,data0)


#first combine the rf predictions and actual scores in a single data frame
rfpltdata1=data.frame(cbind(rf.predictions,data0[,"avggrades"]))
colnames(rfpltdata1)<-c("rf.predictions","avggrades")

# then create the error plot.
errplt.rfD<-ggplot(rfpltdata1,aes(rf.predictions,avggrades))+
  geom_point(aes(color=data0[,"Dalc"]))+
  xlab("Predicted Grades (Random Forest with 500 Trees)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  #geom_smooth(method = "lm", se = FALSE)+
  scale_colour_brewer(palette = "Set1",name = "Daily Alcohol \nConsumption")
#finally, plot the error plot from the random forest with the error plots of the linear and regression tree models.

errplt.rfW<-ggplot(rfpltdata1,aes(rf.predictions,avggrades))+
  geom_point(aes(color=data0[,"Walc"]))+
  xlab("Predicted Grades (Random Forest with 500 Trees)")+
  ylab("Actual Grades")+
  geom_abline(intercept=0,slope=1,color="#0066CC",size=1)+
  scale_colour_brewer(palette = "Set1",name = "Weekly Alcohol \nConsumption")

grid.arrange(errplt.rfD,errplt.rfW,nrow=2)


