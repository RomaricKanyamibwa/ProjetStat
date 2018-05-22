#VOISEMBERT Colette
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

#on fait la moyenne de notes en Portogais et en Mathe et on les injecte dans notre jeu de donnees
data$avggrades=rowMeans(cbind(data$G1.x,data$G2.x,data$G3.y))
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

# data$Fedu <- as.factor(data$Fedu)      
# data$Fedu <- mapvalues(data$Fedu, 
#                        from = 0:4, 
#                        to = c("None", "Primary education (4th grade)", "Primary education (5th to 9th grade)", "Secondary education", "Higher education"))
# 
# data$Medu <- as.factor(data$Medu)      
# data$Medu <- mapvalues(data$Medu, 
#                        from = 0:4, 
#                        to = c("None", "Primary education (4th grade)", "Primary education (5th to 9th grade)", "Secondary education", "Higher education"))

print(nrow(data)) # 85 students
head(data)#on verifie que la table est bien une table bien structuré
str(data)

############### ACM , Kmean et CAH
# index<-c("Walc","sex","address","famsize","Pstatus","Mjob","Fjob","internet","guardian","higher","famsup","activities","romantic")
# ACM.data=data[index]
library(FactoMineR)
ACM.data=Filter(is.factor, data)#or data[sapply( data,is.factor)]
ACM.data=ACM.data[,-c(1,2,3,19,20)]#on enleve  paid.x, paid.y,school,sex,address
drops <- c("Mjob")
ACM.data=ACM.data[ , !(names(ACM.data) %in% drops)]
drops <- c("Fjob")
ACM.data=ACM.data[ , !(names(ACM.data) %in% drops)]
drops <- c("reason")
ACM.data=ACM.data[ , !(names(ACM.data) %in% drops)]
drops <- c("famsize")
ACM.data=ACM.data[ , !(names(ACM.data) %in% drops)]
drops <- c("nursery")
ACM.data=ACM.data[ , !(names(ACM.data) %in% drops)]
tab.disjonctif(ACM.data)
str(ACM.data)
par(mfrow=c(1,3))
acm=MCA(ACM.data,quali.sup = 11,graph = T)#ou MCA(data)
acm2=MCA(ACM.data,quali.sup = 10,graph = F)#ou MCA(data)
summary(acm)#Pour les valeurs propres le pourcentge n'est pas aussi eleve que pour le acp car en acm
#on a bcp de modalites (on garde au moins le 4 premiers)
plot(acm,choix="ind",invisible="var",habillage=11)

summary(acm2)#Pour les valeurs propres le pourcentge n'est pas aussi eleve que pour le acp car en acm
#on a bcp de modalites (on garde au moins le 4 premiers)
plot(acm2,choix="ind",invisible="var",habillage=10)

# Kmeans sur deux premières composantes principales de l'ACM
K=nlevels(ACM.data[,11])
kmeans.acm <-  kmeans(acm$ind$coord[,1:2],centers=K,nstart=100)
table(kmeans.acm$cluster,ACM.data[,11])

# representation des clusters dans le 1er plan factoriel :
X11();plot(acm, choix="ind", invisible="var",col.ind=as.integer(kmeans.acm$cluster))


# sans a-priori sur  K ?
# évaluer la proportion d'inertie intra-classe
inertie.intra <- rep(0,times=10)
for (k in 1:10){
  kmeans.acm <- kmeans(acm$ind$coord[,1:2],centers=k,nstart=100)
  inertie.intra[k] <- kmeans.acm$tot.withinss/kmeans.acm$totss
}
# graphique
plot(1:10,inertie.intra,type="b",xlab="Nb. de groupes",ylab="% inertie intra")
# plutôt 4 classes ?



# CAH sur deux premières composantes principales de l'ACM
cah.acm <- HCPC(acm,nb.clust=K)
# comparaison des clustering obtenus avec kmeans et cah :
table(kmeans.acm$cluster,cah.acm$data.clust$clust)

# sans préciser nb.clust :
cah.acm <- HCPC(acm)
table(ACM.data[,11],cah.acm$data.clust$clust)

# data0=data[,-seq(30,38,by=1)]
# str(data0)
# library(FactoMineR)
# FAMD(data)
# kmeans(data,centers = 5)
# 
# K=5
# library(cluster)
# pam.result <- pam(data0, K)
# par(mfrow=c(1,2))
# plot(pam.result)
# # le premier graphe correspond à l'ACP. 
# # le deuxième graphe aux silhouettes.
# 
# 
# #acm avec la fonction en supplémentaire
# acm=MCA(data0)
# plot(acm, choix="ind", invisible="var", habillage=7)
# 
# cah.acm <- HCPC(data0,nb.clust=K)

