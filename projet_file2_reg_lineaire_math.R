library(ggplot2)
library(plyr)
library(gridExtra)

data=read.table("student-mat.csv",sep=",",header=TRUE)
# d2=read.table("student-por.csv",sep=",",header=TRUE)
# data=merge(d1,d2,by=c("school","sex","age","address","famsize","Pstatus",
#                       "Medu","Fedu","Mjob","Fjob","reason","nursery","internet",
#                       "guardian","guardian","traveltime","studytime","failures",
#                       "schoolsup","famsup","activities","higher","romantic",
#                       "famrel","freetime","goout","Dalc","Walc","health","absences"))

#on fait la moyenne de notes en Portogais et en Mathe et on les injecte dans notre jeu de données
data$mathgrades=rowMeans(cbind(data$G1.x,data$G2.x,data$G3.x))
data$portgrades=rowMeans(cbind(data$G1.y,data$G2.y,data$G3.y))

data$Dalc <- as.factor(data$Dalc)      
data$Dalc <- mapvalues(data$Dalc, 
                       from = 1:5, 
                       to = c("Very Low", "Low", "Medium", "High", "Very High"))

str1=ggplot(data, aes(x=mathgrades, y=portgrades)) +
  geom_point(aes(colour=factor(Dalc)))+ scale_colour_hue(l=25,c=150)+
  geom_smooth(method = "lm", se = FALSE)

data$Walc <- as.factor(data$Walc)      
data$Walc <- mapvalues(data$Walc, 
                       from = 1:5, 
                       to = c("Very Low", "Low", "Medium", "High", "Very High"))

str2=ggplot(data, aes(x=mathgrades, y=portgrades))+
  geom_point(aes(colour=factor(Walc)))+ scale_colour_hue(l=25,c=150)+
  geom_smooth(method = "lm", se = FALSE)

#consomation d'alcool
grid.arrange(str1,str2,nrow=2)

data$Fedu <- as.factor(data$Fedu)      
data$Fedu <- mapvalues(data$Fedu, 
                       from = 0:4, 
                       to = c("None", "Primary education (4th grade)", "Primary education (5th to 9th grade)", "Secondary education", "Higher education"))

data$Medu <- as.factor(data$Medu)      
data$Medu <- mapvalues(data$Medu, 
                       from = 0:4, 
                       to = c("None", "Primary education (4th grade)", "Primary education (5th to 9th grade)", "Secondary education", "Higher education"))

str1=ggplot(data, aes(x=mathgrades, y=portgrades)) +
  geom_point(aes(colour=factor(Fedu)))+ scale_colour_hue(l=25,c=150)+
  geom_smooth(method = "lm", se = FALSE)

str2=ggplot(data, aes(x=mathgrades, y=portgrades))+
  geom_point(aes(colour=factor(Medu)))+ scale_colour_hue(l=25,c=150)+
  geom_smooth(method = "lm", se = FALSE)


#Parents education
grid.arrange(str1,str2,nrow=2)



str1=ggplot(data, aes(x=mathgrades, y=portgrades)) +
  geom_point(aes(colour=factor(Fjob)))+ scale_colour_hue(l=25,c=150)+
  geom_smooth(method = "lm", se = FALSE)

str2=ggplot(data, aes(x=mathgrades, y=portgrades))+
  geom_point(aes(colour=factor(Mjob)))+ scale_colour_hue(l=25,c=150)+
  geom_smooth(method = "lm", se = FALSE)


#Parents' Job
grid.arrange(str1,str2,nrow=2)

#Family size
ggplot(data, aes(x=mathgrades, y=portgrades)) +
  geom_point(aes(colour=factor(famsize)))+ scale_colour_hue(l=25,c=150)+
  geom_smooth(method = "lm", se = FALSE)
