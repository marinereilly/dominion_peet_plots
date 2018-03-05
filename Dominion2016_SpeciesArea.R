# Load Data
library(readr)
SpeciesArea <- read.csv("H:/0_HarrisLab/1 PROJECT FOLDERS/Dominion/DATA/PEET PLOT/SpeciesArea.csv")
View(SpeciesArea)

SpeciesArea$Year<-as.factor(SpeciesArea$Year)

#subset by plot and year
SA.a<-subset(SpeciesArea, Plot=="Plot A")
SA.b<-subset(SpeciesArea, Plot=="Plot B")
SA.c<-subset(SpeciesArea, Plot=="Plot C")

#Calculate Power Equations
#Don't use 2016 equation for Plot A, 2013 Plot B, Plot C because there are only 2 points
lm(formula = log(SpeciesNum) ~ log(Size), data= subset(SA.c, Year=="2012"))
lm(formula = log(SpeciesNum) ~ log(Size), data= subset(SA.c, Year=="2013"))
lm(formula = log(SpeciesNum) ~ log(Size), data= subset(SA.c, Year=="2014"))
lm(formula = log(SpeciesNum) ~ log(Size), data= subset(SA.c, Year=="2015"))
lm(formula = log(SpeciesNum) ~ log(Size), data= subset(SA.c, Year=="2016"))

#Create Power Equations
#exp(Intercept)*x^log(Size)

A.2012<- function(x) {exp(1.966) * x ^ 0.3226}
A.2013<- function(x) {exp(2.6728) * x ^ 0.2118}
A.2014<- function(x) {exp(2.5947) * x ^ 0.2732}
A.2015<- function(x) {exp(2.5296) * x ^ 0.1921}
#A.2016<- function(x) {exp(2.8011) * x ^ 0.1341}
A.2017<- function(x) {exp(2.0086) * x ^ 0.3483}
B.2012<- function(x) {exp(2.0491) * x ^ 0.1807}
#B.2013<- function(x) {exp(1.8717) * x ^ 0.1426}
B.2014<- function(x) {exp(1.9249) * x ^ 0.1746}
B.2015<- function(x) {exp(2.2993) * x ^ 0.1787}
B.2016<- function(x) {exp(1.6685) * x ^ 0.1467}
B.2017<- function(x) {exp(2.039) * x ^ 0.2144}
C.2012<- function(x) {exp(1.6687) * x ^ 0.2229}
#C.2013<- function(x) {exp(1.8486) * x ^ 0.1725}
C.2014<- function(x) {exp(1.550) * x ^ 0.232}
C.2015<- function(x) {exp(1.5576) * x ^ 0.3433}
C.2016<- function(x) {exp(1.9436) * x ^ 0.2515}
C.2017<- function(x) {exp(1.4886) * x ^ 0.2389}

#Color Brewer Set1 values
#E41A1C #377EB8 #4DAF4A #984EA3 #FF7F00 #FFFF33 #A65628 #F781BF #999999

#make pretty graph
library("ggplot2", lib.loc="~/R/win-library/3.3")
mycolors<-c("2010"="#E41A1C", "2011"="#377EB8", "2012"="#4DAF4A", "2013"="#984EA3", "2014"="#FF7F00", "2015"="#FFFF33", "2016"="#A65628")
a<-ggplot(SA.c, aes(x=Size, y=SpeciesNum, color=Year))+
  geom_point(size=2)+
  stat_function(fun = C.2012, color= "#4DAF4A")+
  stat_function(fun = C.2013, color= "#984EA3", linetype=2, alpha=0.5)+
  stat_function(fun = C.2014, color= "#FF7F00")+
  stat_function(fun = C.2015, color= "#FFFF33")+
  stat_function(fun = C.2016, color= "#A65628")+
  scale_color_manual(values=mycolors)+
  ylab("Number of Species")+
  xlab("Plot Area (m2)")+
  ggtitle("Peet Plot C Species Area Curves")
a

#Load Exponent Data
SpeciesAreaExp <- read.csv("H:/0_HarrisLab/1 PROJECT FOLDERS/Dominion/DATA/PEET PLOT/SpeciesAreaExp.csv")
View(SpeciesAreaExp)
#Plot Data
b<-ggplot(SpeciesAreaExp, aes(x=Plot, y=Exponent, fill=Year))+
  geom_bar(position="dodge",stat="identity", color="black")+
  scale_fill_manual(values=mycolors)+
  geom_text(aes(label= sprintf("%0.2f", round(Exponent, digits = 2))), position=position_dodge(.9), vjust=2.0, size=2.75)+
  xlab("Plot")+ylab("Exponent")+ggtitle("PEET Plot Species Area Curve Exponents Over Time")
b
