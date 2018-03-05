#####Packages#####
library(tidyverse)
library(cowplot)

#####Load Data#####
peet2017 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/DATA/2017 Data/2017_PEET_Plots_Species_Area.csv")
peet2012 <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/DATA/PEET PLOT/SpeciesArea.csv")
##***NOTE: Peet plot areas from before 2017 were 0.25 and 0.5.  
#****The 0.5 comes from adding 2 of the 0.25 plots within the same 2m plot together
#****2017 and beyond, 0.25m2 plots and 4m2 plots were surveyed in addition to the 40m2
#****Plots are nested as the PEET plot method requires

#####Formatting#####
peet2017$Plot<-if_else(peet2017$Plot=="A", "Plot A", if_else(peet2017$Plot=="B", "Plot B", "Plot C"))

peet_av<-peet2017 %>% 
  group_by(Plot, Size) %>% 
  summarise(SpeciesNum=mean(Species.Number))
peet_av$Year<-2017

peet<- peet2012 %>% 
  bind_rows(., peet_av)
peet$Plot<-as.factor(peet$Plot)
peet$Year<-as.factor(peet$Year)

#####Calculating the Species Area curves#####
models <- peet_av %>%
  split(.$Plot) %>% 
  map(~lm(log(SpeciesNum) ~ log(Size), data = .))
models %>% 
  map(summary) %>% 
  map_dbl(.$coefficients)
  


#####Plotting#####
peet_palA <- c("2010"="#fee6ce", "2011"="#fdd0a2", "2012"="#fdae6b", "2013"="#fd8d3c", 
           "2014"="#f16913", "2015"="#d94801", "2016"="#a63603", "2017"="#7f2704")
peet_palB <- c("2010"="#deebf7", "2011"="#c6dbef", "2012"="#9ecae1", "2013"="#6baed6", 
           "2014"="#4292c6", "2015"="#2171b5", "2016"="#08519c", "2017"="#08306b")
peet_palC <- c("2010"="#e5f5e0", "2011"="#c7e9c0", "2012"="#a1d99b", "2013"="#74c476", 
               "2014"="#41ab5d", "2015"="#238b45", "2016"="#006d2c", "2017"="#00441b")

a<-peet %>% 
  filter(Plot=="Plot A") %>% 
  ggplot(., aes(x=Size, y=SpeciesNum, fill = Year))+
  stat_function(fun = A.2012, color= "#fdae6b", size=.9, linetype="longdash")+
  stat_function(fun = A.2013, color= "#fd8d3c", size=.9, linetype="longdash")+
  stat_function(fun = A.2014, color= "#f16913", size=.9, linetype="longdash")+
  stat_function(fun = A.2015, color= "#d94801", size=.9, linetype="longdash")+
  stat_function(fun = A.2017, color= "#7f2704", size=1)+
  scale_y_continuous(limits = c(0,40))+
  geom_point(size=2, shape=21)+scale_fill_manual(values = peet_palA)+
  ylab("Number of Species")+xlab("Plot Size (m2)")
a  

b<-peet %>% 
  filter(Plot=="Plot B") %>% 
  ggplot(., aes(x=Size, y=SpeciesNum, fill = Year))+
  stat_function(fun = B.2012, color= "#9ecae1", size=.9, linetype="longdash")+
  stat_function(fun = B.2014, color= "#4292c6", size=.9, linetype="longdash")+
  stat_function(fun = B.2015, color= "#2171b5", size=.9, linetype="longdash")+
  stat_function(fun = B.2016, color= "#08519c", size=.9, linetype="longdash")+
  stat_function(fun = B.2017, color= "#08306b", size=1)+
  scale_y_continuous(limits = c(0,40))+
  geom_point(size=2, shape=21)+scale_fill_manual(values = peet_palB)+
  ylab("Number of Species")+xlab("Plot Size (m2)")
b    

c<-peet %>% 
  filter(Plot=="Plot C") %>% 
  ggplot(., aes(x=Size, y=SpeciesNum, fill = Year))+
  stat_function(fun = C.2012, color= "#a1d99b", size=.9, linetype="longdash")+
  stat_function(fun = C.2014, color= "#41ab5d", size=.9, linetype="longdash")+
  stat_function(fun = C.2015, color= "#238b45", size=.9, linetype="longdash")+
  stat_function(fun = C.2016, color= "#006d2c", size=.9, linetype="longdash")+
  stat_function(fun = C.2017, color= "#00441b", size=1)+
  scale_y_continuous(limits = c(0,40))+
  geom_point(size=2, shape=21)+scale_fill_manual(values = peet_palC)+
  ylab("Number of Species")+xlab("Plot Size (m2)")
c  

d<-plot_grid(a, b, c, labels = c("A", "B", "C"), align = "h", ncol=3)
d

#####Species Exponent Plots
SpeciesAreaExp <- read.csv("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/DATA/PEET PLOT/SpeciesAreaExp.csv")
SpeciesAreaExp$Year<-as.factor(SpeciesAreaExp$Year)

e<-SpeciesAreaExp %>%
  filter(Plot=="A") %>% 
  ggplot(., aes(x=Year, y=Exponent, fill=Year))+
  geom_col(width = 1, color= "black")+xlab(NULL)+
  scale_y_continuous(limits = c(0,0.4))+
  scale_fill_manual(values=peet_palA)+guides(fill=FALSE)
e

f<-SpeciesAreaExp %>%
  filter(Plot=="B") %>% 
  ggplot(., aes(x=Year, y=Exponent, fill=Year))+
  geom_col(width = 1, color= "black")+xlab(NULL)+
  scale_y_continuous(limits = c(0,0.4))+
  scale_fill_manual(values=peet_palB)+guides(fill=FALSE)
f

g<-SpeciesAreaExp %>%
  filter(Plot=="C") %>% 
  ggplot(., aes(x=Year, y=Exponent, fill=Year))+
  geom_col(color= "black", width = 1)+xlab(NULL)+
  scale_y_continuous(limits = c(0,0.4))+
  scale_fill_manual(values=peet_palC)+guides(fill=FALSE)
g

h<-plot_grid(e, f, g, labels = c("A", "B", "C"), align = "h", ncol=3)
h

#####Species Number Plots
i<-peet %>% 
  filter(Size==40) %>% 
  filter(Plot=="Plot A") %>% 
  ggplot(., aes(x=Year, y=SpeciesNum, fill=Year))+
  geom_col(color= "black", width = 1)+xlab(NULL)+
  scale_fill_manual(values=peet_palA)+guides(fill=FALSE)+
  ylab("Total Number of Species")+
  scale_y_continuous(limits = c(0,40))
i  

j<-peet %>% 
  filter(Size==40) %>% 
  filter(Plot=="Plot B") %>% 
  ggplot(., aes(x=Year, y=SpeciesNum, fill=Year))+
  geom_col(color= "black", width = 1)+xlab(NULL)+
  scale_fill_manual(values=peet_palB)+guides(fill=FALSE)+
  ylab("Total Number of Species")+
  scale_y_continuous(limits = c(0,40))
j 

k<-peet %>% 
  filter(Size==40) %>% 
  filter(Plot=="Plot C") %>% 
  ggplot(., aes(x=Year, y=SpeciesNum, fill=Year))+
  geom_col(color= "black", width = 1)+xlab(NULL)+
  scale_fill_manual(values=peet_palC)+guides(fill=FALSE)+
  ylab("Total Number of Species")+
  scale_y_continuous(limits = c(0,40))
k 

l<-plot_grid(i, j, k, labels = c("A", "B", "C"), align = "h", ncol=3)
l
