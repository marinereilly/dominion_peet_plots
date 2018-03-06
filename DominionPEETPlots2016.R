#Peet Plot Species Number by year
#Load Data
library("readr", lib.loc="~/R/win-library/3.3")
library("ggplot2", lib.loc="~/R/win-library/3.3")
PeetSpecies <- read_csv("H:/0_HarrisLab/1 PROJECT FOLDERS/Dominion/DATA/PEET PLOT/Peet_Species.csv",
                        col_types = cols(Year = col_character()))
PeetSpecies$Year<-as.factor(PeetSpecies$Year)
PeetSpecies$Plot<-as.factor(PeetSpecies$Plot)
PeetSpecies$SpeciesNum<-as.numeric(PeetSpecies$SpeciesNum)
PeetSpecies<-na.omit(PeetSpecies)
View(PeetSpecies)

#Make Chart
colo4<-c("2012"="#4daf4a", "2013"="#984ea3", "2014"="#ff7f00", "2015" ="#ffff33", "2016"="#a65628")

a<-ggplot(PeetSpecies, aes(x=Plot, y=SpeciesNum, fill=Year,))
species<-a+geom_bar(stat = "identity", position="dodge", colour="black")+
  scale_fill_manual(values = colo4)+
  ylab("Number of Species")+
  xlab("Peet Plot")+
  ggtitle("Number of Species by Peet Plot and Year")+
  theme(axis.title = element_text(face = "bold"))+
  theme(plot.title = element_text(face="bold"))
species


#FQI Scores
FQI <- read_csv("H:/0_HarrisLab/1 PROJECT FOLDERS/Dominion/DATA/PEET PLOT/PEETplots_FQI.csv",
col_types = cols(Date = col_character()))
View(FQI)
FQI$Date<-as.POSIXct.date(FQI$Date)
FQI$Date<-mdy(FQI$Date)
mean.fqi<-FQI %>%
  group_by(Plot) %>%
  summarise(mean_FQI=mean(FQI_Score), sd_FQI=sd(FQI_Score), mean_adj=mean(Adjusted_FQI), sd_adj=sd(Adjusted_FQI))
View(mean.fqi)

#FQI Plots - Better colors below...
f<-ggplot(FQI)+
  geom_line(aes(x=Date, y=FQI_Score, color=Plot), size=1)+
  scale_x_date(limits = as_date(c("2012-08-28", "2016-07-07")))+
  geom_point(aes(x=Date, y=FQI_Score, color=Plot), size=2)+
  geom_hline(yintercept=12.9, show.legend=FALSE, colour="#619CFF", linetype=4, size=1)+
  geom_hline(yintercept=8.25, show.legend=FALSE, colour="#F564E3", linetype=4, size=1)+
  geom_hline(yintercept=12.0, show.legend=FALSE, colour="#00BFC4", linetype=4, size=1)+
  ylab("FQI Score")+ggtitle("FQI Scores over Time")
f

f<-ggplot(FQI)+
  geom_line(aes(x=Date, y=Adjusted_FQI, color=Plot), size=1)+
  scale_x_date(limits = as_date(c("2012-08-28", "2016-07-07")))+
  geom_point(aes(x=Date, y=Adjusted_FQI, color=Plot), size=2)+
  geom_hline(yintercept=36.7, show.legend=FALSE, colour="#619CFF", linetype=4, size=1)+
  geom_hline(yintercept=25.0, show.legend=FALSE, colour="#F564E3", linetype=4, size=1)+
  geom_hline(yintercept=44.0, show.legend=FALSE, colour="#00BFC4", linetype=4, size=1)+
  ylab("FQI Score")+ggtitle("Adjusted FQI Scores over Time")
f
#Better colors
FQI.a<-subset(FQI, Plot=="Plot A" | Plot == "Plot B" | Plot == "Plot C")
colo3<-c("Plot A"="#69B5B3", "Plot B"="#E87770", "Plot C"="blue4")  

f<-ggplot(FQI.a)+
  geom_line(aes(x=Date, y=FQI_Score, color=Plot), size=1)+
  scale_x_date(limits = as_date(c("2012-08-28", "2016-07-07")))+
  scale_color_manual(values=colo3, breaks= c("Plot A", "Plot B", "Plot C"), labels=c("Plot A", "Plot B", "Plot C"))+ 
  geom_point(aes(x=Date, y=FQI_Score, color=Plot), size=2)+
  geom_hline(yintercept=12.9, show.legend=FALSE, colour="#E87770", linetype=4, size=1)+
  geom_hline(yintercept=8.25, show.legend=FALSE, colour="blue4", linetype=4, size=1)+
  geom_hline(yintercept=12.0, show.legend=FALSE, colour="#69B5B3", linetype=4, size=1)+
  ylab("FQI Score")+ggtitle("FQI Scores over Time")
f

f<-ggplot(FQI.a)+
  geom_line(aes(x=Date, y=Adjusted_FQI, color=Plot), size=1)+
  scale_x_date(limits = as_date(c("2012-08-28", "2016-07-07")))+
  scale_color_manual(values=colo3, breaks= c("Plot A", "Plot B", "Plot C"), labels=c("Plot A", "Plot B", "Plot C"))+ 
  geom_point(aes(x=Date, y=Adjusted_FQI, color=Plot), size=2)+
  geom_hline(yintercept=36.7, show.legend=FALSE, colour="#E87770", linetype=4, size=1)+
  geom_hline(yintercept=25.0, show.legend=FALSE, colour="blue4", linetype=4, size=1)+
  geom_hline(yintercept=44.0, show.legend=FALSE, colour="#69B5B3", linetype=4, size=1)+
  ylab("Adjusted FQI Score")+ggtitle("AdjustedFQI Scores over Time")
f
