#I'm looking at the species area curves so I am going to just Import the first sheet
library(readxl)
Peet <- read_excel("H:/0_HarrisLab/1_CURRENT PROJECT FOLDERS/Dominion/DATA/2017 Data/2017 PEET Plots.xlsx",
sheet = "Area Level", col_types = c("date",
"text", "text", "text", "text", "text"))
View(Peet)

#Calc mean species per area and sd species per area
library("dplyr", lib.loc="~/R/win-library/3.3")
Peet_stats<- Peet %>%
group_by(Peet_plot, `Level(m2)`, Subplot) %>%
summarize(SpNum=length(Species))
View(Peet_stats)
#Haven't figured out how to make level 4 have both 4 and 0.25 in it yet so this doesn't work
#To figure out the # per 40m plot
Peet %>%
group_by(Peet_plot) %>%
summarize(length(Species))
#I calculated the 4's by hand cuz I couldn't figure out an easy way to code it
TotSp<-c(3,5,6,6,11,14,13,12,58,8,9,4,3,9,12,11,6,40,4,3,4,3,6,4,5,5,25,NA)
Peet_stats$TotSp<-TotSp
#To delete the NA row:
Peet_stats<-Peet_stats[-c(28),]
#View the data and check to make sure it is right!
View(Peet_stats)


P_sum<- Peet_stats %>%
group_by(Peet_plot, `Level(m2)`) %>%
summarize(mean=mean(TotSp), sd=sd(TotSp))
View(P_sum)

#Plot Species Area Curves

#for all Plots
library("ggplot2", lib.loc="~/R/win-library/3.3")
P_sum$`Level(m2)`<-as.numeric(P_sum$`Level(m2)`)

a<-ggplot(P_sum)+geom_point(aes(x=`Level(m2)`, y=mean, color=Peet_plot))
a

