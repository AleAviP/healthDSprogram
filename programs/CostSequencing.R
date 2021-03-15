###### Set up libraries######
#install.packages(ggplot2)
library(ggplot2)

###### Get DNA sequencing cost data url from Github######
urlfile<-'https://raw.githubusercontent.com/AleAviP/healthDSprogram/main/data/Sequencing_Cost_Aug2020.csv'

###### Read data and change format######
data.seq.cost<-read.csv(urlfile)
data.seq.cost$Date<-as.Date(data.seq.cost$Date, origin = "1899-12-30")

###### Plots costs######
ggplot(data.seq.cost,aes(x=Date,y=Mb))+geom_point(color="#1c9099",size=2)+geom_line(color="#1c9099")+
  geom_smooth(formula="y~ x",method = 'loess',span=.5,se=F)+
  theme_minimal()+
  scale_y_continuous(labels=scales::dollar_format())


ggplot(data.seq.cost,aes(x=Date,y=Genome))+geom_point(color="#1c9099",size=2)+geom_line(color="#1c9099")+
  geom_smooth(formula="y~ x",method = 'loess',span=.5,se=F)+
  theme_minimal()+
  scale_y_continuous(labels=scales::dollar_format())
