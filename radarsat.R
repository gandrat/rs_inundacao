#Tidesate

library(ggplot2)
library(curl)
library(dplyr)
library(lubridate)


df<-read.csv(curl('http://app.tidesatglobal.com/rig1/RIG1_filtered_data.txt'),sep='\t',dec=',')

df<-df%>%transmute(time=as_datetime(DATA..AAA.MM.DD....HORA.POA...HH.MM.),
                   nivel=Nivel.Guaiba...CCMAR...metros.)
nivel<-df
save(nivel,file='output_data/nivel.Rda')

ggplot(df,aes(x=time,y=nivel))+geom_path()
