library(readr)
enf_cardio2_0 <- read_delim("INPUT.DATA/enf_cardio2.0.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)

#Los datos de la tabla vienen en tantos por 100.000

str(enf_cardio2_0)

library(dplyr)
datos <- select(enf_cardio2_0, 'Causa de defunción', 'Comunidades y Ciudades Autónomas', 'Periodo', 'Total')

colnames(datos) <- c ('Causa_Defuncion' , 'Comunidades' , 'fecha' , 'Total')
str(datos)


library(tidyverse)

#grafica 2020
ggplot(datos, aes(x = Comunidades, y = Total, fill = fecha[1])) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

#gráfica 2019
ggplot(datos, aes(x = Comunidades, y = Total, fill = fecha[2])) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

#gráfica 2018
ggplot(datos, aes(x = Comunidades, y = Total, fill = fecha[3])) +
  geom_bar(stat = 'identity') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

#grafica dispersión todos los datos
final <-  ggplot(datos, aes(x = Comunidades , y = Total)) +
  geom_point(aes (colour = factor(fecha))) +
  geom_smooth() +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1)) 

final

ggplot(datos, aes(x = Comunidades, y = Total, fill = fecha)) +
  geom_bar(stat = 'identity', position='dodge') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

#gráfica para el documento html 
  
gr_barras <- ggplot(datos, aes( x= Comunidades, y = Total))+
  geom_bar(aes(fill=factor(fecha)), stat = 'identity', position = 'dodge')+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

gr_barras


