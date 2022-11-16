library(readr)
enf_cardio2_0 <- read_delim("INPUT.DATA/enf_cardio2.0.csv", 
                            delim = "\t", escape_double = FALSE, 
                            col_names = FALSE, trim_ws = TRUE)

#Los datos de la tabla vienen en tantos por 100.000

#pongo nombre a las columnas del dataframe 

colnames(enf_cardio2_0)<- c('Causa Defuncion', 'Comunidades',
                            'X3', 'X4', 'Año', 'Total')
#elimino las columnas que NO nos interesan (X3: Edades y X4: Sexo) y la primera fila
enf_cardio2_0$X3 <- NULL
enf_cardio2_0$X4 <- NULL

#enf_cardio2_0$Causa Defunción <-NULL   ya que sabemos q esta tabla solo contiene los datos en cuanto a enfermedades cardiovasculares

enf_cardio2_0 <- enf_cardio2_0[-1,]

#Como todas las columnas me aparecen tipo caracter; modifico el total como valor numérico 
enf_cardio2_0 <- transform(enf_cardio2_0, Total=as.numeric(Total))

View(enf_cardio2_0)
str(enf_cardio2_0)

#gráfico de barras
library(tidyverse)
ggplot(data = enf_cardio2_0, aes(x = Comunidades, y = Total))

