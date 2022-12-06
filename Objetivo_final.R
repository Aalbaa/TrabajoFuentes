
#tabla datos enfermedad
library(readr)
enf_cardio2_0 <- read_delim("INPUT.DATA/enf_cardio2.0.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
str(enf_cardio2_0)

library(dplyr)
datos <- select(enf_cardio2_0, 'Causa de defunción', 'Comunidades y Ciudades Autónomas', 'Periodo', 'Total')

colnames(datos) <- c ('Causa_Defuncion' , 'Comunidades' , 'Periodo' , 'Total')
str(datos)

#tabla tiempo