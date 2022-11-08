library(readr)
enf_cardio2_0 <- read_delim("enf_cardio2.0.csv", 
                            delim = "\t", escape_double = FALSE, 
                            col_names = FALSE, trim_ws = TRUE)
View(enf_cardio2_0)
#Los datos de la tabla vienen en tantos por 100.000

#pongo nombrea a las columnas del dataframe y elimino las q no son necesarias; en este caso la del sexo (X4) y las edades (X3)

colnames(enf_cardio2_0)<- c('Causa Defuncion', 'Comunidades autonomas',
                            'X3', 'X4', 'Año', 'Total')

