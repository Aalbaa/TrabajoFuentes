
#tabla datos enfermedad
library(readr)
enf_cardio2_0 <- read_delim("INPUT.DATA/enf_cardio2.0.csv", 
                            delim = ";", escape_double = FALSE, trim_ws = TRUE)
str(enf_cardio2_0)

library(dplyr)
datos <- select(enf_cardio2_0, 'Causa de defunción', 'Comunidades y Ciudades Autónomas', 'Periodo', 'Total')

colnames(datos) <- c ('Causa_Defuncion' , 'ComunidadesAutónomas' , 'fecha' , 'Total')
str(datos)
view(datos)

datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '01 Andalucía'] <- 'ANDALUCIA'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '02 Aragón'] <- 'ARAGON'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '03 Asturias, Principado de'] <- 'ASTURIAS'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '04 Balears, Illes'] <- 'ISLAS BALEARES'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '05 Canarias'] <- 'ISLAS CANARIAS'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '06 Cantabria'] <- 'CANTABRIA'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '07 Castilla y León'] <- 'CyL'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '08 Castilla - La Mancha'] <- 'CLM'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '09 Cataluña'] <- 'CATALUÑA'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '10 Comunitat Valenciana'] <- 'VALENCIA'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '11 Extremadura'] <- 'EXTREMADURA'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '12 Galicia'] <- 'GALICIA'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '13 Madrid, Comunidad de'] <- 'MADRID'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '14 Murcia, Región de'] <- 'MURCIA'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '15 Navarra, Comunidad Foral de'] <- 'NAVARRA'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '16 País Vasco'] <- 'PAIS VASCO'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '17 Rioja, La'] <- 'LA RIOJA'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '18 Ceuta'] <- 'CEUTA'
datos$ComunidadesAutónomas[datos$ComunidadesAutónomas== '19 Melilla'] <- 'MELILLA'



#tabla tiempo
library(remotes)
install_github("ropenspain/climaemet")
library(climaemet)
aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJubXExMDAxQGFsdS51YnUuZXMiLCJqdGkiOiJlOWRmMTc0OC05NjcxLTRiMjctODM2OS01NmYxNTZjNjNjYzIiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTY2NjE2NzExNCwidXNlcklkIjoiZTlkZjE3NDgtOTY3MS00YjI3LTgzNjktNTZmMTU2YzYzY2MyIiwicm9sZSI6IiJ9.mIlOZxnREIqfi6hTeh37A_yMnryWyI0NPJzilMMAMmU", install = TRUE)

stations <- aemet_stations() # Need to have the API Key registered

knitr::kable(head(stations))


## Get last observation values 
data_observation <- aemet_last_obs("all")
data_observation


summary(data_observation)


## Get daily/annual climatology values, tabla completa
data_daily <- aemet_daily_period_all(start = 2018, end = 2020) 
data_daily
#se crea una nueva columna denomicada ccaa a partir de la columna provincia
tablaComunidades <- data_daily %>% 
  mutate(ComunidadesAutónomas = 
           case_when(provincia %in% c("BARCELONA", "TARRAGONA", "GIRONA","LLEIDA") ~ 'CATALUÑA', 
                     provincia %in% c("BURGOS", "SORIA", "SALAMANCA", "LEON","AVILA","PALENCIA","SEGOVIA","VALLADOLID","ZAMORA") ~ 'CyL',
                     provincia %in% c("ASTURIAS") ~ 'ASTURIAS',
                     provincia %in% c("A CORUÑA","LUGO","OURENSE","PONTEVEDRA") ~ 'GALICIA',
                     provincia %in% c("BIZKAIA","GIPUZKOA","ARABA/ALAVA") ~ 'PAIS VASCO',
                     provincia %in% c("CANTABRIA") ~ 'CANTABRIA',
                     provincia %in% c("NAVARRA") ~ 'NAVARRA',
                     provincia %in% c("MADRID") ~ 'MADRID',
                     provincia %in% c("CACERES","BADAJOZ") ~ 'EXTREMADURA',
                     provincia %in% c("CUENCA","GUADALAJARA","TOLEDO","CIUDAD REAL","ALBACETE") ~ 'CLM',
                     provincia %in% c("CORDOBA","HUELVA","CADIZ","GRANADA","JAEN","SEVILLA","ALMERIA","MALAGA") ~ 'ANDALUCIA',
                     provincia %in% c("MELILLA") ~ 'MELILLA',
                     provincia %in% c("ALICANTE","CASTELLON","VALENCIA") ~ 'VALENCIA',
                     provincia %in% c("MURCIA") ~ 'MURCIA',
                     provincia %in% c("TERUEL","HUESCA","ZARAGOZA") ~ 'ARAGON',
                     provincia %in% c("LA RIOJA") ~ 'LA RIOJA',
                     provincia %in% c("ILLES BALEARS") ~ 'ISLAS BALEARES',
                     provincia %in% c("LAS PALMAS","STA. CRUZ DE TENERIFE") ~ 'ISLAS CANARIAS',
                     provincia %in% c("CEUTA") ~ 'CEUTA',
                     
                     
                     
           ))

#View(tablaComunidades)


#install.packages('lubridate',dependencies=TRUE)
library(lubridate)


#filtrar tabla comunidadesAutonomas solo para 2018
fecha2018 <- select(filter(tablaComunidades, year(tablaComunidades$fecha)==2018 ), ComunidadesAutónomas, fecha,tmax,tmin)


#Crear la media de tmax y tmin por comunidad autonoma en 2018
tmax2018<-aggregate(tmax~ComunidadesAutónomas, data=fecha2018, mean)
tmin2018<-aggregate(tmin~ComunidadesAutónomas, data=fecha2018, mean)

#se juntan ambas tablas para tener las comunidades, tmax y tmin
tmaxmin2018<-merge(x = tmax2018, y = tmin2018)

#se crea la columna fecha poniendo 2018
tabla2018 <- cbind(tmaxmin2018,fecha=c(2018))










#filtrar tabla comunidadesAutonomas solo para 2019
fecha2019 <- select(filter(tablaComunidades, year(tablaComunidades$fecha)==2019 ), ComunidadesAutónomas, fecha,tmax,tmin)
#View(fecha2019)

#Crear la media de tmax y tmin por comunidad autonoma en 2019
tmax2019<-aggregate(tmax~ComunidadesAutónomas, data=fecha2019, mean)
tmin2019<-aggregate(tmin~ComunidadesAutónomas, data=fecha2019, mean)

#se juntan ambas tablas para tener las comunidades, tmax y tmin
tmaxmin2019<-merge(x = tmax2019, y = tmin2019)

#se crea la columna fecha poniendo 2019
tabla2019 <- cbind(tmaxmin2019,fecha=c(2019))








#filtrar tabla comunidadesAutonomas solo para 2020
fecha2020 <- select(filter(tablaComunidades, year(tablaComunidades$fecha)==2020 ), ComunidadesAutónomas, fecha,tmax,tmin)


#Crear la media de tmax y tmin por comunidad autonoma en 2020
tmax2020<-aggregate(tmax~ComunidadesAutónomas, data=fecha2020, mean)
tmin2020<-aggregate(tmin~ComunidadesAutónomas, data=fecha2020, mean)

#se juntan ambas tablas para tener las comunidades, tmax y tmin
tmaxmin2020<-merge(x = tmax2020, y = tmin2020)

#se crea la columna fecha poniendo 2020
tabla2020 <- cbind(tmaxmin2020,fecha=c(2020))






#se juntan las 3 tablas de los 3 años distintos
tabla1819 = rbind(tabla2018, tabla2019)
tabladefinitiva = rbind(tabla1819, tabla2020)
#View (tabladefinitiva)




#unión tablas
union <- merge(x = datos, y = tabladefinitiva, by = c('ComunidadesAutónomas', 'fecha') )
View (union)
union