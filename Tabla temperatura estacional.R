#https://github.com/rOpenSpain/climaemet

library(remotes)
#install_github("ropenspain/climaemet")
library(climaemet)
#aemet_api_key("eyJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJubXExMDAxQGFsdS51YnUuZXMiLCJqdGkiOiJlOWRmMTc0OC05NjcxLTRiMjctODM2OS01NmYxNTZjNjNjYzIiLCJpc3MiOiJBRU1FVCIsImlhdCI6MTY2NjE2NzExNCwidXNlcklkIjoiZTlkZjE3NDgtOTY3MS00YjI3LTgzNjktNTZmMTU2YzYzY2MyIiwicm9sZSI6IiJ9.mIlOZxnREIqfi6hTeh37A_yMnryWyI0NPJzilMMAMmU", install = TRUE)

stations <- aemet_stations() # Need to have the API Key registered

knitr::kable(head(stations))


## Get last observation values 
data_observation <- aemet_last_obs("all")
data_observation


summary(data_observation)


## Get daily/annual climatology values, tabla completa
data_daily <- aemet_daily_period_all(start = 2018, end = 2020) 
data_daily
View(data_daily) #da error en html


# Plot a climate stripes graph for a period of years for a station
library(ggplot2) 

climatestripes_station("2331", start = 2015, end = 2021) + theme(plot.title = element_text(size = 10))
climatestripes_station("1505", start = 2015, end = 2021) + theme(plot.title = element_text(size = 10))
climatestripes_station("1014", start = 2015, end = 2021) + theme(plot.title = element_text(size = 10))


# Plot a windrose showing the wind speed and direction for a station over a days period.
windrose_days(
  "2331",
  start = "2010-01-01",
  end = "2020-12-31",
  n_speeds = 5,
  speed_cuts = c(2.5, 5, 7.5, 10, 12.5, 15)
) +
  theme(plot.title = element_text(size = 10))


#mapa españa con temperaturas maximas la ultima hora del dia de hoy
library(ggplot2)
library(dplyr)

library(climaemet)
aemet_last_obs("9434")

all_stations <- aemet_last_obs(return_sf = TRUE)
# Last hour
all_last <-
  all_stations %>% filter(fint == all_stations[["fint"]][1])

last_hour <- max(all_last$fint)


ggplot(all_last) +
  geom_sf(aes(colour = ta),
          shape = 19,
          size = 2,
          alpha = 0.5
  ) +
  labs(
    title = "Temperatura en España",
    subtitle = last_hour,
    color = "Max temp.\n(celsius)",
    caption = "Source: AEMET"
  ) +
  scale_colour_gradientn(
    colours = hcl.colors(10, "RdBu", rev = TRUE, alpha = 0.5),
    guide = "legend"
  ) +
  guides(colour = guide_legend(n.breaks = 10)) +
  theme_bw() +
  theme(
    panel.border = element_blank(),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(face = "italic")
  )



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

View(tablaComunidades)


#install.packages('lubridate',dependencies=TRUE)
library(lubridate)


#filtrar tabla comunidadesAutonomas solo para 2018
fecha2018 <- select(filter(tablaComunidades, year(tablaComunidades$fecha)==2018 ), ComunidadesAutónomas, fecha,tmax,tmin)
#View(fecha2018)

#Crear la media de tmax y tmin por comunidad autonoma en 2018
tmax2018<-aggregate(tmax~ComunidadesAutónomas, data=fecha2018, mean)
tmin2018<-aggregate(tmin~ComunidadesAutónomas, data=fecha2018, mean)

#se juntan ambas tablas para tener las comunidades, tmax y tmin
tmaxmin2018<-merge(x = tmax2018, y = tmin2018)

#se crea la columna fecha poniendo 2018
tabla2018 <- cbind(tmaxmin2018,fecha=c(2018))
View(tabla2018)









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
View(tabla2019)







#filtrar tabla comunidadesAutonomas solo para 2020
fecha2020 <- select(filter(tablaComunidades, year(tablaComunidades$fecha)==2020 ), ComunidadesAutónomas, fecha,tmax,tmin)
#View(fecha2020)

#Crear la media de tmax y tmin por comunidad autonoma en 2020
tmax2020<-aggregate(tmax~ComunidadesAutónomas, data=fecha2020, mean)
tmin2020<-aggregate(tmin~ComunidadesAutónomas, data=fecha2020, mean)

#se juntan ambas tablas para tener las comunidades, tmax y tmin
tmaxmin2020<-merge(x = tmax2020, y = tmin2020)

#se crea la columna fecha poniendo 2020
tabla2020 <- cbind(tmaxmin2020,fecha=c(2020))
View(tabla2020)





#se juntan las 3 tablas de los 3 años distintos
tabla1819 = rbind(tabla2018, tabla2019)
tabladefinitiva = rbind(tabla1819, tabla2020)
View (tabladefinitiva)






#se agrupan los valores por comunidades autonomas (calculando el valor medio) PRUEBA
# tablasAgrupamientos <- tablaComunidades %>%
#   group_by(ComunidadesAutónomas) %>%
#   summarise(across(c(tmax,tmin, tmed,altitud,fecha), ~ mean(.x, na.rm = TRUE)))
# 
# View(tablasAgrupamientos)

#grafica temperatura maxima por comunidad autonoma
ggplot(data = tabladefinitiva, aes(x =ComunidadesAutónomas , y = tmax))+
  geom_point(aes(colour = fecha))+
  labs(x = "tmax", y = "ComunidadesAutónomas",title = 'Temperatura máxima por comunidad autonoma')+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))

#grafica temperatura mínima por comunidad autonoma
ggplot(data = tabladefinitiva, aes(x =ComunidadesAutónomas , y = tmin))+
  geom_point(aes(colour = fecha))+
  labs(x = "tmax", y = "ComunidadesAutónomas",title = 'Temperatura mínima por comunidad autonoma')+
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))


#grafica de barras temperatura máxima por comunidad autonoma
ggplot(tabladefinitiva, aes(x = ComunidadesAutónomas, y = tmax )) +
  geom_bar(stat = 'identity', aes(fill = factor(fecha)), colour ='black',position = 'dodge') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))


#grafica de barras temperatura mínima por comunidad autonoma
ggplot(tabladefinitiva, aes(x = ComunidadesAutónomas, y = tmin )) +
  geom_bar(stat = 'identity', aes(fill = factor(fecha)), colour ='black',position = 'dodge') +
  theme(axis.text.x = element_text(angle = 60, vjust = 1, hjust = 1))












